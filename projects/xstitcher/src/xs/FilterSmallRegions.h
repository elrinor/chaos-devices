#ifndef __XSX_FILTER_SMALL_REGIONS_H__
#define __XSX_FILTER_SMALL_REGIONS_H__

#include "config.h"
#include <cassert>
#include <algorithm> /* for std::swap() */
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp> 
#include <boost/spirit/include/phoenix_statement.hpp> 
#include <boost/spirit/include/phoenix_bind.hpp> 
#include <vigra/pixelneighborhood.hxx>
#include <vigra/flatmorphology.hxx>
#include <vigra/combineimages.hxx>
#include <vigra/inspectimage.hxx>
#include <vigra/labelimage.hxx>
#include <arx/ext/Vigra.h>
#include "NeighborhoodBasedTransform.h"
#include "NeighborhoodWalker.h"

namespace xs {
// -------------------------------------------------------------------------- //
// FilterSizeOneRegions
// -------------------------------------------------------------------------- //
  template<class Neighborhood, class PixelType, class Alloc>
  class FilterSizeOneRegions {
    typedef vigra::BasicImage<PixelType, Alloc> Image;
    typedef vigra::BasicImage<bool> Mask;

    class NeedsReplacementVisitor {
    public:
      typedef bool result_type;
      
      template<class SrcImageIterator, class SrcAccessor>
      bool begin(const SrcImageIterator& pos, const SrcAccessor& src) {
        mResult = true;
        mCenterColor = src(pos);
        return true;
      }

      template<class SrcImageIterator, class SrcAccessor>
      bool operator() (const SrcImageIterator& pos, const SrcAccessor& src) {
        if(src(pos) == mCenterColor) {
          mResult = false;
          return false;
        } else {
          return true;
        }
      }

      bool end() {
        return mResult;
      }

    private:
      PixelType mCenterColor;
      bool mResult;
    };

    class BooleanErosionVisitor {
    public:
      typedef bool result_type;

      template<class SrcImageIterator, class SrcAccessor>
      bool begin(const SrcImageIterator&, const SrcAccessor&) {
        mResult = true;
        return true;
      }

      template<class SrcImageIterator, class SrcAccessor>
      bool operator() (const SrcImageIterator& pos, const SrcAccessor& src) {
        if(!src(pos)) {
          mResult = false;
          return false;
        } else {
          return true;
        }
      }

      bool end() {
        return mResult;
      }

    private:
      bool mResult;
    };

    class MaskedReplaceVisitor {
      typedef typename vigra::NormTraits<PixelType>::SquaredNormType SquaredNormType;
      typedef typename vigra::BasicImage<PixelType, Alloc>::const_traverser Traverser;

    public:
      typedef PixelType result_type;

      MaskedReplaceVisitor(Mask& mask, Traverser srcUpperLeft): mMask(mask), mUpperLeft(srcUpperLeft) {}

      template<class SrcImageIterator, class SrcAccessor>
      bool begin(const SrcImageIterator& pos, const SrcAccessor& src) {
        mNewColor = src(pos);
        if(mMask[pos - mUpperLeft]) {
          mCenterColor = mNewColor;
          mMinSquaredDist = std::numeric_limits<SquaredNormType>::max();
          return true;
        } else {
          return false;
        }
      }

      template<class SrcImageIterator, class SrcAccessor>
      bool operator() (const SrcImageIterator& pos, const SrcAccessor& src) {
        if(mMask[pos - mUpperLeft])
          return true; /* Skip masked neighbors. */

        PixelType color = src(pos);
        SquaredNormType squaredDist = vigra::squaredNorm(color - mCenterColor);
        if(squaredDist < mMinSquaredDist) {
          mMinSquaredDist = squaredDist;
          mNewColor = color;
        }
        return true;
      }

      PixelType end() {
        return mNewColor;
      }

    private:
      SquaredNormType mMinSquaredDist;
      PixelType mCenterColor, mNewColor;
      Mask& mMask;
      Traverser mUpperLeft;
    };


  public:
    FilterSizeOneRegions(const Image& srcImage, Image& dstImage): mSrcImage(srcImage), mDstImage(dstImage) {
      assert(srcImage.size() == dstImage.size());
    }

    void operator() () {
      /* Create mask of pixels to replace. */
      Mask needsReplacement(mSrcImage.size());
      neighborhoodBasedTransform(srcImageRange(mSrcImage), destImage(needsReplacement), NeighborhoodWalker<Neighborhood, NeedsReplacementVisitor>());

      mDstImage = mSrcImage;
      processLayer(needsReplacement);
    }

  private:
    void processLayer(Mask& needsReplacement) {
      using namespace boost::phoenix::arg_names;
      using namespace boost::phoenix;

      /* Erode mask. */
      Mask erodedNeedsReplacement(needsReplacement.size());
      neighborhoodBasedTransform(srcImageRange(needsReplacement), destImage(erodedNeedsReplacement), NeighborhoodWalker<Neighborhood, BooleanErosionVisitor>());

      /* Count marked area. */
      int erodedNeedsReplacementCount = 0;
      inspectImage(srcImageRange(erodedNeedsReplacement), if_(arg1)[ref(erodedNeedsReplacementCount)++]);

      /* Recurse if needed. */
      if(erodedNeedsReplacementCount > 0) {
        /* Subtract eroded mask. This will leave only border in needsReplacement. */
        vigra::combineTwoImages(srcImageRange(needsReplacement), srcImage(erodedNeedsReplacement), destImage(needsReplacement), arg1 && !arg2 );

        /* Recurse. */
        processLayer(erodedNeedsReplacement);
      }

      /* Replace. */
      neighborhoodBasedTransform(srcImageRange(mDstImage), destImage(mDstImage), NeighborhoodWalker<Neighborhood, MaskedReplaceVisitor>(MaskedReplaceVisitor(needsReplacement, mDstImage.upperLeft())));
    }

  private:
    const Image& mSrcImage;
    Image& mDstImage;
  };

  /**
   * Filters out connected regions of size one by replacing them with the closest color of the neighboring pixels.
   */
  template<class Neighborhood, class PixelType, class Alloc>
  void filterSizeOneRegions(const vigra::BasicImage<PixelType, Alloc>& srcImage, vigra::BasicImage<PixelType, Alloc>& dstImage) {
    FilterSizeOneRegions<Neighborhood, PixelType, Alloc>(srcImage, dstImage)();
  }

  template<class Neighborhood, class PixelType, class Alloc>
  void filterSizeOneRegions(vigra::BasicImage<PixelType, Alloc>& image) {
    vigra::BasicImage<PixelType> tmpImage(image.size());
    filterSizeOneRegions<Neighborhood>(image, tmpImage);
    image = tmpImage;
  }


// -------------------------------------------------------------------------- //
// FilterSmallRegions
// -------------------------------------------------------------------------- //
  template<class Neighborhood, class PixelType, class Alloc>
  class FilterSmallRegions {
    typedef vigra::BasicImage<PixelType, Alloc> Image;
    typedef vigra::BasicImage<int> ComponentImage;
    typedef vigra::BasicImage<bool> Mask;
    typedef typename vigra::NormTraits<PixelType>::SquaredNormType SquaredNormType;

    class Region {
    public:
      Region(): mSize(0), mClosestDist(std::numeric_limits<SquaredNormType>::max()), mClosestRegionId(-1), mRegionId(-1) {}

      void update(const PixelType& color, int regionId) {
        SquaredNormType dist = vigra::squaredNorm(color - mColor);
        if(dist < mClosestDist) {
          mClosestDist = dist;
          mClosestColor = color;
          mClosestRegionId = regionId;
        }
      }

      bool isUpdated() const {
        return mClosestRegionId != -1;
      }

      const PixelType& closestColor() const {
        return mClosestColor;
      }

      const PixelType& color() const {
        return mColor;
      }

      void setColor(const PixelType& color) {
        mColor = color;
      }

      int closestRegionId() const {
        return mClosestRegionId;
      }

      int size() const {
        return mSize;
      }

      void addSize() {
        mSize++;
      }

      int regionId() const {
        return mRegionId;
      }

      void setRegionId(int regionId) {
        mRegionId = regionId;
      }

      friend bool operator< (const Region& l, const Region& r) {
        return l.mClosestDist < r.mClosestDist;
      }

    private:
      SquaredNormType mClosestDist;
      PixelType mColor, mClosestColor;
      int mClosestRegionId, mRegionId;
      int mSize;
    };

    class RegionVisitor {
      typedef typename vigra::BasicImage<PixelType, Alloc>::const_traverser Traverser;
    public:
      typedef PixelType result_type;

      RegionVisitor(Traverser srcUpperLeft, FilterSmallRegions& owner): mUpperLeft(srcUpperLeft), mOwner(owner) {}

      template<class SrcImageIterator, class SrcAccessor>
      bool begin(const SrcImageIterator& i, const SrcAccessor& /*src*/) {
        vigra::Diff2D pos = i - mUpperLeft;
        mRegionId = mOwner.mRegionIds[pos];
        return mOwner.mMask[pos];
      }

      template<class SrcImageIterator, class SrcAccessor>
      bool operator() (const SrcImageIterator& i, const SrcAccessor& src) {
        vigra::Diff2D pos = i - mUpperLeft;

        if(mOwner.mRegionIds[pos] == mRegionId)
          return true; /* Skip self. */

        mOwner.mRegions[mRegionId].update(src(i), mOwner.mRegionIds[pos]);
        return true;
      }

      PixelType end() {
        return mOwner.mRegions[mRegionId].color();
      }

    private:
      int mRegionId;
      Traverser mUpperLeft;
      FilterSmallRegions& mOwner;
    };

    friend class RegionVisitor;

  public:
    FilterSmallRegions(const Image& srcImage, Image& dstImage, int minRegionSize): mSrcImage(srcImage), mDstImage(dstImage), mMinRegionSize(minRegionSize) {
      assert(mSrcImage.size() == mDstImage.size());
    }

    void operator() () {
      using namespace boost::phoenix::arg_names;
      using namespace boost::phoenix;

      mDstImage = mSrcImage;
      mRegionIds.resize(mSrcImage.size());
      mMask.resize(mDstImage.size());

      while(true) {
        /* Find connected components. */
        int regionCount = labelImage(srcImageRange(mDstImage), destImage(mRegionIds), false) + 1;
        mRegions.clear();
        mRegions.resize(regionCount);

        for(int i = 0; i < regionCount; i++)
          mRegions[i].setRegionId(i);

        /* Init regions. */
        for(int y = 0; y < mRegionIds.height(); y++) {
          for(int x = 0; x < mRegionIds.width(); x++) {
            int regionId = mRegionIds(x, y);
            Region& region = mRegions[regionId];
            region.addSize();
            region.setColor(mDstImage(x, y));
          }
        }

        /* Create mask. */
        bool smallRegionsExist = false;
        for(int y = 0; y < mRegionIds.height(); y++) {
          for(int x = 0; x < mRegionIds.width(); x++) {
            if(mRegions[mRegionIds(x, y)].size() < mMinRegionSize) {
              mMask(x, y) = true;
              smallRegionsExist = true;
            } else {
              mMask(x, y) = false;
            }
          }
        }
        if(!smallRegionsExist)
          break;

        /* Process regions. */
        mRegions.resize(regionCount);
        neighborhoodBasedTransform(srcImageRange(mDstImage), destImage(mDstImage), NeighborhoodWalker<Neighborhood, RegionVisitor>(RegionVisitor(mDstImage.upperLeft(), *this)));

        /* Sort regions. */
        std::sort(mRegions.begin(), mRegions.end());

        /* Find valid. */
        std::vector<PixelType> replacement(regionCount);
        std::vector<char> state(regionCount, 0); /* 0 - untouched, 1 - dst, 2 - src */
        foreach(Region& region, mRegions) {
          int regionId = region.regionId();
          if(state[regionId] == 2 || !region.isUpdated() || state[region.closestRegionId()] == 1)
            continue;

          replacement[regionId] = region.closestColor();
          state[regionId] = 1;
          state[region.closestRegionId()] = 2;
        }

        /* Replace. */
        for(int y = 0; y < mRegionIds.height(); y++)
          for(int x = 0; x < mRegionIds.width(); x++)
            if(mMask(x, y) && state[mRegionIds(x, y)] == 1)
              mDstImage(x, y) = replacement[mRegionIds(x, y)];
      }
    }

  private:
    const Image& mSrcImage;
    Image& mDstImage;
    int mMinRegionSize;
    ComponentImage mRegionIds;
    Mask mMask;
    std::vector<Region> mRegions;
  };

  template<class Neighborhood, class PixelType, class Alloc>
  void filterSmallRegions(const vigra::BasicImage<PixelType, Alloc>& srcImage, vigra::BasicImage<PixelType, Alloc>& dstImage, int minRegionSize) {
    FilterSmallRegions<Neighborhood, PixelType, Alloc>(srcImage, dstImage, minRegionSize)();
  }

  template<class Neighborhood, class PixelType, class Alloc>
  void filterSmallRegions(vigra::BasicImage<PixelType, Alloc>& image, int minRegionSize) {
    vigra::BasicImage<PixelType> tmpImage(image.size());
    filterSmallRegions<Neighborhood>(image, tmpImage, minRegionSize);
    image = tmpImage;
  }

} // namespace xs

#endif // __XSX_FILTER_SMALL_REGIONS_H__
