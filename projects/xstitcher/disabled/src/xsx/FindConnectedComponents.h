#ifndef __XSX_FIND_CONNECTED_COMPONENTS_H__
#define __XSX_FIND_CONNECTED_COMPONENTS_H__

#include "config.h"
#include <cassert>
#include <boost/mpl/for_each.hpp>
#include <arx/ext/Vigra.h>


namespace xsx {
// -------------------------------------------------------------------------- //
// FindConnectedComponents
// -------------------------------------------------------------------------- //
  template<class Neighborhood, class PixelType, class SrcAlloc, class DstAlloc>
  class FindConnectedComponents {
    typedef vigra::BasicImage<PixelType, SrcAlloc> Image;
    typedef vigra::BasicImage<int, DstAlloc> ComponentImage;

    class ComponentVisitor {
    public:
      ComponentVisitor(int x, int y, int value, FindConnectedComponents& owner): mX(x), mY(y), mValue(value), mOwner(owner) {}

      template<class T>
      void operator() (T) {
        const int x = mX + T::first::value;
        const int y = mY + T::second::value;

        if(x >= 0 && x < mOwner.mSrcImage.width() && y >= 0 && y < mOwner.mSrcImage.height() && mOwner.mDstImage(x, y) == -1 && mOwner.mSrcImage(x, y) == mOwner.mSrcImage(mX, mY)) {
          mOwner.mDstImage(x, y) = mValue;
          mOwner.mNewCoords.push_back(vigra::Diff2D(x, y));
        }
      }

    private:
      int mX, mY;
      int mValue;
      FindConnectedComponents& mOwner;
    };

    friend class ComponentVisitor;

  public:
    FindConnectedComponents(const Image& srcImage, ComponentImage& dstImage): mSrcImage(srcImage), mDstImage(dstImage) {
      assert(mSrcImage.size() == mDstImage.size());
    }

    int operator() () {
      mDstImage.init(-1);

      int index = 0;
      for(int y = 0; y < mSrcImage.height(); y++) {
        for(int x = 0; x < mSrcImage.width(); x++) {
          if(mDstImage(x, y) == -1) {
            mCoords.push_back(vigra::Diff2D(x, y));
            mDstImage(x, y) = index;
            
            while(!mCoords.empty()) {
              foreach(const vigra::Diff2D& pos, mCoords)
                boost::mpl::for_each<Neighborhood>(ComponentVisitor(pos.x, pos.y, index, *this));

              using std::swap;
              swap(mCoords, mNewCoords);
              mNewCoords.clear();
            }

            index++;
          }
        }
      }

      return index;
    }

  private:
    std::vector<vigra::Diff2D> mCoords;
    std::vector<vigra::Diff2D> mNewCoords;
    const Image& mSrcImage;
    ComponentImage& mDstImage;
  };

  template<class Neighborhood, class PixelType, class SrcAlloc, class DstAlloc>
  int findConnectedComponents(const vigra::BasicImage<PixelType, SrcAlloc>& srcImage, vigra::BasicImage<int, DstAlloc>& dstImage) {
    return FindConnectedComponents<Neighborhood, PixelType, SrcAlloc, DstAlloc>(srcImage, dstImage)();
  }


} // namespace xsx


#endif // __XSX_FIND_CONNECTED_COMPONENTS_H__