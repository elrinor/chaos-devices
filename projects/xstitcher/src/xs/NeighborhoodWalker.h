#ifndef __XSX_NEIGHBORHOOD_WALKER_H__
#define __XSX_NEIGHBORHOOD_WALKER_H__

#include "config.h"
#include <boost/mpl/int.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/less.hpp>
#include <arx/Utility.h>
#include "BorderDistance.h"

namespace xs {
// -------------------------------------------------------------------------- //
// NeighborhoodVisitor
// -------------------------------------------------------------------------- //
  template<class SrcImageIterator, class SrcAccessor, class DstPixelType>
  class NeighborhoodVisitor {
  public:
    typedef DstPixelType result_type;

    bool begin(const SrcImageIterator& pos, const SrcAccessor& src);

    result_type end();

    /**
     * @returns                        Whether the walk should be continued.
     */
    bool operator() (const SrcImageIterator& pos, const SrcAccessor& src);
  };


// -------------------------------------------------------------------------- //
// NeighborhoodWalker
// -------------------------------------------------------------------------- //
  template<class Neighborhood, class NeighborhoodVisitor>
  class NeighborhoodWalker {
    template<class Offset>
    struct OffsetToBorderDistance {
      enum {
        firstValue = Offset::first::value,
        secondValue = Offset::second::value
      };

      typedef BorderDistance<
        (firstValue < 0 ? -firstValue : 0),
        (firstValue > 0 ? firstValue : 0),
        (secondValue < 0 ? -secondValue : 0),
        (secondValue > 0 ? secondValue : 0)
      > type;
    };

    template<class NeighborhoodIterator, class LastOffsetIterator, class BorderDistance, class SrcImageIterator, class SrcAccessor>
    struct Iterate {
      FORCEINLINE void operator() (const SrcImageIterator& pos, const SrcAccessor& src, NeighborhoodVisitor& visitor) {
        typedef typename boost::mpl::deref<NeighborhoodIterator>::type Offset;
        if(BorderDistanceIsInside<typename OffsetToBorderDistance<Offset>::type, BorderDistance>::value) {
          if(!visitor(pos + vigra::Diff2D(Offset::first::value, Offset::second::value), src))
            return;
        }
        Iterate<typename boost::mpl::next<NeighborhoodIterator>::type, LastOffsetIterator, BorderDistance, SrcImageIterator, SrcAccessor>()(pos, src, visitor);
      }
    };

    template<class NeighborhoodIterator, class BorderDistance, class SrcImageIterator, class SrcAccessor>
    struct Iterate<NeighborhoodIterator, NeighborhoodIterator, BorderDistance, SrcImageIterator, SrcAccessor> {
      FORCEINLINE void operator() (const SrcImageIterator&, const SrcAccessor&, NeighborhoodVisitor&) {
        return;
      }
    };

  public:
    typedef typename NeighborhoodVisitor::result_type result_type;

    NeighborhoodWalker(NeighborhoodVisitor& visitor = NeighborhoodVisitor()): mVisitor(visitor) {}

    template<class BorderDistance, class SrcImageIterator, class SrcAccessor>
    result_type operator() (const BorderDistance&, const SrcImageIterator& pos, const SrcAccessor& src) {
      if(mVisitor.begin(pos, src)) {
        Iterate<
          typename boost::mpl::begin<Neighborhood>::type, 
          typename boost::mpl::end<Neighborhood>::type,
          BorderDistance, 
          SrcImageIterator,
          SrcAccessor
        >()(pos, src, mVisitor);
      }
      return mVisitor.end();
    }

  private:
    NeighborhoodVisitor& mVisitor;
  };


// -------------------------------------------------------------------------- //
// FourNeighborhood
// -------------------------------------------------------------------------- //
  typedef boost::mpl::vector4<
    boost::mpl::pair<boost::mpl::int_<-1>, boost::mpl::int_< 0> >,
    boost::mpl::pair<boost::mpl::int_< 1>, boost::mpl::int_< 0> >,
    boost::mpl::pair<boost::mpl::int_< 0>, boost::mpl::int_<-1> >,
    boost::mpl::pair<boost::mpl::int_< 0>, boost::mpl::int_< 1> >
  > FourNeighborhood;


// -------------------------------------------------------------------------- //
// EightNeighborhood
// -------------------------------------------------------------------------- //
  typedef boost::mpl::vector8<
    boost::mpl::pair<boost::mpl::int_<-1>, boost::mpl::int_< 0> >,
    boost::mpl::pair<boost::mpl::int_< 1>, boost::mpl::int_< 0> >,
    boost::mpl::pair<boost::mpl::int_< 0>, boost::mpl::int_<-1> >,
    boost::mpl::pair<boost::mpl::int_< 0>, boost::mpl::int_< 1> >,
    boost::mpl::pair<boost::mpl::int_< 1>, boost::mpl::int_< 1> >,
    boost::mpl::pair<boost::mpl::int_< 1>, boost::mpl::int_<-1> >,
    boost::mpl::pair<boost::mpl::int_<-1>, boost::mpl::int_<-1> >,
    boost::mpl::pair<boost::mpl::int_<-1>, boost::mpl::int_< 1> >
  > EightNeighborhood;

} // namespace xs

#endif // __XSX_NEIGHBORHOOD_WALKER_H__
