#ifndef __XSX_BORDER_DISTANCE_H__
#define __XSX_BORDER_DISTANCE_H__

#include "config.h"
#include <boost/mpl/bool.hpp>
#include <vigra/pixelneighborhood.hxx>

namespace xs {
  enum {
    NoBorder = 32767
  };

// -------------------------------------------------------------------------- //
// BorderDistance
// -------------------------------------------------------------------------- //
  template<int l, int r, int t, int b>
  struct BorderDistance {
    enum {
      left = l,
      right = r,
      top = t,
      bottom = b
    };
  };


// -------------------------------------------------------------------------- //
// BorderDistanceIsInside
// -------------------------------------------------------------------------- //
  template<class SmallBorderDistance, class LargeBorderDistance> 
  struct BorderDistanceIsInside: public boost::mpl::bool_<
    (SmallBorderDistance::left   <= LargeBorderDistance::left) &&
    (SmallBorderDistance::right  <= LargeBorderDistance::right) &&
    (SmallBorderDistance::top    <= LargeBorderDistance::top) &&
    (SmallBorderDistance::bottom <= LargeBorderDistance::bottom)
  > {};


// -------------------------------------------------------------------------- //
// ImageBorderToBorderDistance
// -------------------------------------------------------------------------- //
  template<vigra::AtImageBorder atImageBorder>
  struct ImageBorderToBorderDistance {
    typedef BorderDistance<
      (atImageBorder & vigra::LeftBorder)   != 0 ? 0 : NoBorder,
      (atImageBorder & vigra::RightBorder)  != 0 ? 0 : NoBorder,
      (atImageBorder & vigra::TopBorder)    != 0 ? 0 : NoBorder,
      (atImageBorder & vigra::BottomBorder) != 0 ? 0 : NoBorder
    > type;
  };


} // namespace xs

#endif // __XSX_BORDER_DISTANCE_H__
