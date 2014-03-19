#ifndef __XSX_NEIGHBORHOOD_BASED_TRANSFORM_H__
#define __XSX_NEIGHBORHOOD_BASED_TRANSFORM_H__

#include "config.h"
#include <cassert>
#include <arx/ext/Vigra.h>
#include "BorderDistance.h"

namespace xs {
  template<class SrcImageIterator, class SrcAccessor, class DestImageIterator, class DestAccessor, class Functor>
  void neighborhoodBasedTransform(SrcImageIterator srcUpperLeft, SrcImageIterator srcLowerRight, SrcAccessor src, DestImageIterator dstUpperLeft, DestAccessor dst, Functor& functor) {
    using boost::mpl::integral_c;

    int w = srcLowerRight.x - srcUpperLeft.x;
    int h = srcLowerRight.y - srcUpperLeft.y;

    assert(w > 1 && h > 1);

    int x, y;
    SrcImageIterator sy = srcUpperLeft, sx;
    DestImageIterator dy = dstUpperLeft, dx;

    /* Process first row. */
    sx = sy;
    dx = dy;
    dst.set(functor(BorderDistance<0, NoBorder, 0, NoBorder>(), sx, src), dx);
    for(x = 2, ++sx.x, ++dx.x; x < w; ++x, ++sx.x, ++dx.x)
      dst.set(functor(BorderDistance<NoBorder, NoBorder, 0, NoBorder>(), sx, src), dx);
    dst.set(functor(BorderDistance<NoBorder, 0, 0, NoBorder>(), sx, src), dx);

    /* Process central rows. */
    for(y = 2, ++sy.y, ++dy.y; y < h; ++y, ++sy.y, ++dy.y) {
      sx = sy;
      dx = dy;

      dst.set(functor(BorderDistance<0, NoBorder, NoBorder, NoBorder>(), sx, src), dx);
      for(x = 2, ++sx.x, ++dx.x; x < w; ++x, ++sx.x, ++dx.x)
        dst.set(functor(BorderDistance<NoBorder, NoBorder, NoBorder, NoBorder>(), sx, src), dx);
      dst.set(functor(BorderDistance<NoBorder, 0, NoBorder, NoBorder>(), sx, src), dx);
    }

    /* Process last row. */
    sx = sy;
    dx = dy;
    dst.set(functor(BorderDistance<0, NoBorder, NoBorder, 0>(), sx, src), dx);
    for(x = 2, ++sx.x, ++dx.x; x < w; ++x, ++sx.x, ++dx.x)
      dst.set(functor(BorderDistance<NoBorder, NoBorder, NoBorder, 0>(), sx, src), dx);
    dst.set(functor(BorderDistance<NoBorder, 0, NoBorder, 0>(), sx, src), dx);
  }

  template<class SrcImageIterator, class SrcAccessor, class DestImageIterator, class DestAccessor, class Functor>
  inline void neighborhoodBasedTransform(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src, std::pair<DestImageIterator, DestAccessor> dest, Functor& functor) {
    neighborhoodBasedTransform(src.first, src.second, src.third, dest.first, dest.second, functor);
  }



} // namespace xs

#endif __XSX_NEIGHBORHOOD_BASED_TRANSFORM_H__
