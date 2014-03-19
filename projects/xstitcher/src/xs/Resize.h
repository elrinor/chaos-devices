#ifndef __XSX_RESIZE_H__
#define __XSX_RESIZE_H__

#include "config.h"
#include <vigra/resizeimage.hxx>
#include <arx/ext/Vigra.h>
//#include <arx/ext/OpenCV.h>

namespace xs {
  /* TODO: create BitHacks.h in arxlib */
  bool isPowerOf2(int n) {
    return ((n != 0) && (n & (n - 1)) == 0);
  }

  void clusterResize(const vigra::BRGBImage& src, vigra::BRGBImage& dst, int level, double linkThreshold, double clusterThreshold) {
    assert(src.width() > 0 && src.width() > 0 && dst.width() > 0 && dst.height() > 0 && isPowerOf2(level));

    vigra::BRGBImage clusterized;
    //pyramidClusterize(src, clusterized, level, linkThreshold, clusterThreshold);

    //exportImage(clusterized, "out.png");

    resizeImageLinearInterpolation(srcImageRange(src), destImageRange(dst));
  }

} // namespace xs

#endif // __XSX_RESIZE_H__
