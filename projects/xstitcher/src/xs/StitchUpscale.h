#ifndef __XSX_STITCH_UPSCALE_H__
#define __XSX_STITCH_UPSCALE_H__

#include "config.h"
#include <vigra/resizeimage.hxx>
#include <arx/ext/Vigra.h>

#include <vigra/rgbvalue.hxx>

namespace xs {
// -------------------------------------------------------------------------- //
// stitchUpscale
// -------------------------------------------------------------------------- //
  template<class PixelType, class SrcAlloc, class DstAlloc>
  void stitchUpscale(const vigra::BasicImage<PixelType, SrcAlloc>& srcImage, vigra::BasicImage<PixelType, DstAlloc>& dstImage, float stitchDarkness) {
    assert(srcImage.size() * 2 == dstImage.size());

    resizeImageNoInterpolation(srcImageRange(srcImage), destImageRange(dstImage));

    for(int y = 1; y < dstImage.height(); y += 2)
      for(int x = 1; x < dstImage.width(); x += 2)
        dstImage(x, y) *= stitchDarkness;
  }

}


#endif // __XSX_STITCH_UPSCALE_H__
