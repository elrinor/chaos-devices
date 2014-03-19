#include "config.h"
#include <map>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include <arx/Foreach.h>
#include <arx/ext/Vigra.h>
#include <arx/ext/Magick.h>
#include "xs/Palette.h"
#include "xs/FilterSmallRegions.h"
#include "xs/StitchUpscale.h"
#include "xs/Resize.h"

#ifdef XSX_MEASURE_TIMINGS
#  include <boost/timer.hpp>
#endif

int main(int argc, char** argv) {
  using namespace boost::program_options;
  using namespace std;

  try {
    vigra::Size2D maxSize;
    int colors, clipFactor, minRegionSize;
    bool showHelp;
    std::string inputFileName, paletteFileName, outputFileName;
    std::string name;
    float colorCompression;

    options_description desc("Allowed options");
    desc.add_options()
      ("help",                    bool_switch(&showHelp),                                   "Produce help message.")
      ("input,i",                 value<string>(&inputFileName)->required(),                "Input file name.")
      ("size,s",                  value<vigra::Size2D>(&maxSize)->required(),               "Maximal image size, in format w:h.")
      ("colors,c",                value<int>(&colors)->required(),                          "Maximal number of colors.")
      ("palette,p",               value<string>(&paletteFileName)->required(),              "Palette file name.")
      ("output,o",                value<string>(&outputFileName)->default_value("out.png"), "Output file name.")
      ("min-region-size,r",       value<int>(&minRegionSize)->default_value(5),             "Minimal size of a connected color region in resulting image.")
      ("clip-factor,f",           value<int>(&clipFactor)->default_value(16),               "Clip factor for image size. Input image will be clipped to make its width & height a multiple of this value. Must be a power of 2.")
      ("dmc-color-compression,d", value<float>(&colorCompression)->default_value(1.0f),     "Colorspace compression factor to cover up for non-uniform distribution of DMC colors.");

    try {
      variables_map vm;
      store(command_line_parser(argc, argv).options(desc).run(), vm);
      notify(vm);
    } catch (required_option&) {
      showHelp = true;
    }

    /* Output help & exit if needed. */
    if(showHelp) {
      cout << "xspng, version " << XSTITCHER_VERSION << "." << endl;
      cout << endl;
      cout << "USAGE:" << endl;
      cout << "  xspng [options]" << endl;
      cout << endl;
      cout << desc << endl;
      return 1;
    }

    /* Check that clip factor is a power of 2. */
    if(!xs::isPowerOf2(clipFactor))
      throw std::logic_error("Specified clip factor is not a power of 2");

    /* Import Image */
    vigra::BRGBImage rgbImage;
    importImage(rgbImage, inputFileName);

    XSX_TIMING(boost::timer t);

    /* Clip using given clip factor. */
    if(rgbImage.width() % clipFactor != 0 || rgbImage.height() % clipFactor != 0) {
      vigra::BRGBImage tmpImage(rgbImage.width() & ~(clipFactor - 1), rgbImage.height() & ~(clipFactor - 1));
      copyImage(srcImageRange(rgbImage, vigra::Rect2D(0, 0, tmpImage.width(), tmpImage.height())), destImage(tmpImage));
      tmpImage.swap(rgbImage);
    }

    /* Calculate actual size of output image and apply cluster resizing. */
    float scale = min(1.0f, min(maxSize.width() / static_cast<float>(rgbImage.width()), maxSize.height() / static_cast<float>(rgbImage.height())));
    vigra::BRGBImage dstImage(rgbImage.width() * scale, rgbImage.height() * scale);
    xs::clusterResize(rgbImage, dstImage, 2, 200, 50);

    /* Scale. */
    for(int y = 0; y < dstImage.height(); y++) {
      for(int x = 0; x < dstImage.width(); x++) {
        xs::RgbValue& v = dstImage(x, y);

        float r = v.red(), g = v.green(), b = v.blue();

        float t0 = b / (g + b);
        float t1 = 2 * t0 - 1;
        t1 = 0.5f * pow(abs(t1), colorCompression) * vigra::sign(t1) + 0.5f;

        float t2 = pow((r + g + b) / (255.0f * 3), 2);

        float t = t0 * (1 - t2) + t1 * t2;

        v.setGreen((g + b) * (1 - t));
        v.setBlue((g + b) * t);
      }
    }

    /* Quantize. */
    quantize(dstImage, colors); /* TODO: quantization without creation of new colors. */

    /* Map to palette. */
    xs::Palette palette(paletteFileName);
    mapToPalette(dstImage, palette);

    /* Filter out small connected components. */
    xs::filterSizeOneRegions<xs::FourNeighborhood>(dstImage);
    if(minRegionSize > 2)
      xs::filterSmallRegions<xs::FourNeighborhood>(dstImage, minRegionSize);

    XSX_TIMING(cout << t.elapsed() << " secs spent on processing." << endl);

    /* Output. */
    exportImage(dstImage, outputFileName);
  } catch(exception& e) {
    cerr << "error: " << e.what() << endl;
    return 1;
  } catch(...) {
    cerr << "error: Exception of unknown type." << endl;
    return 1;
  }

  return 0;
}