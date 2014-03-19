#ifndef __XSTITCHER_CONFIG_H__
#define __XSTITCHER_CONFIG_H__

/* Link with ImageMagick */
#ifdef _MSC_VER
#  ifdef _DEBUG
#    pragma comment(lib, "CORE_DB_bzlib_.lib")
#    pragma comment(lib, "CORE_DB_coders_.lib")
#    pragma comment(lib, "CORE_DB_filters_.lib")
#    pragma comment(lib, "CORE_DB_jbig_.lib")
#    pragma comment(lib, "CORE_DB_jp2_.lib")
#    pragma comment(lib, "CORE_DB_jpeg_.lib")
#    pragma comment(lib, "CORE_DB_lcms_.lib")
#    pragma comment(lib, "CORE_DB_libxml_.lib")
#    pragma comment(lib, "CORE_DB_magick_.lib")
#    pragma comment(lib, "CORE_DB_Magick++_.lib")
#    pragma comment(lib, "CORE_DB_png_.lib")
#    pragma comment(lib, "CORE_DB_tiff_.lib")
#    pragma comment(lib, "CORE_DB_ttf_.lib")
#    pragma comment(lib, "CORE_DB_wand_.lib")
#    pragma comment(lib, "CORE_DB_wmf_.lib")
#    pragma comment(lib, "CORE_DB_xlib_.lib")
#    pragma comment(lib, "CORE_DB_zlib_.lib")
#  else
#    pragma comment(lib, "CORE_RL_bzlib_.lib")
#    pragma comment(lib, "CORE_RL_coders_.lib")
#    pragma comment(lib, "CORE_RL_filters_.lib")
#    pragma comment(lib, "CORE_RL_jbig_.lib")
#    pragma comment(lib, "CORE_RL_jp2_.lib")
#    pragma comment(lib, "CORE_RL_jpeg_.lib")
#    pragma comment(lib, "CORE_RL_lcms_.lib")
#    pragma comment(lib, "CORE_RL_libxml_.lib")
#    pragma comment(lib, "CORE_RL_magick_.lib")
#    pragma comment(lib, "CORE_RL_Magick++_.lib")
#    pragma comment(lib, "CORE_RL_png_.lib")
#    pragma comment(lib, "CORE_RL_tiff_.lib")
#    pragma comment(lib, "CORE_RL_ttf_.lib")
#    pragma comment(lib, "CORE_RL_wand_.lib")
#    pragma comment(lib, "CORE_RL_wmf_.lib")
#    pragma comment(lib, "CORE_RL_xlib_.lib")
#    pragma comment(lib, "CORE_RL_zlib_.lib")
#  endif
#  pragma comment(lib, "Ws2_32.lib")
#endif

#if 0
/* Link with OpenCV */
#ifdef _MSC_VER
#  ifdef _DEBUG
#    pragma comment(lib, "cv200d.lib")
#    pragma comment(lib, "cxcore200d.lib")
#    pragma comment(lib, "highgui200d.lib")
#    pragma comment(lib, "zlibd.lib")
#    pragma comment(lib, "opencv_lapackd.lib")
#    pragma comment(lib, "flannd.lib")
#    pragma comment(lib, "libjasperd.lib")
#    pragma comment(lib, "libpngd.lib")
#    pragma comment(lib, "libtiffd.lib")
#    pragma comment(lib, "libjpegd.lib")
#    pragma comment(linker, "/NODEFAULTLIB:libcmt.lib")
#  else
#    pragma comment(lib, "cv200.lib")
#    pragma comment(lib, "cxcore200.lib")
#    pragma comment(lib, "highgui200.lib")
#    pragma comment(lib, "zlib.lib")
#    pragma comment(lib, "opencv_lapack.lib")
#    pragma comment(lib, "flann.lib")
#    pragma comment(lib, "libjasper.lib")
#    pragma comment(lib, "libpng.lib")
#    pragma comment(lib, "libtiff.lib")
#    pragma comment(lib, "libjpeg.lib")
#    pragma comment(linker, "/NODEFAULTLIB:libcpmtd.lib") /* Required by videoInput.lib, but apparently not used. */
#  endif
#  pragma comment(lib, "vfw32.lib") /* For Video for Windows. */
#  pragma comment(lib, "comctl32.lib") /* For CreateToolbarEx. */
#  pragma comment(linker, "/NODEFAULTLIB:atlthunk.lib") /* Required by videoInput.lib, but apparently not used. */
#endif
#endif

/* Disable MSVC bogus warnings. */
#ifdef _MSC_VER
#  ifndef _CRT_SECURE_NO_WARNINGS
#    define _CRT_SECURE_NO_WARNINGS
#  endif
#  ifndef _SCL_SECURE_NO_WARNINGS
#    define _SCL_SECURE_NO_WARNINGS
#  endif
#endif

#define XSTITCHER_VERSION "0.1.0"

#define XSX_MEASURE_TIMINGS

#ifdef XSX_MEASURE_TIMINGS
#  define XSX_TIMING(X) X
#else 
#  define XSX_TIMING(X)
#endif

#endif // __XSTITCHER_CONFIG_H__
