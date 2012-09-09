#ifndef __ARXMATH_H__
#define __ARXMATH_H__

#include <limits>

#ifdef max
	#undef max
#endif

#ifdef min
	#undef min
#endif

#ifdef sqr
	#undef sqr
#endif

#define EPS 1.0e-10
#define FLOAT_PLUS_INF std::numeric_limits<float>::max()

inline int sqr(int x)
{
	return x * x;
}

inline unsigned int sqr(unsigned int x)
{
	return x * x;
}

inline float sqr(float x)
{
	return x * x;
}

inline int intlog2(int v)
{
	static const unsigned int magic[32] = {
		0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
		31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9};
	return magic[(v*0x077CB531UL)>>27];
}

inline int pow2(int v)
{
	static const unsigned int power2[32] = {
		1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 
		2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648};
	return power2[v];
}

template<class T> inline T min(T a, T b)
{
	return (a > b) ? b : a;
}

template<class T> inline T max(T a, T b)
{
	return (a > b) ? a : b;
}

#endif