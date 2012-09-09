#ifndef QW_CONFIG_H
#define QW_CONFIG_H

/* Use foreach from boost. */
#define foreach BOOST_FOREACH

/* Define override specifier for MSVC. */
#ifdef _MSC_VER
#  define OVERRIDE override
#else
#  define OVERRIDE
#endif

/* Unreachable code. */
// TODO: Turn into warning.
#define unreachable() assert(!"Unreachable code executed")

#endif // QW_CONFIG_H
