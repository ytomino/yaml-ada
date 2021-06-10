#if defined(__APPLE__)
/* avoiding circular dependency about pid_t */
#include <sys/types.h>
#elif defined(__gnu_linux__)
#if !defined(_BITS_LIBIO_H)
#include <features.h> /* __GLIBC_PREREQ */
#if !__GLIBC_PREREQ(2, 27)
#include <libio.h> /* before stdio.h */
#else
#define _LIBIO_H
#include <bits/libio.h> /* before stdio.h */
#undef _LIBIO_H
#endif
#endif
#endif
#include <yaml.h>

#if defined(__gnu_linux__)
#if __GLIBC_PREREQ(2, 27)
#pragma for Ada "stdio.h" include "bits/types/FILE.h"
#endif
#endif
