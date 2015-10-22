#ifndef TRACE_H_
#define TRACE_H_
#include <stdio.h>
#define __TRACE(typ, fmt, ...) printf(typ " (%s:%d): " fmt "\n%s", __FILE__, __LINE__,  __VA_ARGS__)
#if TRACE_LEVEL >= 0
#define __TRACE_ERROR(fmt, ...) __TRACE("ERROR", fmt, __VA_ARGS__)
#define TRACE_ERROR(...) __TRACE_ERROR(__VA_ARGS__, "")
#else
#define TRACE_ERROR(...)
#endif
#if TRACE_LEVEL >= 1
#define __TRACE_WARNING(fmt, ...) __TRACE("WARNING", fmt, __VA_ARGS__)
#define TRACE_WARNING(...) __TRACE_WARNING(__VA_ARGS__, "")
#else
#define TRACE_WARNING(...)
#endif
#if TRACE_LEVEL >= 2
#define __TRACE_INFO(fmt, ...) __TRACE("INFO", fmt, __VA_ARGS__)
#define TRACE_INFO(...) __TRACE_INFO(__VA_ARGS__, "")
#else
#define TRACE_INFO(...)
#endif
#endif /* TRACE_H_ */