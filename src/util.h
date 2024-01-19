#pragma once

#define panic(fmt, ...) (fprintf(stderr, fmt, __VA_ARGS__), exit(1))
#define assert(cond, fmt, ...) if (!(cond)) panic(fmt, __VA_ARGS__)
