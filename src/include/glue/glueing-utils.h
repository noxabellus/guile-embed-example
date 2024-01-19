#pragma once

#define EXPORT_CONSTANT(name, val) \
    scm_c_define(name, val); \
    scm_c_export(name);

#define EXPORT_PROCEDURE(name, req, opt, rest, fn) \
    scm_c_define_gsubr(name, req, opt, rest, fn); \
    scm_c_export(name);

#define scm_panic(sym, msg) \
    scm_throw(scm_from_utf8_symbol(sym), scm_from_utf8_string(msg));

#define scm_assert(cond, sym, msg) \
    if (!(cond)) scm_panic(sym, msg);
