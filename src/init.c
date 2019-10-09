/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2017   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include "port.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define C_DEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CMethodDef CEntries[]  = {
    // C_DEF(rcont2,  8),
    {NULL, NULL, 0}
};

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

#define CALLDEF_DO(name, n) {#name, (DL_FUNC) &do_##name, n}
#define CALLDEF_MATH2_1(name) CALLDEF_DO(name, 3)
#define CALLDEF_MATH2_2(name) CALLDEF_DO(name, 4)
#define CALLDEF_MATH3_1(name) CALLDEF_DO(name, 4)
#define CALLDEF_MATH3_2(name) CALLDEF_DO(name, 5)
#define CALLDEF_MATH4_1(name) CALLDEF_DO(name, 5)
#define CALLDEF_MATH4_2(name) CALLDEF_DO(name, 6)

#define CALLDEF_RAND1(name) CALLDEF_DO(name, 2)
#define CALLDEF_RAND2(name) CALLDEF_DO(name, 3)
#define CALLDEF_RAND3(name) CALLDEF_DO(name, 4)

#define FDEF(name)  {#name, (DL_FUNC) &F77_NAME(name), sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(port_ivset, 3),
    CALLDEF(port_nlminb, 9),
    CALLDEF(port_nlsb, 7),
    {NULL, NULL, 0}
};

// static const R_FortranMethodDef FortEntries[] = {
//     // FDEF(lowesw),
//     // FDEF(lowesp),
//     // {"setppr", (DL_FUNC) &F77_NAME(setppr),  6},
//     {NULL, NULL, 0}
// };

// #define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}
// // These argument counts are not checked
// static const R_ExternalMethodDef ExtEntries[] = {
//     {NULL, NULL, 0}
// };


void attribute_visible R_init_stats(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL); // FortEntries, ExtEntries
    R_useDynamicSymbols(dll, FALSE);
    // R_forceSymbols(dll, TRUE);
    // R_RegisterCCallable("stats", "nlminb_iterate", (DL_FUNC) nlminb_iterate);
    // R_RegisterCCallable("stats", "nlsb_iterate", (DL_FUNC) nlsb_iterate);
    // R_RegisterCCallable("stats", "Rf_divset", (DL_FUNC) Rf_divset);
}
