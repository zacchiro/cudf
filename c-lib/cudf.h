/*****************************************************************************/
/*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  */
/*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             */
/*                                                                           */
/*  This library is free software: you can redistribute it and/or modify     */
/*  it under the terms of the GNU Lesser General Public License as           */
/*  published by the Free Software Foundation, either version 3 of the       */
/*  License, or (at your option) any later version.  A special linking       */
/*  exception to the GNU Lesser General Public License applies to this       */
/*  library, see the COPYING file for more information.                      */
/*****************************************************************************/

#ifndef _CUDF_H
#define _CUDF_H

#include <caml/mlvalues.h>

typedef value package_t;
typedef value universe_t;
typedef value request_t;

typedef struct cudf_doc {
  package_t *packages;	/* Array of packages */
  int length;		/* Number of packages */
  int has_request;	/* Whether user request was provided or not */
  request_t request;	/* User request (meaningful iff has_request != 0) */
} cudf_doc;

typedef struct cudf {
  universe_t universe;	/* Abstract package universe */
  int length;		/* Number of packages */
  int has_request;	/* Whether user request was provided or not */
  request_t request;	/* User request (meaningful iff has_request != 0) */
} cudf;

cudf_doc cudf_parse_from_file(char *fname);
cudf cudf_load_from_file(char *fname);

void free_cudf_doc(cudf_doc doc);
void free_cudf(cudf doc);

/** Macros for accessing package_t values */

#define PKG_NAME(p)	(String_val(Field(p, 0)))	/* char *  */
#define PKG_VERSION(p)	(Int_val(Field(p, 1)))		/* int */
#define PKG_INST(p)	(Int_val(Field(p, 5)))		/* int (i.e., bool) */

/** Possible values returned by PKG_EXTRA */

#define KEEP_NONE	0	/* no "Keep" property */
#define KEEP_VERSION	1	/* "Keep: version" */
#define	KEEP_PACKAGE	2	/* "Keep: package" */
#define	KEEP_FEATURE	3	/* "Keep: feature" */

int cudf_pkg_keep(package_t pkg);	/* "Keep" property, see KEEP_* macros */

#endif	/* end of cudf.h */

