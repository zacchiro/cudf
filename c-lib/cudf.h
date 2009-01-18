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

typedef void* package_t;
typedef void* universe_t;
typedef void* request_t;

typedef struct cudf_doc {
  package_t *packages;		/* Array of packages */
  request_t request;		/* User request */
} cudf_doc;

typedef struct cudf {
  universe_t universe;		/* Abstract package universe */
  request_t request;		/* User request */
} cudf;

cudf_doc *cudf_parse_from_file(char *fname);
cudf *cudf_load_from_file(char *fname);

#endif	/* end of cudf.h */
