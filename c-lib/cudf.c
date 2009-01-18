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

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>

#include <cudf.h>


cudf_doc *cudf_parse_from_file(char *fname)
{
  cudf_doc *doc;
  static value *closure_f = NULL;
  value *ml_doc;
  
  if (closure_f == NULL) {
    closure_f = caml_named_value("parse_from_file");
  }
  
  *ml_doc = caml_callback(*closure_f, caml_copy_string(fname));
  doc = malloc(sizeof(cudf_doc));
}

