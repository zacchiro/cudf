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
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <cudf.h>

#define Val_none Val_int(0)

static int caml_list_length(value l)
{
  int length = 0;

  while (l != Val_emptylist) {
    length++;
    l = Field(l, 1);
  }
  return length;
}

cudf_doc cudf_parse_from_file(char *fname)
{
  cudf_doc doc;
  static value *closure_f = NULL;
  value ml_doc, ml_pkgs;
  int i = 0;
  
  if (closure_f == NULL) {
    closure_f = caml_named_value("parse_from_file");
  }
  
  ml_doc = caml_callback(*closure_f, caml_copy_string(fname));	/* request */
  caml_register_global_root(&doc.request);
  doc.request = Field(ml_doc, 1);
  doc.has_request = (doc.request != Val_none);

  ml_pkgs = Field(ml_doc, 0);					/* packages */
  doc.length = caml_list_length(ml_pkgs);
  if (doc.length > 0) {
    doc.packages = malloc(doc.length * sizeof(value));
    while (ml_pkgs != Val_emptylist) {
      caml_register_global_root(&doc.packages[i]);
      doc.packages[i] = Field(ml_pkgs, 0);
      i++;
      ml_pkgs = Field(ml_pkgs, 1);
    }
  } else {
    doc.packages = NULL;
  }

  return doc;
}

void free_cudf_doc(cudf_doc doc)
{
  int i;

  caml_remove_global_root(&doc.request);
  for (i = 0; i < doc.length ; i++) {
    caml_remove_global_root(&doc.packages[i]);
  }
  free(doc.packages);
}

