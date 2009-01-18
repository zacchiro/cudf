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

// TODO should check / handle exceptions for all invoked caml_callback-s

#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <cudf.h>

#define Val_none	Val_int(0)
#define Some_val(v)	Field(v,0)

#define MLPVAR_version     (-251800451)    /* caml hash for "Keep_version" */
#define MLPVAR_package     (2054970713)    /* caml hash for "Keep_package" */
#define MLPVAR_feature     (739503033)     /* caml hash for "Keep_feature" */



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
  static value *closure_f = NULL;
  cudf_doc doc;
  value ml_doc, ml_pkgs;
  int i = 0;
  
  if (closure_f == NULL) {
    closure_f = caml_named_value("parse_from_file");
  }
  
  ml_doc = caml_callback(*closure_f, caml_copy_string(fname));	/* request */
  caml_register_global_root(&doc.request);
  if (Field(ml_doc, 1) != Val_none) {
    doc.has_request = 1;
    doc.request = Some_val(Field(ml_doc, 1));
  } else {
    doc.has_request = 0;
    doc.request = Val_none;
  }

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

int cudf_pkg_keep(package_t p)
{
  value keep = Field(p, 6);

  if (keep == Val_none)
    return KEEP_NONE;
  else
    switch (Some_val(keep)) {
    case MLPVAR_version : return KEEP_VERSION;
    case MLPVAR_package : return KEEP_PACKAGE;
    case MLPVAR_feature : return KEEP_FEATURE;
    default:
      fprintf(stderr, "Internal error: unexpected variant for \"keep\": %d\n",
	      Some_val(p));
      exit(3);
    }
}

char *cudf_pkg_property(package_t pkg, const char *prop)
{
  static value *closure_f = NULL;
  value prop_val;
  
  if (closure_f == NULL) {
    closure_f = caml_named_value("lookup_package_property");
  }
  prop_val = caml_callback2_exn(*closure_f, pkg, caml_copy_string(prop));
  return Is_exception_result(prop_val) ? NULL : strdup(String_val(prop_val));
}

char *cudf_req_property(request_t req, const char *prop)
{
  static value *closure_f = NULL;
  value prop_val;
  
  if (closure_f == NULL) {
    closure_f = caml_named_value("lookup_request_property");
  }
  prop_val = caml_callback2_exn(*closure_f, req, caml_copy_string(prop));
  return Is_exception_result(prop_val) ? NULL : strdup(String_val(prop_val));
}

