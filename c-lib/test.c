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

/* Compile with:

   CUDF_LIBS="-lcudf -lpcre -lm -ldl -lpcre_stubs -lunix -lncurses"
   CUDF_PATHS="-L$(ocamlc -where) -L$(ocamlfind query pcre)"
   cc -o test $(CUDF_PATHS) test.o $(CUDF_LIBS)
 */

#include <stdio.h>
#include <caml/callback.h>

#include <cudf.h>

int main(int argc, char **argv)
{
  cudf_doc doc;
  package_t pkg;
  int i;
  char *prop_val;

  caml_startup(argv);
  if (argc < 2) {
    printf("Usage: %s CUDF_FILE\n", argv[0]);
    exit(2);
  }

  doc = cudf_parse_from_file(argv[1]);
  printf("Universe size: %d\n", doc.length);
  printf("Has request: %s\n", doc.has_request ? "yes" : "no");
  printf("Universe:\n");
  for (i = 0; i < doc.length; i++) {
    pkg = doc.packages[i];
    printf("  Package: %s\n", PKG_NAME(pkg));
    prop_val = cudf_pkg_property(pkg, "Foo");
    if (prop_val != NULL) {
      printf("  Depends: %s\n", prop_val);
      free(prop_val);
    }
    switch (cudf_pkg_keep(pkg)) {
    case KEEP_NONE :
      break;
    case KEEP_VERSION :
      printf("  Keep: version\n");
      break;
    case KEEP_PACKAGE :
      printf("  Keep: package\n");
      break;
    case KEEP_FEATURE :
      printf("  Keep: feature\n");
      break;
    }
    printf("\n");
  }
}
