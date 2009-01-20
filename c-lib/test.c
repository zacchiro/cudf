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

   CUDF_LIBS="-lcudf -lpcre -lm -ldl -lpcre_stubs -lunix -lncurses -lglib-2.0"
   CUDF_PATHS="-L$(ocamlc -where) -L$(ocamlfind query pcre)"
   cc -o test `pkg-config --cflags glib-2.0` $CUDF_PATHS test.c $CUDF_LIBS
 */

#include <stdio.h>
#include <caml/callback.h>
#include <glib.h>

#include <cudf.h>

/** Print to stdout a relational operator (on versions) */
void print_relop(int relop) {
  switch (relop) {
  case RELOP_EQ : printf("=") ; break ;
  case RELOP_NEQ : printf("!=") ; break ;
  case RELOP_GEQ : printf(">=") ; break ;
  case RELOP_GT	: printf(">") ; break ;
  case RELOP_LEQ : printf("<=") ; break ;
  case RELOP_LT : printf("<") ; break ;
  case RELOP_NOP :
  default : g_error("Unexpected integer, which is not a RELOP_*: %d", relop);
  }
}

/** Print to stdout a list of package predicates, separated by a given
    separator */
void print_vpkglist(cudf_vpkglist l, const char *sep) {
  cudf_vpkg *vpkg;
  GList *last;

  last = g_list_last(l);
  while (l != NULL) {
    vpkg = g_list_nth_data(l, 0);
    printf("%s", vpkg->name);
    if (vpkg->relop) {
      printf(" ");
      print_relop(vpkg->relop);
      printf(" %d", vpkg->version);
    }
    if (l != last)
      printf(sep);
    l = g_list_next(l);
  }
}

/** Print to stdout a package formula */
void print_vpkgformula(cudf_vpkgformula fmla) {
  GList *last;

  last = g_list_last(fmla);
  while (fmla != NULL) {
    print_vpkglist(g_list_nth_data(fmla, 0), " | ");
    if (fmla != last)
      printf(", ");
    fmla = g_list_next(fmla);
  }
}

void print_keep(int keep) {
  switch (keep) {
  case KEEP_NONE : break;
  case KEEP_VERSION : printf("  Keep: version\n"); break;
  case KEEP_PACKAGE : printf("  Keep: package\n"); break;
  case KEEP_FEATURE : printf("  Keep: feature\n"); break;
  default : g_error("Unexpected \"keep\" value: %d", keep);
  }
}


int main(int argc, char **argv) {
  cudf_doc doc;
  cudf cudf, sol;
  cudf_package pkg;
  cudf_vpkglist vpkglist;
  cudf_vpkgformula fmla;
  cudf_universe univ;
  GList *l;

  caml_startup(argv);
  if (argc < 2) {
    printf("Usage: %s CUDF_FILE [ SOLUTION_FILE ]\n", argv[0]);
    exit(2);
  }

  g_message("Parsing CUDF document and do \"stuff\" on it ...");
  doc = cudf_parse_from_file(argv[1]);
  printf("Has request: %s\n", doc.has_request ? "yes" : "no");
  printf("Universe:\n");
  l = doc.packages;
  while (l != NULL) {
    pkg = * (cudf_package *) g_list_nth_data(l, 0);
    printf("  Package: %s\n", cudf_pkg_name(pkg));
    printf("  Version: %d\n", cudf_pkg_version(pkg));
    printf("  Installed: %s\n", cudf_pkg_installed(pkg) ? "true" : "false");

    fmla = cudf_pkg_depends(pkg);
    printf("  Depends: ");
    print_vpkgformula(fmla);
    printf("\n");
    cudf_free_vpkgformula(fmla);

    vpkglist = cudf_pkg_conflicts(pkg);		/* Conflicts */
    printf("  Conflicts: ");
    print_vpkglist(vpkglist, ", ");
    printf("\n");
    cudf_free_vpkglist(vpkglist);

    vpkglist = cudf_pkg_provides(pkg);		/* Provides */
    printf("  Provides: ");
    print_vpkglist(vpkglist, ", ");
    printf("\n");
    cudf_free_vpkglist(vpkglist);

    print_keep(cudf_pkg_keep(pkg));		/* Keep */
    printf("\n");

    l = g_list_next(l);
  }
  g_message("Try packages -> universe conversion ...");
  cudf_load_universe(&univ, doc.packages);
  printf("Universe size: %d/%d (installed/total)\n",
	 cudf_installed_size(univ), cudf_universe_size(univ));
  printf("Universe consistent: %s\n", cudf_is_consistent(univ) ? "yes" : "no");

  cudf_free_universe(&univ);
  cudf_free_doc(doc);

  g_message("Try direct CUDF loading ...");
  cudf = cudf_load_from_file(argv[1]);
  printf("Universe size: %d/%d (installed/total)\n",
	 cudf_installed_size(cudf.universe),
	 cudf_universe_size(cudf.universe));
  printf("Universe consistent: %s\n",
	 cudf_is_consistent(cudf.universe) ? "yes" : "no");
  if (argc >= 3) {
    g_message("Loading solution ...");
    sol = cudf_load_from_file(argv[2]);
    printf("Is solution: %s\n",
	   cudf_is_solution(cudf, sol.universe) ? "yes" : "no");
  }
  cudf_free_cudf(sol);
  cudf_free_cudf(cudf);

  return(0);
}
