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

   cc -o test test.c `pkg-config --cflags cudf` `pkg-config --cflags cudf`
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
	default :
		g_error("Unexpected integer, which is not a RELOP_*: %d",
			relop);
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

void print_preamble(cudf_preamble pre) {
	printf("  %s: %s\n", "preamble", cudf_pre_property(pre, "preamble"));
	printf("  %s: %s\n", "property", cudf_pre_property(pre, "property"));
	printf("  %s: %s\n", "univ-checksum",
	       cudf_pre_property(pre, "univ-checksum"));
	printf("  %s: %s\n", "status-checksum",
	       cudf_pre_property(pre, "status-checksum"));
	printf("  %s: %s\n", "req-checksum",
	       cudf_pre_property(pre, "req-checksum"));
}

void print_request(cudf_request req) {
	printf("  %s: %s\n", "request", cudf_req_property(req, "request"));
	printf("  %s: %s\n", "install", cudf_req_property(req, "install"));
	printf("  %s: %s\n", "remove", cudf_req_property(req, "remove"));
	printf("  %s: %s\n", "upgrade", cudf_req_property(req, "upgrade"));
}

void print_keep(int keep) {
	switch (keep) {
	case KEEP_NONE : printf("  keep: version\n"); break;
	case KEEP_VERSION : printf("  keep: version\n"); break;
	case KEEP_PACKAGE : printf("  keep: package\n"); break;
	case KEEP_FEATURE : printf("  keep: feature\n"); break;
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

	g_message("Parsing CUDF document %s ...", argv[1]);
	doc = cudf_parse_from_file(argv[1]);
	printf("Has preamble: %s\n", doc.has_preamble ? "yes" : "no");
	if (doc.has_preamble) {
		printf("Preamble: \n");
		print_preamble(doc.preamble);
		printf("\n");
	}
	printf("Has request: %s\n", doc.has_request ? "yes" : "no");
	if (doc.has_request) {
		printf("Request: \n");
		print_request(doc.request);
		printf("\n");
	}
	printf("Universe:\n");
	l = doc.packages;
	while (l != NULL) {
		pkg = * (cudf_package *) g_list_nth_data(l, 0);
		printf("  package: %s\n", cudf_pkg_name(pkg));
		printf("  version: %d\n", cudf_pkg_version(pkg));
		printf("  installed: %s\n", cudf_pkg_installed(pkg) ?
		       "true" : "false");
		printf("  was-installed: %s\n", cudf_pkg_was_installed(pkg) ?
		       "true" : "false");

		fmla = cudf_pkg_depends(pkg);
		printf("  depends: ");
		print_vpkgformula(fmla);
		printf("\n");
		cudf_free_vpkgformula(fmla);

		vpkglist = cudf_pkg_conflicts(pkg);		/* Conflicts */
		printf("  conflicts: ");
		print_vpkglist(vpkglist, ", ");
		printf("\n");
		cudf_free_vpkglist(vpkglist);

		vpkglist = cudf_pkg_provides(pkg);		/* Provides */
		printf("  provides: ");
		print_vpkglist(vpkglist, ", ");
		printf("\n");
		cudf_free_vpkglist(vpkglist);

		print_keep(cudf_pkg_keep(pkg));			/* Keep */
		printf("\n");

		l = g_list_next(l);
	}
	g_message("Try packages -> universe conversion ...");
	cudf_load_universe(&univ, doc.packages);
	printf("Universe size: %d/%d (installed/total)\n",
	       cudf_installed_size(univ), cudf_universe_size(univ));
	printf("Universe consistent: %s\n", cudf_is_consistent(univ) ?
	       "yes" : "no");

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
		g_message("Loading solution %s ...", argv[2]);
		sol = cudf_load_from_file(argv[2]);
		printf("Is solution: %s\n",
		       cudf_is_solution(cudf, sol.universe) ? "yes" : "no");
	}
	g_message("Freeing memory ...");
	cudf_free_cudf(sol);
	cudf_free_cudf(cudf);
	g_message("All done.");

	return(0);
}
