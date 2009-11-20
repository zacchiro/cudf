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

#include <glib.h>
#include <caml/mlvalues.h>

typedef value cudf_package;
typedef value cudf_universe;
typedef value cudf_request;
typedef value cudf_preamble;

typedef struct cudf_doc {
	int has_preamble;	/* Whether user request was provided or not */
	int has_request;	/* Whether request was provided or not */
	cudf_preamble preamble;	/* Preamble (iff has_preamble != 0) */
	cudf_request request;	/* User request (iff has_request != 0) */
	GList *packages;	/* List of packages */
} cudf_doc;

typedef struct cudf {
	int has_preamble;	/* Whether user request was provided or not */
	int has_request;	/* Whether request was provided or not */
	cudf_preamble preamble;	/* Preamble (iff has_preamble != 0) */
	cudf_request request;	/* User request (iff has_request != 0) */
	cudf_universe universe; /* Abstract package universe */
} cudf;

cudf_doc cudf_parse_from_file(char *fname);
cudf cudf_load_from_file(char *fname);

/** Package predicate

    Examples:
    - bar	--->	{ name="bar" ; relop=0 ; version = UNSPECIFIED }
    - foo >= 2	--->	{ name="foo" ; relop=RELOP_GEQ ; version = 2 }
*/
typedef struct cudf_vpkg {
	char *name;	/* Package name */
	int relop;	/* Version constraint operator, see RELOP_* constants.
			   0 (i.e. RELOP_NOP) means no constraint */
	int version;	/* Version constraint value (iff constr != 0) */
} cudf_vpkg;

typedef GList *cudf_vpkglist;		/* List of cudf_vpkg */

/* List of cudf_vpkg lists.
   CNF encoding: the inner lists are OR-ed, while the outer are AND-ed */
typedef GList *cudf_vpkgformula;

#define RELOP_EQ	1
#define RELOP_NEQ	2
#define RELOP_GEQ	3
#define RELOP_GT	4
#define RELOP_LEQ	5
#define RELOP_LT	6
#define RELOP_NOP	0	/* 0 can be used safely instead */

#define TYPE_INT		1
#define TYPE_POSINT		2
#define TYPE_NAT		3
#define TYPE_BOOL		4
#define TYPE_STRING		5
#define TYPE_ENUM		6
#define TYPE_PKGNAME		7
#define TYPE_IDENT		8
#define TYPE_VPKG		9
#define TYPE_VPKGFORMULA	10
#define TYPE_VPKGLIST		11
#define TYPE_VEQPKG		12
#define TYPE_VEQPKGLIST		13
#define TYPE_TYPEDECL		14
#define TYPE_NOTYPE		0


/** Typed CUDF value */
typedef struct cudf_value {
	int typ;	/* CUDF type, one of the TYPE_* constants */
	union {
		int i;
		char *s;
		cudf_vpkg pkg;
		cudf_vpkgformula f;
		cudf_vpkglist pkgs;
		/* cudf_typedecl types;	/\* currently not supported *\/ */
	} val;
} cudf_value;

/** Macros for accessing cudf_package values */

#define cudf_pkg_name(p)	(String_val(Field(p, 0)))	/* (char *) */
#define cudf_pkg_version(p)	(Int_val(Field(p, 1)))		/* int */
#define cudf_pkg_installed(p)	(Int_val(Field(p, 5)))		/* int (/bool) */
#define cudf_pkg_was_installed(p)	(Int_val(Field(p, 6)))	/* int (/bool) */

/** Possible values returned by PKG_EXTRA */

#define KEEP_NONE	0	/* Keep: none */
#define KEEP_VERSION	1	/* Keep: version */
#define	KEEP_PACKAGE	2	/* Keep: package */
#define	KEEP_FEATURE	3	/* Keep: feature */

int cudf_pkg_keep(cudf_package pkg);	/* "Keep" prop. See KEEP_* macros */
cudf_vpkgformula cudf_pkg_depends(cudf_package pkg);	/* "Depends" prop. */
cudf_vpkglist cudf_pkg_conflicts(cudf_package pkg);	/* "Conflicts" prop. */
cudf_vpkglist cudf_pkg_provides(cudf_package pkg);	/* "Provides" prop. */


/** Return a hashtable mapping property names (string) to typed values
    (cudf_value) */
GHashTable *cudf_pkg_extra(cudf_package pkg);		/* Extra properties */

/** Lookup package property by name. Returned string should be manually freed.
    Return NULL if the property is missing (and had no default value). */
char *cudf_pkg_property(cudf_package pkg, const char *prop);

/** Lookup request property by name. Returned string should be manually freed.
    Return NULL if the property is missing (and had no default value). */
char *cudf_req_property(cudf_request req, const char *prop);


/** Universe management */

/** @param univ pointer to a cudf_universe which will be filled. After
    use the universe should be freed using cudf_free_universe.
    @param packages list of (pointers to) cudf_package-s; the packages
    member of a cudf_doc structure is a suitable value */
void cudf_load_universe(cudf_universe *univ, GList *packages);

int cudf_universe_size(cudf_universe univ);
int cudf_installed_size(cudf_universe univ);
int cudf_is_consistent(cudf_universe univ);
int cudf_is_solution(cudf cudf, cudf_universe solution);


/** Memory management */

void cudf_free_doc(cudf_doc doc);
void cudf_free_cudf(cudf cudf);
void cudf_free_universe(cudf_universe *univ);
void cudf_free_vpkglist(cudf_vpkglist l);
void cudf_free_vpkgformula(cudf_vpkgformula fmla);


#endif	/* end of cudf.h */

