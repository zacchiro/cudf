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
  cudf_preamble preamble;
  GList *packages;	      /* List of packages */
  int has_request;	      /* Whether user request was provided or not */
  int has_preamble;	    
  cudf_request request;	  /* User request (meaningful iff has_request != 0) */
} cudf_doc;

typedef struct cudf {
  cudf_preamble preamble;
  cudf_universe universe;	/* Abstract package universe */
  int has_request;	      /* Whether user request was provided or not */
  int has_preamble;	     
  cudf_request request;	  /* User request (meaningful iff has_request != 0) */
} cudf;

cudf_doc cudf_parse_from_file(char *fname);
cudf cudf_load_from_file(char *fname);

/** Examples:
    - foo >= 2	--->	{ name="foo" ; relop=RELOP_GEQ ; version = 2 }
    - bar	--->	{ name="bar" ; relop=0 ; version = UNSPECIFIED }
*/
typedef struct cudf_vpkg {
  char *name;	/* Package name */
  int relop;	/* Version constraint operator, see RELOP_* constants.
		   0 (i.e. RELOP_NOP) means no constraint */
  int version;	/* Version constraint value. Meaningful only if constr != 0 */
} cudf_vpkg;

typedef GList *cudf_vpkglist;	/* List of cudf_vpkg */

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

/** Macros for accessing cudf_package values */

#define cudf_pkg_name(p)	(String_val(Field(p, 0)))	/* char *  */
#define cudf_pkg_version(p)	(Int_val(Field(p, 1)))	/* int */
#define cudf_pkg_installed(p)	(Int_val(Field(p, 5)))	/* int (i.e., bool) */

/** Possible values returned by PKG_EXTRA */

#define KEEP_NONE	0	/* no "Keep" property */
#define KEEP_VERSION	1	/* "Keep: version" */
#define	KEEP_PACKAGE	2	/* "Keep: package" */
#define	KEEP_FEATURE	3	/* "Keep: feature" */

int cudf_pkg_keep(cudf_package pkg);	/* "Keep" prop. See KEEP_* macros */
cudf_vpkgformula cudf_pkg_depends(cudf_package pkg);	/* "Depends" prop. */
cudf_vpkglist cudf_pkg_conflicts(cudf_package pkg);	/* "Conflicts" prop. */
cudf_vpkglist cudf_pkg_provides(cudf_package pkg);	/* "Provides" prop. */

/* Lookup package property by name. Returned string should be manually freed.
   Return NULL if the property is missing (and had no default value). */
char *cudf_pkg_property(cudf_package pkg, const char *prop);

/* Lookup request property by name. Returned string should be manually freed.
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

