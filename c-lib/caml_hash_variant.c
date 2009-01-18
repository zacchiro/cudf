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

#include <caml/mlvalues.h>
#include <stdio.h>

int main(int argc, char **argv)
{
  int i;

  if (argc < 2) {
    printf("Usage: hash_variant VARIANT...\n");
    exit(2);
  }
  
  for (i = 1; i < argc; i++) {
    printf("#define\tMLPVAR_%s\t(%d)\t/* caml hash for \"%s\" */\n",
	   argv[i], caml_hash_variant(argv[i]), argv[i]);
  }

  return 0;
}
