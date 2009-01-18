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
