#include <stdio.h>
#include <caml/callback.h>

#include <cudf.h>

int main(int argc, char **argv)
{
  caml_startup(argv);
  if (argc < 2) {
    printf("Usage: %s CUDF_FILE\n", argv[0]);
    exit(2);
  }
}
