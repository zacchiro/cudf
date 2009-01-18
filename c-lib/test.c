#include <stdio.h>
#include <caml/callback.h>

#include <cudf.h>

int main(int argc, char **argv)
{
  cudf_doc doc;
  int i;

  caml_startup(argv);
  if (argc < 2) {
    printf("Usage: %s CUDF_FILE\n", argv[0]);
    exit(2);
  }

  doc = cudf_parse_from_file(argv[1]);
  printf("Universe size: %d\n", doc.length);
  printf("Has request: %s\n", doc.has_request ? "yes" : "no");
  for (i = 0; i < doc.length; i++) {
    printf("  Package: %s\n", PKG_NAME(doc.packages[i]));
  }
}
