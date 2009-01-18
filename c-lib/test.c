#include <stdio.h>
#include <caml/callback.h>

#include <cudf.h>

int main(int argc, char **argv)
{
  cudf_doc doc;
  package_t pkg;
  int i;

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
