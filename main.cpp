#include "obj.hpp"
#include <cstdio>

int main(void) {
  using namespace cube;
  obj o;
  o.load("data/conference.obj");
  fprintf(stderr, "\n%d triangles\n%d vertices\n%d materials\n",
    o.trinum, o.vertnum, o.matnum);
}

