#include <cstdint>
#include <cfloat>
#include <cstdio>
#include "base/math.hpp"
#include "base/vector.hpp"

using namespace cube;

// hit point when tracing a ray inside a bvh
struct hit {
  INLINE hit(float tmax=FLT_MAX) : t(tmax), id(~0x0u) {}
  INLINE bool ishit(void) const { return id != ~0x0u; }
  float t,u,v;
  u32 id;
};

// simple sphere
struct sphere {
  INLINE sphere(void) {}
  INLINE sphere(vec3f v, float d) : org(v), radius(d) {}
  INLINE vec3f getnormal(vec3f v) const {return (v-org)*rcp(radius);}
  INLINE float intersect(ray r) const {
    const vec3f v(org-r.org);
    const float b=dot(r.dir,v), disc=b*b-dot(v,v)+radius*radius;
    if (disc < 0.f) return FLT_MAX;
    const float d = sqrt(disc), t2=b+d, t1=b-d;
    return t2 < 0.f ? FLT_MAX : (t1 > 0.f ? t1 : t2);
  }
  vec3f org;
  float radius;
};

// screen resolution
static const u32 w = 1024;
static const u32 h = 1024;
static const float huge = 1e9f;

int main(int argc, char *argv[]) {
  FILE *f = fopen("d.ppm", "wb");
  const sphere s(vec3f(zero), 1.f);
  const camera c(vec3f(5.f,-10.f,10.f), vec3f(0.f,0.f,1.f), vec3f(0.f,1.f,0.f), 45.f, 1.f);
  const u32 dim = 16;
  vector<sphere> spheres(dim*dim*dim);
  for (u32 z = 0; z < dim; ++z)
  for (u32 y = 0; y < dim; ++y)
  for (u32 x = 0; x < dim; ++x)
    spheres[z*dim*dim+y*dim+x] = sphere(vec3f(float(x), float(y), float(z)), 0.5);

  const auto ldir = normalize(vec3f(1.f,1.f,1.f));
  fprintf(f, "P6\n%i %i\n255\n",w,h);
  for (u32 y = 0; y < h; ++y) {
    for (u32 x = 0; x < w; ++x) {
      const ray r = c.generate(w,h,x,y);
      float closest = huge;
      vec3f closestnormal = vec3f(zero);
      for (auto s : spheres) {
        const auto d = s.intersect(r);
        if (d < closest) {
          closest = d;
          closestnormal = s.getnormal(r.org+r.dir*d);
        }
      }
      if (closest == huge) {
        const u8 b[] = {0,0,0};
        fwrite(&b, 1, sizeof(b), f);
        continue;
      }
      const auto l = 255.f*clamp(abs(dot(closestnormal, ldir)), 0.f, 1.f);
      const u8 b[] = {u8(l),u8(l),u8(l)};
      fwrite(&b, 1, sizeof(b), f);
    }
  }
  fclose(f);
  return 0;
}

