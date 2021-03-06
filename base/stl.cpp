#include "stl.hpp"
#include <cstdio>

namespace cube {
static int islittleendian_ = 1;
void initendiancheck(void) { islittleendian_ = *((char *)&islittleendian_); }
int islittleendian(void) { return islittleendian_; }

char *newstring(const char *s, size_t l, const char *filename, int linenum) {
  char *b = (char*) memalloc(l+1, filename, linenum);
  strncpy(b,s,l);
  b[l] = 0;
  return b;
}
char *newstring(const char *s, const char *filename, int linenum) {
  return newstring(s, strlen(s), filename, linenum);
}
char *newstringbuf(const char *s, const char *filename, int linenum) {
  return newstring(s, _MAXDEFSTR-1, filename, linenum);
}
void formatstring(char *d, const char *fmt, va_list v) {
  _vsnprintf(d, _MAXDEFSTR, fmt, v);
  d[_MAXDEFSTR-1] = 0;
}
void sprintf_s_f::operator()(const char* fmt, ...) {
  va_list v;
  va_start(v, fmt);
  _vsnprintf(d, _MAXDEFSTR, fmt, v);
  va_end(v);
  d[_MAXDEFSTR-1] = 0;
}

char* tokenize(char *s1, const char *s2, char **lasts) {
 char *ret;
 if (s1 == NULL)
   s1 = *lasts;
 while(*s1 && strchr(s2, *s1))
   ++s1;
 if(*s1 == '\0')
   return NULL;
 ret = s1;
 while(*s1 && !strchr(s2, *s1))
   ++s1;
 if(*s1)
   *s1++ = '\0';
 *lasts = s1;
 return ret;
}

bool strequal(const char *s1, const char *s2) {
  if (strcmp(s1, s2) == 0) return true;
  return false;
}

bool contains(const char *haystack, const char *needle) {
  if (strstr(haystack, needle) == NULL) return false;
  return true;
}


char *path(char *s) {
  for (char *t = s; (t = strpbrk(t, "/\\")) != 0; *t++ = PATHDIV);
  return s;
}

char *loadfile(char *fn, int *size) {
  FILE *f = fopen(fn, "rb");
  if (!f) return NULL;
  fseek(f, 0, SEEK_END);
  unsigned int len = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *buf = (char *)MALLOC(len+1);
  if (!buf) return NULL;
  buf[len] = 0;
  size_t rlen = fread(buf, 1, len, f);
  fclose(f);
  if (len!=rlen || len<=0) {
    FREE(buf);
    return NULL;
  }
  if (size!=NULL) *size = len;
  return buf;
}

void endianswap(void *memory, int stride, int length) {
  if (*((char *)&stride)) return;
  loop(w, length) loop(i, stride/2) {
    u8 *p = (u8 *)memory+w*stride;
    u8 t = p[i];
    p[i] = p[stride-i-1];
    p[stride-i-1] = t;
  }
}

intrusive_list_base::intrusive_list_base(void) : m_root() {}
intrusive_list_base::size_type intrusive_list_base::size(void) const {
  size_type numnodes(0);
  const intrusive_list_node* iter = &m_root;
  do {
    iter = iter->next;
    ++numnodes;
  } while (iter != &m_root);
  return numnodes - 1;
}
void append(intrusive_list_node *node, intrusive_list_node *prev) {
  ASSERT(!node->in_list());
  node->next = prev->next;
  node->next->prev = node;
  prev->next = node;
  node->prev = prev;
}
void prepend(intrusive_list_node *node, intrusive_list_node *next) {
  ASSERT(!node->in_list());
  node->prev = next->prev;
  node->prev->next = node;
  next->prev = node;
  node->next = next;
}
void link(intrusive_list_node* node, intrusive_list_node* nextNode) {
  prepend(node, nextNode);
}
void unlink(intrusive_list_node* node) {
  ASSERT(node->in_list());
  node->prev->next = node->next;
  node->next->prev = node->prev;
  node->next = node->prev = node;
}
} // namespace cube

