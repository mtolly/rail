/***
 * Rail-to-C compiler runtime
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STR 0
#define NIL 1
#define PAIR 2

/***
 * Types and globals
 */

struct pair {
  struct value *car;
  struct value *cdr;
};

union uvalue {
  char *str;
  struct pair *pair;
};

struct value {
  int type;
  union uvalue *uvalue; // if value is NIL, uvalue is undefined
  int references;
};

struct stack_node {
  struct value *value;
  struct stack_node *next;
};

struct stack_node *stack = NULL;
int stack_size = 0;
int condition = 0;

/***
 * Pushing and creating values
 */

void push(struct value *v) {
  if (v == NULL) {
    printf("push: tried to push null pointer\n");
    exit(0);
  }
  struct stack_node *node = malloc(sizeof(struct stack_node));
  node->value = v;
  (v->references)++;
  node->next = stack;
  stack = node;
  stack_size++;
}

// Constructs a new value containing a string. Note that when the value is
// garbage collected, the string pointer will be freed.
struct value *new_str(char *str) {
  union uvalue *uv = malloc(sizeof(union uvalue));
  uv->str = str;
  struct value *v = malloc(sizeof(struct value));
  v->type = STR;
  v->uvalue = uv;
  v->references = 0;
  return v;
}

// Makes a new copy of a string, and creates a value from that.
struct value *new_str_copy(char *str) {
  int len = strlen(str);
  char *buf = malloc(len + 1);
  strncpy(buf, str, len + 1);
  return new_str(buf);
}

struct value *new_int(int i) {
  char *s = malloc(20 * sizeof(char));
  sprintf(s, "%i", i);
  return new_str(s);
}

struct pair *make_pair(struct value *car, struct value *cdr) {
  struct pair *p = malloc(sizeof(struct pair));
  p->car = car;
  p->cdr = cdr;
  (car->references)++;
  (cdr->references)++;
  return p;
}

struct value *new_pair(struct pair *p) {
  union uvalue *uv = malloc(sizeof(union uvalue));
  uv->pair = p;
  struct value *v = malloc(sizeof(struct value));
  v->type = PAIR;
  v->uvalue = uv;
  v->references = 0;
  return v;
}

struct value *new_nil() {
  struct value *v = malloc(sizeof(struct value));
  v->type = NIL;
  v->uvalue = NULL;
  v->references = 0;
  return v;
}

/***
 * Popping and deconstructing values
 */

// If the returned value's references is 0, caller is responsible for freeing.
struct value *pop() {
  if (stack == NULL) {
    printf("pop: empty stack\n");
    exit(0);
  }
  struct stack_node *node = stack;
  stack = node->next;
  struct value *v = node->value;
  free(node);
  (v->references)--;
  return v;
}

char *get_str(struct value *v) {
  if (v->type != STR) {
    printf("get_str: not a string\n");
    exit(0);
  }
  return v->uvalue->str;
}

int get_int(struct value *v) {
  int i;
  int scanned = sscanf(get_str(v), "%i", &i);
  if (scanned != 1) {
    printf("get_int: could not parse int\n");
    exit(0);
  }
  return i;
}

struct pair *get_pair(struct value *v) {
  if (v->type != PAIR) {
    printf("get_pair: not a pair\n");
    exit(0);
  }
  return v->uvalue->pair;
}

// Checks reference count and possibly frees the value. If the value is a pair,
// decrements the car and cdr's reference counts and checks those as well.
void collect(struct value *v) {
  if (v != NULL && v->references == 0) {
    union uvalue *uv = v->uvalue;
    if (v->type == STR) {
      free(uv->str);
    }
    else if (v->type == PAIR) {
      struct pair *pr = uv->pair;
      (pr->car->references)--;
      collect(pr->car);
      (pr->cdr->references)--;
      collect(pr->cdr);
      free(pr);
    }
    free(uv);
    free(v);
  }
}

// Performs garbage collection on the popped value.
int pop_int() {
  struct value *v = pop();
  int i = get_int(v);
  collect(v);
  return i;
}

/***
 * Built-in operations
 */

void builtin_boom() {
  char *s = get_str(pop());
  printf(s);
  // Obviously no need to garbage collect
  exit(0);
}

void builtin_eof() {
  push(new_int(feof(stdin) ? 1 : 0));
}

void builtin_input() {
  if (feof(stdin)) {
    printf("input: end of file\n");
    exit(0);
  }
  char s[2];
  s[0] = getchar();
  s[1] = '\0';
  push(new_str_copy(s));
}

void builtin_output() {
  struct value *v = pop();
  char *s = get_str(v);
  printf(s);
  collect(v);
}

void builtin_underflow() {
  push(new_int(stack_size));
}

void builtin_type() {
  struct value *v = pop();
  switch (pop()->type) {
    case STR:
      push(new_str_copy("string"));
      break;
    case PAIR:
      push(new_str_copy("list"));
      break;
    case NIL:
      push(new_str_copy("nil"));
      break;
    // TODO: lambda
  }
  collect(v);
}

void builtin_add() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a + b));
}

void builtin_div() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a / b));
}

void builtin_mult() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a * b));
}

void builtin_rem() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a % b));
}

void builtin_sub() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a - b));
}

void builtin_cut() {
  int len0 = pop_int();
  struct value *v = pop();
  char *s = get_str(v);
  int len = strlen(s);
  int len1 = len - len0;
  if (len0 < 0 || len1 < 0) {
    printf("cut: index out of bounds\n");
    exit(0);
  }
  char *a = malloc(len0 * sizeof(char));
  char *b = malloc(len1 * sizeof(char));
  strncpy(a, s, len0);
  a[len0] = '\0';
  strncpy(b, s + len0, len1);
  b[len1] = '\0';
  push(new_str(a));
  push(new_str(b));
}

void builtin_append() {
  struct value *x, *y;
  x = pop();
  y = pop();
  char *b = get_str(x);
  char *a = get_str(y);
  int lena = strlen(a);
  int lenb = strlen(b);
  char *s = malloc((lena + lenb + 1) * sizeof(char));
  strcpy(s, a);
  strcpy(s + lena, b);
  push(new_str(s));
  collect(x);
  collect(y);
}

void builtin_size() {
  struct value *v = pop();
  char *s = get_str(v);
  push(new_int(strlen(s)));
  collect(v);
}

void builtin_cons() {
  struct value *x, *y;
  x = pop();
  y = pop();
  struct value *v = new_pair(make_pair(x, y));
  // No need to check for garbage collection
  push(v);
}

void builtin_uncons() {
  struct value *v = pop();
  struct pair *p = get_pair(v);
  push(p->cdr);
  push(p->car);
  collect(v);
}

void builtin_greater() {
  int b = pop_int();
  int a = pop_int();
  push(new_int(a > b));
}

int equal(struct value *x, struct value *y) {
  if (x->type != y->type) {
    return 0;
  }
  if (x->type == STR) {
    return strcmp(x->uvalue->str, y->uvalue->str) == 0;
  }
  if (x->type == PAIR) {
    struct pair *xp = x->uvalue->pair;
    struct pair *yp = y->uvalue->pair;
    return equal(xp->car, yp->car) && equal(xp->cdr, yp->cdr);
  }
  if (x->type == NIL) {
    return 1;
  }
  // TODO: lambda
  return 0;
}

void builtin_equal() {
  struct value *x, *y;
  x = pop();
  y = pop();
  push(new_int(equal(x, y)));
  collect(x);
  collect(y);
}

void builtin_setbranch() {
  int i = pop_int();
  if (i == 0 || i == 1) {
    condition = i;
  }
  else {
    printf("builtin_setbranch: not 0 or 1\n");
    exit(0);
  }
}
