#include <re.h>

#include <tokens.h>
#include "lex.h"

#include <stdlib.h>
#include <stdio.h>

static void test(const char *text)
{
  printf("test '%s'....", text);
  fflush(stdout);
  enum token_kind kind = 0;
  unsigned length = lex(text, &kind);
  if (length == 0) {
    printf("NO MATCH!\n");
  } else {
    // TODO I can test length against strlen(text)
    printf("%s\n", token_name_table[kind]);

    enum token_kind next_kind = kind + 1;
    if (lex(text, &next_kind)) {
      printf("ERROR: text '$s' matched %s and %s\n", token_name_table[kind], token_name_table[next_kind]);
      exit(1);
    }
  }
}

int main(int argc, char *argv[])
{
  printf("There are %d patterns\n", TOKEN_COUNT);
  printf("--------------------------------------------------------------------------------\n");
  for (unsigned i = 0; i < TOKEN_COUNT; i++) {
    printf("%d: %s: %s\n", i, token_name_table[i], token_pattern_table[i]);
  }
  printf("--------------------------------------------------------------------------------\n");

  test(" ");
  test("(");
  test(")");
  test("@@");
  test("@$");
  test("@echo");
  test("a");
  test("#");
  test("#hello");
}
