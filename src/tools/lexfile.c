#include <stddef.h>
#include <errno.h>
#include <re.h>

#include <tokens.h>
#include "lex.h"

#include <stdlib.h>
#include <stdio.h>

static int lexfile(const char *text, const char *limit)
{
  while (text < limit) {
    enum token_kind kind = 0;
    unsigned length = lex(text, limit, &kind);
    if (length == 0) {
      fprintf(stderr, "Error: unnown sequence: %s\n", text);
      return 1;
    }
    if (kind == TOKEN_KIND_NEWLINE) {
      printf("NEWLINE\n");
    } else {
      printf("%s(%u): %.*s\n", token_name_table[kind], length, length, text);
    }
    {
      enum token_kind next_kind = kind + 1;
      if (lex(text, limit, &next_kind)) {
        fprintf(stderr, "Error: text '%.*s' matched %s and %s\n", length, text,
               token_name_table[kind], token_name_table[next_kind]);
        return 1;
      }
    }
    text += length;
  }
  return 0;
}

int main(int argc, char *argv[])
{
  if (argc <= 1) {
    printf("Usage: lexfile FILE\n");
    return 1;
  }
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  // TODO: use mmap!!
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  const char *filename = argv[1];
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error: failed to open '%s'\n", filename);
    return 1;
  }
  if (0 != fseek(file, 0, SEEK_END)) {
    fprintf(stderr, "Error: fseek END failed, errno=%d\n", errno);
    return 1;
  }
  long filesize = ftell(file);
  if (filesize == -1) {
    fprintf(stderr, "Error: ftell failed, errno=%d\n", errno);
    return 1;
  }
  char *buffer = malloc(filesize);
  if (!buffer) {
    fprintf(stderr, "Error: malloc %ld failed, errno=%d\n", filesize, errno);
    return 1;
  }
  if (0 != fseek(file, 0, SEEK_SET)) {
    fprintf(stderr, "Error: fseek START failed, errno=%d\n", errno);
    return 1;
  }
  size_t read = fread(buffer, 1, filesize, file);
  if (read != filesize) {
    fprintf(stderr, "Error: fread %ld returned %llu, errno=%d\n", filesize, (unsigned long long)read, errno);
    return 1;
  }
  return lexfile(buffer, buffer + filesize);
}
