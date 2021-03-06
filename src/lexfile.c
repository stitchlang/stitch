#include <errno.h>
#include <re.h>

#include <tokens.h>
#include "lex.h"

#include <stdlib.h>
#include <stdio.h>

static int lexfile(const char *text, long text_length)
{
  long offset = 0;
  while (offset < text_length) {
    enum token_kind kind = 0;
    const char *next = text + offset;
    unsigned length = lex(next, &kind);
    if (length == 0) {
      fprintf(stderr, "Error: unnown sequence: %s\n", next);
      return 1;
    }
    if (kind == TOKEN_KIND_NEWLINE) {
      printf("NEWLINE\n");
    } else {
      printf("%s(%u): %.*s\n", token_name_table[kind], length, length, next);
    }
    {
      enum token_kind next_kind = kind + 1;
      if (lex(next, &next_kind)) {
        fprintf(stderr, "Error: text '%.*s' matched %s and %s\n", length, next,
               token_name_table[kind], token_name_table[next_kind]);
        return 1;
      }
    }
    offset += length;
  }
  return 0;
}

int main(int argc, char *argv[])
{
  if (argc <= 1) {
    printf("Usage: lexfile FILE\n");
    return 1;
  }
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
  char *buffer = malloc(filesize+1);
  if (!buffer) {
    fprintf(stderr, "Error: malloc %ld failed, errno=%d\n", filesize+1, errno);
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
  buffer[filesize] = 0;
  return lexfile(buffer, filesize);
}
