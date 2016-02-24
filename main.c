#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int our_code_starts_here() asm("our_code_starts_here");
extern void error() asm("error");
extern int print(int val) asm("print");

int print(int val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%d\n", val >> 1);
  }
  else if(val == 0xFFFFFFFF) {
    printf("true\n");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false\n");
  }
  else {
    printf("Unknown value: %#010x\n", val);
  }
  return val;
}

void error(int i) {
  if (i == 0) {
    fprintf(stderr, "Error: comparison operator got boolean");
  }
  else if (i == 1) {
    fprintf(stderr, "Error: arithmetic operator got boolean");
  }
  else if (i == 2) {
    fprintf(stderr, "Error: if condition got number");
  }
  else if (i == 3) {
    fprintf(stderr, "Error: Integer overflow");
  }
  else {
    fprintf(stderr, "Error: Unknown error code: %d\n", i);
  }
  exit(i);
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
