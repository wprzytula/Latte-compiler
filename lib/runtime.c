#include <stdio.h>

void error() {
    printf("runtime error\n");
}

void printInt(long long i) {
    printf("%lli", i);
}

void printString(char* str, unsigned long long len) {
    printf("%.*s", (int)len, str);
}

long long readInt() {
    long long in;
    scanf("%lli", &in);
    return in;
}

unsigned long long readString(char** dst) {
    // size_t getline (char **string, size_t *n, FILE *stream);
    *dst = NULL;
    return getline(dst, NULL, stdin);
}