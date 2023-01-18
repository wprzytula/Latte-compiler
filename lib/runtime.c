#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error() {
    printf("runtime error\n");
    exit(1);
}

void printInt(long long i) {
    printf("%lli\n", i);
}

void printString(char* str) {
    // fprintf(stderr, "printString arg: %p\n", str);
    unsigned long long const len = *(unsigned long long*)str;
    str += sizeof(unsigned long long);
    printf("%.*s\n", (int)len, str);
}

long long readInt() {
    long long in;
    scanf("%lli", &in);
    int i = getchar();
    if (i != '\n') {
        fprintf(stderr, "RUNTIME: int not followed by newline, nonsupported case. Terminating.\n");
        exit(1);
    }
    return in;
}

char* readString() {
    // size_t getline (char **string, size_t *n, FILE *stream);
    char *dst = NULL;
    size_t buf_len = 0;
    long long len = getline(&dst, &buf_len, stdin);
    if (len < 0) {
        fprintf(stderr, "RUNTIME: getline returned %lli; terminating.\n", len);
        exit(1);
    }
    // fprintf(stderr, "Read len bytes: %lli\n", len);
    dst = realloc(dst, len + sizeof(long long));
    memmove(dst + sizeof(long long), dst, len);
    *(unsigned long long*)dst = len - 1; // drop newline
    return dst;
}

char* __new(unsigned long long len) {
    // fprintf(stderr, "Allocating space for len %lli\n", len);
    char *const str = malloc(len);
    if (!str) {
        fprintf(stderr, "RUNTIME: malloc returned NULL; terminating.\n");
        exit(1);
    }
    return str;
}

char* __concat_strings(char const*const restrict str1, char const*const restrict str2) {
    // fprintf(stderr, "Concatenating strings:\n");
    unsigned long long const len1 = *(unsigned long long*)str1;
    // fprintf(stderr, "str1: len %lli, contents: \"%.*s\"\n", len1, (int)len1, str1 + sizeof(unsigned long long));
    unsigned long long const len2 = *(unsigned long long*)str2;
    // fprintf(stderr, "str2: len %lli, contents: \"%.*s\"\n", len2, (int)len2, str2 + sizeof(unsigned long long));
    unsigned long long const len3 = len1 + len2;
    char *const str3 = __new(len3 + sizeof(long long));
    *(unsigned long long*)str3 = len3;
    char* str3_writer = str3 + sizeof(unsigned long long);
    memcpy(str3_writer, str1 + sizeof(unsigned long long), len1);
    str3_writer += len1;
    memcpy(str3_writer, str2 + sizeof(unsigned long long), len2);
    // fprintf(stderr, "str3: len %lli, contents: \"%.*s\"\n", len3, (int)len3, str3 + sizeof(unsigned long long));
    return str3;
}
