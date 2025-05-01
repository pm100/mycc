#include <stdio.h>

#define typename(x) _Generic((x),        /* Get the name of a type */             \
                                                                                  \
        _Bool: "_Bool",                  unsigned char: "unsigned char",          \
         char: "char",                     signed char: "signed char",            \
    short int: "short int",         unsigned short int: "unsigned short int",     \
          int: "int",                     unsigned int: "unsigned int",           \
     long int: "long int",           unsigned long int: "unsigned long int",      \
int*[3]: "long long int", unsigned long long int: "unsigned long long int", \
        float: "float",                         double: "double",                 \
  long double: "long double",                   char *: "pointer to char",        \
       void *: "pointer to void",                int *: "pointer to int",         \
      default: "other")
#define typeof(x) #x
      int main(){
       // int x;
        int x[4][3] = { {1,2,3}, { 4, 5, 6 }, { 7, 8, 9 }, { 10, 11, 12 } };
        int **y=x;
        int k = y[1][2];
        printf("%s\n", *(x+0)); // prints "int"
      }