#ifndef __VECTOR_H__
#define __VECTOR_H__

#include <stdio.h>

typedef struct _Vector {
    int size;
    float* data;
} Vector;

Vector* vector_new(int size);

void vector_delete(Vector* v);

Vector* vector_clone(Vector* v);

void vector_print(FILE* f, Vector* v);

#endif