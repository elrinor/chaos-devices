#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "vector.h"

Vector* vector_new(int size) {
    Vector* v = (Vector*) malloc(sizeof(Vector));
    v->size = size;
    v->data = (float*) malloc(sizeof(float) * size);
    return v;
}

void vector_delete(Vector* v) {
    assert(v != NULL);
    free(v->data);
    free(v);
}

Vector* vector_clone(Vector* v) {
    int i;
    Vector* result = vector_new(v->size);
    for(i = 0; i < v->size; i++) 
        result->data[i] = v->data[i];
    return result;
}

void vector_print(FILE* f, Vector* v) {
    int i;
    assert(v != NULL);
    for(i = 0; i < v->size; i++)
        fprintf(f, "% 6.2f", v->data[i]);
}

