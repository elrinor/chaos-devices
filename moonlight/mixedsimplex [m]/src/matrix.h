#ifndef __MATRIX_H__
#define __MATRIX_H__


typedef struct _Matrix {
    int rows;
    int cols;
    float** data;
} Matrix;

Matrix* matrix_new(int rows, int cols);

void matrix_delete(Matrix* m);

void matrix_expandrows(Matrix* m, int newrows);

void matrix_print(Matrix* m);

Matrix* matrix_clone(Matrix* m);

#endif