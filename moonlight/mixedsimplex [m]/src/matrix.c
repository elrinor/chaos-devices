#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "matrix.h"

Matrix* matrix_new(int rows, int cols) {
    int i;
    Matrix* m = (Matrix*) malloc(sizeof(Matrix));
    m->cols = cols;
    m->rows = rows;
    m->data = malloc(sizeof(float*) * rows);
    for(i = 0; i < rows; i++)
        m->data[i] = malloc(sizeof(float) * cols);
    return m;
}

void matrix_delete(Matrix* m) {
    int i;
    
    assert(m != NULL);
    
    for(i = 0; i < m->rows; i++)
        free(m->data[i]);
    free(m->data);
    free(m);
}


void matrix_expandrows(Matrix* m, int newrows) {
    int i;

    assert(m != NULL && m->rows <= newrows);

    if(m->rows == newrows)
        return;

    m->data = realloc(m->data, newrows * sizeof(float*));
    for(i = m->rows; i < newrows; i++)
        m->data[i] = malloc(sizeof(float) * m->cols);
    m->rows = newrows;
}

void matrix_print(Matrix* m) {
    int r, c;
    for(r = 0; r < m->rows; r++) {
        for(c = 0; c < m->cols; c++)
            printf("% 6.2f", m->data[r][c]);
        printf("\n");
    }
}


Matrix* matrix_clone(Matrix* m) {
    int r, c;
    Matrix* result = matrix_new(m->rows, m->cols);
    for(r = 0; r < m->rows; r++)
        for(c = 0; c < m->cols; c++)
            result->data[r][c] = m->data[r][c];
    return result;
}