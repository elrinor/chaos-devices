#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "problem.h"
#include "simplex.h"

Problem* problem_new(int m, int n, int k) {
    Problem* result = (Problem*) malloc(sizeof(Problem));
    result->k = k;
    result->m = m;
    result->n = n;

    result->matrix = matrix_new(k + 1, n + m + 1);
    result->solution = vector_new(n + m);

    return result;
}

void problem_delete(Problem* p) {
    assert(p != NULL);
    matrix_delete(p->matrix);
    vector_delete(p->solution);
    free(p);
}

int problem_solve(Problem* p) {
    Matrix* m = matrix_clone(p->matrix);
    int result = simplex(m, &p->maxvalue, p->solution);
    matrix_delete(m);
    return result;
}

Problem* problem_clone(Problem* p) {
    Problem* result = (Problem*) malloc(sizeof(Problem));
    result->k = p->k;
    result->m = p->m;
    result->n = p->n;

    result->matrix = matrix_clone(p->matrix);
    result->solution =  vector_clone(p->solution);

    result->effectiveness = p->effectiveness;
    result->maxvalue = p->maxvalue;

    return result;    
}

int problem_print(FILE* f, Problem* p) {
    int i, r;
    fprintf(f, "ID: %d\n", p->id);
    for(i = 1; i <= p->m; i++) {
        fprintf(f, "% 6.2f * x%02d ", -p->matrix->data[0][i], i - 1);
        if(i < p->m + p->n)
            fprintf(f, "+ ");
    }
    for(i = p->m + 1; i <= p->m + p->n; i++) {
        fprintf(f, "% 6.2f * y%02d ", -p->matrix->data[0][i], i - p->m - 1);
        if(i < p->m + p->n)
            fprintf(f, "+ ");
    }
    fprintf(f, "-> max\n");

    for(r = 1; r <= p->k; r++) {
        for(i = 1; i <= p->m; i++) {
            fprintf(f, "% 6.2f * x%02d ", p->matrix->data[r][i], i - 1);
            if(i < p->m + p->n)
                fprintf(f, "+ ");
        }
        for(i = p->m + 1; i <= p->m + p->n; i++) {
            fprintf(f, "% 6.2f * y%02d ", p->matrix->data[r][i], i - p->m - 1);
            if(i < p->m + p->n)
                fprintf(f, "+ ");
        }
        fprintf(f, "<= % 6.2f\n",  p->matrix->data[r][0]);
    }

    return 0;
}