#ifndef __PROBLEM_H__
#define __PROBLEM_H__

#include <stdio.h>
#include "matrix.h"
#include "vector.h"

typedef struct _Problem {
    Matrix* matrix;
    int m; /* кол-во эл-тов вектора X */
    int n; /* кол-во эл-тов вектора Y */
    int k; /* кол-во ограничений */

    int id; /* номер (идентификатор) задачи */

    Vector* solution; /* решение */
    float maxvalue;  /* достигнутый максимум */

    float effectiveness; /* оценка эффективности */
} Problem;


Problem* problem_new(int m, int n, int k);

void problem_delete(Problem* p);

int problem_solve(Problem* p);

Problem* problem_clone(Problem* p);

int problem_print(FILE* f, Problem* p);

#endif