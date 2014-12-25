#ifndef __PROBLEM_H__
#define __PROBLEM_H__

#include <stdio.h>
#include "matrix.h"
#include "vector.h"

typedef struct _Problem {
    Matrix* matrix;
    int m; /* ���-�� ��-��� ������� X */
    int n; /* ���-�� ��-��� ������� Y */
    int k; /* ���-�� ����������� */

    int id; /* ����� (�������������) ������ */

    Vector* solution; /* ������� */
    float maxvalue;  /* ����������� �������� */

    float effectiveness; /* ������ ������������� */
} Problem;


Problem* problem_new(int m, int n, int k);

void problem_delete(Problem* p);

int problem_solve(Problem* p);

Problem* problem_clone(Problem* p);

int problem_print(FILE* f, Problem* p);

#endif