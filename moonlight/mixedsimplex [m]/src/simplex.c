#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "simplex.h"
#include "config.h"


/**
* Результат пишется в b
*/
void mzi(int n, int m, int row, int col, float** a, float** b, int* x) {
    int r, c, i;
    /* модифицируем матрицу */
    for(r = 0; r <= m; r++) for(c = 0; c <= n; c++) {
        if(r == row && c == col)
            b[r][c] = 1 / a[row][col];
        else if(r == row)
            b[row][c] = a[row][c] / a[row][col];
        else if(c == col)
            b[r][col] = -a[r][col] / a[row][col];
        else
            b[r][c] = a[r][c] - a[r][col] * a[row][c] / a[row][col];
    }

    /* переставим переменные */
    for(i = 0; i < n; i++) {
        if(x[i] == -col)
            x[i] = row;
        else if(x[i] == row)
            x[i] = -col;
    }
}

void solution(int n, int m, float** a, int* x, float* target) {
    int i;
    for(i = 0; i < n; i++) {
        if(x[i] < 0)
            target[i] = 0;
        else
            target[i] = a[x[i]][0];
    }
}

void dbgprint(int n, int m, float** a, int* x) {
    int r, c, i;
    float* sol;
    for(r = 0; r <= m; r++) {
        for(c = 0; c <= n; c++)
            printf("% 6.2f", a[r][c]);
        printf("\n");
    }

    sol = malloc(sizeof(float) * n);
    solution(n, m, a, x, sol);
    printf("Solution: f(");
    for(i = 0; i < n; i++)
        printf("% 6.2f", sol[i]);
    printf(") = % 6.2f\n\n", a[0][0]);
    free(sol);
}

/*
 * a: a[m+1][n+1]
 *
 * 0-я строка a - целевая ф-я * -1
 * 0-й столбец - свободные члены
 * n - размерность
 * m - кол-во ограничений
 *
 * ограничения имеют вид:
 * 1*a[i][0]   -x1*a[i][1]   -x2*a[i][2] + ... >= 0 
 *
 * коэф-ты оптимизируемой ф-ии записаны с минусом!
 * портит m0!!!
 */
int simplex(Matrix* m0, float* maxvalue, Vector* varvector) {
    int k, i;
    int n = m0->cols - 1;
    int m = m0->rows - 1;
    Matrix* m1 = matrix_new(m0->rows, m0->cols);
    float **a = m0->data, **b = m1->data;
    float **tmp;
    int retcode = 0;
    int* x;

    assert(varvector->size == m0->cols - 1);

    /* положение переменных.
     * положительное число => переменная "сбоку".
     * отрицательное => "сверху" */
    x = malloc(sizeof(int) * n);
    for(i = 0; i < n; i++)
        x[i] = -(i + 1);

    DBG((dbgprint(n, m, a, x)));

    /* устраняем отрицательные свободные члены */
    for(k = 1; k <= m; k++) if(a[k][0] < 0) {
        int r, c;
        float min;
        
        /* просматриваем соотв. строку - ищем отрицательный эл-т */
        c = -1;
        for(i = 1; i <= n; i++) if(a[k][i] < 0) {
            c = i;
            break;
        }
        if(c == -1) {
            /* система несовместна / нет максимума */
            goto ret;
        }
        /* иначе с-й столбец - разрешающий */

        /* ищем минимум Bi / Aic >= 0 по столбцу */
        min = 1.0e10;
        for(i = 1; i <= m; i++) {
            float v;
            if(a[i][c] == 0)
                continue;

            v = a[i][0] / a[i][c];
            if((v > 0 || (v == 0 && a[r][i] > 0)) && v < min) {
                min = v;
                r = i;
            }
        }

        /* шаг МЖИ */
        mzi(n, m, r, c, a, b, x);
        tmp = a; a = b; b = tmp;

        DBG((printf("%d %d\n", r, c)));
        DBG((dbgprint(n, m, a, x)));

        k = 0;
    }

    /* ищем оптимальное решение */

    /* ищем отрицательный коэф-т в z-строке */
    for(k = 1; k <= n; k++) if(a[0][k] < 0) {
        int r, c;
        float min;

        /* выбираем наибольший по модулю отриц. коэф-т */
        c = -1;
        min = 0;
        for(i = 1; i <= n; i++) if(a[0][i] < min) {
            min = a[0][i];
            c = i;
        }
        /* c != -1 */

        /* с-й столбец - решающий */
        r = -1;
        min = 1.0e10;
        for(i = 1; i <= m; i++) {
            float v;
            if(a[i][c] == 0)
                continue;

            v = a[i][0] / a[i][c];
            if((v > 0 || (v == 0 && a[i][c] > 0)) && v < min) {
                min = v;
                r = i;
            }
        }

        if(r == -1) {
            /* целевая ф-я неограничена */
            goto ret;
        }

        mzi(n, m, r, c, a, b, x);
        tmp = a; a = b; b = tmp;

        DBG((printf("%d %d\n", r, c)));
        DBG((dbgprint(n, m, a, x)));

        k = 0;
    }

    /* оптимальное решение найдено */
    retcode = 1;
    *maxvalue = a[0][0];
    solution(n, m, a, x, varvector->data);

ret:
    matrix_delete(m1);
    free(x);
    return retcode;
}