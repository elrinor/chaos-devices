#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "simplex.h"

#define READKEY getc(stdin)
#define MAX_PROBLEMS 1024
#define EPS 1.0e-9

#ifndef FALSE
#  define FALSE 0
#endif

#ifndef TRUE
#  define TRUE 1
#endif

typedef struct {
    float* a;
    int k;
    int n;
    int x;
    int y;
    float* sol;
    float max;

    float efficiency;

    int solved;

    int num;
} problem_t;

problem_t* problems[MAX_PROBLEMS];
int problem_count;      // кол-во задач
problem_t* best;

int total_problems_created = 0; // сколько всего задач было создано за время работы программы

int is_bool(float v) {
    return fabs(v) < EPS || fabs(v - 1) < EPS;
}

problem_t* create_problem(int k, int n, int x, int y, float efficiency) {
    problem_t* result = (problem_t*) malloc(sizeof(problem_t));

    result->k = k;
    result->n = n;
    result->x = x;
    result->y = y;
    result->efficiency = efficiency;

    result->solved = FALSE;

    result->a = (float*) malloc((n + 1) * (k + 1) * sizeof(float));

    result->sol = (float*) malloc((n) * sizeof(float));

    result->num = total_problems_created++;

    return result;
}

void destroy_problem(problem_t* p) {
    free(p->a);
    free(p->sol);
    free(p);
}

void solve_problem(problem_t* p) {
    int size = (p->n + 1) * (p->k + 1) * sizeof(float);
    float* a = (float*)malloc(size);
    memcpy(a, p->a, size);
    p->solved = symplex(p->k, p->n, a, p->sol);
    p->max = a[p->n];
    free(a);
}

void sort_problems() {
    int i,j;
    for(i = 0; i < problem_count; i++) {
        float max_eff = problems[i]->efficiency;
        int max_j = i;
        for( j = i + 1; j < problem_count; j++) {
            if(problems[j]->efficiency > max_eff) {
                max_eff = problems[j]->efficiency;
                max_j = j;
            }
        }

        problem_t* tmp = problems[i];
        problems[i] = problems[max_j];
        problems[max_j] = tmp;
    }
}

void output_problem(problem_t* p) {
    int i,j;
    float* A = p->a;
    assert(p != NULL);

    printf("Problem #%d with efficiency = %f\n", p->num, p->efficiency);
    for(j = 0; j < p->n ;j++)
        printf("%.2f\t", -1 * A[j]);
    printf("-> max\n");
    for(i = 1; i <= p->k; i++) {
        for (j = 0; j < p->n ;j++)
            printf("%.2f\t", A[i*(p->n+1)+j]);
        printf("<= ");
        printf("%.2f\t", A[i*(p->n+1) + p->n]);
        printf("\n");
    }

    if(!p->solved) {
        printf("Solution does not exist or is unlimited\n");
    } else {
        printf("Solution: F(");
        for(i = 0; i < p->n; i++)
            printf("% 6.2f", p->sol[i]);
        printf(") = %f\n", p->max);
    }
}

void remove_problem(int index) {
    int i;
    assert(index >= 0 && index < problem_count);
    for(i = index; i < problem_count - 1; i++)
        problems[i] = problems[i + 1];
    problem_count--;
}

void add_problem_sorted(problem_t* p) {
    assert(p != NULL);
    problems[problem_count++] = p;
    sort_problems();
}

int main() {
    int x, y, k, n;
    int i, j;
    FILE* f = fopen("input.txt", "r");

    //while(1) {

    fscanf(f, "%d %d %d", &x, &y, &k);

    n = x + y;

    problem_t* p = create_problem(k, n, x, y, -1.0e10);

    for(i = 1; i <= k; i++) for(j = 0; j <= n; j++) 
        fscanf(f, "%f", &p->a[i * (n + 1) + j]);

    for(i = 0; i < n; i++) {
        fscanf(f, "%f", &p->a[i]);
        p->a[i] *= -1;
    }

    p->a[n] = 0;

    problem_count = 0;
    add_problem_sorted(p);
    best = NULL;

    while(problem_count > 0) {
        problem_t* p = problems[0];
        remove_problem(0);
        solve_problem(p);

        printf("\n\n\n");
        output_problem(p);

        if(!p->solved) {
            destroy_problem(p);
        } else {
            int not_bool = -1;
            for(i = p->x; i < p->n; i++) {
                if(!is_bool(p->sol[i])) {
                    not_bool = i;
                    break;
                }
            }
            
            if(not_bool == -1) {
                if(best == NULL || p->max > best->max) {
                    printf("New best solution found\n");
                    if(best != NULL)
                        destroy_problem(best);
                    best = p;

                    for(i = problem_count - 1; i >= 0; i--) {
                        if(problems[i]->efficiency < best->max) {
                            printf("Problem #%d seem to be inefficient, removed\n", problems[i]->num);
                            remove_problem(i);
                        }
                    }
                } else {
                    printf("Solution is worse than the current one, discarded");
                    destroy_problem(p);
                }
            } else {
                problem_t* p0 = create_problem(p->k + 2, p->n, p->x, p->y, p->max);
                problem_t* p1 = create_problem(p->k + 2, p->n, p->x, p->y, p->max);
                
                memcpy(p0->a, p->a, (p->n + 1) * (p->k + 1) * sizeof(float));
                memcpy(p1->a, p->a, (p->n + 1) * (p->k + 1) * sizeof(float));

                for(int i = 0; i <= p->n; i++) {
                    p0->a[(p->k + 1) * (p0->n + 1) + i] = 0;
                    p0->a[(p->k + 2) * (p0->n + 1) + i] = 0;
                    p1->a[(p->k + 1) * (p1->n + 1) + i] = 0;
                    p1->a[(p->k + 2) * (p1->n + 1) + i] = 0;
                }

                // == 0
                p0->a[(p->k + 1) * (p0->n + 1) + not_bool] = 1;
                p0->a[(p->k + 2) * (p0->n + 1) + not_bool] = -1;

                // == 1
                p1->a[(p->k + 1) * (p0->n + 1) + not_bool] = 1;
                p1->a[(p->k + 1) * (p0->n + 1) + p1->n]    = 1;
                p1->a[(p->k + 2) * (p0->n + 1) + not_bool] = -1;
                p1->a[(p->k + 2) * (p0->n + 1) + p1->n]    = -1;

                add_problem_sorted(p0);
                add_problem_sorted(p1);

                printf("Solution is floating-point, spawned 2 new problems #%d and #%d", p0->num, p1->num);
                destroy_problem(p);
            }
        }
    }
    
    printf("\n\n\n\n\n");

    if(best == NULL) {
        printf("Solution does not exist :(\n");
    } else {
        printf("Problem #%d resulted in best solution: F(", best->num);
        for(i = 0; i < best->n; i++)
            printf("% 6.2f", best->sol[i]);
        printf(") = %f\n", best->max);
        destroy_problem(best);
    }

    //}

    fclose(f);
    //READKEY;
	return 0;
}