#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "problemset.h"
#include "config.h"

/* Solution:
 * -------X------|--------Y---------
 *        m               n
 */

FILE* fi;
FILE* fo;

int next_id;

int is_int(float v) {
    return v - floor(v) < 1.0e-10 || ceil(v) - v < 1.0e-10;
}

int is_intvec(float* vec, int size) {
    int i;
    for(i = 0; i < size; i++)
        if(!is_int(vec[i]))
            return 0;
    return 1;
}

Problem* solve(ProblemSet* set) {
    ProblemSet* failset = problemset_new(1);
    Problem* best = NULL;
    Problem* p;
    int i;

    while(set->count > 0) {
        p = problemset_remove(set, 0);

        fprintf(fo, "----------------------------------------------------------------\nWorking with new problem (effectiveness = %f):\n", p->effectiveness);
        problem_print(fo, p);
        
        if(best != NULL && p->effectiveness < best->maxvalue) {
            /* нет смысла решать */
            fprintf(fo, "No need to solve, problem is inefficient\n");
            problemset_pushback(failset, p);
        } else if(problem_solve(p)) {
            /* если решение есть */
            fprintf(fo, "F(");
            vector_print(fo, p->solution);
            fprintf(fo, ") = %f\n", p->maxvalue);

            if(is_intvec(p->solution->data + p->m, p->n)) {
                /* если все y - целые */
                if(best == NULL || best->maxvalue < p->maxvalue) {
                    /* если новое решение круче того что было */
                    fprintf(fo, "Problem resulted in best solution\n");
                    if(best != NULL)
                        problemset_pushback(failset, best);
                    best = p;
                    for(i = set->count - 1; i >= 1; i--) {
                        if(set->problems[i]->effectiveness < best->maxvalue) {
                            fprintf(fo, "Problem %d was removed from queue.\n", set->problems[i]->id);
                            problemset_pushback(failset, problemset_remove(set, i));
                        }
                    }
                } else /* если не круче */ {
                    problemset_pushback(failset, p);
                    fprintf(fo, "Problem resulted in solution that is worse that the current one\n");
                }
            } else if(best == NULL || p->maxvalue > best->maxvalue){
                /* если есть не целые с хорошей эффективностью */
                Problem* p1;
                Problem* p2;
                int floatindex;

                fprintf(fo, "Problem resulted in floating-point solution, spawning new problems with ids %d and %d...\n", next_id, next_id + 1);

                floatindex;
                for(floatindex = p->m; floatindex < p->m + p->n; floatindex++)
                    if(!is_int(p->solution->data[floatindex]))
                        break;

                /* Yi <= floor(Yi) */
                p1 = problem_clone(p);
                p1->effectiveness = p->maxvalue;
                matrix_expandrows(p1->matrix, p1->matrix->rows + 1);
                for(i = 1; i < p1->matrix->cols; i++)
                    p1->matrix->data[p1->matrix->rows - 1][i] = (i - 1 == floatindex) ? 1.0f : 0.0f;
                p1->matrix->data[p1->matrix->rows - 1][0] = (float) floor(p->solution->data[floatindex]);
                p1->k++;
                p1->id = next_id++;
                problemset_addsorted(set, p1);

                /* Yi >= ceil(Yi) */
                p2 = problem_clone(p);
                p2->effectiveness = p->maxvalue;
                matrix_expandrows(p2->matrix, p2->matrix->rows + 1);
                for(i = 1; i < p2->matrix->cols; i++)
                    p2->matrix->data[p2->matrix->rows - 1][i] = (i - 1 == floatindex) ? -1.0f : 0.0f;
                p2->matrix->data[p2->matrix->rows - 1][0] = -(float) ceil(p->solution->data[floatindex]);
                p2->k++;
                p2->id = next_id++;
                problemset_addsorted(set, p2);

                problemset_pushback(failset, p);
            } else /* если решение фиговое */ {
                problemset_pushback(failset, p);
                fprintf(fo, "Problem resulted in solution that is worse that the current one\n");
            }
        } else /* если решения нет */ {
            problemset_pushback(failset, p);
            fprintf(fo, "Failed to solve problem...\n");
        }

        fprintf(fo, "\n\n\n");

        DBG((fprintf(fo, "---------------------------------------------------------------------\n\n\n\n")));
    }

    /* почистим за собой */
    for(i = 0; i < failset->count; i++)
        problem_delete(failset->problems[i]);
    problemset_delete(failset);

    return best;
}


int main() {
    fi = fopen("input.txt", "r");
    if(fi == NULL) {
        printf("input.txt not found");
        return 1;
    }

    fo = fopen("output.txt", "w");

    while(1) {
    ProblemSet* set;
    Problem* best;
    int r, c, k, n, m, i;

    /* задача:
     * Ax + By <= f
     * (c,x) + (d,y) -> max
     * x - вещ.
     * y - цел.
     */

    set = problemset_new(1);

    /* читаем размеры */
    if(fscanf(fi, "%d %d %d", &k, &m, &n) == -1)
      return 0; /* выходим в конце файла */
    problemset_pushback(set, problem_new(m, n, k));
    
    /* читаем A */
    for(r = 1; r <= k; r++) for(c = 1; c <= m; c++)
        fscanf(fi, "%f", &set->problems[0]->matrix->data[r][c]);

    /* читаем B */
    for(r = 1; r <= k; r++) for(c = m + 1; c <= m + n; c++)
        fscanf(fi, "%f", &set->problems[0]->matrix->data[r][c]);

    /* читаем f */
    for(r = 1; r <= k; r++)
        fscanf(fi, "%f", &set->problems[0]->matrix->data[r][0]);

    /* читаем c d */
    for(c = 1; c <= m + n; c++) {
        fscanf(fi, "%f", &set->problems[0]->matrix->data[0][c]);
        set->problems[0]->matrix->data[0][c] *= -1; /* на -1 */
    }

    set->problems[0]->matrix->data[0][0] = 0;
    set->problems[0]->effectiveness = -1.0e10;
    set->problems[0]->id = 0;

    next_id = 1;

    best = solve(set);

    if(best != NULL) {
        fprintf(fo, "F(");
        vector_print(fo, best->solution);
        fprintf(fo, ") = %f\n", best->maxvalue);
        
        fprintf(fo, "x = (");
        for(i = 0; i < m; i++)
          fprintf(fo, "% 6.2f", best->solution->data[i]);
        fprintf(fo, "), y = (");
        for(i = m; i < m + n; i++)
          fprintf(fo, "% 6.2f", best->solution->data[i]);
        fprintf(fo, ")\n\n\n\n\n\n\n\n\n\n\n");
    } else
        fprintf(fo, "Problem has either no solutions, or infinite solution\n");

    problemset_delete(set);
    fflush(stdout);
    if(best != NULL)
        problem_delete(best);
    }

    return 0;
}