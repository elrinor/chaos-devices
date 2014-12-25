#define _CRT_SECURE_NO_DEPRECATE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define _BAD_REG  -1
#define _BAD_FUNC -2
double symplex (int m, int n, double *A, double *v);

/*int newcalls = 0;
int killcalls = 0;*/

typedef struct 
{
    int k, x, y, n;
    double* a;
    double* result;
    double maxval;
    double eff;
    int no;
} Task;

int taskno;

Task* newtask(int x, int y, int k, double eff) 
{
    int n = x + y;
    Task* task = malloc(sizeof(Task));

    task->k = k;
    task->x = x;
    task->y = y;
    task->n = n;

    task->eff = eff;

    task->no = taskno++;
    
    task->result = malloc(n * sizeof(double));
    task->a = malloc((n + 1) * (k + 1) * sizeof(double));

    /*newcalls++;*/

    return task;
}

int solve(Task* t) 
{
    int i;
    double* a = malloc((t->n + 1) * (t->k + 1) * sizeof(double));
    memcpy(a, t->a, (t->n + 1) * (t->k + 1) * sizeof(double));
    for(i = 0; i < t->n; i++)
        a[i] *= -1;
    t->maxval = symplex(t->k, t->n, a, t->result);
    free(a);
    if(t->maxval == _BAD_REG || t->maxval == _BAD_FUNC)
        return 0;
    else
        return 1;
}

void killtask(Task* t) 
{
    free(t->result);
    free(t->a);
    free(t);

    /*killcalls++;*/
}

void printtask(Task* t) 
{
    int i, j;
    printf("--------------------------------------------------\n");
    printf("task: no = %d, k = %d, eff = %.2f\n", t->no, t->k, t->eff);
    for(i = 0; i < t->n; i++)
        printf("%.2f\t", t->a[i]);
    printf("\n");
    for(j = 1; j <= t->k; j++) 
    {
        for(i = 0; i <= t->n; i++)
            printf("%.2f\t", t->a[j * (t->n + 1) + i]);
        printf("\n");
    }
}

int boolean(double v) 
{
    return (fabs(v - 1) < 1.0e-8) || (fabs(v) < 1.0e-8);
}

int task_n;
Task* deadtasks[4096];
int deadtask_n;

void addtodead(Task* t) 
{
    deadtasks[deadtask_n++] = t;
}

int firstnonboolean(Task* t) 
{
    int i;
    for(i = t->x; i < t->n; i++) 
        if(!boolean(t->result[i])) 
            return i;
    return -1;
}

Task* solvemixed(Task* t) {
    int i;
    printtask(t);

    if(solve(t)) 
    {
        int nonbool_n;

        printf("solved: ");
        for(i = 0; i < t->n; i++)
            printf("%.2f\t", t->result[i]);
        printf("-> %.2f\n", t->maxval);

        nonbool_n = firstnonboolean(t);

        if(nonbool_n != -1) 
        {
            Task *t0, *t1;

            t0 = newtask(t->x, t->y, t->k + 2, t->maxval);
            t1 = newtask(t->x, t->y, t->k + 2, t->maxval);

            memcpy(t0->a, t->a, (t->n + 1) * (t->k + 1) * sizeof(double));
            memcpy(t1->a, t->a, (t->n + 1) * (t->k + 1) * sizeof(double));

            for(i = 0; i <= t->n; i++) 
            {
                t0->a[(t->k + 1) * (t0->n + 1) + i] = 0;
                t0->a[(t->k + 2) * (t0->n + 1) + i] = 0;
            }
            t0->a[(t->k + 1) * (t0->n + 1) + nonbool_n] = 1;
            t0->a[(t->k + 2) * (t0->n + 1) + nonbool_n] = -1;

            for(i = 0; i <= t->n; i++) 
            {
                t1->a[(t->k + 1) * (t1->n + 1) + i] = 0;
                t1->a[(t->k + 2) * (t1->n + 1) + i] = 0;
            }
            t1->a[(t->k + 1) * (t0->n + 1) + nonbool_n] = 1;
            t1->a[(t->k + 1) * (t0->n + 1) + t1->n]     = 1;
            t1->a[(t->k + 2) * (t0->n + 1) + nonbool_n] = -1;
            t1->a[(t->k + 2) * (t0->n + 1) + t1->n]     = -1;

            printf("not boolean, working with tasks with no = %d and no = %d\n\n\n", t0->no, t1->no);

            addtodead(t);
            t0=solvemixed(t0);
            t1=solvemixed(t1);
            if(t0 != NULL && t1 != NULL) {
                if(t0->maxval > t1->maxval) {
                    addtodead(t1);
                    return t0;
                } else {
                    addtodead(t0);
                    return t1;
                }
            } else if(t0 != NULL) {
                return t0;
            } else if(t1 != NULL) {
                return t1;
            } else
                return NULL;
        }
        else
        {
            printf("Boolean!\n\n\n");
            return t;
        } 
    } 
    else 
    {
        printf("no solution | infinite solution\n\n\n");
        addtodead(t);
        return NULL;
    }
}

int main(void) 
{
    int i, j;
    int x, y, k;
    Task* t;

    while(1) {
        deadtask_n = 0;

        if(scanf("%d %d %d", &x, &y, &k) == -1)
            return 0;

        taskno = 0;
        task_n = 1;
        t = newtask(x, y, k, -1000000.0);

        for(i = 0; i < t->n; i++)
            scanf("%lf", t->a + i);
        t->a[t->n] = 0;

        for(j = 1; j <= t->k; j++) for(i = 0; i <= t->n; i++)
            scanf("%lf", t->a + j * (t->n + 1) + i);

        t = solvemixed(t);
        
        for(i = 0; i < deadtask_n; i++)
            killtask(deadtasks[i]);

        if(t == NULL) 
        {
            printf("SOLUTION NOT FOUND!\n");
        } 
        else 
        {
            printf("SOLUTION: ");
            for(i = 0; i < t->n; i++)
                printf("%.2f\t", t->result[i]);
            printf("-> %.2f\n", t->maxval);
            killtask(t);
        }

        printf("\n\n\n\n\n\n\n");
    }

    return 0;
}
