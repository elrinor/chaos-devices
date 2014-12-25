#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>

// объявления функций и констант из symplex.c
#define _BAD_REG  -1
#define _BAD_FUNC -2
double symplex (int m, int n, double *A, double *v);

// размер буфера под задачи
#define BUFSIZE 30000


// структура, в которой хранится информация о задаче
typedef struct {
    double e; // эф-ть
    int i; // номер задачи

    int n; // сколько переменных
    int k; // сколько ограничений
    int x; // вещественные
    int y; // целочисленные
    int z; // булевы

    double* a; // матрица
    double* s; // вектор решения

    double m; // максимум

} TProblem;


// создает задачу с заданными параметрами
// e - эф-ть
// k - сколько ограничений
// x - сколько вещественных переменных
// y - сколько целочисленных переменных
// z - сколько булевых переменных
// i - номер задачи
TProblem* MakeProblem(double e, int k, int x, int y, int z, int i) {
    TProblem* p=(TProblem*)malloc(sizeof(TProblem));
    p->n=x+y+z;
    p->x=x;
    p->y=y;
    p->z=z;
    p->k=k;
    p->a=(double*)malloc((p->k+1)*(p->n+1)*sizeof(double));
    p->s=(double*)malloc((p->n)*sizeof(double));
    p->e=e;
    p->m=0;
    p->i=i;
    return p;
}
    

// освобождает память, занятую задачей
void FreeProblem(TProblem* p) {
    free(p->a);
    free(p->s);
    free(p);
}

TProblem* Unsolved[BUFSIZE]; // массив еще не решенных задач
int UnsolvedCount; // кол-во еще не решенных задач в этом массиве
TProblem* Removed[BUFSIZE]; // массив удаленных задач
int RemovedCount; // кол-во удаленных задач в этом массиве
TProblem* CurrentSolution; // текущее решенее (лучшее найденное)

// добавить задачу p в список удаленных
void Remove(TProblem* p) {
    int i, j;
    for(i=0;i<RemovedCount;i++)
        if(Removed[i]->e>p->e)
            break;
    for(j=RemovedCount-1;j>=i;j--)
        Removed[j+1]=Removed[j];
    Removed[i]=p;
    RemovedCount++;
}

// считать задачу, присвоив ей номер Number
TProblem* ReadProblem(int Number) {
    TProblem* p = NULL;
    int i, j;
    int k, x, y, z;
    scanf("%d %d %d %d", &x, &y, &z, &k);
    if(feof(stdin))
        return NULL;
    p=MakeProblem(0, k, x, y, z, Number);
    for(i=0;i<p->n;i++)
        scanf("%lf", &p->a[i]);
    for(i=1;i<=p->k;i++) 
        for(j=0;j<=p->n;j++) 
            scanf("%lf", &p->a[i*(p->n+1)+j]);
    p->a[p->n] = 0;
    return p;
}

int main(void) {
    for(;;){
        int i, j;
        int TotalProblemCount = 0;
        int TotalProblemSolved = 0;

        // сначала считываем задачу
        TProblem* p=ReadProblem(TotalProblemCount++);
        
        // если не получилось - выходим
        if(p==NULL)
            break;

        // инициализируемся
        CurrentSolution=NULL;
        RemovedCount=0;
        UnsolvedCount=1;
        Unsolved[0]=p;

        // крутим цикл до тех пор, пока есть нерешенные задачи
        while(UnsolvedCount>0){
            double* tmp;
            double maxe=-DBL_MAX;
            int maxn;
            int ok;
            TProblem* p=NULL;

            TotalProblemSolved++;
            
            // извлекаем из массива нерешенных наиболее эффективную задачу, сохраняем ее в p
            for(i=0;i<UnsolvedCount;i++){
                if(Unsolved[i]->e > maxe){
                    maxe=Unsolved[i]->e;
                    maxn=i;
                }
            }
            p=Unsolved[maxn];
            for(i=maxn;i<UnsolvedCount-1;i++)
                Unsolved[i]=Unsolved[i+1];
            UnsolvedCount--;
            
            // выводим задачу
            printf("\n\n\nProblem %d with efficiency %f:\n", p->i, p->e);
            for(i=0;i<p->n;i++)
                printf("%.2f\t", p->a[i]);
            printf("\n");
            for(j=1;j<=p->k;j++){
                for(i=0;i<=p->n;i++)
                    printf("%.2f\t", p->a[j*(p->n+1)+i]);
                printf("\n");
            }

            // решаем задачу симплексом
            tmp=(double*)malloc((p->n+1)*(p->k+1)*sizeof(double));
            memcpy(tmp, p->a, (p->n+1)*(p->k+1)*sizeof(double));
            for(i=0;i<p->n;i++)
                tmp[i]=-tmp[i];
            p->m=symplex(p->k, p->n, tmp, p->s);
            free(tmp);
            ok=!(p->m==_BAD_FUNC||p->m==_BAD_REG);

            
            if(ok){
                // если решение найдено - выводим его
                printf("Solution Vector: ");
                for(i=0;i<p->n;i++)
                    printf("%.2f\t", p->s[i]);
                printf("\n");
                printf("Maximum: %f\n", p->m);

                // если новое решение хуже лучшего найденного - не рассматриваем его, добавляем в список удаленных
                if(CurrentSolution!=NULL&&CurrentSolution->m>=p->m){
                    printf("Solution is worse than the current one\n");
                    Remove(p);
                }else{
                    // ищем не целое число в решении, которое должно быть целым
                    ok=1;
                    for(i=p->x;i<p->x+p->y;i++){
                        if(fabs(floor(p->s[i]) - p->s[i]) > 0.000001 && fabs(ceil(p->s[i]) - p->s[i]) > 0.000001){
                            ok=0;
                            break;
                        }
                    }

                    // если не нашли - ищем не-булево, которое должно быть булевым
                    if(ok) {
                        for(i=p->x+p->y;i<p->n;i++){
                            if(fabs(p->s[i])>0.000001 && fabs(p->s[i]-1)>0.000001) {
                                ok=0;
                                break;
                            }
                        }
                    }
               
                    // если не нашли - значит решение удовлетворяет всем требованиям
                    if(ok){
                        // печатаем, что нашли лучшее решение и сохраняем его как текущее
                        printf("Best solution\n");
                        if(CurrentSolution!=NULL)
                            Remove(CurrentSolution);
                        CurrentSolution=p;

                        // удаляем из списка задач на решение те, которые стали неэффективными
                        for(i=UnsolvedCount-1;i>=0;i--){
                            if(Unsolved[i]->e < CurrentSolution->m - 0.0000000001){
                                Remove(Unsolved[i]);
                                printf("Problem %d removed\n", Unsolved[i]->i);
                                for(j=i;j<UnsolvedCount-1;j++)
                                    Unsolved[j] = Unsolved[j+1];
                                UnsolvedCount--;
                            }
                        }
                    }else{
                        int nonint=i;
                        TProblem *pa, *pb;

                        if(nonint<p->x+p->y) {
                            // если у нас есть нецелочисленный элемент - добавляем 2 задачи
                            pa = MakeProblem(p->m, p->k+1, p->x, p->y, p->z, TotalProblemCount++);
                            memcpy(pa->a, p->a, (p->n+1)*(p->k+1)*sizeof(double));
                            for(i=0;i<pa->n+1;i++)
                                pa->a[pa->k*(pa->n+1)+i] = 0;
                            pa->a[pa->k*(pa->n+1)+nonint] = 1;
                            pa->a[pa->k*(pa->n+1)+pa->n] = floor(p->s[nonint]);
                            Unsolved[UnsolvedCount++]=pa;

                            pb = MakeProblem(p->m, p->k+1, p->x, p->y, p->z, TotalProblemCount++);
                            memcpy(pb->a, p->a, (p->n+1)*(p->k+1)*sizeof(double));
                            for(i=0;i<pb->n+1;i++)
                                pb->a[pb->k*(pb->n+1)+i] = 0;
                            pb->a[pb->k*(pb->n+1)+nonint] = -1;
                            pb->a[pb->k*(pb->n+1)+pa->n] = -ceil(p->s[nonint]);
                            Unsolved[UnsolvedCount++]=pb;

                            printf("Solution is non-int, 2 new problems created: %d and %d", pa->i, pb->i);
                        } else {
                            // если есть не-булев элемент - добавляем 2 задачи
                            pa = MakeProblem(p->m, p->k+2, p->x, p->y, p->z, TotalProblemCount++);
                            memcpy(pa->a, p->a, (p->n+1)*(p->k+1)*sizeof(double));
                            for(i=0;i<pa->n+1;i++){
                                pa->a[pa->k*(pa->n+1)+i] = 0;
                                pa->a[(pa->k-1)*(pa->n+1)+i] = 0;
                            }
                            pa->a[pa->k*(pa->n+1)+nonint] = 1;
                            pa->a[(pa->k-1)*(pa->n+1)+nonint] = -1;
                            Unsolved[UnsolvedCount++]=pa;

                            pb = MakeProblem(p->m, p->k+2, p->x, p->y, p->z, TotalProblemCount++);
                            memcpy(pb->a, p->a, (p->n+1)*(p->k+1)*sizeof(double));
                            for(i=0;i<pb->n+1;i++){
                                pb->a[pb->k*(pb->n+1)+i] = 0;
                                pb->a[(pb->k-1)*(pb->n+1)+i] = 0;
                            }
                            pb->a[pb->k*(pb->n+1)+nonint] = 1;
                            pb->a[pb->k*(pb->n+1)+pa->n] = 1;
                            pb->a[(pb->k-1)*(pb->n+1)+nonint] = -1;
                            pb->a[(pb->k-1)*(pb->n+1)+pa->n] = -1;
                            Unsolved[UnsolvedCount++]=pb;

                            printf("Solution is non-bool, 2 new problems created: %d and %d", pa->i, pb->i);
                        }
                        Remove(p);
                    }
                }
            } else {
                // если решение не найдено - печатаем, что фигня )
                if(p->m==_BAD_FUNC)
                    printf("Function is unlimited\n");
                else
                    printf("Solution doesn't exist\n");
                Remove(p);
            }
        }

        // если решение есть - выводим его
        if(CurrentSolution==NULL){
            printf("\n\n\n\nSOLUTION NOT FOUND\n");
        } else {
            printf("\n\n\n\nSOLUTION FOUND\n");
            printf("Solution Vector: ");
            for(i=0;i<CurrentSolution->n;i++)
                printf("%.2f\t", CurrentSolution->s[i]);
            printf("\n");
            printf("Maximum: %f\n", CurrentSolution->m);
            FreeProblem(CurrentSolution);
        }
        printf("\n\n\n\n\n\n\n");
        for(i=0;i<RemovedCount;i++)
            FreeProblem(Removed[i]);

        printf("TOTAL PROBLEMS CREATED: %d\n", TotalProblemCount);
        printf("TOTAL PROBLEMS SOLVED:  %d\n", TotalProblemSolved);
    }

    return 0;
}