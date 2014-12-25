#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>

// ���������� ������� � �������� �� symplex.c
#define _BAD_REG  -1
#define _BAD_FUNC -2
double symplex (int m, int n, double *A, double *v);

// ������ ������ ��� ������
#define BUFSIZE 30000


// ���������, � ������� �������� ���������� � ������
typedef struct {
    double e; // ��-��
    int i; // ����� ������

    int n; // ������� ����������
    int k; // ������� �����������
    int x; // ������������
    int y; // �������������
    int z; // ������

    double* a; // �������
    double* s; // ������ �������

    double m; // ��������

} TProblem;


// ������� ������ � ��������� �����������
// e - ��-��
// k - ������� �����������
// x - ������� ������������ ����������
// y - ������� ������������� ����������
// z - ������� ������� ����������
// i - ����� ������
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
    

// ����������� ������, ������� �������
void FreeProblem(TProblem* p) {
    free(p->a);
    free(p->s);
    free(p);
}

TProblem* Unsolved[BUFSIZE]; // ������ ��� �� �������� �����
int UnsolvedCount; // ���-�� ��� �� �������� ����� � ���� �������
TProblem* Removed[BUFSIZE]; // ������ ��������� �����
int RemovedCount; // ���-�� ��������� ����� � ���� �������
TProblem* CurrentSolution; // ������� ������� (������ ���������)

// �������� ������ p � ������ ���������
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

// ������� ������, �������� �� ����� Number
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

        // ������� ��������� ������
        TProblem* p=ReadProblem(TotalProblemCount++);
        
        // ���� �� ���������� - �������
        if(p==NULL)
            break;

        // ����������������
        CurrentSolution=NULL;
        RemovedCount=0;
        UnsolvedCount=1;
        Unsolved[0]=p;

        // ������ ���� �� ��� ���, ���� ���� ���������� ������
        while(UnsolvedCount>0){
            double* tmp;
            double maxe=-DBL_MAX;
            int maxn;
            int ok;
            TProblem* p=NULL;

            TotalProblemSolved++;
            
            // ��������� �� ������� ���������� �������� ����������� ������, ��������� �� � p
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
            
            // ������� ������
            printf("\n\n\nProblem %d with efficiency %f:\n", p->i, p->e);
            for(i=0;i<p->n;i++)
                printf("%.2f\t", p->a[i]);
            printf("\n");
            for(j=1;j<=p->k;j++){
                for(i=0;i<=p->n;i++)
                    printf("%.2f\t", p->a[j*(p->n+1)+i]);
                printf("\n");
            }

            // ������ ������ ����������
            tmp=(double*)malloc((p->n+1)*(p->k+1)*sizeof(double));
            memcpy(tmp, p->a, (p->n+1)*(p->k+1)*sizeof(double));
            for(i=0;i<p->n;i++)
                tmp[i]=-tmp[i];
            p->m=symplex(p->k, p->n, tmp, p->s);
            free(tmp);
            ok=!(p->m==_BAD_FUNC||p->m==_BAD_REG);

            
            if(ok){
                // ���� ������� ������� - ������� ���
                printf("Solution Vector: ");
                for(i=0;i<p->n;i++)
                    printf("%.2f\t", p->s[i]);
                printf("\n");
                printf("Maximum: %f\n", p->m);

                // ���� ����� ������� ���� ������� ���������� - �� ������������� ���, ��������� � ������ ���������
                if(CurrentSolution!=NULL&&CurrentSolution->m>=p->m){
                    printf("Solution is worse than the current one\n");
                    Remove(p);
                }else{
                    // ���� �� ����� ����� � �������, ������� ������ ���� �����
                    ok=1;
                    for(i=p->x;i<p->x+p->y;i++){
                        if(fabs(floor(p->s[i]) - p->s[i]) > 0.000001 && fabs(ceil(p->s[i]) - p->s[i]) > 0.000001){
                            ok=0;
                            break;
                        }
                    }

                    // ���� �� ����� - ���� ��-������, ������� ������ ���� �������
                    if(ok) {
                        for(i=p->x+p->y;i<p->n;i++){
                            if(fabs(p->s[i])>0.000001 && fabs(p->s[i]-1)>0.000001) {
                                ok=0;
                                break;
                            }
                        }
                    }
               
                    // ���� �� ����� - ������ ������� ������������� ���� �����������
                    if(ok){
                        // ��������, ��� ����� ������ ������� � ��������� ��� ��� �������
                        printf("Best solution\n");
                        if(CurrentSolution!=NULL)
                            Remove(CurrentSolution);
                        CurrentSolution=p;

                        // ������� �� ������ ����� �� ������� ��, ������� ����� ��������������
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
                            // ���� � ��� ���� ��������������� ������� - ��������� 2 ������
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
                            // ���� ���� ��-����� ������� - ��������� 2 ������
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
                // ���� ������� �� ������� - ��������, ��� ����� )
                if(p->m==_BAD_FUNC)
                    printf("Function is unlimited\n");
                else
                    printf("Solution doesn't exist\n");
                Remove(p);
            }
        }

        // ���� ������� ���� - ������� ���
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