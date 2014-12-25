#include <stdio.h>
#include <malloc.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int *bas, *nebas;

#define _BAD_REG  -1
#define _BAD_FUNC -2

double symplex (int m, int n, double *A, double *v);
int _est_negative_stolb  (int k,int m, int n, double *A);
int _est_negative_stroka (int k,int m, int n, double *A);
int _est_positive_stolb  (int k,int m, int n, double *A);
void MGI (int r, int s, int m, int n, double *A);
void print (int m, int n, double *A);


// функция symplex возвращает -1 - если область пуста, -2 - если функция не ограничена,
// значение целевой функции на оптимальном решении в случае, если работа прошла корректно
// в вектор v записывается оптимальное решение
double symplex (int m, int n, double *A, double *v)
{
  int i,j;
  int k,s,r;
  double min,work;
  //prepare
  bas   = (int*) malloc((m+1)*sizeof(int));
  nebas = (int*)malloc((n+1)*sizeof(int));
  for (i=1; i<=m;  i++) bas[i]=n+1;
  for (i=0; i<n;  i++) nebas[i]=i;

  //опорное решение
  while( (k = _est_negative_stolb (n,m,n,A)) != -1)
  {
    //print(m,n,A);
    if ((s = _est_negative_stroka(k,m,n,A))== -1) 
    {
      free(bas);
      free(nebas);
      return _BAD_REG;
    }//нужен код пустой области
    r=0; min = DBL_MAX;
    for (i=1; i<=m;i++)
    {
      if (A[i*(n+1)+s])
        if (((work = A[i*(n+1)+n]/A[i*(n+1)+s]) > 0) && (work < min))
        {
          min = work;
          r=i;
        }
        else if (work==0 && A[i*(n+1)+s]>0)
        {
          min = work;
          r=i;
          break;
        }
    }
    MGI (r,s, m,n, A);
    //			print(m,n,A);
  }	
  //оптимальное решение
  while (_est_negative_stroka(0,m,n,A) != -1)
  {
    s=0; min = DBL_MAX;
    for (j=0; j<n; j++)
      if(A[0*(n+1)+j] < min)
      {
        s=j;
        min = A[0*(n+1)+j];
      }
      if(!(_est_positive_stolb(s,m,n,A))) 
      {
        free(bas);
        free(nebas);
        return _BAD_FUNC;
      }//целевая функция не ограничена

      //print(m,n,A);
      r=0; min = DBL_MAX;
      for (i=1;i<=m;i++)
        if(A[i*(n+1)+s]>0)
          if(((work = A[i*(n+1)+n]/A[i*(n+1)+s])>=0) && (work < min))
          {
            min = work;
            r=i;
          }
          MGI(r,s,m,n,A);
          //print(m,n,A);
  }
  for (i=0; i<n; i++)
    v[i]=0;
  for (i=1; i<=m; i++)
    if(bas[i]<=n)
      v[bas[i]]=A[i*(n+1)+n];

  free(bas);
  free(nebas);
  return A[n];
}
int _est_negative_stolb (int k,int m, int n, double *A)
{
  int i;
  for (i=1; i<=m; i++)
    if (A[i*(n+1)+k]<0)
      return i;
  return -1;
}
//есть ли отрицательный в k-той строке
int _est_negative_stroka (int k,int m, int n, double *A)
{
  int j;
  for (j=0; j<n; j++)
    if (A[k*(n+1)+j]<0)
      return j;
  return -1;
}
//есть ли положительный в k-том столбце
int _est_positive_stolb  (int k,int m, int n, double *A)
{
  int i;
  for (i=1; i<=m; i++)
    if (A[i*(n+1)+k]>0)
      return i;
  return 0;
}
//модифицированное жорданова исключение
void MGI (int r, int s, int m, int n, double *A)
{
  int i,j;
  for (i=0; i<=m; i++)
    for (j=0; j<=n; j++)
      if ((i!=r) && (j!=s))
        A[i*(n+1) + j] = A[i*(n+1)+j]-A[r*(n+1)+j]*A[i*(n+1)+s]/A[r*(n+1)+s];
  for (j=0;j<=n; j++)
    if(j!=s) A[r*(n+1)+j]/= A[r*(n+1)+s];
  for (i=0;i<=m; i++)
    if(i!=r) A[i*(n+1)+s]/=-A[r*(n+1)+s];
  A[r*(n+1)+s]=1/A[r*(n+1)+s];

  i = nebas[s];
  nebas[s] = bas[r];
  bas[r] = i;

  return;
}

void print (int m, int n, double *A)
{
  int i,j;
  printf("\n");
  for (i= 0; i<=m; i++)
  {
    for (j= 0; j<=n ;j++)
      printf("%.2f\t", A[i*(n+1)+j]);
    printf("\n");
  }
  printf("\n");
  return;
}

/*
в приведённом примере решается задача
x-y ->max
2<=y<=20
x<=5.55

ответ: x=5.55, y=2,  <c,x>+<d,y>=x-y=3.55

*/
/*int main()
{
  int m=3, n=2;
  double *A;
  double *v;
  //цикл - для проверки на текучесть памяти симплекса
  //while(1)
  {
    A = (double*)malloc((m+1)*(n+1)*sizeof(double));
    v = (double*)malloc(n*sizeof(double));
    double tgtval;//Значение целевой функции после работы симплекса

    A[0*(n+1)+0]=-1;
    A[0*(n+1)+1]=1;
    A[0*(n+1)+2]=0;

    A[1*(n+1)+0]=0;
    A[1*(n+1)+1]=1;
    A[1*(n+1)+2]=20;

    A[2*(n+1)+0]=0;
    A[2*(n+1)+1]=-1;
    A[2*(n+1)+2]=-2;

    A[3*(n+1)+0]=1;
    A[3*(n+1)+1]=0;
    A[3*(n+1)+2]=5.55;

    print (m, n, A);
    tgtval=symplex(m,n,A,v);
    printf("%.2f %.2f %.2f\n",tgtval,v[0],v[1]);
    free(A);
    free(v);
  }

  return 0;
}*/


//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//

#define FALSE 0
#define TRUE 1
#define MAX 10000

int n, x, y;
int vsego_zadach;

typedef struct {
  double effectivnost;
  int nomer;
  double* matrix;
  double* resheniye;
  double maximum;
  int k;
} zadacha;

int z_cmp(const void* pz1, const void* pz2) {
  zadacha* z1 = *((zadacha**)pz1);
  zadacha* z2 = *((zadacha**)pz2);
  if(z1->effectivnost < z2->effectivnost)
    return -1;
  else if(z1->effectivnost == z2->effectivnost)
    return 0;
  else
    return 1;
}

zadacha* zadachi[MAX];
zadacha* zadachi_na_udaleniye[MAX];
zadacha* luchshee_resheniye;

int n_zadach;
int n_zadach_na_udaleniye;

void na_udaleniye(zadacha* z) {
  zadachi_na_udaleniye[n_zadach_na_udaleniye] = z;
  n_zadach_na_udaleniye++;
  qsort(zadachi_na_udaleniye, n_zadach_na_udaleniye, sizeof(zadacha*), z_cmp);
  return;
}

void z_udalit(int index) {
  int i;
  printf("zadacha nomer %d udalena\n", zadachi[index]->nomer);
  na_udaleniye(zadachi[index]);
  for(i=index; i<n_zadach-1; i++)
    zadachi[i] = zadachi[i+1];
  n_zadach--;
}

void v_zadachi(zadacha* z) {
  printf("dobavlena zadacha s nomerom %d\n", z->nomer);
  zadachi[n_zadach] = z;
  n_zadach++;
  qsort(zadachi, n_zadach, sizeof(zadacha*), z_cmp);
  return;
}

zadacha* z_malloc(int k, double effectivnost) 
{
  zadacha* z;
  z = malloc(sizeof(zadacha));
  z->matrix = malloc((n+1)*(k+1)*sizeof(double));
  z->resheniye = malloc(n*sizeof(double));
  z->maximum = 0;
  z->effectivnost = effectivnost;
  z->nomer = vsego_zadach;
  z->k = k;
  vsego_zadach++;
  return z;
}

void z_free(zadacha* z) {
  free(z->matrix);
  free(z->resheniye);
  free(z);
  return;
}

int z_reshit(zadacha* z) {
  int i;
  double* a = malloc((n+1)*(z->k+1)*sizeof(double));
  memcpy(a, z->matrix, (n+1)*(z->k+1)*sizeof(double));
  for(i=0; i<n; i++)
    a[i] *= -1;
  z->maximum = symplex(z->k, n, a, z->resheniye);
  free(a);
  if(z->maximum == _BAD_FUNC || z->maximum == _BAD_REG)
    return FALSE;
  else return TRUE;
}

int integer(double v) {
  return fabs(ceil(v)-v) < 1.0e-20 || fabs(floor(v)-v) < 1.0e-20;
}

int main() 
{
  for(;;) // цикл для тестирования утечек
  { 
    int kk, i, j;
    luchshee_resheniye = NULL;
    n_zadach = 1;
    n_zadach_na_udaleniye = 0;
    vsego_zadach = 0;

    if(scanf("%d%d%d", &x, &y, &kk) == -1)
      break;
    n = x+y;

    zadachi[0] = z_malloc(kk, -DBL_MAX);
    for(i=0; i<n; i++) 
      scanf("%lf", &(zadachi[0]->matrix[i]));
    zadachi[0]->matrix[n] = 0;
    for(i=1; i<=zadachi[0]->k; i++) for(j=0; j<=n; j++) 
      scanf("%lf", &zadachi[0]->matrix[i*(n+1)+j]);

    for(;;) {
      int reshili;
      zadacha* z;
      if(n_zadach == 0)
        break;

      z = zadachi[n_zadach - 1];
      n_zadach--;

      printf("\n\n\nReshaem zadachu:\n");
      for(i=0; i<=z->k; i++) 
      { 
        for(j=0; j<=n; j++) 
          printf("%.2f\t", z->matrix[i*(n+1)+j]);
        printf("\n");
      }
      printf("nomer=%d\n", z->nomer);
      printf("effectivnost=%f\n", z->effectivnost);
      printf("...\n");
      reshili = z_reshit(z);
      if(reshili) 
      {
        printf("reshili!\n");
        printf("maximum=%.2f\n", z->maximum);
        printf("resheniye=(\t");
        for(i=0; i<n; i++)
          printf("%.2f\t", z->resheniye[i]);
        printf(")\n");
      } 
      else printf("net resheniya ili est' beskonechnoye resheniye\n");
      
      if(reshili) 
      {
        if(luchshee_resheniye != NULL && luchshee_resheniye->maximum >= z->maximum)
        {
          printf("poluchennoe resheniye huzhe luchshego\n");
          na_udaleniye(z);
        }
        else
        {
          int nashli_ne_int = FALSE;
          for(i=x; i<n; i++)
          {
            if(!integer(z->resheniye[i]))
            {
              nashli_ne_int = TRUE;
              break;
            }
          }

          if(nashli_ne_int) {
            int ne_int = i;
            zadacha* z1;
            zadacha* z2;

            printf("poluchennoe resheniye ne udovletvoryaet usloviyu celochislennosti\n");

            z1 = z_malloc(z->k+1, z->maximum);
            memcpy(z1->matrix, z->matrix, (n+1)*(z->k+1)*sizeof(double));
            z1->effectivnost = z->maximum;
            for(i=0; i<n+1; i++)
              z1->matrix[(z->k+1)*(n+1)+i] = 0;
            z1->matrix[(z->k+1)*(n+1)+n] = floor(z->resheniye[ne_int]);
            z1->matrix[(z->k+1)*(n+1) + ne_int] = 1;
            v_zadachi(z1);

            z2 = z_malloc(z->k+1, z->maximum);
            memcpy(z2->matrix, z->matrix, (n+1)*(z->k+1)*sizeof(double));
            z2->effectivnost = z->maximum;
            for(i=0; i<n+1; i++)
              z2->matrix[(z->k+1)*(n+1)+i] = 0;
            z2->matrix[(z->k+1)*(n+1)+n] = -ceil(z->resheniye[ne_int]);
            z2->matrix[(z->k+1)*(n+1) + ne_int] = -1;
            v_zadachi(z2);

            na_udaleniye(z);
          } 
          else
          {
            printf("poluchennoe resheniye - luchshee\n");
            if(luchshee_resheniye != NULL)
              na_udaleniye(luchshee_resheniye);
            luchshee_resheniye = z;
            for(i=n_zadach-1; i>=0; i--)
              if(zadachi[i]->effectivnost < luchshee_resheniye->maximum) 
                z_udalit(i);
          }
        }
      }
      else na_udaleniye(z);
    }

    printf("\n\n\n////////////////////////////////////////////////////////////\n");

    if(luchshee_resheniye == NULL) 
      printf("resheniye ne sushestvuyet ili beskonechno\n");
    else
    {
      printf("luchshee resheniye:\n");
      printf("maximum=%.2f\n", luchshee_resheniye->maximum);
      printf("resheniye=(");
      for(i=0; i<n; i++)
        printf("%.2f\t", luchshee_resheniye->resheniye[i]);
      printf(")\n");
    }

    for(i=0; i<n_zadach_na_udaleniye; i++)
      z_free(zadachi_na_udaleniye[i]);
    if(luchshee_resheniye != NULL)
      z_free(luchshee_resheniye);

    printf("\n\n\n\n\n\n\n\n\n\n\n\n\n");
  }
}
