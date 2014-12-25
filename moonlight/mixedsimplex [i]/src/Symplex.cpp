#include "Symplex.h"
#include <stdio.h>
#include <malloc.h>
#include <float.h>


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
			//prepare
		bas   = (int*) malloc((m+1)*sizeof(int));
		nebas = (int*)malloc((n+1)*sizeof(int));
		int i,j;
		for (i=1; i<=m;  i++) bas[i]=n+1;
		for (i=0; i<n;  i++) nebas[i]=i;

		//опорное решение
		int k,s,r;
		double min,work;
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
		for (int i=1; i<=m; i++)
			if (A[i*(n+1)+k]<0)
				return i;
		return -1;
}
	//есть ли отрицательный в k-той строке
int _est_negative_stroka (int k,int m, int n, double *A)
{
		for (int j=0; j<n; j++)
			if (A[k*(n+1)+j]<0)
				return j;
		return -1;
}
	//есть ли положительный в k-том столбце
int _est_positive_stolb  (int k,int m, int n, double *A)
{
		for (int i=1; i<=m; i++)
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

#if 0
int main()
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
}

#endif