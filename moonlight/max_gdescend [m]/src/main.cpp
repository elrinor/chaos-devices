#define _CRT_SECURE_NO_DEPRECATE

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Windows.h>

#define sqr(x) ((x)*(x))

double f(double x, double* k)
{
	return k[0]*sin(k[1]*x)+k[2]*cos(k[3]*x)+k[4]*exp(k[5]*x)+k[6]*x*x+k[7]*x+k[8];
}

double k[9]; // коэф-ты ф-ии
int n; // кол-во разбиений
double* y; // значения в точках разбиения
double* x; // точки разбиения
int s; // степень многочлена
double* kf; // коэф-ты многочлена
double eps; // точность
int itn=0; // кол-во итераций
double errb=1.0,erra=1.0; //ошибки вычислений
double c, d; // границы интервала


double* grad(double* ret, double* kfs)
{
	for(int j=0; j<=s; j++)
	{
		ret[j] = 0;
		for(int i=0; i<=n; i++)
		{
			double t=0;
			for(int k=0; k<=s; k++)
				t=t*x[i]+kfs[k];
			t=(y[i]-t);
			for(int k=0; k<s-j; k++)
				t*=x[i];
			ret[j]+=t;
		}
		ret[j]*=-2;
	}
	return ret;
}

double ff(double* kfs)
{
	double result=0;
	for(int i=0; i<=n; i++)
	{
		double t=0;
		for(int k=0; k<=s; k++)
			t=t*x[i]+kfs[k];
		t=sqr(y[i]-t);
		result+=t;
	}
	return result; 
}

int solve()
{
	double *a0, *a1;
	double *v0, *v1;
	double *g0, *g1;
	double *t0, *t1;
	double *tmp;
	a0 = (double*)malloc(sizeof(double)*(s+1));
	a1 = (double*)malloc(sizeof(double)*(s+1));
	v0 = (double*)malloc(sizeof(double)*(s+1));
	v1 = (double*)malloc(sizeof(double)*(s+1));
	g0 = (double*)malloc(sizeof(double)*(s+1));
	g1 = (double*)malloc(sizeof(double)*(s+1));
	t0 = (double*)malloc(sizeof(double)*(s+1));
	t1 = (double*)malloc(sizeof(double)*(s+1));

	for(int i=0; i<=s; i++)
	{
		v0[i] = 0;
		v1[i] = 0;
		a0[i] = 0;
		a1[i] = 0;
		g0[i] = 1;
		g1[i] = 0;
		t0[i] = 0;
		t1[i] = 0;
	}

	double eeps;
	do{

		grad(g1,a0);
		double bt,s0=0,s1=0;
    for(int i=0; i<=s; i++)
		{
			s0+=sqr(g0[i]);
			s1+=sqr(g1[i]);
		}
		bt=errb*s1/s0;
		for(int i=0; i<=s; i++)
			v1[i]=-g1[i]+bt*v0[i];

    for(int i=0; i<=s; i++)
		{
			t0[i]=a0[i]-v1[i];
			t1[i]=a0[i]+v1[i];
		}

		// считаем мю итое и по ним - лямбда
		double m1=ff(t0),m2=ff(a0),m3=ff(t1);
		double lmb=erra*(m1-m3)/(2*(m1-2*m2+m3));

		for(int i=0; i<=s; i++)
			a1[i]=a0[i]+lmb*v1[i];

		// теперь считаем дельта
		double na0=0;
		eeps=0;
		for(int i=0; i<=s; i++)
		{
			na0+=sqr(a0[i]);
			if(fabs(a0[i]-a1[i])>eeps)
				eeps=fabs(a0[i]-a1[i]);
		}
		eeps=eeps/sqrt(na0);


		tmp=a0;a0=a1;a1=tmp;
		tmp=v0;v0=v1;v1=tmp;
		tmp=g0;g0=g1;g1=tmp;

		itn++;
	} while (eeps>eps);

	for(int i=0; i<=s; i++)
		kf[i] = a0[i];

	return 0;
}


int test13()
{
	printf("\nAdditional task #13:\n");
	printf("Time for 1000000 iterations.\n");

	unsigned int start_time = GetTickCount();
	//gorner
	for(int i=0; i<1000000; i++)
	{
		double t=0;
		for(int k=0; k<=s; k++)
			t=t*x[1]+kf[k];
	}
	unsigned int g_time = GetTickCount() - start_time;

	printf("Gorner: %.3lf sec\n",(double)g_time/1000);

	start_time = GetTickCount();
	//simple
	for(int i=0; i<1000000; i++)
	{
		double t=0;
		for(int k=0; k<=s; k++)
		{
			double xn = 1;
			for(int j=s; j>k; j--)
				xn*=x[1];
			t+=xn*kf[k];
		}
	}
	unsigned int s_time = GetTickCount() - start_time;

	printf("Simple: %.3lf sec\n",(double)s_time/1000);

	return 0;
}

int Test12BeginTickCount;

int test12begin()
{
	Test12BeginTickCount = GetTickCount();
	return 0;
}

int test12end()
{
	printf("\nAdditional task #12:\n");
	printf("Solving time: %.4lf sec.\n", (GetTickCount()-Test12BeginTickCount)/1000.0f);
	return 0;
}


int test01()
{
	int n1 = n;
	float eps1 = eps;

	y = (double*)realloc(y, sizeof(double)*(4*n+1));
	x = (double*)realloc(x, sizeof(double)*(4*n+1));

	printf("\nAdditional task #1:\n");
	printf("    eps           eps/10        eps/100\n");
	printf(" N  ");

	eps=eps1;    solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/10; solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/100;solve(); printf("(% 4d % 6.2f)\n",itn,ff(kf));

	printf("2N  ");
	n=n1*2;
	for(int i=0; i<=n; i++)
	{
		x[i] = c+(d-c)*i/n;
		y[i] = f(x[i],k);
	}
	eps=eps1;    solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/10; solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/100;solve(); printf("(% 4d % 6.2f)\n",itn,ff(kf));

	printf("4N  ");
	n=n1*4;
	for(int i=0; i<=n; i++)
	{
		x[i] = c+(d-c)*i/n;
		y[i] = f(x[i],k);
	}
	eps=eps1;    solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/10; solve(); printf("(% 4d % 6.2f) ",itn,ff(kf));
	eps=eps1/100;solve(); printf("(% 4d % 6.2f)\n",itn,ff(kf));

	return 0;
}

int test10()
{
	double err;
	printf("\nAdditional task #10:\n");
	printf("Enter error value: ");
	scanf("%lf", &err);
	
	erra=err;
	itn=0;
	solve();
	printf("When disturbing lambda(k): %d iterations, F(a)=%lf\n",itn,ff(kf));
	
	errb=err;
	erra=1.0;
	itn=0;
	solve();
	printf("When disturbing  betta(k): %d iterations, F(a)=%lf\n",itn,ff(kf));
	return 0;
}


int main()
{
	FILE* in=fopen("in", "r");
	
	for(int i=0; i<9; i++)
		fscanf(in,"%lf",k+i);

	fscanf(in,"%lf%lf",&c,&d);

	fscanf(in,"%d",&n);
  y = (double*)malloc(sizeof(double)*(n+1));
	x = (double*)malloc(sizeof(double)*(n+1));
	
	for(int i=0; i<=n; i++)
	{
		x[i] = c+(d-c)*i/n;
		y[i] = f(x[i],k);
	}
	
	fscanf(in,"%d",&s);
	kf = (double*)malloc(sizeof(double)*(s+1));

	for(int i=0; i<=s; i++)
		kf[i] = 0;

	fscanf(in,"%lf", &eps);


	test12begin();
	solve();

	printf("f=%.2lfsin(%.2lfx)+%.2lfcos(%.2lfx)+%.2lfexp(%.2lfx)+%.2lfx^2+%.2lfx+%.2lf\n",k[0],k[1],k[2],k[3],k[4],k[5],k[6],k[7],k[8]);
	printf("P=");
	for(int i=0; i<=s; i++)
	{
		if(kf[i]>0 && i!=0)
			printf("+");
		if(i==s)
			printf("%.4lf\n",kf[i]);
		else if(i==s-1)
			printf("%.4lfx",kf[i]);
		else
			printf("%.4lfx^%d",kf[i],s-i);
	}

	printf("F(a)=%lf\n",ff(kf));
	printf("%d iterations.\n",itn);

	test12end();
	test01();
	test10();
	test13();

	scanf("%d");

	return 0;
}
