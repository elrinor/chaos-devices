#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define READKEY getc(stdin)

int n, m; // размер таблицы коэффициентов
float **a, **b; // (n+1)*(m+1) таблица коэффициентов
float* sol; // текущее приближение решения
bool* up; // вверху ли Xi?
int* xi; // положение Xi в строке / столбце
int* xrow; // какие Xi у нас слева
int* xcol; // какие Xi у нас сверху (если число отрицательно - то Yi)
int k; // кол-во свободных переменных
bool* used; // исполизовать ли i-ю строку в вычислениях (побочный эф-т свободных переменных)
float eps; // текущее eps

int counteps()
{
	eps = 0;
	for(int i = 0; i <= m; i++) for(int j = 0; j <= n; j++)
		eps = __max(eps, fabs(a[i][j]));
	eps *= 1e-7f;
	return 0;
}

int out(int row, int col)
{
	if(row != -1)
	{
		printf("%s", "Current Solution: (");
		for(int i = 0; i < n; i++)
		{
			printf("% 6.2f", sol[i]);
			if(i != n - 1)
				printf("%s", ",");
		}
		printf("%s", ")\n");
		printf("Base Point: (% 2d, % 2d)\n", row, col);
	}

	printf("%s","   |");
	for(int i = 0; i < n; i++)
		if(xcol[i] < 0)
			printf("   -Y%02d", -xcol[i]-1);
		else
			printf("   -X%02d", xcol[i]);
	printf("%s","      1\n---+");
	for(int i = 0; i <= n; i++)
		printf("%s", "-------");
	printf("\n");
	for(int i = 0; i <= m; i++)
	{
		if(i == m)
			printf("%s", "  Z|");
		else if(xrow[i] < 0)
			printf("Y%02d|", -xrow[i]-1);
		else
			printf("X%02d|", xrow[i]);
		for(int j = 0; j <= n; j++)
			printf("% 7.2f", a[i][j]);
		printf("\n");
	}
	printf("%s", "---+");
	for(int i = 0; i <= n; i++)
		printf("%s", "-------");
	printf("\n\n");
	READKEY;
	return 0;
}

int mzi(int row, int col)
{
	for(int i = 0; i <= m; i++) for(int j = 0; j <= n; j++)
	{
		if(i == row && j == col)
			b[i][j] = 1 / a[row][col];
		else if(i == row)
			b[i][j] = a[i][j] / a[row][col];
		else if(j == col)
			b[i][j] = -a[i][j] / a[row][col];
		else
			b[i][j] = a[i][j] - a[i][col] * a[row][j] / a[row][col];
	}
	float** tmp = a;
	a = b;
	b = tmp;

	if(xcol[col] >= 0)
	{
		xi[col] = row;
		up[xcol[col]] = !up[xcol[col]];
	}
	else
	{
		xi[row] = col;
		up[xrow[row]] = !up[xrow[row]];
	}
	int itmp = xrow[row];
	xrow[row] = xcol[col];
	xcol[col] = itmp;

	for(int i = 0; i < n; i++) 
		if(up[i])
			sol[i] = 0;
		else
			sol[i] = a[xi[i]][n];

	counteps();
	out(row, col);

	return 0;
}

int main()
{
	FILE* f = fopen("in.txt", "r");
	fscanf(f, "%d%d", &n, &m);
	
	a = (float**)malloc((m + 1) * sizeof(float*));
	b = (float**)malloc((m + 1) * sizeof(float*));
	for(int i = 0; i <= m; i++)
	{
		a[i] = (float*)malloc((n + 1) * sizeof(float));
		b[i] = (float*)malloc((n + 1) * sizeof(float));
	}

	for(int j = 0; j < m; j++) for(int i = 0; i <= n; i++)
		fscanf(f, "%f", &a[j][i]);

	for(int i = 0; i < n; i++)
		fscanf(f, "%f", &a[m][i]);
	a[m][n] = 0;

	counteps();

	sol = (float*)malloc(n * sizeof(float));
	up = (bool*)malloc(n * sizeof(bool));
	memset(up, 0xFF, n * sizeof(bool));
	xi = (int*)malloc(n * sizeof(int));
	for(int i = 0; i < n; i++)
		xi[i] = i;
	xrow = (int*)malloc(m * sizeof(int));
	for(int i = 0; i < m; i++)
		xrow[i] = -i - 1;
	xcol = (int*)malloc(n * sizeof(int));
	for(int i = 0; i < n; i++)
		xcol[i] = i;

	out(-1,-1);
	used = (bool*)malloc((m + 1) * sizeof(bool));
	memset(used, 0x00, (m + 1) * sizeof(bool));
	fscanf(f, "%d", &k);
	for(int i = 0; i < k; i++)
	{
		int l;
		fscanf(f, "%d", &l);
		int j;
		for(j = 0; j < m; j++) if(!used[j])
			if(fabs(a[j][l]) > eps)
				break;
		if(j != m)
		{
			mzi(j, l);
			used[j] = true;
		}
		else
		{
			//all zeros? - doesn't affect solution
			continue;
		}
	}

	while(true)
	{
		int i;
		for(i = 0; i < m; i++) if(!used[i])
			if(a[i][n] < -eps)
				break;
		if(i != m)
		{
			int j;
			for(j = 0; j < n; j++)
				if(a[i][j] < -eps)
					break;
			if(j != n)
			{
				int mink = -1;
				float minv = 1e20f;
				for(int l = 0; l < m; l++) if(!used[l])
					if(fabs(a[l][j]) > eps && a[l][n]/a[l][j] < minv)
					{
						if((fabs(a[l][n]) < eps && a[l][j] < -eps) || a[l][n]/a[l][j] < -eps)
							continue;
						mink = l;
						minv = a[l][n]/a[l][j];
					}
				mzi(mink, j);
			}
			else
			{
				printf("Solution does not exist, the specified region contains no points.");
				READKEY;
				exit(0);
			}
		}
		else
			break;
	}

	for(int i = 0; i < n; i++) 
		if(up[i])
			sol[i] = 0;
		else
			sol[i] = a[xi[i]][n];
	printf("%s", "Partial Solution Found: (");
	for(int i = 0; i < n; i++)
	{
		printf("% 6.2f", sol[i]);
		if(i != n - 1)
			printf("%s", ",");
	}
	printf("%s", ")\n\n");

	while(true)
	{
		int p;
		for(p = 0; p < n; p++)
			if(a[m][p] > eps)
				break;
		if(p != n)
		{
			int mink = -1;
			float minv = 0;
			for(int i = 0; i < n; i++)
				if(a[m][i] > minv)
				{
					minv = a[m][i];
					mink = i;
				}
			int q;
			for(q = 0; q < m; q++) if(!used[q])
				if(a[q][mink] > eps)
					break;
			if(q != m)
			{
				int minn = -1;
				float minv = 1e20f;
				for(int i = 0; i < m; i++) if(!used[i])
					if(fabs(a[i][mink]) > eps && a[i][n]/a[i][mink] < minv)
					{
						if((fabs(a[i][n]) < eps && a[i][mink] < -eps) || a[i][n]/a[i][mink] < -eps)
							continue;
						minn = i;
						minv = a[i][n]/a[i][mink];
					}
				mzi(minn, mink);
			}
			else
			{
				printf("Solution does not exist, Z(X) is unlimited.");
				READKEY;
				exit(0);
			}
		}
		else
			break;
	}

	for(int i = 0; i < n; i++) 
		if(up[i])
			sol[i] = 0;
		else
			sol[i] = a[xi[i]][n];
	printf("%s", "Minimum Found: Z(");
	for(int i = 0; i < n; i++)
	{
		printf("% 6.2f", sol[i]);
		if(i != n - 1)
			printf("%s", ",");
	}
	printf(") = % 6.2f\n\n", a[m][n]);
	READKEY;

	fclose(f);
	return 0;
}