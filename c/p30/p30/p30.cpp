// p30.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

int sumfifth(int);
int fifth[10];

int main()
{
	int sum = 0;
	for (int i = 0; i < 10; i++)
	{
		fifth[i] = i;
		for (int j = 1; j < 5; j++)
			fifth[i] *= i;
	}

	for (int i = 2; i < 400000; i++)
	{
		int j = sumfifth(i);
		if (i == j)
		{
			sum += i;
		}
	}
	printf("%d", sum);
	
    return 0;
}
int sumfifth(int i)
{
	int n = i;
	int sum = 0;
	int j = 0;
	while (n > 0)
	{
		int d = n % 10;
		sum += fifth[d];
		n /= 10;
	}
	return sum;
}


