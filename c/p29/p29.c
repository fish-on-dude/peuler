#include <stdio.h>
#define MAXLIM 100
int ipow(int, int);
int main(void)
{
  int i, j, a, count=0;
  for (i=2;i<=MAXLIM;i++)
    {
      for (j=2;j<=MAXLIM;j++)
	{
	  count++;
	  for (a=2; (a*j)<=MAXLIM; a++)
	    count--;
	}
    }
  printf("%d\n", count);


}
int ipow(int a, int b)
{
  if(a<=0) return 0;
  else if (b <= 0) return 1;
  else return a * ipow(a, b-1);
}
