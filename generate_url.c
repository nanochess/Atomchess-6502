/*
** Creates a program URL for visual6502.org
**
** Public domain by Oscar Toledo G.
**
** Creation date: Jan/16/2017.
*/

#include <stdio.h>

/*
** Main program
*/
int main(void)
{
  FILE *a;
  int c;

  a = fopen("toledo_atomchess_6502.bin", "rb");
  printf("http://www.visual6502.org/JSSim?graphics=false&headlesssteps=1000&r=0100&a=0100&d=");
  while (!feof(a))
    printf("%02x", fgetc(a) & 0xff);
  printf("\n");
  fflush(stdout);  
  fclose(a);
}
