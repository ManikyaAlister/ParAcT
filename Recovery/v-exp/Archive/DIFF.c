#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>

int DIFF(double *z, double *v,double *aU, double *aL, double *ter,
          double *sv, double *sz, double *ster,
          double *s, double *h, double *resp, double *rt,
          double *n, double *maxiter,
          int *rangeLow, int *rangeHigh, double *randomTable)
{
  double rhs,x,randNum,sampleV,sampleTer;
  int N,i,j,iter,Maxiter,blah;
  
  N=(int) *n;
  Maxiter =(int) *maxiter;
  GetRNGstate();
  rhs=sqrt(*h)*(*s);
  
  
  
  for (i=0;i<N;i++) {
    if (*sz < 0.00001) {
      x=*z;
    } else {
      x = (*z) + ((rand()/(1.0 + RAND_MAX))*(*sz)) - ((*sz)/2);
    }
    
    if (*sv < 0.00001) {
      sampleV=*v;
    } else {
      randNum = rand()/(1.0 + RAND_MAX);
      blah = (*rangeHigh) - (*rangeLow) + 1;
      blah = (randNum * blah) + (*rangeLow);
      randNum = randomTable[blah];
      
      sampleV = (*v) + ((*sv)*randNum);
    }
    
    if (*ster < 0.00001) {
      sampleTer=*ter;
    } else {
      sampleTer = (*ter) + ((rand()/(1.0 + RAND_MAX))*(*ster)) - ((*ster)/2);
    }
    
    
    iter=0;
    resp[i]=(double) -1.0 ;
    do 
    {
      iter = iter+1;
      
      randNum = rand()/(1.0 + RAND_MAX);
      blah = (*rangeHigh) - (*rangeLow) + 1;
      blah = (randNum * blah) + (*rangeLow);
      randNum = randomTable[blah];
      
      x = x + (sampleV*(*h)) + (rhs*randNum);
      
      if (x>=*aU) {
        resp[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        resp[i]=(double) 2.0 ; 
        break ;
      }
    } while (iter<Maxiter) ; 
    rt[i]=(((double) iter)*(*h) - (*h)/((double) 2.0)) + sampleTer;
  }
  PutRNGstate();
}

