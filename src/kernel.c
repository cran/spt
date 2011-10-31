/* Compile under R 
 R CMD SHLIB kernel.c -o kernel.so
*/

#include <R.h>
#include <Rmath.h>

void maxedge(double *xy, double *result)
{
  double l1,l2,l3;
  l1 = sqrt(pow(xy[0]-xy[1],2.)+pow(xy[3]-xy[4],2.));
  l2 = sqrt(pow(xy[0]-xy[2],2.)+pow(xy[3]-xy[5],2.));
  l3 = sqrt(pow(xy[1]-xy[2],2.)+pow(xy[4]-xy[5],2.));
  result[0] = l1;
  if(result[0]<l2) result[0] = l2;
  if(result[0]<l3) result[0] = l3;
}


// to compute the pedal point of point pa on line (pb,pc)
void pedalxy(double *pa, double *pb, double *pc,double *out){
  double x[3],y[3],d[3],l1;
  x[0] = pc[0]; y[0] = pc[1];
  x[1] = pa[0]; y[1] = pa[1];
  x[2] = pb[0]; y[2] = pb[1];
  d[0] = sqrt(pow(x[1]-x[2],2.0)+pow(y[1]-y[2],2.0));
  d[1] = sqrt(pow(x[0]-x[2],2.0)+pow(y[0]-y[2],2.0));
  d[2] = sqrt(pow(x[1]-x[0],2.0)+pow(y[1]-y[0],2.0));
  l1 = 0.5 * (d[1]*d[1]+d[2]*d[2]-d[0]*d[0])/d[1]/d[2];//cos(theta)
  l1 *= d[2];
  out[0] = x[2] - (x[2]-x[0])*(d[1]-l1)/d[1];
  out[1] = y[2] - (y[2]-y[0])*(d[1]-l1)/d[1];
}


/*
To compute the area of a triangle using Heron's formula:
 */
void stri(double *xy, double *result)
{
  double a2,b2,c2, t1,t2;
  a2 = pow(xy[0]-xy[1],2.)+pow(xy[3]-xy[4],2.);
  b2 = pow(xy[0]-xy[2],2.)+pow(xy[3]-xy[5],2.);
  c2 = pow(xy[1]-xy[2],2.)+pow(xy[4]-xy[5],2.);
  t1 = a2+b2+c2;
  t2 = t1 * t1 -2.0*(a2*a2+b2*b2+c2*c2);
  if(t2 > 0.0){t1 = 0.25 * sqrt(t2);}
  else{t1 = 0.0;}
  result[0] = t1;
}

void SPTChild2(double *xy, double *result)
{
  double pa[2], pb[2], pc[2],out[2];
  pa[0] = xy[0]; pb[0] = xy[1]; pc[0] = xy[2];
  pa[1] = xy[3]; pb[1] = xy[4]; pc[1] = xy[5];
  out[0]=0.0; out[1]=0.0;
  pedalxy(pa,pb,pc,out);
  result[0] = out[0];
  result[1] = out[1];
}

void SPTChild3(double *xy, double *result)
{
  double pa[2], pb[2], pc[2],out[2];
  pa[0] = xy[0]; pb[0] = xy[1]; pc[0] = xy[2];
  pa[1] = xy[3]; pb[1] = xy[4]; pc[1] = xy[5];
  out[0]=0.0; out[1]=0.0;

  pedalxy(pa,pb,pc,out);
  result[0] = out[0];
  result[3] = out[1];
  pedalxy(pb,pc,pa,out);
  result[1] = out[0];
  result[4] = out[1];
  pedalxy(pc,pa,pb,out);
  result[2] = out[0];
  result[5] = out[1];
}

void PTChild(double *xy, double *result)
{
  result[0] = 0.5*(xy[1]+xy[2]);
  result[1] = 0.5*(xy[0]+xy[2]);
  result[2] = 0.5*(xy[1]+xy[0]);
  result[3] = 0.5*(xy[4]+xy[5]);
  result[4] = 0.5*(xy[3]+xy[5]);
  result[5] = 0.5*(xy[3]+xy[4]);
}

double fsptdim(double a, double b, double c, double d){
  return(R_pow(cos(a), d)+R_pow(cos(b),d)+R_pow(cos(c),d)-1.);
}

void SptDimFP(double *angles, double *dim){
  /*
    Find the dimension of acute siepinski triangle by finding the 
    root of d in an equation
    cos(A)^d+cos(B)^d+cos(C)^d =1.

    Secant method is used.  (a generalization of Secant method: 
    Method of False position)
  */
  int i;
  double a,b,c, tol = 6.123234e-17;
  double x0=log(3.0)/log(2.0), x1 = 2.0, xi,fc = 1.; //initial values;
  a = angles[0]*PI/180.;
  b = angles[1]*PI/180.;
  c = angles[2]*PI/180.;
  //a suppose to be the larget angle
  if(R_pow(cos(a),x0)<= tol)
    dim[0] = 2.0;
  else{
    i=0;
    while(fabs(fc) > tol && i<100){
      xi = 0.5 *(x0+x1);
      fc = fsptdim(a,b,c,xi);
      if(fc<0){
	x1=xi;}
      else{
	x0=xi;
      }
      i++;
    }
    dim[0] = xi;
  }
}
