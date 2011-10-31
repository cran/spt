spt <- function(A,B,x0,y0,iter,plot=TRUE,
                spt=TRUE,chaosgame=FALSE,main=NULL,tol=0.0001,...){
  if(missing(A)||missing(B))
    stop("Sizes of at least two angles need to be specified.")
  if(A<=0 || B<=0 || A>=180 || B >=180 || A+B>=180)
    stop("Invaid angle size(s) specified: 0<A<180, 0<B<180, and 0<A+B<180.")
  A1 = A; B1 = B; C1 = 180.0-(A1+B1)
  triname = ifelse(spt,"SPT","ST")
  triname = paste(triname,"(",as.character(format(A1,20)),",",
    as.character(format(B1,20)),",",
    as.character(format(180-A1-B1)),")",sep='')

  angles = sort(c(A1,B1,C1),decreasing=TRUE)
  minA = min(angles); maxA = max(angles);
  r = pi/180;  h=100;
  C = angles[1]*r; B = angles[3]*r; A = angles[2]*r;
  xa = 0; ya=0; xb = h/tan(A)+h/tan(B); yb=0; xc = h/tan(A); yc = h;
  A0 = 0; B0 = pi-B; C0 = pi+A;
  ABC = list(A=A,B=B,C=C,xa=xa,xb=xb,xc=xc,ya=ya,yb=yb,yc=yc,A0=A0,B0=B0,C0=C0);

  xmin = min(xa,xb,xc); xmax=max(xa,xb,xc); 
  ymin = 0; ymax=h;
  if(maxA>90) sptdim = NA
  else if(maxA==90) sptdim = 2.0
  else sptdim = .Fortran(.F_SptDimFP, as.double(angles),as.double(0))[[2]]
  ##  sptdim = .Fortran(F_SptDimFP, as.double(angles),as.double(0))[[2]]
  if(plot){
    if(is.null(main)){
      ##    if(is.na(sptdim)||sptdim < 0)
      cond1 = is.null(sptdim)
      cond2 = is.na(sptdim)
      if(cond1 || cond2)
        main=paste(triname, ", Dim = NA",sep='')
      else
        main=paste(triname, ", Dim = ",as.character(format(sptdim,20)),sep='')
    }
    if(maxA > 90 && spt){
      delta = abs(h * sin(maxA))*.25
      xmin = xmin - delta;
      delta = (xmax-xmin-ymax+ymin)*.75;
      ymin = ymin - delta;
      ymax = ymax + delta;
    }
    
    ## draw the initial triangle
    plot(0,0, type='n', bty='n', xaxt='n',yaxt='n',
         xlab='', ylab='', asp=1, main = main,
         xlim=c(xmin,xmax), ylim=c(ymin,ymax))
    lines(c(ABC$xa,ABC$xb,ABC$xc,ABC$xa),
          c(ABC$ya,ABC$yb,ABC$yc,ABC$ya),
          lwd=2);
    
    if(chaosgame){
      if(missing(x0)) x0 = (xmax-xmin)*runif(1)+xmin;
      if(missing(y0)) y0 = (ymax-ymin)*runif(1)+ymin;
      if(missing(iter)) iter=10000;
      
      points(x0,y0,pch='*', col=5)
      points(ABC$xa,ABC$ya,pch=20, col=2)
      points(ABC$xb,ABC$yb,pch=20, col=3)
      points(ABC$xc,ABC$yc,pch=20, col=4)
      if(spt){
        .GameSPT(x0,y0,ABC,iter)
      }else{
        .GameST(x0,y0,ABC,iter)
      }
    }else{
      if(missing(iter)) iter = 20
      Tri0 = cbind(c(xa,xb,xc), c(ya,yb,yc));
      tol = tol * (xmax-xmin) * (ymax-ymin)
      if(iter>0){
        if(!spt) .ptpedal(iter,Tri0,tol)
        else if(angles[1]==90){
          .sptpedal2(iter,Tri0,tol)
        }else{
          iter = ifelse(maxA>90, min(12,iter),iter)
          .sptpedal3(iter,Tri0,tol)
        }
      }
    }
  }
  
  if(spt){
    out = structure(list(angles,
      data.name = triname,
      Dim = sptdim, iter=iter
      ), class = "spt")
    cat("\n\nThe SPT dimension is", format(sptdim,10),"\n\n");
  }else out=NULL
  
  invisible(out)
}

##  Draw PT/SPT: tri1 is the initial triangle, n is the iteration number.
.ptpedal <- function(n, tri1,tol){
  if(n>1){
    tmp = .Fortran(.F_PTChild, as.double(as.vector(tri1)), as.double(rep(0,6)));
    result = matrix(tmp[[2]], ncol=2, nrow=3);
    polygon(result[,1],result[,2])
    tri1.1 = rbind(tri1[3,], result[2,], result[1,]);
    tri1.2 = rbind(tri1[2,], result[1,], result[3,]);
    tri1.3 = rbind(tri1[1,], result[3,], result[2,]);
    if(.Stop(tri1) > tol ){
      Recall(n-1, tri1.2,tol);
      Recall(n-1, tri1.3,tol);
      Recall(n-1, tri1.1,tol);
    }
  }
}

.sptpedal2 <- function(n, tri1,tol){
  if(n>1){
    tmp = .Fortran(.F_SPTChild2, as.double(as.vector(tri1)), as.double(rep(0,2)));
    tmp = matrix(tmp[[2]],nrow=1);
    segments(tmp[1],tmp[2],tri1[1,1],tri1[1,2]);
    tri1.1 = rbind(tmp, tri1[3,], tri1[1,]);
    tri1.2 = rbind(tmp, tri1[1,], tri1[2,]);
    if(.Stop(tri1) > tol ){
      Recall(n-1, tri1.2,tol);
      Recall(n-1, tri1.1,tol);
    }
  }
}

.sptpedal3 <- function(n, tri1, tol){
  if(n>1){
    tmp = .Fortran(.F_SPTChild3, as.double(as.vector(tri1)), as.double(rep(0,6)));
    result = matrix(tmp[[2]], ncol=2, nrow=3);
    polygon(result[,1],result[,2])
    tri1.1 = rbind(tri1[3,], result[2,], result[1,]);
    tri1.2 = rbind(tri1[2,], result[1,], result[3,]);
    tri1.3 = rbind(tri1[1,], result[3,], result[2,]);
    if(.Stop(tri1) > tol ){
      Recall(n-1, tri1.2,tol);
      Recall(n-1, tri1.3,tol);
      Recall(n-1, tri1.1,tol);
    }
  }
}

.Stop <- function(triangle){
  axis = as.vector(triangle);
  .Fortran(.F_stri, as.double(axis), as.double(0))[[2]]
}

##########################################################################
##   Chaos Games:

.stepSPT <- function(x0,y0,xa,ya,A0,A){
  ratio = cos(A);
  tx0 = ratio*(x0-xa);
  ty0 = ratio*(y0-ya);
  l = sqrt(tx0^2+ty0^2);
  ang = atan(ty0/tx0);
  if(is.nan(ang)){
    x=0;y=0;
  }else{
    if(tx0<0) ang = pi+ang;
    if(ang<0) ang=2*pi+ang;
    angt = 2*A0-ang+A;
    y = l*sin(angt); x = l*cos(angt);
  }
  list(x=x+xa,y=y+ya);
}

##  Go randomly towards each of the three vertices, A,B and C with
##  probability pa = (cos(a))^2, pb = (cos(b))^2 and pc = (cos(c))^2,
##  respectively. Each step, stop at point 'e' having distnace
##  sqrt(prob)*l to the vertice. Then reflect point 'e' over the line
##  equally dividing "angle a".

## Obtuse triangle: not applicable; Right triangle, move the the
## vertice of the right angle.  So assign zero probability to it
## (Two color only).  Dividing p is necessary!  The probability
## does not matter much.  Can be adjuested to mke the graph looks
## better.

.GameSPT <- function(x0,y0,ABC,iter)
  {
    ## dividing p is necessary!
    pwr=2
    pa = (cos(ABC$A))^pwr
    pb = (cos(ABC$B))^pwr
    pc = (cos(ABC$C))^pwr
    p = pa + pb + pc 
    pa = pa/p; pb = pb/p;
    pa = 1/3;pb=1/3
    for(i in 1:iter){
      coin=runif(1);
      if(coin<pa){
         X1 = .stepSPT(x0,y0,ABC$xa,ABC$ya,ABC$A0, ABC$A);
         x0=X1$x; y0=X1$y;
         points(x0,y0, col=2,pch='.');
      }else if(coin<pb+pa){
        X1 = .stepSPT(x0,y0,ABC$xb,ABC$yb,ABC$B0, ABC$B);
        x0=X1$x; y0=X1$y;
        points(x0,y0, col=3,pch='.')
      }else{
        X1 = .stepSPT(x0,y0,ABC$xc,ABC$yc,ABC$C0, ABC$C);
        x0=X1$x; y0=X1$y;
        points(x0,y0, col=4,pch='.')
      }
    }
    list(x=x0,y=y0);
  }

##  with prob 1/3, go towards each of the three vertices randomly.
##  Each step, go half distance and mark the end points the same color
##  as the approaching vertice.
.GameST<- function(x0,y0,ABC,iter) 
  {
    for(i in 1:iter){
      coin=runif(1);
      if(coin<1/3){
      	x0=(x0+ABC$xa)/2; y0=(y0+ABC$ya)/2;
      	points(x0,y0, col=2,pch='.')
      }else if(coin<2/3){
      	x0=(x0+ABC$xc)/2; y0=(y0+ABC$yc)/2;
        points(x0,y0, col=4,pch='.')
      }else{
      	x0=(x0+ABC$xb)/2; y0=(y0+ABC$yb)/2;
        points(x0,y0, col=3,pch='.')
      }
    }
    list(x=x0,y=y0);
  }



##########################################################################


.arc <- function(xa,ya,xb,yb,xc,yc,l=10, iter=20, col='blue', n=1)
  # arcs will be drawn counter-clockwisely;
  # theta1: starting line
  # theta2: angle in-between
  # 
{
  theta1 = atan((yb-ya)/(xb-xa));
  if(xb-xa<0) theta1 = pi+theta1;
  theta2 = atan((yc-ya)/(xc-xa));
  if(xc-xa<0) theta2 = pi+theta2;
  if(theta2<theta1) theta2=theta2+2*pi;
  da = (theta2-theta1)/iter;
  dl=.2*l;
  for(j in 1:n){
    l=l+dl;
    X=rep(0,iter);
    Y=rep(0,iter);
    for(i in 1:iter){
      X[i] = l * cos(theta1 + i*da) + xa;
      Y[i] = l * sin(theta1 + i*da) + ya;
    }
    lines(X,Y, col=col);
  }
}
