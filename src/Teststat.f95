!======================================================================
!  
! X: Covariates       res: Y-mu_0 
! n: sample size      p: dimension
! z: the value of standardized statistic U   
! 
!======================================================================


subroutine Teststat(a,n,p,res,s1)
implicit double precision (a-h,q-z)
integer n,p,i,j,k
double precision a(n,p)
double precision h,res,z1,t1,z,t,s1,s,t2 
dimension h(n),res(n)
z=0
t=0
t2=0
do i=1,(n-1)
  do j=(i+1),n
    z1=0
    t1=0
    
    do k=1,p
      z1=z1+a(i,k)*a(j,k)
     
    enddo
    z=z+z1*res(i)*res(j)
    t=t+z1*z1*res(i)*res(j)*res(i)*res(j)
   
  enddo
enddo
s=sqrt(t)
s1=z/s 


end
