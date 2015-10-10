!This is S1 situation
!here is to caculate matrix X1
!here a is epsi1, e is epsi0, b=a
!p is the dimension, res is the vector 
subroutine generX(Z,rou,mu,p,T,res)
implicit double precision (a-h)
integer k,p,j,T
double precision res,Z,rou,mu
dimension res(p),Z(T+p),rou(T),mu(p)
do j=1,p
   res(j)=mu(j)
   do k=1,T
   res(j)=res(j)+rou(k)*Z(j+k-1)
   enddo
enddo

end