subroutine Compute_Acceleration(cs,N,h,dh,rho_0,mu,k,eta,damping,vol,F,Couchy,PK1,x,x_old,v_old,nabla_W_0,nabla_W,W,Wper1,Wper2,Wper3,Wper4,acc,count_hole,count_section,index_section,index_hole,Ci,Ci_new,table,friction)
    integer :: N,i,j,alpha,beta,k1,k2,count_hole,count_section
    
    real*8 :: cs
    real*8 :: dh
    real*8 :: dt
    real*8 :: rho_0
    real*8 :: mu
    real*8 :: k
    real*8 :: eta
    real*8 :: damping
    real*8 :: friction
    real*8 :: vol(N)
    real*8 :: h(N)
    real*8 :: F(2,2,N)
    real*8 :: Ci(2,2,N)
    real*8 :: Ci_new(3,3,N)
    real*8 :: Couchy(2,2,N)
    real*8 :: PK1(2,2,N)
    real*8 :: x(2,N)
    real*8 :: x_old(2,N)
    real*8 :: v_old(2,N)
    real*8 :: nabla_W_0(2,N,N)
    real*8 :: nabla_W(2,N,N)
    real*8 :: W(N,N)
    real*8 ::Wper1(N,N)
    real*8 ::Wper2(N,N)
    real*8 ::Wper3(N,N)
    real*8 ::Wper4(N,N)
    real*8 :: acc(2,N)
     real*8 :: Pi_ij
    integer :: index_section(count_section)
    integer :: index_hole(count_hole)
    integer :: table(N,120)

   ! call compute_W_cor(x,x,h,N,vol,W)
    !call Compute_nabla_W(x,h,vol,N,W,Wper1,Wper2,Wper3,Wper4,nabla_W,dh)
    call Compute_F(vol,x,x_old,nabla_W_0,N,F,table)
    call  OneStepPlasticity(F,mu,k,eta,dt,Ci,N,Couchy,Ci_new,PK1,friction)
    Ci(1:2,1:2,1:N)=Ci_new(1:2,1:2,1:N)
  !  call Compute_Stress_PK1(F,Couchy,PK1,mu,k,N)

    acc=0

    do i=1,N
        do j=1,table(i,1)
            do beta=1,2
                do alpha=1,2  
                    acc(alpha,i)=acc(alpha,i)+(vol(table(i,j+1)))*(PK1(alpha,beta,table(i,j+1))+PK1(alpha,beta,i))*nabla_W_0(beta,i,table(i,j+1))
                enddo
            enddo
        enddo

        do alpha=1,2
            !Pi_ij=Compute_Viscosity(h(1),h(1),cs,rho_0,x(1:2,i),v_old(1:2,i),x(1:2,table(i,j+1)),v_old(1:2,table(i,j+1)))
           ! acc(alpha,i)=acc(alpha,i)+Pi_ij*nabla_W_0(beta,table(i,j+1),i)*(vol(table(i,j+1)))
            acc(alpha,i)=acc(alpha,i)-damping*v_old(alpha,i)
            acc(alpha,i)=acc(alpha,i)/rho_0
        enddo
    enddo
    
 
  
    
  

    
    return
    end