subroutine Compute_W_cor(x,xper,h,N,vol,W,table)
     integer :: N
       real*8 :: x(2,N)
       real*8 :: xper(2,N)
       real*8 :: h(N)
       real*8 :: vol(N)
       real*8 :: W(N,N)
        
        real*8 ::xi(2)
        real*8 ::xj(2)
        real*8 ::r(2)
        real*8 ::sumW(2)
        real*8 ::betaij(2)
        real*8 ::cormat(2,2)
        real*8 ::cormat_tmp(2,2)
        real*8 ::alpha,detcormat_tmp
        integer :: table(N,120)
        
        do i=1,N
            sumW=0
            alpha=0
            betaij=0
            cormat=0
            cormat_tmp=0
            xi=xper(1:2,i)
            
            do j=1,table(i,1)
                xj=x(1:2,table(i,j+1))
                r=xi-xj
                W(i,table(i,j+1))=Compute_W(xi,xj,h(i),h(table(i,j+1)))
            enddo
            
        enddo
        return
    end 