subroutine dev (M,devM)
            real*8:: M(3,3)
            real*8:: devM(3,3)
            devM=M
          
            do alpha=1,3
                devM(alpha,alpha)=devM(alpha,alpha)-(1.0d0/3.0d0)*(M(1,1)+M(2,2)+M(3,3))
            enddo
             
    end