        !COMPILER-GENERATED INTERFACE MODULE: Sat May 18 13:37:33 2019
        MODULE COMPUTE_W_COR__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W_COR(X,XPER,H,N,VOL,W,TABLE)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: XPER(2,N)
              REAL(KIND=8) :: H(N)
              REAL(KIND=8) :: VOL(N)
              REAL(KIND=8) :: W(N,N)
              INTEGER(KIND=4) :: TABLE(N,120)
            END SUBROUTINE COMPUTE_W_COR
          END INTERFACE 
        END MODULE COMPUTE_W_COR__genmod
