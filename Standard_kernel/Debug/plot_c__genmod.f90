        !COMPILER-GENERATED INTERFACE MODULE: Sat May 18 13:37:05 2019
        MODULE PLOT_C__genmod
          INTERFACE 
            SUBROUTINE PLOT_C(N,COUCHY,X,INDEX_SECTION,COUNT_SECTION)
              INTEGER(KIND=4) :: COUNT_SECTION
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: COUCHY(2,2,N)
              REAL(KIND=8) :: X(2,N)
              INTEGER(KIND=4) :: INDEX_SECTION(COUNT_SECTION)
            END SUBROUTINE PLOT_C
          END INTERFACE 
        END MODULE PLOT_C__genmod
