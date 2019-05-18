        !COMPILER-GENERATED INTERFACE MODULE: Sat May 18 18:56:29 2019
        MODULE COMPUTE_VISCOSITY__genmod
          INTERFACE 
            FUNCTION COMPUTE_VISCOSITY(HI,HJ,CS,RHO_0,XI,VI,XJ,VJ)
              REAL(KIND=8) :: HI
              REAL(KIND=8) :: HJ
              REAL(KIND=8) :: CS
              REAL(KIND=8) :: RHO_0
              REAL(KIND=8) :: XI(1:2)
              REAL(KIND=8) :: VI(1:2)
              REAL(KIND=8) :: XJ(1:2)
              REAL(KIND=8) :: VJ(1:2)
              REAL(KIND=4) :: COMPUTE_VISCOSITY
            END FUNCTION COMPUTE_VISCOSITY
          END INTERFACE 
        END MODULE COMPUTE_VISCOSITY__genmod
