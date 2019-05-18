 SUBROUTINE Plot_C(N,Couchy,x,index_section,count_section)
    use ogpf
    integer :: N,k1,count_section
    integer :: index_section(count_section)
    real*8 :: x(2,N)
    real*8 :: Couchy(2,2,N)
        !...............................................................................
        !Example 1: A very basic example
        !...............................................................................
        TYPE(gpf):: gp

        Real(wp):: xg(n)
        Real(wp):: yg(n)
        Real(wp):: xh(count_section)
        Real(wp):: yh(count_section)

        ! Input data
      
            xg=x(1,1:N)
            yg=x(2,1:N)
            xh=0
            yh=0
            do k1=1,count_section 
                    xh(k1)=x(1,index_section(k1))-0.7d0
                    yh(k1)=Couchy(2,2,index_section(k1))
            enddo
    
            ! Annotation: set title, xlabel, ylabel
            CALL gp%title('Example 1')
            CALL gp%xlabel('x [mm]')
            CALL gp%ylabel('C [Mpa]')
            Call gp%options('set style data linespoints')
            !Call Plot to draw a vector against a vector of data
            call gp%options('set xrange[-0.1:1.26];set yrange [-1000:1000];')
           call gp%plot(xh, yh,'title "True Stress" lt 6 lc rgb "#000000"')
           ! call gp%plot(xh, yh,'title "square coordinates" with points lt 6 lc rgb "#000000"')
  
        
      
    END SUBROUTINE Plot_C