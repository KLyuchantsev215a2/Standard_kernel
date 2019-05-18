subroutine Create_Table(x,h,table,N,dh)
    integer :: N,i,j
    real*8 :: x(2,N)
    real*8:: h(N)
    real*8:: dh
    
    integer :: table(N,120)
    
    real*8::neighbour
    real*8::xi(2)
    real*8::xj(2)
    
    
    do i=1,N
        xi=x(1:2,i)
        cout=0

        
        do j=1,N
        
        xj=x(1:2,j)
        
        neighbour=Compute_W(xi,xj,h(i)+2*dh,h(j)+2*dh) 
           
            if (neighbour>0) then
                 if((abs((x(1,i)>=0.7))+abs((x(1,j)>=0.7))+abs((x(2,i)*x(2,j)>0))+abs(i==j))/=0) then
                    cout=cout+1
                    table(i,cout+1)=j
                endif
            endif
            
        enddo
        table(i,1)=cout
    enddo

end