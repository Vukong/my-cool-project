program live_histogram
implicit none
integer win,key,L,D,total,count,i,j,k,i0,k0
real a(25,25),p1,p2,mass(25),h,mass_total
a=0
total=1
mass=0
L=300
D=25
h=real(4*L)/D
mass_total=0

call gopen(4*L,2*L,win)
call newwindow(win,0.0,0.0,real(4*L-1),1.1*2*L)
call gsetbgcolor(win,'white'//CHAR(0))
call layer(win,0,1)
call gclr(win)
call system_clock(count)
call srand(count)
p1=rand()
do while(1.le.10)
 do i0=1,50
 i=1
 j=1
  do l=1,24
   i=i
   j=j
   p2=rand()

    if(p2 .GT. 0.5) then
     i=i+1
    else
     j=j+1
    end if
  end do
 a(i,j)=a(i,j)+1
 end do
 do k=1,25
 mass(k)=a(k,26-k)/(50*total)
 end do
 total=total+1
 call gclr(win)
 do k0=1,D
  call newcolor(win,'green'//CHAR(0))
  call fillrect(win,h*k0,0.0,h,mass(k0)*120*L)
  call newcolor(win,'black'//CHAR(0))
  call drawrect(win,h*k0,0.0,h,mass(k0)*120*L)
 end do
 call msleep(18)
 call copylayer(win,1,0)
end do
call ggetch(key)
call gclose(win)
end
