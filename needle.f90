program aaa
implicit none
integer win1,key,w,h,win2,const,i,count,num,j,exp
real l,wr,hr,gr1,gr2,pi,x1,x2,y1,y2,p,alpha,x(100000),y(100000)
!win1 for needles, win2 for graphics

w=800
h=800
wr=real(w)
hr=real(h)
const=8
l=real(w)/const
gr1=-1
gr2=6
pi=3.1415926535
num=0
!open 2 windows
call open_windows(win1,win2,gr1,gr2,pi,w,h)
!draw field for needles
call newcolor(win1,'black'//char(0))
do i=0,const-1
  call drawline(win1,l+l*i,0.,l+l*i,h*1.)
end do
!draw axes
call draw_axes(win2,gr1,gr2,pi)
!brosok igly na pole
call system_clock(count)
call srand(count)
p=rand()

do exp=1,5
 i=0
 num=0
 x=0
 y=0
 do i=1,100000
  x1=0
  y1=0
  x2=0
  y2=0
  x1=rand()*(w-2*l)+l
  y1=rand()*(h-2*l)+l
  alpha=rand()*2*pi
  x2=x1+l*cos(alpha)
  y2=y1+l*sin(alpha)
  call color_selection1(win1)
  call drawline(win1,x1,y1,x2,y2)
  !line crossing check
  do j=1,const
    if((x1-l*j)*(x2-l*j).le.0) num=num+1
  end do
  x(i)=log10(real(i))
  if (num .eq. 0) then
   y(i)=999999999.
  else
   y(i)=real(2)*i/num
  end if
  call color_selection2(win2,exp)
  call drawlines(win2,x,y,i)
  call copylayer(win1,1,0)
  call copylayer(win2,3,2)
 end do
 call gclr(win1)
 !draw field for needles
 call newcolor(win1,'black'//char(0))
 do j=0,const-1
  call drawline(win1,l+l*j,0.,l+l*j,h*1.)
 end do
end do
call ggetch(key)
call gclose(win1)
call gclose(win2)
end

subroutine open_windows(win1,win2,gr1,gr2,pi,w,h)
  implicit none
  integer win1,w,h,win2
  real gr1,gr2,pi
  call gopen(w,h,win1)
  call gopen(w,h,win2)
  call newwindow(win2,gr1,gr1,gr2,2*pi)
  call layer(win1,0,1)
  call layer(win2,2,3)
  call gsetbgcolor(win1,'white'//char(0))
  call gsetbgcolor(win2,'white'//char(0))
  call gclr(win1)
  call gclr(win2)
end

subroutine draw_axes(win2,gr1,gr2,pi)
  implicit none
  integer win2,i
  real gr1,gr2,pi
  call newcolor(win2,'black'//char(0))
  call drawline(win2,gr1,0,gr2,0.)
  call drawline(win2,0.,gr1,0.,2*pi)
  do i=1,5
    call drawline(win2,real(i),-0.02,real(i),0.02)
  end do
  call drawstr(win2,1.,-0.2,10.,'1',0.,1)
  call drawstr(win2,2.,-0.2,10.,'2',0.,1)
  call drawstr(win2,3.,-0.2,10.,'3',0.,1)
  call drawstr(win2,4.,-0.2,10.,'4',0.,1)
  call drawstr(win2,5.,-0.2,10.,'5',0.,1)
  do i=1,3
    call drawline(win2,-0.02,i*0.5*pi,0.02,i*0.5*pi)
  end do
  call newlinestyle(win2,1)
  call drawline(win2,gr1,pi,gr2,pi)
  call newlinestyle(win2,0)
  call drawstr(win2,-0.5,0.5*pi,20.,'0.5pi',0.,5)
  call drawstr(win2,-0.5,1.5*pi,20.,'1.5pi',0.,5)
  call drawstr(win2,5.3,-0.3,20.,'log10(n)',0.,8)
  call drawstr(win2,-0.2,pi+0.1,30.,'pi',0.,2)
end

subroutine color_selection1(win1)
  implicit none
  integer k,win1
  k=int(rand()*5+1)
  if (k .eq. 1) call newcolor(win1,'red'//char(0))
  if (k .eq. 2) call newcolor(win1,'orange'//char(0))
  if (k .eq. 3) call newcolor(win1,'yellow'//char(0))
  if (k .eq. 4) call newcolor(win1,'green'//char(0))
  if (k .eq. 5) call newcolor(win1,'blue'//char(0))
end

subroutine color_selection2(win2,exp)
  implicit none
  integer win2, exp
  if (exp .eq. 1) call newcolor(win2,'red'//char(0))
  if (exp .eq. 2) call newcolor(win2,'orange'//char(0))
  if (exp .eq. 3) call newcolor(win2,'yellow'//char(0))
  if (exp .eq. 4) call newcolor(win2,'green'//char(0))
  if (exp .eq. 5) call newcolor(win2,'blue'//char(0))
end
