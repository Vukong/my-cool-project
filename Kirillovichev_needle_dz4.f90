program ddd
implicit none
integer win1,win,key,j,i,w,l,m,s,c
real p,l1,f,f1,x1,y1,x2,y2,b,a,x(1000000),y(1000000)

call system_clock(c)
call srand(c)

l=0
p=rand()
x1=rand()*l
y1=rand()*l
a=rand()*3.1415926535*2

key=0
l1=l/7
m=0
l=600
f1=-0.5
f=6.0

call gopen(l,l,win)
call layer(win,0,1)
call gopen(l,l,win1)
call newwindow(win1,f1,f1,f,f)
call layer(win1,2,3)
call gsetbgcolor(win1,'white'//CHAR(0))
call gsetbgcolor(win,'white'//CHAR(0))
call gclr(win1)
call gclr(win)

call newcolor(win1,'black'//CHAR(0))
call drawline(win1,0.,f1,0.,f)
call drawline(win1,f1,0.,f,0.)

call drawstr(win1,f1+0.03,3.1415926535,100.0,'pi',0.0,2)
call drawstr(win1,1-0.1,-0.2,20.0,'1',0.0,1)
call drawstr(win1,0-0.1,-0.2,20.0,'0',0.0,1)
call drawstr(win1,2-0.1,-0.2,20.0,'2',0.0,1)
call drawstr(win1,3-0.1,-0.2,20.0,'3',0.0,1)
call drawstr(win1,5-0.1,-0.2,20.0,'5',0.0,1)
call drawstr(win1,4-0.1,-0.2,20.0,'4',0.0,1)
call drawstr(win1,6-0.1,-0.2,20.0,'6',0.0,1)
call drawstr(win1,f-1,f1+0.05,20.0,'log10(n)',0.0,8)
call drawstr(win1,f1+0.03,f-0.25,20.0,'2/p',0.0,3)

call newcolor(win1,'darkgrey'//CHAR(0))
call drawline(win1,-0.5,3.1415926535,7.,3.1415926535)


do i=1,6
call drawline(win1,real(i),-0.1,real(i),0.2)
enddo

do i=1,5
call newcolor(win,'black'//CHAR(0))
   do j=1,6
   call drawline(win,l1*j,0.0,l1*j,real(l))
   enddo
call copylayer(win,1,0)

do s=1,100000

b=int(rand()*5+1)

if(b.eq.1) then
  call newcolor(win,'blue'//CHAR(0))
endif

if(b.eq.2) then
  call newcolor(win,'brown'//CHAR(0))
endif

if(b.eq.3) then
  call newcolor(win,'green'//CHAR(0))
endif

if(b.eq.4) then
  call newcolor(win,'orange'//CHAR(0))
endif

if(b.eq.5) then
  call newcolor(win,'red'//CHAR(0))
endif

a=rand()*3.1415926535*2
y1=l1+rand()*(l-2*l1)
x1=l1+rand()*(l-2*l1)
y2=l1*sin(a)+y1
x2=l1*cos(a)+x1


do w=1,6
 if((x1-l1*w)*(x2-l1*w).le.0)then
  m=m+1
 endif
enddo

call drawline(win,x1,y1,x2,y2)
call copylayer(win,1,0)

if(i.eq.1) then
  call newcolor(win1,'blue'//CHAR(0))
endif

if(i.eq.2) then
  call newcolor(win1,'brown'//CHAR(0))
endif

if(i.eq.3) then
  call newcolor(win1,'green'//CHAR(0))
endif

if(i.eq.4) then
  call newcolor(win1,'orange'//CHAR(0))
endif

if(i.eq.5) then
  call newcolor(win1,'red'//CHAR(0))
endif

x(s)=log10(real(s))

if(m.eq.0)then
   y(s)=999999999.
else
   y(s)=real(2)*s/m
endif

call drawlines(win1,x,y,s)
call copylayer(win1,3,2)

enddo
call gclr(win)
m=0
write(*,*) y(100000)
enddo

call ggetch(key)
call gclose(win)
call gclose(win1)
end
