program sss
implicit none
integer win,win1,key,i,j,t,l,n,m,count
real p,l0,f0,f,x1,y1,x2,y2,k,a,x(1000000),y(1000000),pi
pi=3.1415926535
call system_clock(count)
call srand(count)
l=0
p=rand()
x1=rand()*l
y1=rand()*l
a=rand()*2*3.1415926535

key=0
l=600
l0=l/7
m=0
f0=-0.5
f=6.0
call gopen(l,l,win)
call layer(win,0,1)
call gopen(l,l,win1)
call newwindow(win1,f0,f0,f,f)
call layer(win1,2,3)
call gsetbgcolor(win,'white'//CHAR(0))
call gsetbgcolor(win1,'white'//CHAR(0))
call gclr(win)
call gclr(win1)
call newcolor(win1,'black'//CHAR(0))
call drawline(win1,f0,0.,f,0.)
call drawline(win1,0.,f0,0.,f)
call drawstr(win1,f0+0.03,pi,100.0,'pi',0.0,2)
call drawstr(win1,0-0.1,-0.2,20.0,'0',0.0,1)
call drawstr(win1,1-0.1,-0.2,20.0,'1',0.0,1)
call drawstr(win1,2-0.1,-0.2,20.0,'2',0.0,1)
call drawstr(win1,3-0.1,-0.2,20.0,'3',0.0,1)
call drawstr(win1,4-0.1,-0.2,20.0,'4',0.0,1)
call drawstr(win1,5-0.1,-0.2,20.0,'5',0.0,1)
call drawstr(win1,6-0.1,-0.2,20.0,'6',0.0,1)
call drawstr(win1,f-1,f0+0.05,20.0,'log10(n)',0.0,8)
call drawstr(win1,f0+0.03,1.5*pi,20.0,'1.5',0.0,3)
call drawstr(win1,f0+0.03,1.45*pi,20.0,'pi',0.0,3)

call newcolor(win1,'darkgrey'//CHAR(0))
call drawline(win1,-0.5,3.1415926535,7.,3.1415926535)
do i=1,6
call drawline(win1,real(i),-0.1,real(i),0.2)
enddo

do i=1,5
call newcolor(win,'black'//CHAR(0))
do j=1,6
 call drawline(win,l0*j,0.0,l0*j,real(l))
enddo
call copylayer(win,1,0)

do n=1,100000

k=int(rand()*5+1)
if(k.eq.1) then
call newcolor(win,'red'//CHAR(0))
endif
if(k.eq.2) then
call newcolor(win,'blue'//CHAR(0))
endif
if(k.eq.3) then
call newcolor(win,'green'//CHAR(0))
endif
if(k.eq.4) then
call newcolor(win,'orange'//CHAR(0))
endif
if(k.eq.5) then
call newcolor(win,'purple'//CHAR(0))
endif

x1=rand()*(l-2*l0)+l0
y1=rand()*(l-2*l0)+l0
a=rand()*2*3.1415926535
x2=x1+l0*cos(a)
y2=y1+l0*sin(a)

do t=1,6
 if((x1-l0*t)*(x2-l0*t).le.0)then
  m=m+1
 endif
enddo

call drawline(win,x1,y1,x2,y2)
call copylayer(win,1,0)

if(i.eq.1) then
call newcolor(win1,'red'//CHAR(0))
endif
if(i.eq.2) then
call newcolor(win1,'blue'//CHAR(0))
endif
if(i.eq.3) then
call newcolor(win1,'green'//CHAR(0))
endif
if(i.eq.4) then
call newcolor(win1,'orange'//CHAR(0))
endif
if(i.eq.5) then
call newcolor(win1,'purple'//CHAR(0))
endif

x(n)=log10(real(n))
if(m.eq.0)then
y(n)=999999999.
else
y(n)=real(2)*n/m
endif

call drawlines(win1,x,y,n)
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
