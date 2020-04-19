program aaa
implicit none

integer h, p, i, j, k
real win, key, R1, R2, a, v0, dt, sp, v(2,2)
real xt1, xt2, yt1, yt2, x(2,3), y(2,3), f, q
!radius orbit i sharov
R1=100.0
R2=50.0
v0=10.0
dt=0.01

h=600
xt1=h/2
yt1=R1*2 + h/2

call system_clock(p)
call srand(p)
a=rand()
a=rand()*360-180
xt2=h/2+R1*sin(a)
yt2=h/2-R1*cos(a)

call gopen(h,h, win)
call layer(win,0,1)
call gsetbgcolor(win, 'white'//CHAR(0))
!nach koordinati
i=1
j=1
x(i,j)=xt1
y(i,j)=yt1
i=2
x(i,j)=xt2
y(i,j)=yt2
sp=(xt1-xt2)**2 + (yt1-yt2)**2
!kosanie
do while (sp .gt. ((2*R2)**2))
 call gclr(win)
 call newcolor(win, 'brown'//CHAR(0))
 call drawcirc(win, h/2.0, h/2.0, 2*R1, 2*R1)
 call drawcirc(win, h/2.0, h/2.0, R1, R1)

 call newcolor(win, 'green'//CHAR(0))
 call fillcirc(win, xt1, yt1, R2, R2)
 call newcolor(win, 'red'//CHAR(0))
 call fillcirc(win, xt2, yt2, R2, R2)

 call copylayer(win, 1, 0)
 call msleep(3)

 yt1=yt1 - 2*v0*dt
 xt2=xt2 - v0*sin(a)*dt
 yt2=yt2 + v0*cos(a)*dt
 sp=(xt1-xt2)**2 + (yt1-yt2)**2
end do

j=2
i=1
x(i,j)=xt1
y(i,j)=yt1
i=2
x(i,j)=xt2
y(i,j)=yt2

do k= 1,1000
 call gclr(win)
  call newcolor(win, 'brown'//CHAR(0))
  call drawcirc(win, h/2.0, h/2.0, 2*R1, 2*R1)
  call drawcirc(win, h/2.0, h/2.0, R1, R1)

  call newcolor(win, 'green'//CHAR(0))
  call fillcirc(win, xt1, yt1, R2, R2)
  call newcolor(win, 'red'//CHAR(0))
  call fillcirc(win, xt2, yt2, R2, R2)

  call newcolor(win, 'black'//CHAR(0))
  call newlinestyle(win, 1)
  call newlinewidth(win, 2)
  f=-1/((y(2,2)-y(1,2))/(x(2,2)-x(1,2)))
  q=(y(1,2)+y(2,2))/2 - f/2*(x(1,2)+x(2,2))
  write(*,*) 0.0, q, h/1.0, f*h+q
  call drawline(win, 0.0, q, h/1.0, f*h+q)
  call newlinestyle(win, 0)

  call copylayer(win, 1, 0)
  call msleep(3)
end do
!pereschet
v(1,1)=v0*(cos(a)*((yt1-yt2)/R2/2)+sin(a)*((xt2-xt1)/(R2*2)))

v(1,2)=2*v0*((xt2-xt1)/(R2*2))

v(2,1)=v0*2*((yt1-yt2)/(R2*2))

v(2,2)=v0*(cos(a)*((xt2-xt1)/(R2*2))+sin(a)*((yt1-yt2)/(R2*2)))

do while (sp .le. (4*R1*R1))
call gclr(win)
 call newcolor(win, 'brown'//CHAR(0))
 call drawcirc(win, h/2.0, h/2.0, 2*R1, 2*R1)
 call drawcirc(win, h/2.0, h/2.0, R1, R1)

 call newcolor(win, 'green'//CHAR(0))
 call fillcirc(win, xt1, yt1, R2, R2)
 call newcolor(win, 'red'//CHAR(0))
 call fillcirc(win, xt2, yt2, R2, R2)

 xt1=xt1+(-v(1,1)*((xt2-xt1)/(R2*2))-v(1,2)*((yt1-yt2)/(R2*2)))*dt

 yt1=yt1+(v(1,1)*((yt1-yt2)/(R2*2))-v(1,2)*((xt2-xt1)/(R2*2)))*dt

 xt2=xt2-(-v(2,1)*((xt2-xt1)/(R2*2))+v(2,2)*((yt1-yt2)/(R2*2)))*dt

 yt2=yt2+(-v(2,1)*((yt1-yt2)/(R2*2))+v(2,2)*((xt2-xt1)/(R2*2)))*dt

 call copylayer(win, 1, 0)
 call msleep(3)

 sp=(300-xt2)**2 + (300-yt2)**2
end do

x(1,3)=xt1
y(1,3)=yt1
x(2,3)=xt2
y(2,3)=yt2

 call newcolor(win, 'blue'//CHAR(0))
 call drawline(win, x(1,1), y(1,1), x(1,2), y(1,2))
 call drawline(win, x(1,2), y(1,2), x(1,3), y(1,3))
 call drawline(win, x(2,1), y(2,1), x(2,2), y(2,2))
 call drawline(win, x(2,2), y(2,2), x(2,3), y(2,3))

call copylayer(win,1,0)
call ggetch(key)
call gclose(win)
end
