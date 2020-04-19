program asdf
implicit none
integer count,win,i,key,k,hatch,win1,k_0,n,num_exp
real x1(24),y1(24),p,x_c,y_c,l,pi,x_0,y_0,x_1,y_1,p1,p2,p3,total
real summ,xhatch(6),yhatch(6),pi_,sum1,sum2,sum3,x_line(100000000),y_line(100000000)
real t1,t2,t3,t4,t5,p4,pi_exp
integer w,h
w=800
h=800
y1=0
x1=0
x_c=0
y_c=0
l=25
total=0
summ=1
xhatch=0
yhatch=100
x_line=0
y_line=0
t1=0
t2=0
t3=0
t4=0
t5=0
pi=3.1415926
pi_=0
sum1=1
sum2=1
sum3=0
pi_exp=0
!call win for needles
call gopen(w,h,win)
call gsetbgcolor(win,'white'//CHAR(0))
call layer(win,0,1)
!call win for graphics
call gopen(w,h,win1)
call gsetbgcolor(win1,'white'//CHAR(0))
call layer(win1,0,1)
call newcolor(win1,'black'//CHAR(0))
call newlinewidth(win1,2)
call drawline(win1,0.0,100.0,w*1.,100.0)
call drawline(win1,100.0,0.0,100.0,h*1.0)
!draw hatches
do hatch=1,6
xhatch(hatch)=100+100*hatch
call newlinewidth(win1,1)
call newcolor(win1,'dimgray'//CHAR(0))
call drawline(win1,xhatch(hatch),yhatch(hatch)-7.,xhatch(hatch),yhatch(hatch)+7.)

call copylayer(win1,1,0)
end do
call newlinewidth(win1,2)
call newlinestyle(win1,1)
call drawline(win1,0.0,450.0,800.0,450.0)
call drawstr(win1,80.0,455.0,16.0,'pi',0.0,2)

call newlinewidth(win1,2)
call drawstr(win1,715.0,74.0,20.0,'log10(n)',0.0,8)
call drawstr(win1,50.0,750.0,20.0,'pi(n)',0.0,5)
call drawstr(win1,88.0,82.0,20.0,'O',0.0,1)
call newlinestyle(win1,0)

call copylayer(win1,1,0)


call newcolor(win,'black'//CHAR(0))
 do k=1,24
call drawline(win,x1(k),y1(k),x1(k),1000.0)
end do
call copylayer(win,1,0)
call ggetch(key)
call gclose(win)
end
