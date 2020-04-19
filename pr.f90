program needle
implicit none
integer win1,win2,key,w,h,const,i
real l,f0,f
!win1 - window for needles, win2 - window for graphics
w=1000
h=1000
const=100
l=real(w)/const
f0=-0.5
f=6.0
!call for win1
call gopen(w,h,win1)
call layer(win1,0,1)
call gsetbgcolor(win,1'white'//char(0))
call gclr(win1)

!call for win2
call gopen(w,h,win2)
call newwindow(win2,f0,f0,f,f)
call layer(win2,2,3)
call gsetbgcolor(win2,1'white'//char(0))
call gclr(win2)


!draw field
do i=0,99
  call newcolor(win1,'grey'//char(0))
  call drawline(win1,l+i*l,0.,l+i*l,h*1.)
end do
!end draw field
call copylayer(win1,1,0)
call ggetch(key)
call gclose(win1)
call gclose(win2)
end
