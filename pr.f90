program   aaa
implicit none
integer win,key
call gopen(400,400,win)
call gsetbgcolor(win,'white'//char(0))
call gclr(win)
call newcolor(win,'red'//char(0))
call drawrect(win,0.,10.,50.,50.)
call drawrect(win,100.,100.,120.,120.)
call ggetch(key)
call gclose(win)
end
