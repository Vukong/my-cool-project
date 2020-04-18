program   aaa
implicit none
integer win,key
call gopen(400,400,win)
call gclr(win)
call gsetbgcolor(win,'white'//char(0))
call ggetch(key)
call gclose(win)
end
