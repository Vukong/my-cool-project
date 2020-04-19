program asdf
implicit none
integer count,win,i,key,k,k_0,win1,k_1,n,num_exp
real x1(24),y1(24),p,x_c,y_c,l,pi,x_0,y_0,x_1,y_1,p1,p2,p3,total
real summ,x_shtrih(6),y_shtrih(6),pi_,sum1,sum2,sum3,x_line(100000000),y_line(100000000)
real t1,t2,t3,t4,t5,p4,pi_exp

y1=0
x1=0
x_c=0
y_c=0
l=25
total=0
summ=1
x_shtrih=0
y_shtrih=100
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
write(*,*) 'Please,choose the num of experiments,you can choose 100000 or 1000000 or even more or another number'
write(*,*) 'But if you chose the 10^5 you must wait about 2 minutes,if the num is 10^6 you will wait about 10 min'
write(*,*) 'The best variant is 100000(10^5)'
read(*,*) num_exp
      call system_clock(count)
      call srand(count)
      p=rand()


      do i=1,24
      x1(i)=32*i
      end do

      call gopen(800,800,win)
      call gsetbgcolor(win,'white'//CHAR(0))
      call layer(win,0,1)

      call gopen(800,800,win1)
      call gsetbgcolor(win1,'white'//CHAR(0))
      call layer(win1,0,1)
      call newcolor(win1,'DimGray'//CHAR(0))
    call newlinewidth(win1,2)
    call drawline(win1,0.0,100.0,800.0,100.0)
    call drawline(win1,100.0,0.0,100.0,800.0)
    do k_1=1,6
    x_shtrih(k_1)=100+100*k_1
    call newlinewidth(win1,1)
    call newcolor(win1,'black'//CHAR(0))
    call drawline(win1,x_shtrih(k_1),y_shtrih(k_1)-10.0,x_shtrih(k_1),y_shtrih(k_1)+10.0)

    call copylayer(win1,1,0)
    end do
    call newlinewidth(win1,1)
    call newlinestyle(win1,1)
    call drawline(win1,0.0,450.0,800.0,450.0)
    call drawstr(win1,80.0,455.0,16.0,'pi',0.0,2)

    call newlinewidth(win1,2)
    call drawstr(win1,745.0,74.0,20.0,'lg(n)',0.0,5)
    call drawstr(win1,50.0,750.0,20.0,'pi(n)',0.0,5)
    call drawstr(win1,88.0,82.0,20.0,'O',0.0,1)
   call newlinestyle(win1,0)
   call newlinewidth(win1,1)
   call drawline(win1,786.0,93.0,800.0,100.0)
   call drawline(win1,786.0,107.0,800.0,100.0)
   call drawline(win1,93.0,786.0,100.0,800.0)
   call drawline(win1,107.0,786.0,100.0,800.0)
    call copylayer(win1,1,0)


     call newcolor(win,'black'//CHAR(0))
       do k=1,24
      call drawline(win,x1(k),y1(k),x1(k),1000.0)
      end do
     call copylayer(win,1,0)


       do n=1,5
      do i=1,num_exp



      p1=rand()
      p2=rand()
      p3=rand()
      p4=rand()
       x_c=p1*800
       y_c=p2*800
  !Координаты начала и конца иглы
       x_0=x_c+l/2*cos(p3*pi)
       y_0=y_c+l/2*sin(p3*pi)

       x_1=x_c-l/2*cos(p3*pi)
       y_1=y_c-l/2*sin(p3*pi)
       !рисование разноцветных игл

     if(p4.le.0.125) then
       call newcolor(win,'red'//CHAR(0))
     elseif(p4.gt.0.125.and.p4.le.0.25) then
      call newcolor(win,'blue'//CHAR(0))
     elseif (p4.gt.0.25.and.p4.le.0.375) then
       call newcolor(win,'violet'//CHAR(0))
     elseif (p4.gt.0.375.and.p4.le.0.5) then
       call newcolor(win,'green'//CHAR(0))
     elseif (p4.gt.0.5.and.p4.le.0.625) then
       call newcolor(win,'brown'//CHAR(0))
     elseif (p4.gt.0.625.and.p4.le.0.75) then
       call newcolor(win,'steelblue'//CHAR(0))
     elseif(p4.gt.0.75.and.p4.le.0.875) then
       call newcolor(win,'orange'//CHAR(0))
     else
        call newcolor(win,'gold'//CHAR(0))
end if

      call drawline(win,x_1,y_1,x_0,y_0)
       call copylayer(win,1,0)


     do k_0=1,26
     if ((abs(x_c-32*(k_0-1)).le.(l/2*sin(p3*pi)))) then
            total=total+1
    end if
   end do

   if (i.ge.10000*summ) then
           call gclr(win)
        call newcolor(win,'black'//CHAR(0))
       do k=1,24
      call drawline(win,x1(k),y1(k),x1(k),1000.0)
      end do
      summ=summ+1
      if ((summ-1).eq.num_exp/10000) then
              summ=1
      else
summ=summ+1
      end if
      end if
      if (num_exp.lt.10000) then
              if (i.eq.num_exp) then
                      call gclr(win)
        call newcolor(win,'black'//CHAR(0))
       do k=1,24
      call drawline(win,x1(k),y1(k),x1(k),1000.0)
      end do
      end if

      end if
  if (i.eq.10*sum1) then
          pi_=2.0*25.0/32.0*i/total
          if(n.eq.1) then
          call newcolor(win1,'red'//CHAR(0))


  elseif (n.eq.2) then
          call newcolor(win1,'gold'//CHAR(0))


  elseif (n.eq.3) then
          call newcolor(win1,'green'//CHAR(0))


  elseif (n.eq.4) then
           call newcolor(win1,'blue'//CHAR(0))


   elseif (n.eq.5) then
          call newcolor(win1,'brown'//CHAR(0))

  end if
  call fillcirc(win1,200.0+100.0*sum3,100+350.0*pi_/pi,2.5,2.5)

  call copylayer(win1,1,0)
          sum1=sum1*10
          sum3=sum3+1
          if(sum3.eq.6) then
                  sum3=0
          end if
          if (sum1.ge.num_exp) then
                  sum1=1
          end if
  end if
          if(i.eq.num_exp) then
           sum2=sum2+1
           summ=1
           sum3=0
   end if


pi_=2.0*25.0/32.0*i/total





  if(i.ge.10) then
          x_line(i-9)=100+log10(i*1.0)*100
          y_line(i-9)=100+350*pi_/pi
  end if

  if(i.eq.num_exp) then
        if (n.eq.1) then
          call newcolor(win1,'red'//CHAR(0))
          t1=total
          total=0
  elseif (n.eq.2) then
          call newcolor(win1,'gold'//CHAR(0))
          t2=total
          total=0
elseif (n.eq.3) then
          call newcolor(win1,'green'//CHAR(0))
          t3=total
          total=0
elseif (n.eq.4) then
          call newcolor(win1,'blue'//CHAR(0))
          t4=total
          total=0
elseif (n.eq.5) then
          call newcolor(win1,'brown'//CHAR(0))
          t5=total
          total=0
  end if

  call newlinestyle(win1,0)
   call drawlines(win1,x_line,y_line,num_exp-9)
          call copylayer(win1,1,0)
  end if

       end do
       end do
       pi_exp=2*25.0/32.0*5*num_exp/(t1+t2+t3+t4+t5)
       write(*,*) 'Real pi',pi
       write(*,*) 'Experimental pi',pi_exp

call ggetch(key)
call gclose(win)
end
