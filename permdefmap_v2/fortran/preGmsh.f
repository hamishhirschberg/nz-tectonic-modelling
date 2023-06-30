      SUBROUTINE pregmsh()
c
      implicit none
      integer maxl,maxsl,maxpl,maxll,
     1      maxf,maxvl,maxrb,
     2      maxeb,maxleb,maxr,maxpr,maxlr,
     3      maxcp,maxlcp,maxgr,
     4      maxvo,maxfo,maxeo,maxduc,maxec
      parameter(maxl=10000,maxsl=100,
     1      maxpl=maxsl+1,maxll=maxsl+6,
     1      maxf=600,maxvl=10,maxrb=10,
     2      maxeb=10,maxleb=10,
     3      maxr=2000,maxpr=1001,maxlr=100,
     4      maxcp=2*maxl,maxlcp=12,maxgr=maxr+maxl,
     5      maxvo=5000,maxfo=maxf*maxsl,maxeo=500,
     6      maxduc=2*maxf,maxec=100)
      integer nl,nf,nvl,nrb,neb,nvo,nfo,neo,nduc,nec,nfL,ndl
      integer ncp,nr,ngl,ngr,nir
      integer i,vl,k,j,p,rb,im,i2,j2,eb,l,p0,p1,leb1,kmax
      integer f,fL,vo,fo,eo,duc,ec
      integer j0,j1,j20,j21,signi,signi2,ia,lsign,ja,ka
      integer i1,lcpj,plcpj,jm,lm,l1
      integer npl(maxl),il(9,maxl),jl(9,maxl)
      integer nlvl(maxvl),lvl(maxll,maxvl)
      integer vptype(maxpl,maxvl),fvl(maxpl,maxvl),ebvl(maxvl)
      integer nlrb(maxrb),lrb(maxll,maxrb),ebrb(maxrb)
      integer nleb(maxeb),kleb(maxleb,maxeb),leb(maxleb,maxeb)
      integer npeb(maxeb)
      integer nlf(maxf),lf(maxll,maxf)
      integer ffL(maxf),nfLp(maxf),pfL(maxpl,maxf)
      integer ffo(maxfo),p1fo(maxfo),p2fo(maxfo),nfoc(maxfo)
      integer neoc(maxeo)
      integer nleo(maxeo),leo(9,maxeo)
      integer fduc(maxduc),nducp(maxduc),pduc(maxpl,maxduc)
      integer necp(maxec)
      integer nlec(maxec),lec(maxll,maxec)
      integer cp0(maxl),cp1(maxl)
      integer nlcp(maxcp),lcp(maxlcp,maxcp),plcp(maxlcp,maxcp)
      integer nlkeep(maxcp)
      integer lmp0(maxl),lpp0(maxl),lmp1(maxl),lpp1(maxl)
      integer rlp(maxl),rlm(maxl)
      integer nlr(maxr),lr(maxlr,maxr)
      integer npr(maxr)
      integer rwithr(maxr),rwithl(maxl),ebr(maxr),reb(maxeb)
      integer gll(maxl),lgl(maxl),ltype(maxl)
      integer grr(maxr),grl(maxl)
      integer rgr(maxgr),lgr(maxgr),rtype(maxgr)
      integer ir(maxgr),sr(maxgr)
      logical scross,pexr,pinr,rinr,linr
      logical meet,again
      logical lkeep(maxl),cpkeep(maxcp)
      logical p0lmp0(maxl),p0lpp0(maxl),p1lmp1(maxl),p1lpp1(maxl)
      logical rsame(maxr),rmain(maxr),rsub(maxr),rtodo(maxr)
      real*8 rE
      real*8 xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1
      real*8 long00,lat00,long10,lat10,long01,lat01,long11,lat11
      real*8 dx,dy,alcpj,dl
      real*8 long(maxpl,maxl),lat(maxpl,maxl)
      real*8 Kc(maxpl,maxf),Ks(maxpl,maxf)
      real*8 ux(maxpl,maxvl),uy(maxpl,maxvl),
     1      uxm(maxpl,maxvl),uym(maxpl,maxvl),
     2      uxp(maxpl,maxvl),uyp(maxpl,maxvl)
      real*8 plat(maxrb),plong(maxrb),prate(maxrb)
      real*8 eblong(maxpr,maxeb),eblat(maxpr,maxeb)
      real*8 yfLm(maxpl,maxf),yfLp(maxpl,maxf)
      real*8 fLcm(maxpl,maxf),fLsm(maxpl,maxf),
     1      fLcp(maxpl,maxf),fLsp(maxpl,maxf)
      real*8 fafLcm(maxpl,maxf),fafLsm(maxpl,maxf),
     1      fafLcp(maxpl,maxf),fafLsp(maxpl,maxf)
      real*8 azfLcm(maxpl,maxf),azfLsm(maxpl,maxf),
     1      azfLcp(maxpl,maxf),azfLsp(maxpl,maxf)
      real*8 volong(maxvo),volat(maxvo),
     1      oux(maxvo),ouy(maxvo),seoux(maxvo),seouy(maxvo),
     2      rouxuy(maxvo)
      real*8 codut(2,maxfo),codun(2,maxfo),
     1      oduc(2,maxfo),seoduc(2,maxfo)
      real*8 rfo12(maxfo)
      real*8 coexx(3,maxeo),coeyy(3,maxeo),coexy(3,maxeo),
     1      oec(3,maxeo),seoec(3,maxeo)
      real*8 reo12(maxeo),reo13(maxeo),reo23(maxeo)
      real*8 cdut(maxpl,maxduc),cdun(maxpl,maxduc),
     1      refduc(maxpl,maxduc),scduc(maxpl,maxduc)
      real*8 cexx(maxpl,maxec),ceyy(maxpl,maxec),cexy(maxpl,maxec),
     1      refec(maxpl,maxec),scec(maxpl,maxec)
      real*8 cplong(maxcp),cplat(maxcp)
      real*8 dmax(maxpl,maxl),dmaxcp(maxcp)
      real*8 alcp(maxlcp)
      real*8 rlong(maxpr,maxr),rlat(maxpr,maxr)
c
      open(1,file='raw_input_when_using_gmsh.dat')
      read(1,*) rE
c
      nl=0
c
c Velocity lines
c
      read(1,*) nvl
      if (nvl.gt.maxvl) then
            write(*,*) 'nvl,maxvl=',nvl,maxvl
            stop 'Recompile with an increased maxvl value'
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            read(1,*) vl,npl(nl)
            if (vl.ne.i) then
                  write(*,*) 'i,vl=',i,vl
                  stop 'Identifying number vl is not equal to i'
            end if
            if (npl(nl).le.2) then
                  write(*,*) 'v-line i,nl,npl(nl)=',i,nl,npl(nl)
                  stop 'npl must be greater than 2'
            end if
            if (npl(nl).gt.maxpl) then
                  write(*,*) 'v-line i,nl,npl(nl),maxpl=',i,nl,
     1                  npl(nl),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(2,nl)=i
            jl(2,nl)=1
            nlvl(i)=1
            lvl(1,i)=nl
            do j=1,npl(nl)
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
            stop 'Point index p is not equal to j on velocity line i'
                  end if
                  read(1,*) vptype(j,i)
                  if ((vptype(j,i).ne.1).and.
     1                  (vptype(j,i).ne.-1).and.
     2                  (vptype(j,i).ne.3).and.
     3                  (vptype(j,i).ne.-3)) then
                        write(*,*) 'i,j,vptype=',i,j,vptype(j,i)
                        stop 'Valid values of vptype are 1,-1,3,-3'
                  end if
                  if (vptype(j,i).eq.1) then
                        read(1,*) ux(j,i),uy(j,i)
                  end if
                  if (vptype(j,i).eq.3) then
                        read(1,*) fvl(j,i),uxm(j,i),uym(j,i),
     1                        uxp(j,i),uyp(j,i)
                  end if
                  if (vptype(j,i).eq.-3) then
                        read(1,*) fvl(j,i)
                  end if
            end do
            ebvl(i)=0
      end do
      end if
c
c Rigid boundaries
c
      read(1,*) nrb
      if (nrb.gt.maxrb) then
            write(*,*) 'nrb,maxrb=',nrb,maxrb
            stop 'Recompile with an increased maxrb value'
      end if
      if (nrb.gt.0) then
      do i=1,nrb
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            read(1,*) rb,npl(nl),plat(i),plong(i),prate(i)
            if (rb.ne.i) then
                  write(*,*) 'i,rb=',i,rb
                  stop 'Identifying number rb is not equal to i'
            end if
            if (npl(nl).le.1) then
                  write(*,*) 'rigid-b i,nl,npl(nl)=',i,nl,npl(nl)
                  stop 'npl must be greater than 1'
            end if
            if (npl(nl).gt.maxpl) then
                  write(*,*) 'rigid-b i,nl,npl(nl),maxpl=',i,nl,
     1                  npl(nl),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(3,nl)=i
            jl(3,nl)=1
            nlrb(i)=1
            lrb(1,i)=nl
            do j=1,npl(nl)
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
      stop 'Point index p is not equal to j on rigid boundary i'
                  end if
            end do
            ebrb(i)=0
      end do
      end if
c
c Check that line segments on velocity lines and rigid bounaries do not
c  intersect between points
c
      do i=2,nl
            im=i-1
            do i2=1,im
                  do j=2,npl(i)
                  do j2=2,npl(i2)
                        xa0=long(j-1,i)
                        ya0=lat(j-1,i)
                        xa1=long(j,i)
                        ya1=lat(j,i)
                        xb0=long(j2-1,i2)
                        yb0=lat(j2-1,i2)
                        xb1=long(j2,i2)
                        yb1=lat(j2,i2)
                        if (scross(xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1))
     1                        then
                              write(*,*) 'i,j,i2,j2=',i,j,i2,j2
            stop 'Line segments must not intersect between points'
                        end if
                  end do
                  end do
            end do
      end do
c
c External boundaries. The exterior boundary must be input first,
c  followed by any interior boundaries
c
      read(1,*) neb
      if (neb.gt.maxeb) then
            write(*,*) 'neb,maxeb=',neb,maxeb
            stop 'Recompile with an increased maxeb value'
      end if
      do i=1,neb
            read(1,*) eb,nleb(i)
            if (eb.ne.i) then
                  write(*,*) 'i,eb=',i,eb
                  stop 'Identifying number eb is not equal to i'
            end if
            if (nleb(i).gt.maxleb) then
                  write(*,*) 'i,nleb(i),maxleb=',i,nleb(i),maxleb
                  stop 'Recompile with an increased maxleb value'
            end if
            do j=1,nleb(i)
                  read(1,*) l,kleb(j,i),leb(j,i)
                  if (l.ne.j) then
                        write(*,*) 'j,l=',j,l
                  stop 'Identifying number l is not equal to j'
                  end if
                  if ((kleb(j,i).ne.2).and.(kleb(j,i).ne.3)) then
                        write(*,*) 'i,j,kleb(j,i)=',i,j,kleb(j,i)
            stop 'Line type kleb must be 2 (v-line) or 3 (rigid-b)'
                  end if
                  if (kleb(j,i).eq.2) ebvl(leb(j,i))=i
                  if (kleb(j,i).eq.3) ebrb(leb(j,i))=i
                  if (j.eq.1) then
                        if (kleb(1,i).eq.2) l=lvl(1,leb(1,i))
                        if (kleb(1,i).eq.3) l=lrb(1,leb(1,i))
                        p0=1
                        p1=npl(l)
                        long00=long(p0,l)
                        lat00=lat(p0,l)
                        long10=long(p1,l)
                        lat10=lat(p1,l)
                  end if
                  if (j.eq.2) then
                        if (kleb(2,i).eq.2) l=lvl(1,leb(2,i))
                        if (kleb(2,i).eq.3) l=lrb(1,leb(2,i))
                        p0=1
                        p1=npl(l)
                        long01=long(p0,l)
                        lat01=lat(p0,l)
                        long11=long(p1,l)
                        lat11=lat(p1,l)
                        meet=.false.
                        if ((long01.eq.long00).and.
     1                        (lat01.eq.lat00)) then
                              meet=.true.
                              leb(1,i)=-leb(1,i)
                        end if
                        if ((long11.eq.long00).and.
     1                        (lat11.eq.lat00)) then
                              meet=.true.
                              leb(1,i)=-leb(1,i)
                              leb(2,i)=-leb(2,i)
                        end if
                        if ((long01.eq.long10).and.
     1                        (lat01.eq.lat10)) meet=.true.
                        if ((long11.eq.long10).and.
     1                        (lat11.eq.lat10)) then
                              meet=.true.
                              leb(2,i)=-leb(2,i)
                        end if
                        if (.not.meet) then
                              write(*,*) 'i,j=',i,j
            stop 'Line j on boundary i is not joined to preceding line'
                        end if
                        if (leb(2,i).lt.0) then
                              long10=long01
                              lat10=lat01
                        else
                              long10=long11
                              lat10=lat11
                        end if
                  end if
                  if (j.gt.2) then
                        if (kleb(j,i).eq.2) l=lvl(1,leb(j,i))
                        if (kleb(j,i).eq.3) l=lrb(1,leb(j,i))
                        p0=1
                        p1=npl(l)
                        long01=long(p0,l)
                        lat01=lat(p0,l)
                        long11=long(p1,l)
                        lat11=lat(p1,l)
                        meet=.false.
                        if ((long01.eq.long10).and.
     1                        (lat01.eq.lat10)) meet=.true.
                        if ((long11.eq.long10).and.
     1                        (lat11.eq.lat10)) then
                              meet=.true.
                              leb(j,i)=-leb(j,i)
                        end if
                        if (.not.meet) then
                              write(*,*) 'i,j=',i,j
            stop 'Line j on boundary i is not joined to preceding line'
                        end if
                        if (leb(j,i).lt.0) then
                              long10=long01
                              lat10=lat01
                        else
                              long10=long11
                              lat10=lat11
                        end if
                  end if
                  if (j.eq.nleb(i)) then
                        if (leb(1,i).lt.0) then
                              leb1=-leb(1,i)
                              if (kleb(1,i).eq.2) l=lvl(1,leb1)
                              if (kleb(1,i).eq.3) l=lrb(1,leb1)
                              p0=npl(l)
                        else
                              leb1=leb(1,i)
                              if (kleb(1,i).eq.2) l=lvl(1,leb1)
                              if (kleb(1,i).eq.3) l=lrb(1,leb1)
                              p0=1
                        end if
                        long01=long(p0,l)
                        lat01=lat(p0,l)
                        meet=(long01.eq.long10).and.
     1                        (lat01.eq.lat10)
                        if (.not.meet) then
                              write(*,*) 'i,j=',i,j
                              stop 'Boundary i does not form a loop'
                        end if
                  end if
            end do
      end do
c
c Check that all rigid boundaries are on external boundaries
c
      if (nrb.gt.0) then
      do i=1,nrb
            if (ebrb(i).eq.0) then
                  write(*,*) 'i=',i
            stop 'Rigid boundaries must be on external boundaries'
            end if
      end do
      end if
c
c Load the points on the external boundaries into storage, and check
c  that no points on velocity lines are outside the exterior boundary or
c  inside any other external boundaries, later doing the same for other
c  types of line
c
      do i=1,neb
            npeb(i)=1
            if (leb(1,i).gt.0) then
                  leb1=leb(1,i)
                  if (kleb(1,i).eq.2) l=lvl(1,leb1)
                  if (kleb(1,i).eq.3) l=lrb(1,leb1)
                  eblong(1,i)=long(1,l)
                  eblat(1,i)=lat(1,l)
            else
                  leb1=-leb(1,i)
                  if (kleb(1,i).eq.2) l=lvl(1,leb1)
                  if (kleb(1,i).eq.3) l=lrb(1,leb1)
                  eblong(1,i)=long(npl(l),l)
                  eblat(1,i)=lat(npl(l),l)
            end if
            do j=1,nleb(i)
                  if (leb(j,i).gt.0) then
                        leb1=leb(j,i)
                        if (kleb(j,i).eq.2) l=lvl(1,leb1)
                        if (kleb(j,i).eq.3) l=lrb(1,leb1)
                        kmax=npl(l)
                        do k=2,kmax
                              npeb(i)=npeb(i)+1
                              if (npeb(i).gt.maxpr) then
                                    write(*,*) 'i,npeb,maxpr=',
     1                                    i,npeb(i),maxpr
                  stop 'Recompile with an increased maxpr value'
                              end if
                              eblong(npeb(i),i)=long(k,l)
                              eblat(npeb(i),i)=lat(k,l)
                        end do
                  else
                        leb1=-leb(j,i)
                        if (kleb(j,i).eq.2) l=lvl(1,leb1)
                        if (kleb(j,i).eq.3) l=lrb(1,leb1)
                        kmax=npl(l)-1
                        do k=kmax,1,-1
                              npeb(i)=npeb(i)+1
                              if (npeb(i).gt.maxpr) then
                                    write(*,*) 'i,npeb,maxpr=',
     1                                    i,npeb(i),maxpr
                  stop 'Recompile with an increased maxpr value'
                              end if
                              eblong(npeb(i),i)=long(k,l)
                              eblat(npeb(i),i)=lat(k,l)
                        end do
                  end if
            end do
      end do
      do i=1,nvl
            l=lvl(1,i)
            do j=1,npl(l)
                  if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1                  long(j,l),lat(j,l))) then
                        write(*,*) 'i,j,long,lat=',i,j,
     1                        long(j,l),lat(j,l)
            stop 'Velocity line point is outside the exterior boundary'
                  end if
                  if (neb.gt.1) then
                        do eb=2,neb
                              if (pinr(npeb(eb),eblong(1,eb),
     1                              eblat(1,eb),long(j,l),lat(j,l)))
     2                              then
                                    write(*,*) 'i,j,long,lat,eb=',i,j,
     1                                    long(j,l),lat(j,l),eb
            stop 'Velocity line point is inside an interior boundary'
                              end if
                        end do
                  end if
            end do
      end do
c
c Faults
c
      read(1,*) nf
      if (nf.gt.maxf) then
            write(*,*) 'nf,maxf=',nf,maxf
            stop 'Recompile with an increased maxf value'
      end if
      if (nf.gt.0) then
      do i=1,nf
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            read(1,*) f,npl(nl)
            if (f.ne.i) then
                  write(*,*) 'i,f=',i,f
                  stop 'Identifying number f is not equal to i'
            end if
            if (npl(nl).le.2) then
                  write(*,*) 'fault i,nl,npl(nl)=',i,nl,npl(nl)
                  stop 'npl must be greater than 2'
            end if
            if (npl(nl).gt.maxpl) then
                  write(*,*) 'fault i,nl,npl(nl),maxpl=',i,nl,
     1                  npl(nl),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(1,nl)=i
            jl(1,nl)=1
            nlf(i)=1
            lf(1,i)=nl
            do j=1,npl(nl)
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
            stop 'Point index p is not equal to j on fault i'
                  end if
                  read(1,*) Kc(j,i),Ks(j,i)
                  if ((Kc(j,i).le.0.0d0).or.(Ks(j,i).le.0.0d0))
     1                  then
                        write(*,*) 'i,j,Kc,Ks=',i,j,Kc(j,i),
     1                        Ks(j,i)
                  stop 'Kc and Ks must be greater than zero'
                  end if
            end do
      end do
      do i=1,nf
            l=lf(1,i)
            do j=1,npl(l)
                  if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1                  long(j,l),lat(j,l))) then
                        write(*,*) 'i,j,long,lat=',i,j,
     1                        long(j,l),lat(j,l)
            stop 'Fault point is outside the exterior boundary'
                  end if
                  if (neb.gt.1) then
                        do eb=2,neb
                              if (pinr(npeb(eb),eblong(1,eb),
     1                              eblat(1,eb),long(j,l),lat(j,l)))
     2                              then
                                    write(*,*) 'i,j,long,lat,eb=',i,j,
     1                                    long(j,l),lat(j,l),eb
            stop 'Fault point is inside an interior boundary'
                              end if
                        end do
                  end if
            end do
      end do
      end if
c
c Specifications of strain-rate capacity at faults
c
      read(1,*) nfL
      if (nfL.gt.maxf) then
            write(*,*) 'nfL,maxf=',nfL,maxf
            stop 'Recompile with an increased maxf value'
      end if
      if (nfL.gt.0) then
      do i=1,nfL
            read(1,*) fL,ffL(i),nfLp(i)
            if (fL.ne.i) then
                  write(*,*) 'i,fL=',i,fL
                  stop 'Identifying number fL is not equal to i'
            end if
            if ((ffL(i).lt.1).or.(ffL(i).gt.nf)) then
                  write(*,*) 'i,ffL(i),nf=',i,ffL(i),nf
                  stop 'ffL value is not valid'
            end if
            l=lf(1,ffL(i))
            if (nfLp(i).lt.2) then
                  write(*,*) 'i,nfLp(i)=',i,nfLp(i)
                  stop 'There must be at least 2 points'
            end if
            if (nfLp(i).gt.npl(l)) then
                  write(*,*) 'i,nfLp(i),npl=',i,nfLp(i),
     1                  npl(l)
                  stop 'Impossibly many points'
            end if
            do j=1,nfLp(i)
                  read(1,*) pfL(j,i),yfLm(j,i),yfLp(j,i)
                  if (j.eq.1) then
                        if (pfL(j,i).lt.1) then
                              write(*,*) 'i,j,pfL(j,i)=',
     1                              i,j,pfL(j,i)
                              stop 'pfL value is not valid'
                        end if
                  end if
                  if (j.gt.1) then
                        if (pfL(j-1,i).ge.pfL(j,i)) then
                              write(*,*) 'i,j,pfL(j-1,i),pfL(j,i)=',
     1                              i,pfL(j-1,i),pfL(j,i)
                              stop 'pfL values are not valid'
                        end if
                  end if
                  if (j.eq.nfLp(i)) then
                        if (pfL(j,i).gt.npl(l)) then
                              write(*,*) 'i,j,pfL(j,i),npl=',
     1                              i,j,pfL(j,i),npl(l)
                              stop 'pfL value is not valid'
                        end if
                  end if
                  if ((yfLm(j,i).le.0.0d0).or.
     1                  (yfLp(j,i).le.0.0d0)) then
                        write(*,*) 'i,j,yfLm,yfLp=',i,j,
     1                        yfLm(j,i),yfLp(j,i)
                  stop 'Half-widths must be greater than zero'
                  end if
                  read(1,*) fLcm(j,i),fafLcm(j,i),azfLcm(j,i)
                  read(1,*) fLsm(j,i),fafLsm(j,i),azfLsm(j,i)
                  read(1,*) fLcp(j,i),fafLcp(j,i),azfLcp(j,i)
                  read(1,*) fLsp(j,i),fafLsp(j,i),azfLsp(j,i)
                  if ((fLcm(j,i).le.0.0d0).or.
     1                  (fLsm(j,i).le.0.0d0)) then
                        write(*,*) 'i,j,fLcm,fLsm=',i,j,
     1                        fLcm(j,i),fLsm(j,i)
            stop 'Strain-rate capacities must be greater than zero'
                  end if
                  if ((fLcp(j,i).le.0.0d0).or.
     1                  (fLsp(j,i).le.0.0d0)) then
                        write(*,*) 'i,j,fLcp,fLsp=',i,j,
     1                        fLcp(j,i),fLsp(j,i)
            stop 'Strain-rate capacities must be greater than zero'
                  end if
                  if ((fafLcm(j,i).lt.0.0d0).or.
     1                  (fafLsm(j,i).lt.0.0d0)) then
                        write(*,*) 'i,j,fafLcm,fafLsm=',i,j,
     1                        fafLcm(j,i),fafLsm(j,i)
                  stop 'Anisotropy fractions cannot be negative'
                  end if
                  if ((fafLcp(j,i).lt.0.0d0).or.
     1                  (fafLsp(j,i).lt.0.0d0)) then
                        write(*,*) 'i,j,fafLcp,fafLsp=',i,j,
     1                        fafLcp(j,i),fafLsp(j,i)
                  stop 'Anisotropy fractions cannot be negative'
                  end if
                  if ((fafLcm(j,i).ge.1.0d0).or.
     1                  (fafLsm(j,i).ge.1.0d0)) then
                        write(*,*) 'i,j,fafLcm,fafLsm=',i,j,
     1                        fafLcm(j,i),fafLsm(j,i)
                  stop 'Anisotropy fractions must be less than 1'
                  end if
                  if ((fafLcp(j,i).ge.1.0d0).or.
     1                  (fafLsp(j,i).ge.1.0d0)) then
                        write(*,*) 'i,j,fafLcp,fafLsp=',i,j,
     1                        fafLcp(j,i),fafLsp(j,i)
                  stop 'Anisotropy fractions must be less than 1'
                  end if
            end do
      end do
      end if
c
c Velocities at points
c
      read(1,*) nvo
      if (nvo.gt.maxvo) then
            write(*,*) 'nvo,maxvo=',nvo,maxvo
            stop 'Recompile with an increased maxvo value'
      end if
      if (nvo.gt.0) then
      do i=1,nvo
            read(1,*) vo,volong(i),volat(i)
            if (vo.ne.i) then
                  write(*,*) 'i,vo=',i,vo
                  stop 'Identifying number vo is not equal to i'
            end if
            read(1,*) oux(i),ouy(i),seoux(i),seouy(i),rouxuy(i)
            if (seoux(i).le.0.0d0) then
                  write(*,*) 'i,seoux=',i,seoux(i)
                  stop 'Standard error must be greater than 0'
            end if
            if (seouy(i).le.0.0d0) then
                  write(*,*) 'i,seouy=',i,seouy(i)
                  stop 'Standard error must be greater than 0'
            end if
            if (1.0d0-rouxuy(i)**2.le.0.0d0) then
                  write(*,*) 'i,rouxuy=',i,rouxuy(i)
            stop ' Correlation magnitude must be less than 1'
            end if
      end do
      do i=1,nvo
            if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1            volong(i),volat(i))) then
                  write(*,*) 'i,long,lat=',i,
     1                  volong(i),volat(i)
      stop 'Velocity observation is outside the exterior boundary'
            end if
            if (neb.gt.1) then
                  do eb=2,neb
                        if (pinr(npeb(eb),eblong(1,eb),
     1                        eblat(1,eb),volong(i),volat(i)))
     2                        then
                              write(*,*) 'i,long,lat,eb=',i,
     1                                    volong(i),volat(i),eb
      stop 'Velocity observation is inside an interior boundary'
                        end if
                  end do
            end if
      end do
      end if
c
c Average values of slip rate on fault segments
c
      read(1,*) nfo
      if (nfo.gt.maxfo) then
            write(*,*) 'nfo,maxfo=',nfo,maxfo
            stop 'nfo is impossibly large'
      end if
      if (nfo.gt.0) then
      do i=1,nfo
            read(1,*) fo,ffo(i),p1fo(i),p2fo(i),nfoc(i)
            if (fo.ne.i) then
                  write(*,*) 'i,fo=',i,fo
                  stop 'Identifying number fo is not equal to i'
            end if
            if ((ffo(i).lt.1).or.(ffo(i).gt.nf)) then
                  write(*,*) 'i,ffo(i),nf=',i,ffo(i),nf
                  stop 'ffo value is not valid'
            end if
            l=lf(1,ffo(i))
            if ((p1fo(i).lt.1).or.(p1fo(i).ge.p2fo(i)).or.
     1            (p2fo(i).gt.npl(l))) then
                  write(*,*) 'i,p1fo(i),p2fo(i),npl=',i,
     1                  p1fo(i),p2fo(i),npl(l)
                  stop 'p1fo and p2fo values are not valid'
            end if
            if (nfoc(i).lt.1) then
                  write(*,*) 'i,nfoc(i)=',i,nfoc(i)
                  stop 'nfoc value is not valid'
            end if
            if (nfoc(i).gt.2) then
                  write(*,*) 'i,nfoc(i)=',i,nfoc(i)
                  stop 'nfoc value is not valid'
            end if
            do j=1,nfoc(i)
                  read(1,*) codut(j,i),codun(j,i),oduc(j,i),
     1                  seoduc(j,i)
                  if (codut(j,i)**2+codun(j,i)**2.eq.0.0d0) then
                        write(*,*) 'i,j,codut,codun=',i,j,
     1                        codut(j,i),codun(j,i)
                        stop 'Coefficient vector cannot be zero'
                  end if
                  if (seoduc(j,i).le.0.0d0) then
                        write(*,*) 'i,j,seoduc=',i,j,
     1                        seoduc(j,i)
                  stop 'Standard error must be greater than 0'
                  end if
            end do
            if (nfoc(i).eq.2) then
                  read(1,*) rfo12(i)
                  if (1.0d0-rfo12(i)**2.le.0.0d0) then
                        write(*,*) 'i,rfo12=',i,rfo12(i)
            stop ' Correlation magnitude must be less than 1'
                  end if
            end if
      end do
      end if
c
c Average values of strain rate in elements
c
      read(1,*) neo
      if (neo.gt.maxeo) then
            write(*,*) 'neo,maxeo=',neo,maxeo
            stop 'Recompile with an increased maxeo value'
      end if
      if (neo.gt.0) then
      do i=1,neo
            read(1,*) eo,neoc(i)
            if (eo.ne.i) then
                  write(*,*) 'i,eo=',i,eo
                  stop 'Identifying number eo is not equal to i'
            end if
            if (neoc(i).lt.1) then
                  write(*,*) 'i,neoc(i)=',i,neoc(i)
                  stop 'neoc value is not valid'
            end if
            if (neoc(i).gt.3) then
                  write(*,*) 'i,neoc(i)=',i,neoc(i)
                  stop 'neoc value is not valid'
            end if
            do j=1,neoc(i)
                  read(1,*) coexx(j,i),coeyy(j,i),coexy(j,i),
     1                  oec(j,i),seoec(j,i)
                  if (coexx(j,i)**2+coeyy(j,i)**2+coexy(j,i)**2
     1                  .eq.0.0d0) then
                        write(*,*) 'i,j,coexx,coeyy,coexy=',i,j,
     1                        coexx(j,i),coeyy(j,i),coexy(j,i)
                        stop 'Coefficient vector cannot be zero'
                  end if
                  if (seoec(j,i).le.0.0d0) then
                        write(*,*) 'i,j,seoec=',i,j,
     1                        seoec(j,i)
                  stop 'Standard error must be greater than 0'
                  end if
            end do
            if (neoc(i).eq.2) then
                  read(1,*) reo12(i)
                  if (1.0d0-reo12(i)**2.le.0.0d0) then
                        write(*,*) 'i,reo12=',i,reo12(i)
            stop ' Correlation magnitude must be less than 1'
                  end if
            end if
            if (neoc(i).eq.3) then
                  read(1,*) reo12(i),reo13(i),reo23(i)
                  if (1.0d0-reo12(i)**2.le.0.0d0) then
                        write(*,*) 'i,reo12=',i,reo12(i)
            stop ' Correlation magnitude must be less than 1'
                  end if
                  if (1.0d0-reo13(i)**2.le.0.0d0) then
                        write(*,*) 'i,reo13=',i,reo13(i)
            stop ' Correlation magnitude must be less than 1'
                  end if
                  if (1.0d0-reo23(i)**2.le.0.0d0) then
                        write(*,*) 'i,reo23=',i,reo23(i)
            stop ' Correlation magnitude must be less than 1'
                  end if
            end if
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            npl(nl)=4
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(4,nl)=i
            jl(4,nl)=1
            nleo(i)=1
            leo(1,i)=nl
            do j=1,3
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
      stop 'Point index p not equal to j for strain-rate observation i'
                  end if
            end do
            long(4,nl)=long(1,nl)
            lat(4,nl)=lat(1,nl)
      end do
      do i=1,neo
            l=leo(1,i)
            do j=1,npl(l)
                  if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1                  long(j,l),lat(j,l))) then
                        write(*,*) 'i,j,long,lat=',i,j,
     1                        long(j,l),lat(j,l)
      stop 'Strain-rate observation is outside the exterior boundary'
                  end if
                  if (neb.gt.1) then
                        do eb=2,neb
                              if (pinr(npeb(eb),eblong(1,eb),
     1                              eblat(1,eb),long(j,l),lat(j,l)))
     2                              then
                                    write(*,*) 'i,j,long,lat,eb=',i,j,
     1                                    long(j,l),lat(j,l),eb
      stop 'Strain-rate observation is inside an interior boundary'
                              end if
                        end do
                  end if
            end do
      end do
      end if
c
c Differences between components of slip rate on fault segments
c
      read(1,*) nduc
      if (nduc.gt.maxduc) then
            write(*,*) 'nduc,maxduc=',nduc,maxduc
            stop 'Recompile with an increased maxduc value'
      end if
      if (nduc.gt.0) then
      do i=1,nduc
            read(1,*) duc,fduc(i),nducp(i)
            if (duc.ne.i) then
                  write(*,*) 'i,duc=',i,duc
                  stop 'Identifying number duc is not equal to i'
            end if
            if ((fduc(i).lt.1).or.(fduc(i).gt.nf)) then
                  write(*,*) 'i,fduc(i),nf=',i,fduc(i),nf
                  stop 'fduc value is not valid'
            end if
            l=lf(1,fduc(i))
            if (nducp(i).lt.2) then
                  write(*,*) 'i,nducp(i)=',i,nducp(i)
                  stop 'There must be at least 2 points'
            end if
            if (nducp(i).gt.npl(l)) then
                  write(*,*) 'i,nducp(i),npl=',i,nducp(i),
     1                  npl(l)
                  stop 'Impossibly many points'
            end if
            do j=1,nducp(i)
                  read(1,*) pduc(j,i),cdut(j,i),cdun(j,i),
     1                  refduc(j,i),scduc(j,i)
                  if (j.eq.1) then
                        if (pduc(j,i).lt.1) then
                              write(*,*) 'i,j,pduc(j,i)=',
     1                              i,j,pduc(j,i)
                              stop 'pduc value is not valid'
                        end if
                  end if
                  if (j.gt.1) then
                        if (pduc(j-1,i).ge.pduc(j,i)) then
                              write(*,*) 'i,j,pduc(j-1,i),pduc(j,i)=',
     1                              i,pduc(j-1,i),pduc(j,i)
                              stop 'pduc values are not valid'
                        end if
                  end if
                  if (j.eq.nducp(i)) then
                        if (pduc(j,i).gt.npl(l)) then
                              write(*,*) 'i,j,pduc(j,i),npl=',
     1                              i,j,pduc(j,i),npl(l)
                              stop 'pduc value is not valid'
                        end if
                  end if
                  if (cdut(j,i)**2+cdun(j,i)**2.eq.0.0d0) then
                        write(*,*) 'i,j,cdut,cdun=',i,j,
     1                        cdut(j,i),cdun(j,i)
                        stop 'Coefficient vector cannot be zero'
                  end if
                  if (scduc(j,i).le.0.0d0) then
                        write(*,*) 'i,j,scduc=',i,j,
     1                        scduc(j,i)
                  stop 'Scale factor must be greater than 0'
                  end if
            end do
      end do
      end if
c
c Differences between components of strain rate in elements
c
      read(1,*) nec
      if (nec.gt.maxec) then
            write(*,*) 'nec,maxec=',nec,maxec
            stop 'Recompile with an increased maxec value'
      end if
      if (nec.gt.0) then
      do i=1,nec
            read(1,*) ec,necp(i)
            if (ec.ne.i) then
                  write(*,*) 'i,ec=',i,ec
                  stop 'Identifying number ec is not equal to i'
            end if
            if (necp(i).le.2) then
                  write(*,*) 'i,necp(i)=',i,necp(i)
                  stop 'necp must be greater than 2'
            end if
            if (necp(i)+1.gt.maxpl) then
                  write(*,*) 'i,necp(i),maxpl=',i,necp(i),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            npl(nl)=necp(i)+1
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(6,nl)=i
            jl(6,nl)=1
            nlec(i)=1
            lec(1,i)=nl
            do j=1,necp(i)
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
      stop 'Point index p not equal to j for strain-rate correlation i'
                  end if
                  read(1,*) cexx(j,i),ceyy(j,i),cexy(j,i),
     1                  refec(j,i),scec(j,i)
                  if (cexx(j,i)**2+ceyy(j,i)**2+cexy(j,i)**2
     1                  .eq.0.0d0) then
                        write(*,*) 'i,j,cexx,ceyy,cexy=',i,j,
     1                        cexx(j,i),ceyy(j,i),cexy(j,i)
                        stop 'Coefficient vector cannot be zero'
                  end if
                  if (scec(j,i).le.0.0d0) then
                        write(*,*) 'i,j,scec=',i,j,
     1                        scec(j,i)
                  stop 'Scale factor must be greater than 0'
                  end if
            end do
            long(npl(nl),nl)=long(1,nl)
            lat(npl(nl),nl)=lat(1,nl)
      end do
      do i=1,nec
            l=lec(1,i)
            do j=1,npl(l)
                  if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1                  long(j,l),lat(j,l))) then
                        write(*,*) 'i,j,long,lat=',i,j,
     1                        long(j,l),lat(j,l)
      stop 'Strain-rate correlation is outside the exterior boundary'
                  end if
                  if (neb.gt.1) then
                        do eb=2,neb
                              if (pinr(npeb(eb),eblong(1,eb),
     1                              eblat(1,eb),long(j,l),lat(j,l)))
     2                              then
                                    write(*,*) 'i,j,long,lat,eb=',i,j,
     1                                    long(j,l),lat(j,l),eb
      stop 'Strain-rate correlation is inside an interior boundary'
                              end if
                        end do
                  end if
            end do
      end do
      end if
c
c Dummy lines that can be used for grid control if wished
c
      read(1,*) ndl
      if (ndl.gt.0) then
      do i=1,ndl
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            read(1,*) l,npl(nl)
            if (l.ne.i) then
                  write(*,*) 'i,l=',i,l
                  stop 'Identifying number l is not equal to i'
            end if
            if (npl(nl).le.1) then
                  write(*,*) 'dummy-line i,nl,npl(nl)=',i,nl,npl(nl)
                  stop 'npl must be greater than 1'
            end if
            if (npl(nl).gt.maxpl) then
                  write(*,*) 'dummy-line i,nl,npl(nl),maxpl=',i,nl,
     1                  npl(nl),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do k=1,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            il(9,nl)=i
            jl(9,nl)=1
            do j=1,npl(nl)
                  read(1,*) p,long(j,nl),lat(j,nl)
                  if (p.ne.j) then
                        write(*,*) 'i,j,p=',i,j,p
      stop 'Point index p is not equal to j on dummy line i'
                  end if
                  if (pexr(npeb(1),eblong(1,1),eblat(1,1),
     1                  long(j,nl),lat(j,nl))) then
                        write(*,*) 'i,j,long,lat=',i,j,
     1                        long(j,nl),lat(j,nl)
      stop 'Dummy line is outside the exterior boundary'
                  end if
                  if (neb.gt.1) then
                        do eb=2,neb
                              if (pinr(npeb(eb),eblong(1,eb),
     1                              eblat(1,eb),long(j,l),lat(j,l)))
     2                              then
                                    write(*,*) 'i,j,long,lat,eb=',i,j,
     1                                    long(j,nl),lat(j,nl),eb
      stop 'Dummy line is inside an interior boundary'
                              end if
                        end do
                  end if
            end do
      end do
      end if
c
c Check that line segments do not intersect between points
c
      do i=2,nl
            im=i-1
            do i2=1,im
                  do j=2,npl(i)
                  do j2=2,npl(i2)
                        xa0=long(j-1,i)
                        ya0=lat(j-1,i)
                        xa1=long(j,i)
                        ya1=lat(j,i)
                        xb0=long(j2-1,i2)
                        yb0=lat(j2-1,i2)
                        xb1=long(j2,i2)
                        yb1=lat(j2,i2)
                        if (scross(xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1))
     1                        then
                              write(*,*) 'i,j,i2,j2=',i,j,i2,j2
            stop 'Line segments must not intersect between points'
                        end if
                  end do
                  end do
            end do
      end do
c
c Combine lines that overlap, creating new lines in the process
c
      i=2
10    i2=1
20    j=1
30    j2=1
40    if (npl(i).eq.0) goto 70
      if (npl(i2).eq.0) goto 60
      if ((long(j,i).eq.long(j2,i2)).and.(lat(j,i).eq.lat(j2,i2)))
     1      then
            j0=j
            j1=j
            j20=j2
            j21=j2
110         continue
            if ((j0-1.ge.1).and.(j21+1.le.npl(i2))) then
                  if ((long(j0-1,i).eq.long(j21+1,i2)).and.
     1                  (lat(j0-1,i).eq.lat(j21+1,i2))) then
                        j0=j0-1
                        j21=j21+1
                        signi=-1
                        signi2=1
                        goto 110
                  end if
            end if
120         continue
            if ((j1+1.le.npl(i)).and.(j20-1.ge.1)) then
                  if ((long(j1+1,i).eq.long(j20-1,i2)).and.
     1                  (lat(j1+1,i).eq.lat(j20-1,i2))) then
                        j1=j1+1
                        j20=j20-1
                        signi=1
                        signi2=-1
                        goto 120
                  end if
            end if
130         continue
            if ((j1+1.le.npl(i)).and.(j21+1.le.npl(i2))) then
                  if ((long(j1+1,i).eq.long(j21+1,i2)).and.
     1                  (lat(j1+1,i).eq.lat(j21+1,i2))) then
                        j1=j1+1
                        j21=j21+1
                        signi=1
                        signi2=1
                        goto 130
                  end if
            end if
            if (j0.eq.j1) goto 50
c
            if ((il(2,i).ne.0).and.(il(1,i2).ne.0)) then
                  write(*,*) 'vl,f=',il(2,i),il(1,i2)
                  stop 'Velocity lines and faults cannot overlap'
            end if
            if ((il(2,i).ne.0).and.(il(3,i2).ne.0)) then
                  write(*,*) 'vl,rb=',il(2,i),il(3,i2)
            stop 'Velocity lines and rigid boundaries cannot overlap'
            end if
            if ((il(2,i2).ne.0).and.(il(1,i).ne.0)) then
                  write(*,*) 'vl,f=',il(2,i2),il(1,i)
                  stop 'Velocity lines and faults cannot overlap'
            end if
            if ((il(2,i2).ne.0).and.(il(3,i).ne.0)) then
                  write(*,*) 'vl,rb=',il(2,i2),il(3,i)
            stop 'Velocity lines and rigid boundaries cannot overlap'
            end if
            if ((il(1,i).ne.0).and.(il(1,i2).ne.0)) then
                  write(*,*) 'f1,f2=',il(1,i),il(1,i2)
                  stop 'Two faults cannot overlap'
            end if
            if ((il(2,i).ne.0).and.(il(2,i2).ne.0)) then
                  write(*,*) 'vl1,vl2=',il(2,i),il(2,i2)
                  stop 'Two velocity lines cannot overlap'
            end if
            if ((il(3,i).ne.0).and.(il(3,i2).ne.0)) then
                  write(*,*) 'rb1,rb2=',il(3,i),il(3,i2)
                  stop 'Two rigid boundaries cannot overlap'
            end if
            if ((il(4,i).ne.0).and.(il(5,i2).ne.0)) then
                  write(*,*) 'eo1,eo2,eo3=',il(4,i),il(4,i2),il(5,i2)
      stop 'At most 2 strain-rate observations can have share a side'
            end if
            if ((il(5,i).ne.0).and.(il(4,i2).ne.0)) then
                  write(*,*) 'eo1,eo2,eo3=',il(4,i),il(5,i),il(4,i2)
      stop 'At most 2 strain-rate observations can have share a side'
            end if
            if ((il(6,i).ne.0).and.(il(8,i2).ne.0)) then
                  write(*,*) 'ec1,ec2,ec3,ec4=',il(6,i),il(6,i2),
     1                  il(7,i2),il(8,i2)
      stop 'At most 3 strain-rate correlations can have share a side'
            end if
            if ((il(7,i).ne.0).and.(il(7,i2).ne.0)) then
                  write(*,*) 'ec1,ec2,ec3,ec4=',il(6,i),il(7,i),
     1                  il(6,i2),il(7,i2)
      stop 'At most 3 strain-rate correlations can have share a side'
            end if
            if ((il(8,i).ne.0).and.(il(6,i2).ne.0)) then
                  write(*,*) 'ec1,ec2,ec3,ec4=',il(6,i),il(7,i),
     1                  il(8,i),il(6,i2)
      stop 'At most 3 strain-rate correlations can have share a side'
            end if
            if ((il(9,i).ne.0).or.(il(9,i2).ne.0)) then
                  write(*,*) 'l1,l2=',il(9,i),il(9,i2)
                  stop 'A dummy line cannot overlap with other lines'
            end if
c
c Subdivide line i if necessary
c
            if (j1.lt.npl(i)) then
                  nl=nl+1
                  if (nl.gt.maxl) then
                        write(*,*) 'nl,maxl=',l,maxl
                        stop 'Recompile with an increased maxl value'
                  end if
                  do k=1,9
                        il(k,nl)=il(k,i)
                        if (il(k,i).eq.0) then
                              jl(k,nl)=0
                        else
                              jl(k,nl)=jl(k,i)+1
                        end if
                  end do
                  if (il(1,i).ne.0) then
                        ia=il(1,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(1,nl)
                        if (ja.le.nlf(ia)) then
                              do k=nlf(ia),ja,-1
                                    jl(1,lf(k,ia))=k+1
                                    lf(k+1,ia)=lf(k,ia)
                              end do
                        end if
                        nlf(ia)=nlf(ia)+1
                        lf(ja,ia)=lsign*nl
                  end if
                  if (il(2,i).ne.0) then
                        ia=il(2,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(2,nl)
                        if (ja.le.nlvl(ia)) then
                              do k=nlvl(ia),ja,-1
                                    jl(2,lvl(k,ia))=k+1
                                    lvl(k+1,ia)=lvl(k,ia)
                              end do
                        end if
                        nlvl(ia)=nlvl(ia)+1
                        lvl(ja,ia)=lsign*nl
                  end if
                  if (il(3,i).ne.0) then
                        ia=il(3,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(3,nl)
                        if (ja.le.nlrb(ia)) then
                              do k=nlrb(ia),ja,-1
                                    jl(3,lrb(k,ia))=k+1
                                    lrb(k+1,ia)=lrb(k,ia)
                              end do
                        end if
                        nlrb(ia)=nlrb(ia)+1
                        lrb(ja,ia)=lsign*nl
                  end if
                  if (il(4,i).ne.0) then
                        ia=il(4,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(4,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(5,i).ne.0) then
                        ia=il(5,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(5,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(6,i).ne.0) then
                        ia=il(6,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(6,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(7,i).ne.0) then
                        ia=il(7,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(7,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(8,i).ne.0) then
                        ia=il(8,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(8,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  npl(nl)=npl(i)-j1+1
                  do k=1,npl(nl)
                        ka=k+j1-1
                        long(k,nl)=long(ka,i)
                        lat(k,nl)=lat(ka,i)
                  end do
            end if
c
c Subdivide line i2 if necessary
c
            if (j21.lt.npl(i2)) then
                  nl=nl+1
                  if (nl.gt.maxl) then
                        write(*,*) 'nl,maxl=',l,maxl
                        stop 'Recompile with an increased maxl value'
                  end if
                  do k=1,9
                        il(k,nl)=il(k,i2)
                        if (il(k,i2).eq.0) then
                              jl(k,nl)=0
                        else
                              jl(k,nl)=jl(k,i2)+1
                        end if
                  end do
                  if (il(1,i2).ne.0) then
                        ia=il(1,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(1,nl)
                        if (ja.le.nlf(ia)) then
                              do k=nlf(ia),ja,-1
                                    jl(1,lf(k,ia))=k+1
                                    lf(k+1,ia)=lf(k,ia)
                              end do
                        end if
                        nlf(ia)=nlf(ia)+1
                        lf(ja,ia)=lsign*nl
                  end if
                  if (il(2,i2).ne.0) then
                        ia=il(2,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(2,nl)
                        if (ja.le.nlvl(ia)) then
                              do k=nlvl(ia),ja,-1
                                    jl(2,lvl(k,ia))=k+1
                                    lvl(k+1,ia)=lvl(k,ia)
                              end do
                        end if
                        nlvl(ia)=nlvl(ia)+1
                        lvl(ja,ia)=lsign*nl
                  end if
                  if (il(3,i2).ne.0) then
                        ia=il(3,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(3,nl)
                        if (ja.le.nlrb(ia)) then
                              do k=nlrb(ia),ja,-1
                                    jl(3,lrb(k,ia))=k+1
                                    lrb(k+1,ia)=lrb(k,ia)
                              end do
                        end if
                        nlrb(ia)=nlrb(ia)+1
                        lrb(ja,ia)=lsign*nl
                  end if
                  if (il(4,i2).ne.0) then
                        ia=il(4,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(4,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(5,i2).ne.0) then
                        ia=il(5,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(5,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(6,i2).ne.0) then
                        ia=il(6,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(6,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(7,i2).ne.0) then
                        ia=il(7,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(7,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do

                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(8,i2).ne.0) then
                        ia=il(8,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(8,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  npl(nl)=npl(i2)-j21+1
                  do k=1,npl(nl)
                        ka=k+j21-1
                        long(k,nl)=long(ka,i2)
                        lat(k,nl)=lat(ka,i2)
                  end do
            end if
c
c Add the new combined line
c
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            do k=1,3
                  il(k,nl)=signi*il(k,i)+signi2*il(k,i2)
                  if (il(k,nl).eq.0) then
                        jl(k,nl)=0
                  else
                        jl(k,nl)=jl(k,i)+jl(k,i2)+1
                  end if
            end do
            do k=4,9
                  il(k,nl)=0
                  jl(k,nl)=0
            end do
            if (il(4,i).ne.0) then
                  il(4,nl)=signi*il(4,i)
                  jl(4,nl)=jl(4,i)+1
                  if (il(5,i).ne.0) then
                        il(5,nl)=signi*il(5,i)
                        jl(5,nl)=jl(5,i)+1
                  else
                        if (il(4,i2).ne.0) then
                              il(5,nl)=signi2*il(4,i2)
                              jl(5,nl)=jl(4,i2)+1
                        end if
                  end if
            else
                  if (il(4,i2).ne.0) then
                        il(4,nl)=signi2*il(4,i2)
                        jl(4,nl)=jl(4,i2)+1
                        if (il(5,i2).ne.0) then
                              il(5,nl)=signi2*il(5,i2)
                              jl(5,nl)=jl(5,i2)+1
                        end if
                  end if
            end if
            if (il(6,i).ne.0) then
                  il(6,nl)=signi*il(6,i)
                  jl(6,nl)=jl(6,i)+1
                  if (il(7,i).ne.0) then
                        il(7,nl)=signi*il(7,i)
                        jl(7,nl)=jl(7,i)+1
                        if (il(8,i).ne.0) then
                              il(8,nl)=signi*il(8,i)
                              jl(8,nl)=jl(8,i)+1
                        else
                              if (il(6,i2).ne.0) then
                                    il(8,nl)=signi2*il(6,i2)
                                    jl(8,nl)=jl(6,i2)+1
                              end if
                        end if
                  else
                        if (il(6,i2).ne.0) then
                              il(7,nl)=signi2*il(6,i2)
                              jl(7,nl)=jl(6,i2)+1
                              if (il(7,i2).ne.0) then
                                    il(8,nl)=signi2*il(7,i2)
                                    jl(8,nl)=jl(7,i2)+1
                              end if
                        end if
                  end if
            else
                  if (il(6,i2).ne.0) then
                        il(6,nl)=signi2*il(6,i2)
                        jl(6,nl)=jl(6,i2)+1
                        if (il(7,i2).ne.0) then
                              il(7,nl)=signi2*il(7,i2)
                              jl(7,nl)=jl(7,i2)+1
                              if (il(8,i2).ne.0) then
                                    il(8,nl)=signi2*il(8,i2)
                                    jl(8,nl)=jl(8,i2)+1
                              end if
                        end if
                  end if
            end if
            if (il(1,nl).ne.0) then
                  ia=il(1,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(1,nl)
                  if (ja.le.nlf(ia)) then
                        do k=nlf(ia),ja,-1
                              jl(1,lf(k,ia))=k+1
                              lf(k+1,ia)=lf(k,ia)
                        end do
                  end if
                  nlf(ia)=nlf(ia)+1
                  lf(ja,ia)=lsign*nl
            end if
            if (il(2,nl).ne.0) then
                  ia=il(2,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(2,nl)
                  if (ja.le.nlvl(ia)) then
                        do k=nlvl(ia),ja,-1
                              jl(2,lvl(k,ia))=k+1
                              lvl(k+1,ia)=lvl(k,ia)
                        end do
                  end if
                  nlvl(ia)=nlvl(ia)+1
                  lvl(ja,ia)=lsign*nl
            end if
            if (il(3,nl).ne.0) then
                  ia=il(3,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(3,nl)
                  if (ja.le.nlrb(ia)) then
                        do k=nlrb(ia),ja,-1
                              jl(3,lrb(k,ia))=k+1
                              lrb(k+1,ia)=lrb(k,ia)
                        end do
                  end if
                  nlrb(ia)=nlrb(ia)+1
                  lrb(ja,ia)=lsign*nl
            end if
            if (il(4,nl).ne.0) then
                  ia=il(4,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(4,nl)
                  if (ja.le.nleo(ia)) then
                        do k=nleo(ia),ja,-1
                              if (il(4,leo(k,ia)).eq.ia)
     1                              jl(4,leo(k,ia))=k+1
                              if (il(5,leo(k,ia)).eq.ia)
     1                              jl(5,leo(k,ia))=k+1
                              leo(k+1,ia)=leo(k,ia)
                        end do
                  end if
                  nleo(ia)=nleo(ia)+1
                  leo(ja,ia)=lsign*nl
            end if
            if (il(5,nl).ne.0) then
                  ia=il(5,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(5,nl)
                  if (ja.le.nleo(ia)) then
                        do k=nleo(ia),ja,-1
                              if (il(4,leo(k,ia)).eq.ia)
     1                              jl(4,leo(k,ia))=k+1
                              if (il(5,leo(k,ia)).eq.ia)
     1                              jl(5,leo(k,ia))=k+1
                              leo(k+1,ia)=leo(k,ia)
                        end do
                  end if
                  nleo(ia)=nleo(ia)+1
                  leo(ja,ia)=lsign*nl
            end if
            if (il(6,nl).ne.0) then
                  ia=il(6,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(6,nl)
                  if (ja.le.nlec(ia)) then
                        do k=nlec(ia),ja,-1
                              if (il(6,lec(k,ia)).eq.ia)
     1                              jl(6,lec(k,ia))=k+1
                              if (il(7,lec(k,ia)).eq.ia)
     1                              jl(7,lec(k,ia))=k+1
                              if (il(8,lec(k,ia)).eq.ia)
     1                              jl(8,lec(k,ia))=k+1
                              lec(k+1,ia)=lec(k,ia)
                        end do
                  end if
                  nlec(ia)=nlec(ia)+1
                  lec(ja,ia)=lsign*nl
            end if
            if (il(7,nl).ne.0) then
                  ia=il(7,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(7,nl)
                  if (ja.le.nlec(ia)) then
                        do k=nlec(ia),ja,-1
                              if (il(6,lec(k,ia)).eq.ia)
     1                              jl(6,lec(k,ia))=k+1
                              if (il(7,lec(k,ia)).eq.ia)
     1                              jl(7,lec(k,ia))=k+1
                              if (il(8,lec(k,ia)).eq.ia)
     1                              jl(8,lec(k,ia))=k+1
                              lec(k+1,ia)=lec(k,ia)
                        end do
                  end if
                  nlec(ia)=nlec(ia)+1
                  lec(ja,ia)=lsign*nl
            end if
            if (il(8,nl).ne.0) then
                  ia=il(8,nl)
                  if (ia.gt.0) lsign=1
                  if (ia.lt.0) lsign=-1
                  ia=iabs(ia)
                  ja=jl(8,nl)
                  if (ja.le.nlec(ia)) then
                        do k=nlec(ia),ja,-1
                              if (il(6,lec(k,ia)).eq.ia)
     1                              jl(6,lec(k,ia))=k+1
                              if (il(7,lec(k,ia)).eq.ia)
     1                              jl(7,lec(k,ia))=k+1
                              if (il(8,lec(k,ia)).eq.ia)
     1                              jl(8,lec(k,ia))=k+1
                              lec(k+1,ia)=lec(k,ia)
                        end do
                  end if
                  nlec(ia)=nlec(ia)+1
                  lec(ja,ia)=lsign*nl
            end if
            npl(nl)=j1-j0+1
            if (signi.eq.-1) then
                  do k=1,npl(nl)
                        ka=k+j20-1
                        long(k,nl)=long(ka,i2)
                        lat(k,nl)=lat(ka,i2)
                  end do
            else
                  do k=1,npl(nl)
                        ka=k+j0-1
                        long(k,nl)=long(ka,i)
                        lat(k,nl)=lat(ka,i)
                  end do
            end if
c
c Reduce the two lines to what is left at their beginnings, if anything
c
            if (j0.gt.1) then
                  npl(i)=j0
            else
                  npl(i)=0
            end if
            if (j20.gt.1) then
                  npl(i2)=j20
            else
                  npl(i2)=0
            end if
      end if
c
50    j2=j2+1
      if (j2.le.npl(i2)) goto 40
      j=j+1
      if (j.le.npl(i)) goto 30
60    i2=i2+1
      if (i2.lt.i) goto 20
70    i=i+1
      if (i.le.nl) goto 10
c
c Subdivide lines where they meet at single points
c
      i=2
210   i2=1
220   j=1
230   j2=1
240   if (npl(i).eq.0) goto 260
      if (npl(i2).eq.0) goto 250
      if ((long(j,i).eq.long(j2,i2)).and.(lat(j,i).eq.lat(j2,i2)))
     1      then
c
c Subdivide line i if necessary
c
            if ((j.gt.1).and.(j.lt.npl(i))) then
                  nl=nl+1
                  if (nl.gt.maxl) then
                        write(*,*) 'nl,maxl=',l,maxl
                        stop 'Recompile with an increased maxl value'
                  end if
                  do k=1,9
                        il(k,nl)=il(k,i)
                        if (il(k,i).eq.0) then
                              jl(k,nl)=0
                        else
                              jl(k,nl)=jl(k,i)+1
                        end if
                  end do
                  if (il(1,i).ne.0) then
                        ia=il(1,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(1,nl)
                        if (ja.le.nlf(ia)) then
                              do k=nlf(ia),ja,-1
                                    jl(1,lf(k,ia))=k+1
                                    lf(k+1,ia)=lf(k,ia)
                              end do
                        end if
                        nlf(ia)=nlf(ia)+1
                        lf(ja,ia)=lsign*nl
                  end if
                  if (il(2,i).ne.0) then
                        ia=il(2,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(2,nl)
                        if (ja.le.nlvl(ia)) then
                              do k=nlvl(ia),ja,-1
                                    jl(2,lvl(k,ia))=k+1
                                    lvl(k+1,ia)=lvl(k,ia)
                              end do
                        end if
                        nlvl(ia)=nlvl(ia)+1
                        lvl(ja,ia)=lsign*nl
                  end if
                  if (il(3,i).ne.0) then
                        ia=il(3,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(3,nl)
                        if (ja.le.nlrb(ia)) then
                              do k=nlrb(ia),ja,-1
                                    jl(3,lrb(k,ia))=k+1
                                    lrb(k+1,ia)=lrb(k,ia)
                              end do
                        end if
                        nlrb(ia)=nlrb(ia)+1
                        lrb(ja,ia)=lsign*nl
                  end if
                  if (il(4,i).ne.0) then
                        ia=il(4,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(4,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(5,i).ne.0) then
                        ia=il(5,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(5,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(6,i).ne.0) then
                        ia=il(6,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(6,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(7,i).ne.0) then
                        ia=il(7,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(7,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do

                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(8,i).ne.0) then
                        ia=il(8,i)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(8,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  npl(nl)=npl(i)-j+1
                  do k=1,npl(nl)
                        ka=k+j-1
                        long(k,nl)=long(ka,i)
                        lat(k,nl)=lat(ka,i)
                  end do
                  npl(i)=j
            end if
c
c Subdivide line i2 if necessary
c
            if ((j2.gt.1).and.(j2.lt.npl(i2))) then
                  nl=nl+1
                  if (nl.gt.maxl) then
                        write(*,*) 'nl,maxl=',l,maxl
                        stop 'Recompile with an increased maxl value'
                  end if
                  do k=1,9
                        il(k,nl)=il(k,i2)
                        if (il(k,i2).eq.0) then
                              jl(k,nl)=0
                        else
                              jl(k,nl)=jl(k,i2)+1
                        end if
                  end do
                  if (il(1,i2).ne.0) then
                        ia=il(1,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(1,nl)
                        if (ja.le.nlf(ia)) then
                              do k=nlf(ia),ja,-1
                                    jl(1,lf(k,ia))=k+1
                                    lf(k+1,ia)=lf(k,ia)
                              end do
                        end if
                        nlf(ia)=nlf(ia)+1
                        lf(ja,ia)=lsign*nl
                  end if
                  if (il(2,i2).ne.0) then
                        ia=il(2,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(2,nl)
                        if (ja.le.nlvl(ia)) then
                              do k=nlvl(ia),ja,-1
                                    jl(2,lvl(k,ia))=k+1
                                    lvl(k+1,ia)=lvl(k,ia)
                              end do
                        end if
                        nlvl(ia)=nlvl(ia)+1
                        lvl(ja,ia)=lsign*nl
                  end if
                  if (il(3,i2).ne.0) then
                        ia=il(3,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(3,nl)
                        if (ja.le.nlrb(ia)) then
                              do k=nlrb(ia),ja,-1
                                    jl(3,lrb(k,ia))=k+1
                                    lrb(k+1,ia)=lrb(k,ia)
                              end do
                        end if
                        nlrb(ia)=nlrb(ia)+1
                        lrb(ja,ia)=lsign*nl
                  end if
                  if (il(4,i2).ne.0) then
                        ia=il(4,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(4,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(5,i2).ne.0) then
                        ia=il(5,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(5,nl)
                        if (ja.le.nleo(ia)) then
                              do k=nleo(ia),ja,-1
                                    if (il(4,leo(k,ia)).eq.ia)
     1                                    jl(4,leo(k,ia))=k+1
                                    if (il(5,leo(k,ia)).eq.ia)
     1                                    jl(5,leo(k,ia))=k+1
                                    leo(k+1,ia)=leo(k,ia)
                              end do
                        end if
                        nleo(ia)=nleo(ia)+1
                        leo(ja,ia)=lsign*nl
                  end if
                  if (il(6,i2).ne.0) then
                        ia=il(6,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(6,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(7,i2).ne.0) then
                        ia=il(7,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(7,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do

                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  if (il(8,i2).ne.0) then
                        ia=il(8,i2)
                        if (ia.gt.0) lsign=1
                        if (ia.lt.0) lsign=-1
                        ia=iabs(ia)
                        ja=jl(8,nl)
                        if (ja.le.nlec(ia)) then
                              do k=nlec(ia),ja,-1
                                    if (il(6,lec(k,ia)).eq.ia)
     1                                    jl(6,lec(k,ia))=k+1
                                    if (il(7,lec(k,ia)).eq.ia)
     1                                    jl(7,lec(k,ia))=k+1
                                    if (il(8,lec(k,ia)).eq.ia)
     1                                    jl(8,lec(k,ia))=k+1
                                    lec(k+1,ia)=lec(k,ia)
                              end do
                        end if
                        nlec(ia)=nlec(ia)+1
                        lec(ja,ia)=lsign*nl
                  end if
                  npl(nl)=npl(i2)-j2+1
                  do k=1,npl(nl)
                        ka=k+j2-1
                        long(k,nl)=long(ka,i2)
                        lat(k,nl)=lat(ka,i2)
                  end do
                  npl(i2)=j2
            end if
      end if
c
      j2=j2+1
      if (j2.le.npl(i2)) goto 240
      j=j+1
      if (j.le.npl(i)) goto 230
250   i2=i2+1
      if (i2.lt.i) goto 220
260   i=i+1
      if (i.le.nl) goto 210
c
c Create arrays of control points at ends of lines
c
      i1=1
310   continue
      if (npl(i1).eq.0) then
            lkeep(i1)=.false.
            cp0(i1)=0
            cp1(i1)=0
            i1=i1+1
            goto 310
      end if
      lkeep(i1)=.true.
      ncp=1
      cp0(i1)=ncp
      cplong(ncp)=long(1,i1)
      cplat(ncp)=lat(1,i1)
      nlcp(ncp)=1
      lcp(1,ncp)=i1
      plcp(1,ncp)=1
      cpkeep(ncp)=.true.
      ncp=2
      cp1(i1)=ncp
      cplong(ncp)=long(npl(i1),i1)
      cplat(ncp)=lat(npl(i1),i1)
      nlcp(ncp)=1
      lcp(1,ncp)=i1
      plcp(1,ncp)=npl(i1)
      cpkeep(ncp)=.true.
      i2=i1+1
      do i=i2,nl
            if (npl(i).eq.0) then
                  lkeep(i)=.false.
                  cp0(i)=0
                  cp1(i)=0
                  goto 330
            end if
            lkeep(i)=.true.
            do j=1,ncp
                  if ((cplong(j).eq.long(1,i)).and.
     1                  (cplat(j).eq.lat(1,i))) then
                        cp0(i)=j
                        nlcp(j)=nlcp(j)+1
                        if (nlcp(j).gt.maxlcp) then
                              write(*,*) 'nlcp,maxlcp=',
     1                              nlcp(j),maxlcp
                  stop 'Recompile with an increased maxlcp value'
                        end if
                        lcp(nlcp(j),j)=i
                        plcp(nlcp(j),j)=1
                        goto 320
                  end if
            end do
            ncp=ncp+1
            if (ncp.gt.maxcp) then
                  write(*,*) 'ncp,maxcp=',ncp,maxcp
                  stop 'Recompile with an increased maxcp value'
            end if
            cp0(i)=ncp
            cplong(ncp)=long(1,i)
            cplat(ncp)=lat(1,i)
            nlcp(ncp)=1
            lcp(1,ncp)=i
            plcp(1,ncp)=1
            cpkeep(ncp)=.true.
320         continue
            do j=1,ncp
                  if ((cplong(j).eq.long(npl(i),i)).and.
     1                  (cplat(j).eq.lat(npl(i),i))) then
                        cp1(i)=j
                        nlcp(j)=nlcp(j)+1
                        if (nlcp(j).gt.maxlcp) then
                              write(*,*) 'nlcp,maxlcp=',
     1                              nlcp(j),maxlcp
                  stop 'Recompile with an increased maxlcp value'
                        end if
                        lcp(nlcp(j),j)=i
                        plcp(nlcp(j),j)=npl(i)
                        goto 330
                  end if
            end do
            ncp=ncp+1
            if (ncp.gt.maxcp) then
                  write(*,*) 'ncp,maxcp=',ncp,maxcp
                  stop 'Recompile with an increased maxcp value'
            end if
            cp1(i)=ncp
            cplong(ncp)=long(npl(i),i)
            cplat(ncp)=lat(npl(i),i)
            nlcp(ncp)=1
            lcp(1,ncp)=i
            plcp(1,ncp)=npl(i)
            cpkeep(ncp)=.true.
330         continue
      end do
c
c Reduce the control points and lines to those on region boundaries
c
410   again=.false.
      do i=1,ncp
            if (cpkeep(i)) then
                  nlkeep(i)=0
                  do j=1,nlcp(i)
                        if (lkeep(lcp(j,i))) nlkeep(i)=nlkeep(i)+1
                  end do
                  cpkeep(i)=(nlkeep(i).gt.1)
                  if (.not.cpkeep(i)) again=.true.
            end if
      end do
      do i=1,nl
            if (lkeep(i)) then
                  lkeep(i)=cpkeep(cp0(i)).and.cpkeep(cp1(i))
                  if (.not.lkeep(i)) again=.true.
            end if
      end do
      if (again) goto 410
c
c Reorder the lines at control points on region boundaries to put those
c  also on the boundaries first, and order them according to their angle
c  at the control point
c
      do i=1,ncp
            if (cpkeep(i)) then
                  do j=2,nlcp(i)
                        if (lkeep(lcp(j,i))) then
                              lcpj=lcp(j,i)
                              plcpj=plcp(j,i)
                              jm=j-1
                              do k=jm,1,-1
                                    if (lkeep(lcp(k,i))) goto 420
                                    lcp(k+1,i)=lcp(k,i)
                                    plcp(k+1,i)=plcp(k,i)
                              end do
                              k=0
420                           k=k+1
                              lcp(k,i)=lcpj
                              plcp(k,i)=plcpj
                        end if
                  end do
                  do j=1,nlkeep(i)
                        l=lcp(j,i)
                        if (plcp(j,i).eq.1) then
                              dx=long(2,l)-long(1,l)
                              dy=lat(2,l)-lat(1,l)
                        else
                              dx=long(npl(l)-1,l)-long(npl(l),l)
                              dy=lat(npl(l)-1,l)-lat(npl(l),l)
                        end if
                        alcp(j)=datan2(dx,dy)
                        if (j.gt.1) then
                              lcpj=lcp(j,i)
                              plcpj=plcp(j,i)
                              alcpj=alcp(j)
                              jm=j-1
                              do k=jm,1,-1
                                    if (alcp(k).lt.alcpj) goto 430
                                    lcp(k+1,i)=lcp(k,i)
                                    plcp(k+1,i)=plcp(k,i)
                                    alcp(k+1)=alcp(k)
                              end do
                              k=0
430                           k=k+1
                              lcp(k,i)=lcpj
                              plcp(k,i)=plcpj
                              alcp(k)=alcpj
                        end if
                  end do
            end if
      end do
c
c Create line linkages around region boundaries and initialise the
c  region number for each linkage
c
      do i=1,ncp
            if (cpkeep(i)) then
                  do j=1,nlkeep(i)
                        if (j.eq.1) then
                              jm=nlkeep(i)
                        else
                              jm=j-1
                        end if
                        l=lcp(j,i)
                        lm=lcp(jm,i)
                        if (plcp(j,i).eq.1) then
                              if (plcp(jm,i).eq.1) then
                                    lmp0(l)=lm
                                    lpp0(lm)=l
                                    p0lmp0(l)=.true.
                                    p0lpp0(lm)=.true.
                              else
                                    lmp0(l)=lm
                                    lpp1(lm)=l
                                    p0lmp0(l)=.false.
                                    p1lpp1(lm)=.false.
                              end if
                        else
                              if (plcp(jm,i).eq.1) then
                                    lmp1(l)=lm
                                    lpp0(lm)=l
                                    p1lmp1(l)=.false.
                                    p0lpp0(lm)=.false.
                              else
                                    lmp1(l)=lm
                                    lpp1(lm)=l
                                    p1lmp1(l)=.true.
                                    p1lpp1(lm)=.true.
                              end if
                        end if
                        rlm(l)=0
                        rlp(lm)=0
                  end do
            end if
      end do
c
c Construct the regions
c
      nr=0
      l=1
510   continue
      if (.not.lkeep(l)) then
            l=l+1
            if (l.gt.nl) goto 570
            goto 510
      end if
      if ((rlm(l).ne.0).and.(rlp(l).ne.0)) then
            l=l+1
            if (l.gt.nl) goto 570
            goto 510
      end if
      nr=nr+1
      if (nr.gt.maxr) then
            write(*,*) 'nr,maxr=',nr,maxr
            stop 'Recompile with an increased maxr value'
      end if
      rsame(nr)=.false.
      nlr(nr)=1
      lsign=1
      lr(1,nr)=l
      if (rlm(l).ne.0) goto 530
520   if (((lsign.eq.1).and.(rlm(l).ne.0)).or.
     1      ((lsign.eq.-1).and.(rlp(l).ne.0)))
     2      stop 'There is a bug in making regions'
      if (lsign.eq.1) then
            rlm(l)=nr
            if (lmp1(l).eq.lr(1,nr)) goto 560
            nlr(nr)=nlr(nr)+1
            if (nlr(nr).gt.maxlr) then
                  write(*,*) 'nlr,maxlr=',nlr(nr),maxlr
                  stop 'Recompile with an increased maxlr value'
            end if
            if (p1lmp1(l)) lsign=-1
            l=lmp1(l)
      else
            rlp(l)=nr
            if (lmp0(l).eq.lr(1,nr)) goto 560
            nlr(nr)=nlr(nr)+1
            if (nlr(nr).gt.maxlr) then
                  write(*,*) 'nlr,maxlr=',nlr(nr),maxlr
                  stop 'Recompile with an increased maxlr value'
            end if
            if (p0lmp0(l)) lsign=1
            l=lmp0(l)
      end if
      lr(nlr(nr),nr)=lsign*l
      goto 520
530   if (((lsign.eq.1).and.(rlp(l).ne.0)).or.
     1      ((lsign.eq.-1).and.(rlm(l).ne.0)))
     2      stop 'There is a bug in making regions'
      if (lsign.eq.1) then
            rlp(l)=nr
            if (lpp1(l).eq.lr(1,nr)) goto 540
            nlr(nr)=nlr(nr)+1
            if (nlr(nr).gt.maxlr) then
                  write(*,*) 'nlr,maxlr=',nlr(nr),maxlr
                  stop 'Recompile with an increased maxlr value'
            end if
            if (p1lpp1(l)) lsign=-1
            l=lpp1(l)
      else
            rlm(l)=nr
            if (lpp0(l).eq.lr(1,nr)) goto 540
            nlr(nr)=nlr(nr)+1
            if (nlr(nr).gt.maxlr) then
                  write(*,*) 'nlr,maxlr=',nlr(nr),maxlr
                  stop 'Recompile with an increased maxlr value'
            end if
            if (p0lpp0(l)) lsign=1
            l=lpp0(l)
      end if
      lr(nlr(nr),nr)=lsign*l
      goto 530
540   continue
      if (lr(1,nr).eq.lr(1,nr-1)) then
            j=1
550         j=j+1
            if (j.gt.nlr(nr)) then
                  rsame(nr)=.true.
                  goto 560
            end if
            if (lr(j,nr).eq.lr(j,nr-1)) goto 550
      end if
560   l=lr(1,nr)
      goto 510
570   continue
c
      do i=1,nr
            if (rsame(i)) goto 580
            npr(i)=1
            rlong(1,i)=long(1,lr(1,i))
            rlat(1,i)=lat(1,lr(1,i))
            do j=1,nlr(i)
                  if (lr(j,i).gt.0) then
                        l=lr(j,i)
                        kmax=npl(l)
                        do k=2,kmax
                              npr(i)=npr(i)+1
                              if (npr(i).gt.maxpr) then
                                    write(*,*) 'i,npr,maxpr=',
     1                                    i,npr(i),maxpr
                  stop 'Recompile with an increased maxpr value'
                              end if
                              rlong(npr(i),i)=long(k,l)
                              rlat(npr(i),i)=lat(k,l)
                        end do
                  else
                        l=-lr(j,i)
                        kmax=npl(l)-1
                        do k=kmax,1,-1
                              npr(i)=npr(i)+1
                              if (npr(i).gt.maxpr) then
                                    write(*,*) 'i,npr,maxpr=',
     1                                    i,npr(i),maxpr
                  stop 'Recompile with an increased maxpr value'
                              end if
                              rlong(npr(i),i)=long(k,l)
                              rlat(npr(i),i)=lat(k,l)
                        end do
                  end if
            end do
580         continue
      end do
c
c Find the smallest region containing each region, with the only region
c  not contained in another region being the exterior boundary
c
      rwithr(1)=0
      do i=2,nr
            rwithr(i)=0
            if (rsame(i)) goto 620
            im=i-1
            do i1=1,im
                  if (rsame(i1)) goto 610
                  if (rinr(npr(i1),rlong(1,i1),rlat(1,i1),
     1                  npr(i),rlong(1,i),rlat(1,i))) then
                        if (rwithr(i).ne.0) then
                              i2=rwithr(i)
                              if (rinr(npr(i2),rlong(1,i2),rlat(1,i2),
     1                              npr(i1),rlong(1,i1),rlat(1,i1)))
     2                              rwithr(i)=i1
                        else
                              rwithr(i)=i1
                        end if
                  end if
                  if (rinr(npr(i),rlong(1,i),rlat(1,i),
     1                  npr(i1),rlong(1,i1),rlat(1,i1))) then
                        if (rwithr(i1).ne.0) then
                              i2=rwithr(i1)
                              if (rinr(npr(i2),rlong(1,i2),rlat(1,i2),
     1                              npr(i),rlong(1,i),rlat(1,i)))
     2                              rwithr(i1)=i
                        else
                              rwithr(i1)=i
                        end if
                  end if
610               continue
            end do
620         continue
      end do
c
c Find the smallest region containing each line that is not on a region
c  boundary
c
      do i=1,nl
            if ((npl(i).eq.0).or.lkeep(i)) goto 640
            rwithl(i)=0
            do i1=1,nr
                  if (rsame(i1)) goto 630
                  if (linr(npr(i1),rlong(1,i1),rlat(1,i1),
     1                  cp0(i),cp1(i),cplong,cplat)) then
                        if (rwithl(i).ne.0) then
                              i2=rwithl(i)
                              if (rinr(npr(i2),rlong(1,i2),rlat(1,i2),
     1                              npr(i1),rlong(1,i1),rlat(1,i1)))
     2                              rwithl(i)=i1
                        else
                              rwithl(i)=i1
                        end if
                  end if
630               continue
            end do
640         continue
      end do
c
c Find the exterior boundary and ascertain whether it needs to be passed
c  to gmsh as a "main" region, with this being true only if it is
c  followed in the sequence by a copy of itself,
c  find any other external boundaries, which are not "main" regions,
c  and then iterate from the exterior boundary to progressively smaller
c  regions determining whether they are enclosed "sub" regions of "main"
c  regions and/or "main" regions themselves.
c
c Given the way regions are constructed (i.e. always turning as far as
c  possible to the right at control points when following the "lm"
c  path, and always turning as far as possible to the left at control
c  points when following the "lp" path), "sub" regions have "main"
c  regions as the smallest regions containing them and have no points in
c  common the surrounding "main" region, and "main" regions (which are 
c  the regions gmsh will grid, with "sub" regions as holes in the "main"
c  regions) are regions satisfying either of the two following sets of
c  conditions:
c    1) the region is either the exterior boundary or a "sub" region (of
c  any "main" region) that is followed in the sequence of regions by a
c  copy of itself (i.e. it is the "lm" path and the "lp" path gives
c  the same region) and it is not an interior external boundary,
c    2) the region is not a "sub" region (i.e. the smallest region
c  containing it is not a "main" region) and it is not an external
c  boundary.
c
      do i=1,nr
            ebr(i)=0
            if (rsame(i)) then
                  rmain(i)=.false.
                  rsub(i)=.false.
                  rtodo(i)=.false.
                  goto 710
            end if
            rtodo(i)=.true.
            if (rwithr(i).eq.0) then
                  ebr(i)=1
                  reb(1)=i
                  if (i.eq.nr) then
                        rmain(i)=.false.
                  else
                        rmain(i)=rsame(i+1)
                  end if
                  rsub(i)=.false.
                  rtodo(i)=.false.
            end if
710         continue
      end do
      if (neb.gt.1) then
      do i=2,neb
            do i1=1,nr
                  if (rsame(i1)) goto 750
                  do j=1,nleb(i)
                        leb1=iabs(leb(j,i))
                        if (kleb(j,i).eq.2) then
                              do k=1,nlvl(leb1)
                                    l=iabs(lvl(k,leb1))
                                    do j1=1,nlr(i1)
                                          if (l.eq.iabs(lr(j1,i1)))
     1                                          goto 720
                                    end do
                                    goto 750
720                                 continue
                              end do
                        end if
                        if (kleb(j,i).eq.3) then
                              do k=1,nlrb(leb1)
                                    l=iabs(lrb(k,leb1))
                                    do j1=1,nlr(i1)
                                          if (l.eq.iabs(lr(j1,i1)))
     1                                          goto 730
                                    end do
                                    goto 750
730                                 continue
                              end do
                        end if
                  end do
                  do j1=1,nlr(i1)
                        l=iabs(lr(j1,i1))
                        do j=1,nleb(i)
                              leb1=iabs(leb(j,i))
                              if (kleb(j,i).eq.2) then
                                    do k=1,nlvl(leb1)
                                          if (l.eq.iabs(lvl(k,leb1)))
     1                                          goto 740
                                    end do
                              end if
                              if (kleb(j,i).eq.3) then
                                    do k=1,nlrb(leb1)
                                          if (l.eq.iabs(lrb(k,leb1)))
     1                                          goto 740
                                    end do
                              end if
                        end do
                        goto 750
740                     continue
                  end do
                  ebr(i1)=i
                  reb(i)=i1
                  rmain(i1)=.false.
                  goto 760
750               continue
            end do
760         continue
      end do
      end if
770   again=.false.
      do i=1,nr
            if (rtodo(i)) then
                  if (rtodo(rwithr(i))) then
                        again=.true.
                  else
                        if (rmain(rwithr(i))) then
                              rsub(i)=.true.
                              if (ebr(i).eq.0) then
                                    if (i.eq.nr) then
                                          rmain(i)=.false.
                                    else
                                          rmain(i)=rsame(i+1)
                                    end if
                              end if
                        else
                              rsub(i)=.false.
                              if (ebr(i).eq.0) rmain(i)=.true.
                        end if
                        rtodo(i)=.false.
                  end if
            end if
      end do
      if (again) goto 770
c
c Construct mappings between lines and regions and their subsets being
c  passed to gmsh, and calculate maximum element sizes for the control
c  points
c
      ngl=0
      do i=1,nl
            if (npl(i).ne.0) then
                  ngl=ngl+1
                  gll(i)=ngl
                  lgl(ngl)=i
                  if (lkeep(i)) then
                        if ((il(2,i).ne.0).or.(il(3,i).ne.0)) then
                              if (il(2,i).ne.0) then
                                    if (ebvl(iabs(il(2,i))).ne.0)
     1                                    ltype(ngl)=1
                                    if (ebvl(iabs(il(2,i))).eq.0)
     1                                    ltype(ngl)=2
                              end if
                              if (il(3,i).ne.0) ltype(ngl)=1
                        else
                              ltype(ngl)=2
                        end if
                  else
                        ltype(ngl)=3
                  end if
            end if
      end do
      ngr=0
      do i=1,nr
            if (rmain(i).or.rsub(i)) then
                  ngr=ngr+1
                  grr(i)=ngr
                  rgr(ngr)=i
                  lgr(ngr)=0
                  if (rmain(i)) then
                        rtype(ngr)=1
                  else
                        rtype(ngr)=2
                  end if
            end if
      end do
      nir=0
      do i=1,ngr
            if (rsub(rgr(i))) then
                  nir=nir+1
                  ir(nir)=i
                  sr(nir)=grr(rwithr(rgr(i)))
            end if
      end do
      do i=1,nl
            if (npl(i).eq.0) goto 810
            if (.not.lkeep(i)) then
                  ngr=ngr+1
                  grl(i)=ngr
                  rgr(ngr)=0
                  lgr(ngr)=i
                  rtype(ngr)=3
                  nir=nir+1
                  ir(nir)=ngr
                  sr(nir)=grr(rwithl(i))
            end if
810         continue
      end do
c
      do i=1,ngl
            l=lgl(i)
            do j=1,npl(l)
                  if (j.eq.1) then
                        dx=long(j,l)-cplong(cp1(l))
                        dy=lat(j,l)-cplat(cp1(l))
                  else
                        dx=long(j,l)-cplong(cp0(l))
                        dy=lat(j,l)-cplat(cp0(l))
                  end if
                  dl=dsqrt(dx**2+dy**2)
                  dmax(j,l)=dl
                  do i1=1,ngl
                        l1=lgl(i1)
                        do j1=1,npl(l1)
                              dx=long(j,l)-long(j1,l1)
                              dy=lat(j,l)-lat(j1,l1)
                              dl=dsqrt(dx**2+dy**2)
                              if (dl.gt.0.0d0) then
                                    if (dl.lt.dmax(j,l))
     1                                    dmax(j,l)=dl
                              end if
                        end do
                  end do
            end do
            dmaxcp(cp0(l))=dmax(1,l)
            dmaxcp(cp1(l))=dmax(npl(l),l)
      end do  
c
c Output the data used by "geometry_for_setup" to create the
c  "geometry_for_setup.geo" file read in by gmsh
c
      open(2,file='gmsh_control.dat')
c
c Control points
c
      write(2,*) ncp
      do i=1,ncp
            write(2,*) i,cplong(i),cplat(i),dmaxcp(i)
      end do
      write(2,*)
c
c Lines
c
      write(2,*) ngl
      do i=1,ngl
            l=lgl(i)
            write(2,*) i,cp0(l),cp1(l),npl(l)-2
            if (ltype(i).eq.1) write(2,*) 'E'
            if (ltype(i).eq.2) write(2,*) 'B'
            if (ltype(i).eq.3) write(2,*) 'I'
            do j=1,npl(l)
                  if ((j.eq.1).or.(j.eq.npl(l))) then
                        write(2,*) long(j,l),lat(j,l)
                  else
                        write(2,*) long(j,l),lat(j,l),dmax(j,l)
                  end if
            end do
      end do
      write(2,*)
c
c Regions
c
      write(2,*) ngr
      do i=1,ngr
            i1=rgr(i)
            if (i1.ne.0) then
                  write(2,*) i,nlr(i1)
                  if (rtype(i).eq.1) write(2,*) 'R'
                  if (rtype(i).eq.2) write(2,*) 'H'
                  do j=1,nlr(i1)
                        l=lr(j,i1)
                        if (l.gt.0) then
                              write(2,*) gll(l)
                        else
                              l=-l
                              write(2,*) -gll(l)
                        end if
                  end do
            else
                  l=lgr(i)
                  write(2,*) i,1
                  write(2,*) 'L'
                  write(2,*) gll(l)
            end if
      end do
      write(2,*)
c
c Interior regions
c
      write(2,*) nir
      do i=1,nir
            write(2,*) i,ir(i),sr(i)
      end do
      write(2,*)
c
      open(3,file='post_gmsh_data.dat')
c
c Control points
c
      write(3,*) ncp
      do i=1,ncp
            write(3,*) i,cplong(i),cplat(i)
      end do
      write(3,*)
c
c Lines
c
      write(3,*) ngl
      do i=1,ngl
            l=lgl(i)
            write(3,*) i,cp0(l),cp1(l)
      end do
      write(3,*)
c
c Velocity lines
c
      write(3,*) nvl
      if (nvl.gt.0) then
      do i=1,nvl
            ndl=0
            do j=1,nlvl(i)
                  l=iabs(lvl(j,i))
                  if (npl(l).ne.0) ndl=ndl+1
            end do
            write(3,*) i,ndl
            do j=1,nlvl(i)
                  l=lvl(j,i)
                  if (npl(iabs(l)).ne.0) then
                        if (l.gt.0) then
                              write(3,*) gll(l),cp0(l),cp1(l)
                        else
                              l=-l
                              write(3,*) -gll(l),cp0(l),cp1(l)
                        end if
                  end if
            end do
      end do
      end if
      write(3,*)
c
c Rigid boundaries
c
      write(3,*) nrb
      if (nrb.gt.0) then
      do i=1,nrb
            ndl=0
            do j=1,nlrb(i)
                  l=iabs(lrb(j,i))
                  if (npl(l).ne.0) ndl=ndl+1
            end do
            write(3,*) i,ndl
            do j=1,nlrb(i)
                  l=lrb(j,i)
                  if (npl(iabs(l)).ne.0) then
                        if (l.gt.0) then
                              write(3,*) gll(l),cp0(l),cp1(l)
                        else
                              l=-l
                              write(3,*) -gll(l),cp0(l),cp1(l)
                        end if
                  end if
            end do
      end do
      end if
      write(3,*)
c
c Faults
c
      write(3,*) nf
      if (nf.gt.0) then
      do i=1,nf
            ndl=0
            do j=1,nlf(i)
                  l=iabs(lf(j,i))
                  if (npl(l).ne.0) ndl=ndl+1
            end do
            write(3,*) i,ndl
            do j=1,nlf(i)
                  l=lf(j,i)
                  if (npl(iabs(l)).ne.0) then
                        if (l.gt.0) then
                              write(3,*) gll(l),cp0(l),cp1(l)
                        else
                              l=-l
                              write(3,*) -gll(l),cp0(l),cp1(l)
                        end if
                  end if
            end do
      end do
      end if
      write(3,*)
c
c Strain-rate observations
c
      write(3,*) neo
      if (neo.gt.0) then
      do i=1,neo
            ndl=0
            do j=1,nleo(i)
                  l=iabs(leo(j,i))
                  if (npl(l).ne.0) ndl=ndl+1
            end do
            write(3,*) i,ndl
            do j=1,nleo(i)
                  l=leo(j,i)
                  if (npl(iabs(l)).ne.0) then
                        if (l.gt.0) then
                              write(3,*) gll(l),cp0(l),cp1(l)
                        else
                              l=-l
                              write(3,*) -gll(l),cp0(l),cp1(l)
                        end if
                  end if
            end do
      end do
      end if
      write(3,*)
c
c Strain-rate correlations
c
      write(3,*) nec
      if (nec.gt.0) then
      do i=1,nec
            ndl=0
            do j=1,nlec(i)
                  l=iabs(lec(j,i))
                  if (npl(l).ne.0) ndl=ndl+1
            end do
            write(3,*) i,ndl
            do j=1,nlec(i)
                  l=lec(j,i)
                  if (npl(iabs(l)).ne.0) then
                        if (l.gt.0) then
                              write(3,*) gll(l),cp0(l),cp1(l)
                        else
                              l=-l
                              write(3,*) -gll(l),cp0(l),cp1(l)
                        end if
                  end if
            end do
      end do
      end if
      write(3,*)
c
c External boundaries
c
      write(3,*) neb
      do i=1,neb
            write(3,*) i,npeb(i)
            do j=1,npeb(i)
                  write(3,*) j,eblong(j,i),eblat(j,i)
            end do
      end do
      write(3,*)
c
      close(1)
      close(2)
      close(3)
      return
      end
c
c
      FUNCTION scross(xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1)
c
c This determines whether the line joining the points (xa0,ya0) and
c  (xa1,ya1) and the line joining the points (xb0,yb0) and (xb1,yb1)
c  intersect between the points. The case where a point on one line lies
c  between the points on the other line also counts as crossing, except
c  when the point on the first line coincides exactly with either point
c  on the second line.
c To get round rounding errors, products of distances (in degrees)
c  between the points are treated as being zero if the products are less
c  than or equal to prec, which is set as 1.0d-12.
c
      implicit none
      logical scross
      real*8 xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1
      real*8 prec,det,pa0,pa1,pb0,pb1,sign
      real*8 pa0b0,pa1b0,pa0b1,pa1b1,pb0a0,pb1a0,pb0a1,pb1a1
c
      prec=1.0d-12
      det=(xa1-xa0)*(yb1-yb0)-(ya1-ya0)*(xb1-xb0)
      pa0=(xa1-xb0)*(yb1-yb0)-(ya1-yb0)*(xb1-xb0)
      pa1=(xb1-xa0)*(yb1-yb0)-(yb1-ya0)*(xb1-xb0)
      pb0=(xa1-xa0)*(yb1-ya0)-(ya1-ya0)*(xb1-xa0)

      pb1=(xa1-xa0)*(ya1-yb0)-(ya1-ya0)*(xa1-xb0)
c
c det is the cross product of the vectors [(xa1,ya1)-(xa0,ya0)] and
c  [(xb1,yb1)-(xb0,yb0)] along the two lines.
c pa0 is the cross product of [(xa1,ya1)-(xb0,yb0)] and
c  [(xb1,yb1)-(xb0,yb0)].
c pa1 is the cross product of [(xb1,yb1)-(xa0,ya0)] and
c  [(xb1,yb1)-(xb0,yb0)].
c pb0 is the cross product of [(xa1,ya1)-(xa0,ya0)] and
c  [(xb1,yb1)-(xa0,ya0)].
c pb1 is the cross product of [(xa1,ya1)-(xa0,ya0)] and
c  [(xa1,ya1)-(xb0,yb0)].
c
      if (dabs(det).le.prec) then
c
c This is the case where the lines are parallel.
c
            if (dabs(pa0).gt.prec) then
                  scross=.false.
                  return
            end if
            if (dabs(pa1).gt.prec) then
                  scross=.false.
                  return
            end if
            if (dabs(pb0).gt.prec) then
                  scross=.false.
                  return
            end if
            if (dabs(pb1).gt.prec) then
                  scross=.false.
                  return
            end if
c

c The above checks are whether there is perpendicular separation between
c  the parallel lines, implying that the lines do not cross.
c
c Note below that pa0b0+pa1b0 = pa0b1+pa1b1 = (xa1-xa0)**2+(ya1-ya0)**2
c  and pb0a0+pb1a0 = pb0a1+pb1a1 = (xb1-xb0)**2+(yb1-yb0)**2.
c For the case of parallel lines with effectively no perpendicular
c  separation, the locations of (xb0,yb0) and (xb1,yb1) relative to
c  (xa0,ya0) and (xa1,ya1) are given to within rounding errors by
c     [pa0b0+pa1b0]*(xb0,yb0) = pa0b0*(xa0,ya0)+pa1b0*(xa1,ya1),
c     [pa0b1+pa1b1]*(xb1,yb1) = pa0b1*(xa0,ya0)+pa1b1*(xa1,ya1),
c  and the locations of (xb0,yb0) and (xb1,yb1) relative to (xa0,ya0)
c  and (xa1,ya1) are given to within rounding errors by
c     [pb0a0+pb1a0]*(xa0,ya0) = pb0a0*(xb0,yb0)+pb1a0*(xb1,yb1),
c     [pb0a1+pb1a1]*(xa1,ya1) = pb0a1*(xb0,yb0)+pb1a1*(xb1,yb1).
c
            pa0b0=(xa1-xa0)*(xa1-xb0)+(ya1-ya0)*(ya1-yb0)
            pa1b0=(xa1-xa0)*(xb0-xa0)+(ya1-ya0)*(yb0-ya0)
            pa0b1=(xa1-xa0)*(xa1-xb1)+(ya1-ya0)*(ya1-yb1)
            pa1b1=(xa1-xa0)*(xb1-xa0)+(ya1-ya0)*(yb1-ya0)
            pb0a0=(xb1-xb0)*(xb1-xa0)+(yb1-yb0)*(yb1-ya0)
            pb1a0=(xb1-xb0)*(xa0-xb0)+(yb1-yb0)*(ya0-yb0)
            pb0a1=(xb1-xb0)*(xb1-xa1)+(yb1-yb0)*(yb1-ya1)
            pb1a1=(xb1-xb0)*(xa1-xb0)+(yb1-yb0)*(ya1-yb0)
            if ((dabs(pa0b1).le.prec).and.
     1            (dabs(pb0a1).le.prec)) then
                  scross=(xa1.ne.xb1).or.(ya1.ne.yb1)
                  return
            end if
            if ((dabs(pa0b0).le.prec).and.
     1            (dabs(pb1a1).le.prec)) then
                  scross=(xa1.ne.xb0).or.(ya1.ne.yb0)
                  return
            end if
            if ((dabs(pa1b1).le.prec).and.
     1            (dabs(pb0a0).le.prec)) then
                  scross=(xa0.ne.xb1).or.(ya0.ne.yb1)
                  return
            end if
            if ((dabs(pa1b0).le.prec).and.
     1            (dabs(pb1a0).le.prec)) then
                  scross=(xa0.ne.xb0).or.(ya0.ne.yb0)
                  return
            end if
c
c These checks are whether a point on one line and a point on the other
c  line have effectively zero separation without being identical, in
c  which case the lines are deemed to cross.
c Otherwise the parallel lines are deemed to cross if one of the points
c  lies between the two points on the other line.
c
            scross=((pa0b0.ge.-prec).and.(pa1b0.ge.-prec)).or.
     1            ((pa0b1.ge.-prec).and.(pa1b1.ge.-prec)).or.
     2            ((pb0a0.ge.-prec).and.(pb1a0.ge.-prec)).or.
     3            ((pb0a1.ge.-prec).and.(pb1a1.ge.-prec))
            return
      else
c
c For the remaining case of non-parallel lines, which meet at a point
c  (x,y), the location of (x,y) relative to the points on each line is
c  given by
c     det*(x,y) = pa0*(xa0,ya0)+pa1*(xa1,ya1) and
c     det*(x,y) = pb0*(xb0,yb0)+pb1*(xb1,yb1).
c  Note that pa0+pa1 = det, so pa1 = det if pa0 = 0, and vice-versa.
c  Equivalently, pb0+pb1 = det.
c
            if ((dabs(pa0).le.prec).and.(dabs(pb0).le.prec)) then
                  scross=(xa1.ne.xb1).or.(ya1.ne.yb1)
                  return
            end if
            if ((dabs(pa0).le.prec).and.(dabs(pb1).le.prec)) then
                  scross=(xa1.ne.xb0).or.(ya1.ne.yb0)
                  return
            end if
            if ((dabs(pa1).le.prec).and.(dabs(pb0).le.prec)) then
                  scross=(xa0.ne.xb1).or.(ya0.ne.yb1)
                  return
            end if
            if ((dabs(pa1).le.prec).and.(dabs(pb1).le.prec)) then
                  scross=(xa0.ne.xb0).or.(ya0.ne.yb0)
                  return
            end if
c
c These checks are whether (x,y) has effectively zero separation from
c  points on both lines without the points on the two lines being
c  identical, in which case the lines are deemed to cross.
c Otherwise the non-parallel lines are deemed to cross when (x,y) lies
c  between the two points on each of the lines.
c
            sign=det/dabs(det)
            pa0=sign*pa0
            pa1=sign*pa1
            pb0=sign*pb0
            pb1=sign*pb1
            scross=(pa0.ge.-prec).and.(pa1.ge.-prec).and.
     1            (pb0.ge.-prec).and.(pb1.ge.-prec)
            return
      end if
      end
c
c
      FUNCTION pexr(npr,rlong,rlat,long,lat)
c
c This determines whether the point at (long,lat) is outside the region.
c
      implicit none
      integer maxpr
      parameter (maxpr=1001)
      logical pexr
      integer npr
      integer i,nlt,ngt,im
      real*8 long,lat,ilong
      real*8 rlong(maxpr),rlat(maxpr)
c
      do i=1,npr
            if ((rlong(i).eq.long).and.(rlat(i).eq.lat)) then
                  pexr=.false.
                  return
            end if
      end do
      nlt=0
      ngt=0
      do i=2,npr
            im=i-1
            if (((rlat(im).le.lat).and.(rlat(i).gt.lat)).or.
     1            ((rlat(im).ge.lat).and.(rlat(i).lt.lat))) then
                  ilong=rlong(im)+(rlong(i)-rlong(im))
     1                  *(lat-rlat(im))/(rlat(i)-rlat(im))
                  if (ilong.lt.long) nlt=nlt+1
                  if (ilong.gt.long) ngt=ngt+1
            end if
      end do
      pexr=(nlt.eq.(nlt/2)*2).and.(ngt.eq.(ngt/2)*2)
      return
      end
c
c
      FUNCTION rinr(npr1,rlong1,rlat1,npr2,rlong2,rlat2)
c
c This determines whether region 2 is inside region 1.
c
      implicit none
      integer maxpr
      parameter (maxpr=1001)
      logical rinr,pinr
      integer npr1,npr2
      integer i
      real*8 rlong1(maxpr),rlat1(maxpr),rlong2(maxpr),rlat2(maxpr)
c
      do i=1,npr2
            if (pinr(npr1,rlong1,rlat1,rlong2(i),rlat2(i)))
     1            goto 10
      end do
      rinr=.false.
      return
10    rinr=.true.
      return
      end
c

c
      FUNCTION linr(npr,rlong,rlat,cp0,cp1,cplong,cplat)
c
c This determines whether the line with control points cp0 and cp1 is
c  inside the region.
c
      implicit none
      integer maxl,maxpr,maxcp
      parameter (maxl=10000,maxpr=1001,maxcp=2*maxl)
      logical linr,pinr
      integer npr,cp0,cp1
      real*8 rlong(maxpr),rlat(maxpr),cplong(maxcp),cplat(maxcp)
c
      if (pinr(npr,rlong,rlat,cplong(cp0),cplat(cp0))) goto 10
      if (pinr(npr,rlong,rlat,cplong(cp1),cplat(cp1))) goto 10
      linr=.false.
      return
10    linr=.true.
      return
      end
c
c
      FUNCTION pinr(npr,rlong,rlat,long,lat)
c
c This determines whether the point at (long,lat) is inside the region.
c
      implicit none
      integer maxpr
      parameter (maxpr=1001)
      logical pinr
      integer npr
      integer i,nlt,ngt,im
      real*8 long,lat,ilong
      real*8 rlong(maxpr),rlat(maxpr)
c
      do i=1,npr
            if ((rlong(i).eq.long).and.(rlat(i).eq.lat)) then
                  pinr=.false.
                  return
            end if
      end do
      nlt=0
      ngt=0
      do i=2,npr
            im=i-1
            if (((rlat(im).le.lat).and.(rlat(i).gt.lat)).or.
     1            ((rlat(im).ge.lat).and.(rlat(i).lt.lat))) then
                  ilong=rlong(im)+(rlong(i)-rlong(im))
     1                  *(lat-rlat(im))/(rlat(i)-rlat(im))
                  if (ilong.lt.long) nlt=nlt+1
                  if (ilong.gt.long) ngt=ngt+1
            end if
      end do
      pinr=(nlt.ne.(nlt/2)*2).and.(ngt.ne.(ngt/2)*2)
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c

