      SUBROUTINE postgmsh()
c
      implicit none
      integer maxl,maxsl,maxpl,
     1      maxf,maxvl,maxrb,maxeb,maxpr,maxcp,
     2      maxvo,maxfo,maxeo,maxduc,maxec,
     3      maxgp,maxs,maxe
      parameter(maxl=10000,maxsl=200,maxpl=maxsl+1,
     1      maxf=600,maxvl=10,maxrb=10,
     2      maxeb=10,maxpr=1001,maxcp=2*maxl,
     3      maxvo=5000,maxfo=maxf*maxsl,maxeo=500,
     4      maxduc=2*maxf,maxec=100,
     5      maxgp=40000,maxs=3*maxgp,maxe=2*maxgp)
      integer nl,nf,nvl,nrb,neb,nvo,nfo,neo,nduc,nec,nfL,ncp
      integer nleb,kleb,leb,ndl
      integer i,vl,j,p,rb,eb,f,fL,vo,fo,eo,duc,ec,cp,l
      integer ngp,ns,ne,lun
      integer ngn,nr,dimen,gp,nge,ge,dum1,dum2,cpn
      integer gem,ln,gp1,gp2,gp3,rn,r,s
      integer k,kmin,kmax,k0,k1,nj,newj,j0,j1,newfo,neweo,e,necs
      integer finds,findgp
      integer npl(maxl)
      integer npvl(maxvl),nprb(maxrb),npf(maxf),npec(maxec)
      integer nlvl(maxvl),lvl(maxsl,maxvl),nvlf(maxvl)
      integer vptype(maxpl,maxvl),fvl(maxpl,maxvl),ebvl(maxvl)
      integer nlrb(maxrb),lrb(maxsl,maxrb),ebrb(maxrb)
      integer nlf(maxf),lf(maxsl,maxf)
      integer ffL(maxf),nfLp(maxf),pfL(maxpl,maxf)
      integer ffo(maxfo),p1fo(maxfo),p2fo(maxfo),nfoc(maxfo)
      integer neoc(maxeo)
      integer nleo(maxeo),leo(3,maxeo)
      integer fduc(maxduc),nducp(maxduc),pduc(maxpl,maxduc)
      integer necp(maxec)
      integer nlec(maxec),lec(maxsl,maxec)
      integer npeb(maxeb)
      integer cp0(maxl),cp1(maxl)
      integer cp0vl(maxsl,maxvl),cp1vl(maxsl,maxvl)
      integer cp0rb(maxsl,maxrb),cp1rb(maxsl,maxrb)
      integer cp0f(maxsl,maxf),cp1f(maxsl,maxf)
      integer cp0eo(3,maxeo),cp1eo(3,maxeo)
      integer cp0ec(maxsl,maxec),cp1ec(maxsl,maxec)
      integer gncp(maxcp),gpcp(maxcp)
      integer gnl(maxl),nsl(maxl),gpl(0:maxsl,maxl)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1s(maxs),gp2s(maxs)
      integer nfs(maxf),gpf(0:maxsl,maxf),sf(maxsl,maxf)
      integer pfpf(maxpl,maxf)
      integer sfo(maxfo)
      integer nducs(maxduc),sduc(maxsl,maxduc)
      integer nvls(maxvl),gpvl(0:maxsl,maxvl),nvlgp(maxvl)
      integer svl(maxsl,maxvl),pvlpvl(maxpl,maxvl)
      integer nrbs(maxrb),gprb(0:maxsl,maxrb),srb(maxsl,maxrb)
      integer eeo(maxeo)
      integer gpec(0:maxsl),pecpec(maxpl)
      integer nece(maxec),eec(maxpl,maxec)
      logical pinr
      real*8 rE
      real*8 dx,dy,p0
      real*8 long(maxgp),lat(maxgp)
      real*8 vllong(maxpl,maxvl),vllat(maxpl,maxvl)
      real*8 rblong(maxpl,maxrb),rblat(maxpl,maxrb)
      real*8 flong(maxpl,maxf),flat(maxpl,maxf)
      real*8 eolong(4,maxeo),eolat(4,maxeo)
      real*8 eclong(maxpl,maxec),eclat(maxpl,maxec)
      real*8 Kc(maxpl,maxf),Ks(maxpl,maxf)
      real*8 ux(maxpl,maxvl),uy(maxpl,maxvl),
     1      uxm(maxpl,maxvl),uym(maxpl,maxvl),
     2      uxp(maxpl,maxvl),uyp(maxpl,maxvl)
      real*8 plat(maxrb),plong(maxrb),prate(maxrb)
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
      real*8 eblong(maxpr,maxeb),eblat(maxpr,maxeb)
      real*8 cplong(maxcp),cplat(maxcp)
      real*8 elong(maxe),elat(maxe)
      real*8 lenl(0:maxsl),Kcp(0:maxsl),Ksp(0:maxsl)
      real*8 cexxp(0:maxsl),ceyyp(0:maxsl),cexyp(0:maxsl),
     1      refecp(0:maxsl),scecp(0:maxsl)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
c
c WARNING: This program assumes that checking of data in
c  'raw_input_when_using_gmsh.dat' has been done in 'pre_gmsh'. The
c  checks retained are those related to values in the maximum array size
c  parameter statement at the beginning, as a reminder that they have to
c  be consistent with the values in 'pre_gmsh'.
c
      open(1,file='raw_input_when_using_gmsh.dat')
      read(1,*) rE
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
            read(1,*) vl,npvl(i)
            if (npvl(i).gt.maxpl) then
                  write(*,*) 'v-line i,npvl(i),maxpl=',i,
     1                  npvl(i),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            nvlf(i)=0
            do j=1,npvl(i)
                  read(1,*) p,vllong(j,i),vllat(j,i)
                  read(1,*) vptype(j,i)
                  if (vptype(j,i).eq.1) then
                        read(1,*) ux(j,i),uy(j,i)
                  end if
                  if (vptype(j,i).eq.3) then
                        read(1,*) fvl(j,i),uxm(j,i),uym(j,i),
     1                        uxp(j,i),uyp(j,i)
                        nvlf(i)=nvlf(i)+1
                  end if
                  if (vptype(j,i).eq.-3) then
                        read(1,*) fvl(j,i)
                        nvlf(i)=nvlf(i)+1
                  end if
            end do
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
            read(1,*) rb,nprb(i),plat(i),plong(i),prate(i)
            if (nprb(i).gt.maxpl) then
                  write(*,*) 'rigid-b i,nprb(i),maxpl=',i,
     1                  nprb(i),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do j=1,nprb(i)
                  read(1,*) p,rblong(j,i),rblat(j,i)
            end do
      end do
      end if
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
            read(1,*) eb,nleb
            do j=1,nleb
                  read(1,*) l,kleb,leb
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
            read(1,*) f,npf(i)
            if (npf(i).gt.maxpl) then
                  write(*,*) 'fault i,npf(i),maxpl=',i,
     1                  npf(i),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            do j=1,npf(i)
                  read(1,*) p,flong(j,i),flat(j,i)
                  read(1,*) Kc(j,i),Ks(j,i)
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
            do j=1,nfLp(i)
                  read(1,*) pfL(j,i),yfLm(j,i),yfLp(j,i)
                  read(1,*) fLcm(j,i),fafLcm(j,i),azfLcm(j,i)
                  read(1,*) fLsm(j,i),fafLsm(j,i),azfLsm(j,i)
                  read(1,*) fLcp(j,i),fafLcp(j,i),azfLcp(j,i)
                  read(1,*) fLsp(j,i),fafLsp(j,i),azfLsp(j,i)
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
            read(1,*) oux(i),ouy(i),seoux(i),seouy(i),rouxuy(i)
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
            do j=1,nfoc(i)
                  read(1,*) codut(j,i),codun(j,i),oduc(j,i),
     1                  seoduc(j,i)
            end do
            if (nfoc(i).eq.2) then
                  read(1,*) rfo12(i)
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
            do j=1,neoc(i)
                  read(1,*) coexx(j,i),coeyy(j,i),coexy(j,i),
     1                  oec(j,i),seoec(j,i)
            end do
            if (neoc(i).eq.2) then
                  read(1,*) reo12(i)
            end if
            if (neoc(i).eq.3) then
                  read(1,*) reo12(i),reo13(i),reo23(i)
            end if
            do j=1,3
                  read(1,*) p,eolong(j,i),eolat(j,i)
            end do
            eolong(4,i)=eolong(1,i)
            eolat(4,i)=eolat(1,i)
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
            do j=1,nducp(i)
                  read(1,*) pduc(j,i),cdut(j,i),cdun(j,i),
     1                  refduc(j,i),scduc(j,i)
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
            if (necp(i)+1.gt.maxpl) then
                  write(*,*) 'i,necp(i),maxpl=',i,necp(i),maxpl
                  stop 'Recompile with an increased maxpl value'
            end if
            nl=nl+1
            if (nl.gt.maxl) then
                  write(*,*) 'nl,maxl=',l,maxl
                  stop 'Recompile with an increased maxl value'
            end if
            do j=1,necp(i)
                  read(1,*) p,eclong(j,i),eclat(j,i)
                  read(1,*) cexx(j,i),ceyy(j,i),cexy(j,i),
     1                  refec(j,i),scec(j,i)
            end do
            npec(i)=necp(i)+1
            eclong(npec(i),i)=eclong(1,i)
            eclat(npec(i),i)=eclat(1,i)
            cexx(npec(i),i)=cexx(1,i)
            ceyy(npec(i),i)=ceyy(1,i)
            cexy(npec(i),i)=cexy(1,i)
            refec(npec(i),i)=refec(1,i)
            scec(npec(i),i)=scec(1,i)
      end do
      end if
c
      close(1)
c
      open(1,file='post_gmsh_data.dat')
c
c Control points
c
      read(1,*) ncp
      do i=1,ncp
            read(1,*) cp,cplong(i),cplat(i)
      end do
c
c Number of lines
c
      read(1,*) nl
      do i=1,nl
            read(1,*) l,cp0(i),cp1(i)
      end do
c
c Velocity lines
c
      read(1,*) vl
      if (vl.ne.nvl) stop 'Input files do not match'
      if (nvl.gt.0) then
      do i=1,nvl
            read(1,*) vl,nlvl(i)
            do j=1,nlvl(i)
                  read(1,*) lvl(j,i),cp0vl(j,i),cp1vl(j,i)
            end do
      end do
      end if
c
c Rigid boundaries
c
      read(1,*) rb
      if (rb.ne.nrb) stop 'Input files do not match'
      if (nrb.gt.0) then
      do i=1,nrb
            read(1,*) rb,nlrb(i)
            do j=1,nlrb(i)
                  read(1,*) lrb(j,i),cp0rb(j,i),cp1rb(j,i)
            end do
      end do
      end if
c
c Faults
c
      read(1,*) f
      if (f.ne.nf) stop 'Input files do not match'
      if (nf.gt.0) then
      do i=1,nf
            read(1,*) f,nlf(i)
            do j=1,nlf(i)
                  read(1,*) lf(j,i),cp0f(j,i),cp1f(j,i)
            end do
      end do
      end if
c
c Strain-rate observations
c
      read(1,*) eo
      if (eo.ne.neo) stop 'Input files do not match'
      if (neo.gt.0) then
      do i=1,neo
            read(1,*) eo,nleo(i)
            do j=1,nleo(i)
                  read(1,*) leo(j,i),cp0eo(j,i),cp1eo(j,i)
            end do
      end do
      end if
c
c Strain-rate correlations
c
      read(1,*) ec
      if (ec.ne.nec) stop 'Input files do not match'
      if (nec.gt.0) then
      do i=1,nec
            read(1,*) ec,nlec(i)
            do j=1,nlec(i)
                  read(1,*) lec(j,i),cp0ec(j,i),cp1ec(j,i)
            end do
      end do
      end if
c
c External boundaries
c
      read(1,*) neb
      do i=1,neb
            read(1,*) eb,npeb(i)
            if (npeb(i).gt.maxpr) then
                  write(*,*) 'i,npeb,maxpr=',i,npeb(i),maxpr
                  stop 'Recompile with an increased maxpr value'
            end if
            do j=1,npeb(i)
                  read(1,*) p,eblong(j,i),eblat(j,i)
            end do
      end do
c
      close(1)
c
      open(2,file='gmsh_geometry_for_setup.msh')
c
c Read in gmsh control information
c
      read(2,*)
      read(2,*)
      read(2,*)
      read(2,*)
      read(2,*) ngn
      nr=ngn-ncp-nl
      do i=1,ncp
            read(2,*) dimen,gncp(i)
            if (dimen.ne.0) stop 'Control point dimension must be 0'
      end do
      do i=1,nl
            read(2,*) dimen,gnl(i)
            if (dimen.ne.1) stop 'Line dimension must be 1'
      end do
      do i=1,nr
            read(2,*) dimen
            if (dimen.ne.2) stop 'Region dimension must be 2'
      end do
c
c Read in grid points and their locations
c
      read(2,*)
      read(2,*)
      read(2,*) ngp
      if (ngp.gt.maxgp) then
            write(*,*) 'ngp,maxgp=',ngp,maxgp
            stop 'Recompile with an increased maxgp value'
      end if
      do i=1,ngp
            read(2,*) gp,long(i),lat(i)
            if (gp.ne.i) stop 'The gmsh msh file is corrupt'
      end do
c
c Read in the grid points for control points, lines and elements
c
      read(2,*)
      read(2,*)
      read(2,*) nge
      do i=1,ncp
            read(2,*) ge,dum1,dum2,cpn,cp,gpcp(i)
            if (ge.ne.i) stop 'The gmsh msh file is corrupt'
            if (dum1.ne.15) stop 'Control point must be a point'
            if (dum2.ne.2) stop 'Spatial type must be a plane'
            if (cpn.ne.gncp(i)) stop 'The gmsh msh file is weird'
            if (cp.ne.i) stop 'The gmsh msh file is weird'
            if ((long(gpcp(i)).ne.cplong(i)).or.
     1            (lat(gpcp(i)).ne.cplat(i)))
     2            stop 'Control point position does not match'
      end do
      do i=1,nl
            nsl(i)=0
10          gem=ge
            read(2,*) ge,dum1,dum2,ln,l,gp1,gp2
            if (ge.ne.gem+1) stop 'The gmsh msh file is corrupt'
            if (dum1.ne.1) stop 'Line must be a line'
            if (dum2.ne.2) stop 'Spatial type must be a plane'
            if (ln.ne.gnl(i)) stop 'The gmsh msh file is weird'
            nsl(i)=nsl(i)+1
            if (nsl(i).gt.maxsl) then
                  write(*,*) 'i,nsl(i),maxsl=',i,nsl(i),maxsl
                  stop 'Recompile with an increased maxsl value'
            end if
            if (nsl(i).eq.1) then
                  if (gp1.ne.gpcp(cp0(i)))
     1                  stop 'Control point on line does not match'
                  gpl(0,i)=gp1
                  gpl(1,i)=gp2
            else
                  if (gp1.ne.gpl(nsl(i)-1,i))
     1                  stop 'Point on line does not match'
                  gpl(nsl(i),i)=gp2
            end if
            if (gp2.ne.gpcp(cp1(i))) goto 10
      end do
      ne=nge-ge
      if (ne.gt.maxe) then
            write(*,*) 'ne,maxe=',ne,maxe
            stop 'Recompile with an increased maxe value'
      end if
      do i=1,ne
            gem=ge
            read(2,*) ge,dum1,dum2,rn,r,gp1e(i),gp2e(i),gp3e(i)
            if (ge.ne.gem+1) stop 'The gmsh msh file is corrupt'
            if (dum1.ne.2) stop 'Element must be an element'
            if (dum2.ne.2) stop 'Spatial type must be a plane'
            gp1=gp1e(i)
            gp2=gp2e(i)
            gp3=gp3e(i)
            elong(i)=(long(gp1)+long(gp2)+long(gp3))/3.0d0
            elat(i)=(lat(gp1)+lat(gp2)+lat(gp3))/3.0d0
      end do
c
c Create element sides and associated arrays
c
      s1e(1)=1
      gp1s(1)=gp2e(1)
      gp2s(1)=gp3e(1)
      s2e(1)=2
      gp1s(2)=gp3e(1)
      gp2s(2)=gp1e(1)
      s3e(1)=3
      gp1s(3)=gp1e(1)
      gp2s(3)=gp2e(1)
      ns=3
      do i=2,ne
            do s=1,ns
                  if (((gp1s(s).eq.gp2e(i)).and.
     1                  (gp2s(s).eq.gp3e(i))).or.
     2                  ((gp1s(s).eq.gp3e(i)).and.
     3                  (gp2s(s).eq.gp2e(i)))) then
                        s1e(i)=s
                        goto 110
                  end if
            end do
            ns=ns+1
            if (ns.gt.maxs) then
                  write(*,*) 'ns,maxs=',ns,maxs
                  stop 'Recompile with an increased maxs value'
            end if
            s1e(i)=ns
            gp1s(ns)=gp2e(i)
            gp2s(ns)=gp3e(i)
110         continue
            do s=1,ns
                  if (((gp1s(s).eq.gp3e(i)).and.
     1                  (gp2s(s).eq.gp1e(i))).or.
     2                  ((gp1s(s).eq.gp1e(i)).and.
     3                  (gp2s(s).eq.gp3e(i)))) then
                        s2e(i)=s
                        goto 120
                  end if
            end do
            ns=ns+1
            if (ns.gt.maxs) then
                  write(*,*) 'ns,maxs=',ns,maxs
                  stop 'Recompile with an increased maxs value'
            end if
            s2e(i)=ns
            gp1s(ns)=gp3e(i)
            gp2s(ns)=gp1e(i)
120         continue
            do s=1,ns
                  if (((gp1s(s).eq.gp1e(i)).and.
     1                  (gp2s(s).eq.gp2e(i))).or.
     2                  ((gp1s(s).eq.gp2e(i)).and.
     3                  (gp2s(s).eq.gp1e(i)))) then
                        s3e(i)=s
                        goto 130
                  end if
            end do
            ns=ns+1
            if (ns.gt.maxs) then
                  write(*,*) 'ns,maxs=',ns,maxs
                  stop 'Recompile with an increased maxs value'
            end if
            s3e(i)=ns
            gp1s(ns)=gp1e(i)
            gp2s(ns)=gp2e(i)
130         continue
      end do
c
c Faults
c
      if (nf.gt.0) then
      do i=1,nf
            nfs(i)=0
            do j=1,nlf(i)
                  l=lf(j,i)
                  if (j.eq.1) then
                        if (l.gt.0) then
                              do k=0,nsl(l)
                                    gpf(k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              do k=0,nsl(l)
                                    gpf(k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  else
                        if (l.gt.0) then
                              if (gpl(0,l).ne.gpf(nfs(i),i)) then
                                    write(*,*) 'i=',i
                        stop 'Grid points on fault do not match'
                              end if
                              if (nfs(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nfs,nsl,maxsl=',
     1                                    i,nfs(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpf(nfs(i)+k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              if (gpl(nsl(l),l).ne.gpf(nfs(i),i)) then
                                    write(*,*) 'i=',i
                        stop 'Grid points on fault do not match'
                              end if
                              if (nfs(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nfs,nsl,maxsl=',
     1                                    i,nfs(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpf(nfs(i)+k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  end if
                  nfs(i)=nfs(i)+nsl(l)
            end do
            do j=1,nfs(i)
                  sf(j,i)=finds(ns,gp1s,gp2s,gpf(j-1,i),gpf(j,i))
            end do
            do j=1,npf(i)
                  gp=findgp(ngp,long,lat,flong(j,i),flat(j,i))
                  if (j.eq.1) then
                        if (gp.ne.gpf(0,i)) then
                              write(*,*) 'i=',i
                        stop 'Grid points on fault do not match'
                        end if
                        pfpf(j,i)=0
                  end if
                  if ((j.gt.1).and.(j.lt.npf(i))) then
                        kmin=pfpf(j-1,i)+1
                        kmax=nfs(i)-1
                        do k=kmin,kmax
                              if (gp.eq.gpf(k,i)) goto 210
                        end do
                        write(*,*) 'i=',i
                        stop 'Grid points on fault do not match'
210                     pfpf(j,i)=k
                  end if
                  if (j.eq.npf(i)) then
                        if (gp.ne.gpf(nfs(i),i)) then
                              write(*,*) 'i=',i
                        stop 'Grid points on fault do not match'
                        end if
                        pfpf(j,i)=nfs(i)
                  end if
            end do
            lenl(0)=0.0d0
            do j=1,nfs(i)
                  dx=long(gpf(j,i))-long(gpf(j-1,i))
                  dy=lat(gpf(j,i))-lat(gpf(j-1,i))
                  lenl(j)=lenl(j-1)+dsqrt(dx**2+dy**2)
            end do
            Kcp(0)=Kc(1,i)
            Ksp(0)=Ks(1,i)
            do j=2,npf(i)
                  k0=pfpf(j-1,i)
                  k1=pfpf(j,i)
                  kmin=k0+1
                  kmax=k1
                  do k=kmin,kmax
                        p0=(lenl(k1)-lenl(k))
     1                        /(lenl(k1)-lenl(k0))
                        Kcp(k)=Kc(j,i)+p0*(Kc(j-1,i)-Kc(j,i))
                        Ksp(k)=Ks(j,i)+p0*(Ks(j-1,i)-Ks(j,i))
                  end do
            end do
            do j=1,nfs(i)
                  Kc(j,i)=0.5*(Kcp(j-1)+Kcp(j))
                  Ks(j,i)=0.5*(Ksp(j-1)+Ksp(j))
            end do
      end do
      end if
c
c Specifications of strain-rate capacity at faults
c
      if (nfL.gt.0) then
      do i=1,nfL
            f=ffL(i)
            do j=1,nfLp(i)
                  pfL(j,i)=pfpf(pfL(j,i),f)
            end do
            lenl(0)=0.0d0
            do j=1,nfs(f)
                  dx=long(gpf(j,f))-long(gpf(j-1,f))
                  dy=lat(gpf(j,f))-lat(gpf(j-1,f))
                  lenl(j)=lenl(j-1)+dsqrt(dx**2+dy**2)
            end do
            nj=nfLp(i)
            do j=nfLp(i),2,-1
                  newj=pfL(j,i)-pfL(j-1,i)-1
                  if (newj.ne.0) then
                        do k=nj,j,-1
                              pfL(k+newj,i)=pfL(k,i)
                              yfLm(k+newj,i)=yfLm(k,i)
                              yfLp(k+newj,i)=yfLp(k,i)
                              fLcm(k+newj,i)=fLcm(k,i)
                              fafLcm(k+newj,i)=fafLcm(k,i)
                              azfLcm(k+newj,i)=azfLcm(k,i)
                              fLsm(k+newj,i)=fLsm(k,i)
                              fafLsm(k+newj,i)=fafLsm(k,i)
                              azfLsm(k+newj,i)=azfLsm(k,i)
                              fLcp(k+newj,i)=fLcp(k,i)
                              fafLcp(k+newj,i)=fafLcp(k,i)
                              azfLcp(k+newj,i)=azfLcp(k,i)
                              fLsp(k+newj,i)=fLsp(k,i)
                              fafLsp(k+newj,i)=fafLsp(k,i)
                              azfLsp(k+newj,i)=azfLsp(k,i)
                        end do
                        j0=j-1
                        j1=j+newj
                        k0=pfL(j0,i)
                        k1=pfL(j1,i)
                        do k=1,newj
                              p0=(lenl(k1)-lenl(k0+k))
     1                              /(lenl(k1)-lenl(k0))
                              pfL(j0+k,i)=k0+k
                              yfLm(j0+k,i)=yfLm(j1,i)
     1                              +p0*(yfLm(j0,i)-yfLm(j1,i))
                              yfLp(j0+k,i)=yfLp(j1,i)
     1                              +p0*(yfLp(j0,i)-yfLp(j1,i))
                              fLcm(j0+k,i)=fLcm(j1,i)
     1                              +p0*(fLcm(j0,i)-fLcm(j1,i))
                              fafLcm(j0+k,i)=fafLcm(j1,i)
     1                              +p0*(fafLcm(j0,i)-fafLcm(j1,i))
                              azfLcm(j0+k,i)=azfLcm(j1,i)
     1                              +p0*(azfLcm(j0,i)-azfLcm(j1,i))
                              fLsm(j0+k,i)=fLsm(j1,i)
     1                              +p0*(fLsm(j0,i)-fLsm(j1,i))
                              fafLsm(j0+k,i)=fafLsm(j1,i)
     1                              +p0*(fafLsm(j0,i)-fafLsm(j1,i))
                              azfLsm(j0+k,i)=azfLsm(j1,i)
     1                              +p0*(azfLsm(j0,i)-azfLsm(j1,i))
                              fLcp(j0+k,i)=fLcp(j1,i)
     1                              +p0*(fLcp(j0,i)-fLcp(j1,i))
                              fafLcp(j0+k,i)=fafLcp(j1,i)
     1                              +p0*(fafLcp(j0,i)-fafLcp(j1,i))
                              azfLcp(j0+k,i)=azfLcp(j1,i)
     1                              +p0*(azfLcp(j0,i)-azfLcp(j1,i))
                              fLsp(j0+k,i)=fLsp(j1,i)
     1                              +p0*(fLsp(j0,i)-fLsp(j1,i))
                              fafLsp(j0+k,i)=fafLsp(j1,i)
     1                              +p0*(fafLsp(j0,i)-fafLsp(j1,i))
                              azfLsp(j0+k,i)=azfLsp(j1,i)
     1                              +p0*(azfLsp(j0,i)-azfLsp(j1,i))
                        end do
                        nj=nj+newj
                  end if
            end do
            nfLp(i)=nj
      end do
      end if
c
c Average values of slip rate on fault segments
c
      if (nfo.gt.0) then
      fo=nfo
      do i=nfo,1,-1
            f=ffo(i)
            p1fo(i)=pfpf(p1fo(i),f)
            p2fo(i)=pfpf(p2fo(i),f)
            newfo=p2fo(i)-1-p1fo(i)
            if (fo+newfo.gt.maxfo) then
                  write(*,*) 'fo,newfo,maxfo=',fo,newfo,maxfo
                  stop 'Recompile with an increased maxfo value'
            end if
            if ((newfo.ne.0).and.(i.lt.fo)) then
                  kmin=i+1
                  kmax=fo
                  do k=kmax,kmin,-1
                        ffo(k+newfo)=ffo(k)
                        p1fo(k+newfo)=p1fo(k)
                        p2fo(k+newfo)=p2fo(k)
                        nfoc(k+newfo)=nfoc(k)
                        do j=1,nfoc(k)
                              codut(j,k+newfo)=codut(j,k)
                              codun(j,k+newfo)=codun(j,k)
                              oduc(j,k+newfo)=oduc(j,k)
                              seoduc(j,k+newfo)=seoduc(j,k)
                        end do
                        if (nfoc(k).eq.2)
     1                        rfo12(k+newfo)=rfo12(k)
                  end do
            end if
            if (newfo.ne.0) then
                  p2fo(i)=p1fo(i)+1
                  do k=1,newfo
                        ffo(i+k)=ffo(i)
                        p1fo(i+k)=p1fo(i)+k
                        p2fo(i+k)=p2fo(i)+k
                        nfoc(i+k)=nfoc(i)
                        do j=1,nfoc(i)
                              codut(j,i+k)=codut(j,i)
                              codun(j,i+k)=codun(j,i)
                              oduc(j,i+k)=oduc(j,i)
                              seoduc(j,i+k)=seoduc(j,i)
                        end do
                        if (nfoc(i).eq.2)
     1                        rfo12(i+k)=rfo12(i)
                  end do
                  fo=fo+newfo
            end if
      end do
      nfo=fo
      do i=1,nfo
            sfo(i)=sf(p2fo(i),ffo(i))
      end do
      end if

c
c Differences between components of slip rate on fault segments
c
      if (nduc.gt.0) then
      do i=1,nduc
            f=fduc(i)
            do j=1,nducp(i)
                  pduc(j,i)=pfpf(pduc(j,i),f)
            end do
            lenl(0)=0.0d0
            do j=1,nfs(f)
                  dx=long(gpf(j,f))-long(gpf(j-1,f))
                  dy=lat(gpf(j,f))-lat(gpf(j-1,f))
                  lenl(j)=lenl(j-1)+dsqrt(dx**2+dy**2)
            end do
            nj=nducp(i)
            do j=nducp(i),2,-1
                  newj=pduc(j,i)-pduc(j-1,i)-1
                  if (newj.ne.0) then
                        do k=nj,j,-1
                              pduc(k+newj,i)=pduc(k,i)
                              cdut(k+newj,i)=cdut(k,i)
                              cdun(k+newj,i)=cdun(k,i)
                              refduc(k+newj,i)=refduc(k,i)
                              scduc(k+newj,i)=scduc(k,i)
                        end do
                        j0=j-1
                        j1=j+newj
                        k0=pduc(j0,i)
                        k1=pduc(j1,i)
                        do k=1,newj
                              p0=(lenl(k1)-lenl(k0+k))
     1                              /(lenl(k1)-lenl(k0))
                              pduc(j0+k,i)=k0+k
                              cdut(j0+k,i)=cdut(j1,i)
     1                              +p0*(cdut(j0,i)-cdut(j1,i))
                              cdun(j0+k,i)=cdun(j1,i)
     1                              +p0*(cdun(j0,i)-cdun(j1,i))
                              refduc(j0+k,i)=refduc(j1,i)
     1                              +p0*(refduc(j0,i)-refduc(j1,i))
                              scduc(j0+k,i)=scduc(j1,i)
     1                              +p0*(scduc(j0,i)-scduc(j1,i))
                        end do
                        nj=nj+newj
                  end if
            end do
            nducp(i)=nj
            nducs(i)=nj-1
            do j=1,nducs(i)
                  sduc(j,i)=sf(pduc(j+1,i),f)
                  cdut(j,i)=0.5d0*(cdut(j,i)+cdut(j+1,i))
                  cdun(j,i)=0.5d0*(cdun(j,i)+cdun(j+1,i))
                  refduc(j,i)=0.5d0*(refduc(j,i)+refduc(j+1,i))
                  scduc(j,i)=0.5d0*(scduc(j,i)+scduc(j+1,i))
            end do
      end do
      end if
c
c Velocity lines
c
      if (nvl.gt.0) then
      do i=1,nvl
            nvls(i)=0
            do j=1,nlvl(i)
                  l=lvl(j,i)
                  if (j.eq.1) then
                        if (l.gt.0) then
                              do k=0,nsl(l)
                                    gpvl(k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              do k=0,nsl(l)
                                    gpvl(k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  else
                        if (l.gt.0) then
                              if (gpl(0,l).ne.gpvl(nvls(i),i))
     1                  stop 'Grid points on v-line do not match'
                              if (nvls(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nvls,nsl,maxsl=',
     1                                    i,nvls(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpvl(nvls(i)+k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              if (gpl(nsl(l),l).ne.gpvl(nvls(i),i))
     1                  stop 'Grid points on v-line do not match'
                              if (nvls(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nvls,nsl,maxsl=',
     1                                    i,nvls(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpvl(nvls(i)+k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  end if
                  nvls(i)=nvls(i)+nsl(l)
                  nvlgp(i)=nvls(i)+1-nvlf(i)
            end do
            do j=1,nvls(i)
                  svl(j,i)=finds(ns,gp1s,gp2s,gpvl(j-1,i),gpvl(j,i))
            end do
            do j=1,npvl(i)
                  gp=findgp(ngp,long,lat,vllong(j,i),vllat(j,i))
                  if (j.eq.1) then
                        if (gp.ne.gpvl(0,i))
     1                  stop 'Grid points on v-line do not match'
                        pvlpvl(j,i)=0
                  end if
                  if ((j.gt.1).and.(j.lt.npvl(i))) then
                        kmin=pvlpvl(j-1,i)+1
                        kmax=nvls(i)-1
                        do k=kmin,kmax
                              if (gp.eq.gpvl(k,i)) goto 220
                        end do
                        stop 'Grid points on v-line do not match'
220                     pvlpvl(j,i)=k
                  end if
                  if (j.eq.npvl(i)) then
                        if (gp.ne.gpvl(nvls(i),i))
     1                  stop 'Grid points on v-line do not match'
                        pvlpvl(j,i)=nvls(i)
                  end if
            end do
      end do
      end if
c
c Rigid boundaries
c
      if (nrb.gt.0) then
      do i=1,nrb
            nrbs(i)=0
            do j=1,nlrb(i)
                  l=lrb(j,i)
                  if (j.eq.1) then
                        if (l.gt.0) then
                              do k=0,nsl(l)
                                    gprb(k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              do k=0,nsl(l)
                                    gprb(k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  else
                        if (l.gt.0) then
                              if (gpl(0,l).ne.gprb(nrbs(i),i))
     1                  stop 'Grid points on rigid-b do not match'
                              if (nrbs(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nrbs,nsl,maxsl=',
     1                                    i,nrbs(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gprb(nrbs(i)+k,i)=gpl(k,l)
                              end do
                        else
                              l=-l
                              if (gpl(nsl(l),l).ne.gprb(nrbs(i),i))
     1                  stop 'Grid points on rigid-b do not match'
                              if (nrbs(i)+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,nrbs,nsl,maxsl=',
     1                                    i,nrbs(i),nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gprb(nrbs(i)+k,i)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  end if
                  nrbs(i)=nrbs(i)+nsl(l)
            end do
            do j=1,nrbs(i)
                  srb(j,i)=finds(ns,gp1s,gp2s,gprb(j-1,i),gprb(j,i))
            end do
            do j=1,nprb(i)
                  gp=findgp(ngp,long,lat,rblong(j,i),rblat(j,i))
                  if (j.eq.1) then
                        if (gp.ne.gprb(0,i))
     1                  stop 'Grid points on rigid-b do not match'
                        kmax=0
                  end if
                  if ((j.gt.1).and.(j.lt.nprb(i))) then
                        kmin=kmax+1
                        kmax=nrbs(i)-1
                        do k=kmin,kmax
                              if (gp.eq.gprb(k,i)) goto 230
                        end do
                        stop 'Grid points on rigid-b do not match'
230                     kmax=k
                  end if
                  if (j.eq.nprb(i)) then
                        if (gp.ne.gprb(nrbs(i),i))
     1                  stop 'Grid points on rigid-b do not match'
                  end if
            end do
      end do
      end if
c
c Average values of strain rate in elements
c
      if (neo.gt.0) then
      eo=neo
      do i=neo,1,-1
            neweo=0
            do e=1,ne
                  if (pinr(4,eolong(1,i),eolat(1,i),
     1                  elong(e),elat(e))) then
                        neweo=neweo+1
                        if (neweo.gt.1) then
                              if (eo+1.gt.maxeo) then
                                    write(*,*) 'eo+1,maxeo=',
     1                                    eo+1,maxeo
                  stop 'Recompile with an increased maxeo value'
                              end if
                              do k=eo,i,-1
                                    eeo(k+1)=eeo(k)
                                    neoc(k+1)=neoc(k)
                                    do j=1,neoc(k)
                                          coexx(j,k+1)=coexx(j,k)
                                          coeyy(j,k+1)=coeyy(j,k)
                                          coexy(j,k+1)=coexy(j,k)
                                          oec(j,k+1)=oec(j,k)
                                          seoec(j,k+1)=seoec(j,k)
                                    end do
                                    if (neoc(k).ge.2)
     1                                    reo12(k+1)=reo12(k)
                                    if (neoc(k).eq.3)
     1                                    reo13(k+1)=reo13(k)
                                    if (neoc(k).eq.3)
     1                                    reo23(k+1)=reo23(k)
                              end do
                              eo=eo+1
                        end if
                        eeo(i)=e
                  end if
            end do
            if (neweo.eq.0)
     1      stop 'Failed to find element for strain-rate observation'
      end do
      neo=eo
      end if
c
c Differences between components of strain rate in elements
c
      if (nec.gt.0) then
      do i=1,nec
            necs=0
            do j=1,nlec(i)
                  l=lec(j,i)
                  if (j.eq.1) then
                        if (l.gt.0) then
                              do k=0,nsl(l)
                                    gpec(k)=gpl(k,l)
                              end do
                        else
                              l=-l
                              do k=0,nsl(l)
                                    gpec(k)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  else
                        if (l.gt.0) then
                              if (gpl(0,l).ne.gpec(necs))
     1      stop 'Grid points for strain-rate correlation do not match'
                              if (necs+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,necs,nsl,maxsl=',
     1                                    i,necs,nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpec(necs+k)=gpl(k,l)
                              end do
                        else
                              l=-l
                              if (gpl(nsl(l),l).ne.gpec(necs))
     1      stop 'Grid points for strain-rate correlation do not match'
                              if (necs+nsl(l).gt.maxsl) then
                                    write(*,*) 'i,necs,nsl,maxsl=',
     1                                    i,necs,nsl(l),maxsl
                  stop 'Recompile with an increased maxsl value'
                              end if
                              do k=1,nsl(l)
                                    gpec(necs+k)=gpl(nsl(l)-k,l)
                              end do
                        end if
                  end if
                  necs=necs+nsl(l)
            end do
            do j=1,npec(i)
                  gp=findgp(ngp,long,lat,eclong(j,i),eclat(j,i))
                  if (j.eq.1) then
                        if (gp.ne.gpec(0))
     1      stop 'Grid points for strain-rate correlation do not match'
                        pecpec(j)=0
                  end if
                  if ((j.gt.1).and.(j.lt.npec(i))) then
                        kmin=pecpec(j-1)+1
                        kmax=necs-1
                        do k=kmin,kmax
                              if (gp.eq.gpec(k)) goto 240
                        end do
            stop 'Grid points for strain-rate correlation do not match'
240                     pecpec(j)=k
                  end if
                  if (j.eq.npec(i)) then
                        if (gp.ne.gpec(necs))
     1      stop 'Grid points for strain-rate correlation do not match'
                        pecpec(j)=necs
                  end if
            end do
            lenl(0)=0.0d0
            do j=1,necs
                  dx=long(gpec(j))-long(gpec(j-1))
                  dy=lat(gpec(j))-lat(gpec(j-1))
                  lenl(j)=lenl(j-1)+dsqrt(dx**2+dy**2)
            end do
            cexxp(0)=cexx(1,i)
            ceyyp(0)=ceyy(1,i)
            cexyp(0)=cexy(1,i)
            refecp(0)=refec(1,i)
            scecp(0)=scec(1,i)
            do j=2,npec(i)
                  k0=pecpec(j-1)
                  k1=pecpec(j)
                  kmin=k0+1
                  kmax=k1
                  do k=kmin,kmax
                        p0=(lenl(k1)-lenl(k))
     1                        /(lenl(k1)-lenl(k0))
                        cexxp(k)=cexx(j,i)
     1                        +p0*(cexx(j-1,i)-cexx(j,i))
                        ceyyp(k)=ceyy(j,i)
     1                        +p0*(ceyy(j-1,i)-ceyy(j,i))
                        cexyp(k)=cexy(j,i)
     1                        +p0*(cexy(j-1,i)-cexy(j,i))
                        refecp(k)=refec(j,i)
     1                        +p0*(refec(j-1,i)-refec(j,i))
                        scecp(k)=scec(j,i)
     1                        +p0*(scec(j-1,i)-scec(j,i))
                  end do
            end do
            necp(i)=necs+1
            do j=1,necp(i)
                  eclong(j,i)=long(gpec(j-1))
                  eclat(j,i)=lat(gpec(j-1))
            end do
            nece(i)=0
            do e=1,ne
                  if (pinr(necp(i),eclong(1,i),eclat(1,i),
     1                  elong(e),elat(e))) then
                        nece(i)=nece(i)+1
                        if (nece(i).gt.maxpl) then
                              write(*,*) 'i,nece(i),maxpl=',
     1                              i,nece(i),maxpl
                  stop 'Recompile with an increased maxpl value'
                        end if
                        eec(nece(i),i)=e
                  end if
            end do
            if (nece(i).lt.2)
     1      stop 'Failed to find elements for strain-rate correlation'
            do j=1,nece(i)
                  e=eec(j,i)
                  call getec(necs,eclong(1,i),eclat(1,i),cexxp,ceyyp,
     1                  cexyp,refecp,scecp,elong(e),elat(e),cexx(j,i),
     2                  ceyy(j,i),cexy(j,i),refec(j,i),scec(j,i))
            end do
      end do
      end if
c
      lun=1
c
      open(lun,file='stress_potentials.dat')
      write(*,*) 'Stress potentials are being determined'
      call getpot(lun,ngp,long,lat,sxxpot,syypot,sxypot,neb,npeb,
     1      eblong,eblat)
      close(lun)
c
      open(lun,file='strain-rate_capacities.dat')
      write(*,*) 'Strain-rate capacities are being determined'
      call getL(lun,ne,elong,elat,Lc,Lcc,Lcs,Ls,Lsc,Lss,neb,npeb,
     1      eblong,eblat)
      close(lun)
c
      if (nfL.gt.0) then
            call getfL(rE,ne,elong,elat,Lc,Lcc,Lcs,Ls,Lsc,Lss,nfL,
     1            ffL,nflp,pfL,gpf,long,lat,yfLm,yfLp,fLcm,fLsm,
     2            fLcp,fLsp,fafLcm,fafLsm,fafLcp,fafLsp,
     3            azfLcm,azfLsm,azfLcp,azfLsp)
      end if
c
      open(1,file='setup_input.dat')
c
c Read the documentation in 'setup' for specification of the input.
c
c Geometry input and apriori model specification:
c
c Radius of the Earth:
      write(1,*) rE,'    radius of the Earth'
      write(1,*)
c
c Grid points:
      write(1,*) ngp,'    number of grid points'
      do i=1,ngp
            write(1,*) i,long(i),lat(i)
            write(1,*) sxxpot(i),syypot(i),sxypot(i)
      end do
      write(1,*)
c
c Sides of elements:
      write(1,*) ns,'    number of sides'
      do i=1,ns
            write(1,*) i,gp1s(i),gp2s(i)
      end do
      write(1,*)
c
c Elements:
      write(1,*) ne,'    number of elements'
      do i=1,ne
            write(1,*) i,s1e(i),s2e(i),s3e(i)
            write(1,*) Lc(i),Lcc(i),Lcs(i),Ls(i),Lsc(i),Lss(i)
      end do
      write(1,*)
c
c Faults:
      write(1,*) nf,'    number of faults'
      if (nf.gt.0) then
      do i=1,nf
            write(1,*) i,nfs(i)
            do j=1,nfs(i)
                  write(1,*) sf(j,i),Kc(j,i),Ks(j,i)
            end do
      end do
      end if
      write(1,*)
c
c Lines where velocities are input:
      write(1,*) nvl,'    number of velocity lines'
      if (nvl.gt.0) then
      do i=1,nvl
            write(1,*) i,nvlgp(i),nvls(i),nvlf(i)
            write(1,*) vptype(1,i),gpvl(0,i)
            if (vptype(1,i).eq.1) then
                  write(1,*) ux(1,i),uy(1,i)
            end if
            if (vptype(1,i).eq.3) then
                  write(1,*) fvl(1,i),uxm(1,i),uym(1,i),
     1                  uxp(1,i),uyp(1,i)
            end if
            if (vptype(1,i).eq.-3) then
                  write(1,*) fvl(1,i)
            end if
            k=2
            p=pvlpvl(k,i)
            do j=1,nvls(i)
                  write(1,*) -2,svl(j,i)
                  if (j.eq.p) then
                        write(1,*) vptype(k,i),gpvl(j,i)
                        if (vptype(k,i).eq.1) then
                              write(1,*) ux(k,i),uy(k,i)
                        end if
                        if (vptype(k,i).eq.3) then
                              write(1,*) fvl(k,i),uxm(k,i),uym(k,i),
     1                              uxp(k,i),uyp(k,i)
                        end if
                        if (vptype(k,i).eq.-3) then
                              write(1,*) fvl(k,i)
                        end if
                        k=k+1
                        p=pvlpvl(k,i)
                  else
                        write(1,*) -1,gpvl(j,i)
                  end if
            end do
      end do
      end if
      write(1,*)
c
c Rigid boundary lines:
      write(1,*) nrb,'    number of rigid boundaries'
      if (nrb.gt.0) then
      do i=1,nrb
            write(1,*) i,nrbs(i),plat(i),plong(i),prate(i)
            do j=1,nrbs(i)
                  write(1,*) srb(j,i)
            end do
      end do
      end if
      write(1,*)
c
c Observation input:
c
c Velocities at points:
      write(1,*) nvo,'    number of velocity observations'
      if (nvo.gt.0) then
      do i=1,nvo
            write(1,*) i,volong(i),volat(i)
            write(1,*) oux(i),ouy(i),seoux(i),seouy(i),rouxuy(i)
      end do
      end if
      write(1,*)
c
c Average values of slip rate on fault segments:
      write(1,*) nfo,'    number of fault slip-rate observations'
      if (nfo.gt.0) then
      do i=1,nfo
            write(1,*) i,ffo(i),sfo(i),nfoc(i)
            do j=1,nfoc(i)
                  write(1,*) codut(j,i),codun(j,i),oduc(j,i),
     1                  seoduc(j,i)
            end do
            if (nfoc(i).eq.2) then
                  write(1,*) rfo12(i)
            end if
      end do
      end if
      write(1,*)
c
c Average values of strain rate in elements:
      write(1,*) neo,'    number of strain-rate observations'
      if (neo.gt.0) then
      do i=1,neo
            write(1,*) i,eeo(i),neoc(i)
            do j=1,neoc(i)
                  write(1,*) coexx(j,i),coeyy(j,i),coexy(j,i),
     1                  oec(j,i),seoec(j,i)
            end do
            if (neoc(i).eq.2) then
                  write(1,*) reo12(i)
            end if
            if (neoc(i).eq.3) then
                  write(1,*) reo12(i),reo13(i),reo23(i)
            end if
      end do
      end if
      write(1,*)
c
c Differences between components of slip rate on fault segments:
      write(1,*) nduc,'    number of fault slip-rate correlations'
      if (nduc.gt.0) then
      do i=1,nduc
            write(1,*) i,fduc(i),nducs(i)
            do j=1,nducs(i)
                  write(1,*) sduc(j,i),cdut(j,i),cdun(j,i),
     1                  refduc(j,i),scduc(j,i)
            end do
      end do
      end if
      write(1,*)
c
c Differences between components of strain rate in elements:
      write(1,*) nec,'    number of strain-rate correlations'
      if (nec.gt.0) then
      do i=1,nec
            write(1,*) i,nece(i)
            do j=1,nece(i)
                  write(1,*) eec(j,i),cexx(j,i),ceyy(j,i),
     1                  cexy(j,i),refec(j,i),scec(j,i)
            end do
      end do
      end if
      write(1,*)
c
      close(1)
      close(2)
      return
      end
c
c
      FUNCTION finds(ns,gp1s,gp2s,gp1,gp2)
c
      implicit none
      integer maxgp,maxs
      parameter(maxgp=40000,maxs=3*maxgp)
      integer finds,ns,gp1,gp2,i
      integer gp1s(maxs),gp2s(maxs)
c
      do i=1,ns
            if (((gp1s(i).eq.gp1).and.(gp2s(i).eq.gp2)).or.
     1            ((gp2s(i).eq.gp1).and.(gp1s(i).eq.gp2))) goto 10
      end do
      write(*,*) 'gp1,gp2=',gp1,gp2
      stop 'Could not find side'
10    finds=i
      return
      end
c
c
      FUNCTION findgp(ngp,long,lat,gplong,gplat)
c
      implicit none
      integer maxgp
      parameter(maxgp=40000)
      integer findgp,ngp
      integer i
      real*8 gplong,gplat
      real*8 long(maxgp),lat(maxgp)
c
      do i=1,ngp
            if ((long(i).eq.gplong).and.(lat(i).eq.gplat)) goto 10
      end do
      write(*,*) 'gplong,gplat=',gplong,gplat
      stop 'Could not find grid point'
10    findgp=i
      return
      end
c
c
      SUBROUTINE getec(necs,eclong,eclat,cexxp,ceyyp,cexyp,refecp,
     1      scecp,elong,elat,cexx,ceyy,cexy,refec,scec)
c
      implicit none
      integer necs
      integer i
      real*8 w,wcexx,wceyy,wcexy,wrefec,wscec
      real*8 dx,dy,x1,tx,ty,x,y,wi,wd,p0,p1
      real*8 elong,elat,cexx,ceyy,cexy,refec,scec
      real*8 eclong(0:necs),eclat(0:necs),
     1      cexxp(0:necs),ceyyp(0:necs),cexyp(0:necs),
     2      refecp(0:necs),scecp(0:necs)
c
      w=0.0d0
      wcexx=0.0d0
      wceyy=0.0d0
      wcexy=0.0d0
      wrefec=0.0d0
      wscec=0.0d0
      do i=1,necs
            dx=eclong(i)-eclong(i-1)
            dy=eclat(i)-eclat(i-1)
            x1=dsqrt(dx**2+dy**2)
            tx=dx/x1
            ty=dy/x1
            x=tx*(elong-eclong(i-1))+ty*(elat-eclat(i-1))
            y=tx*(elat-eclat(i-1))-ty*(elong-eclong(i-1))
            if (dabs(y).lt.x1*1.0d-8) then
                  wi=-y**2*(1/(x1-x)**3+1/x**3)/3.0d0
            else
                  wi=0.5d0*((datan((x1-x)/y)+datan(x/y))/y
     1                  +(x1-x)/((x1-x)**2+y**2)
     2                  +x/(x**2+y**2))
            end if
            wd=0.50d0*(1/(x**2+y**2)-1/((x1-x)**2+y**2))*y**2
            p0=(x1-x)/x1
            p1=x/x1
            w=w+wi
            wcexx=wcexx+wi*(p0*cexxp(i-1)+p1*cexxp(i))
     1            +wd*(cexxp(i)-cexxp(i-1))/x1
            wceyy=wceyy+wi*(p0*ceyyp(i-1)+p1*ceyyp(i))
     1            +wd*(ceyyp(i)-ceyyp(i-1))/x1
            wcexy=wcexy+wi*(p0*cexyp(i-1)+p1*cexyp(i))
     1            +wd*(cexyp(i)-cexyp(i-1))/x1
            wrefec=wrefec+wi*(p0*refecp(i-1)+p1*refecp(i))
     1            +wd*(refecp(i)-refecp(i-1))/x1
            wscec=wscec+wi*(p0*scecp(i-1)+p1*scecp(i))
     1            +wd*(scecp(i)-scecp(i-1))/x1
      end do
      cexx=wcexx/w
      ceyy=wceyy/w
      cexy=wcexy/w
      refec=wrefec/w
      scec=wscec/w
c
      return
      end
c
c
      SUBROUTINE getpot(lun,ngp,long,lat,sxxpot,syypot,sxypot,
     1      neb,npeb,eblong,eblat)
c
      implicit none
      integer maxgp,maxeb,maxpr,maxdim
      parameter(maxgp=40000,maxeb=10,maxpr=1001,
     1      maxdim=1001)
      integer lun,ngp,neb
      integer nlong,nlat,i,p,j,nval,k,eb
      integer npeb(maxeb)
      logical needp,inside,pinr,pexr
      logical have(maxdim,maxdim),done(maxdim,maxdim)
      real*8 plong,plat,sxxp,syyp,sxyp,xm,xp,ym,yp
      real*8 dx,x0,x1,hx0,hx1,kx0,kx1,dy,y0,y1,hy0,hy1,ky0,ky1
      real*8 long(maxgp),lat(maxgp)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 eblong(maxpr,maxeb),eblat(maxpr,maxeb)
      real*8 xlong(maxdim),ylat(maxdim)
      real*8 sxx(maxdim,maxdim),syy(maxdim,maxdim),
     1      sxy(maxdim,maxdim)
      real*8 xsxx(maxdim,maxdim),xsyy(maxdim,maxdim),
     1      xsxy(maxdim,maxdim)
      real*8 ysxx(maxdim,maxdim),ysyy(maxdim,maxdim),
     1      ysxy(maxdim,maxdim)
c
      read(lun,*) nlong
      if (nlong.lt.3) then
            write(*,*) 'nlong=',nlong
            stop 'There must be at least 3 values of longitude'
      end if
      if (nlong.gt.maxdim) then
            write(*,*) 'nlong,maxdim=',nlong,maxdim
            stop 'Recompile with an increased maxdim value'
      end if
      do i=1,nlong
            read(lun,*) p,xlong(i)
      end do
      do i=2,nlong
            if (xlong(i).le.xlong(i-1)) then
                 write(*,*) 'i,xlong(i),xlong(i-1)=',
     1                 i,xlong(i),xlong(i-1)
            stop 'Longitudes must be in strictly increasing order'
            end if
      end do
      read(lun,*) nlat
      if (nlat.lt.3) then
            write(*,*) 'nlat=',nlat
            stop 'There must be at least 3 values of latitude'
      end if
      if (nlat.gt.maxdim) then
            write(*,*) 'nlat,maxdim=',nlat,maxdim
            stop 'Recompile with an increased maxdim value'
      end if
      do i=1,nlat
            read(lun,*) p,ylat(i)
      end do
      do i=2,nlat
            if (ylat(i).le.ylat(i-1)) then
                 write(*,*) 'i,ylat(i),ylat(i-1)=',
     1                 i,ylat(i),ylat(i-1)
            stop 'Latitudes must be in strictly increasing order'
            end if
      end do
c
      do j=1,nlat
      do i=1,nlong
            have(i,j)=.false.
      end do
      end do
      read(lun,*) nval
      do i=1,nval
            read(lun,*) p,plong,plat
            read(lun,*) sxxp,syyp,sxyp
            do j=1,nlong
                  if (xlong(j).eq.plong) goto 10
            end do
            write(*,*) 'p,plong,plat=',p,plong,plat
            stop 'Could not find plong among longitudes'
10          continue
            do k=1,nlat
                  if (ylat(k).eq.plat) goto 20
            end do
            write(*,*) 'p,plong,plat=',p,plong,plat
            stop 'Could not find plat among latitudes'
20          continue
            have(j,k)=.true.
            sxx(j,k)=sxxp
            syy(j,k)=syyp
            sxy(j,k)=sxyp
      end do
c
      needp=.false.
      do j=1,nlat
      do i=1,nlong
            inside=pinr(npeb(1),eblong(1,1),eblat(1,1),
     1                  xlong(i),ylat(j))
            if (neb.gt.1) then
                  do eb=2,neb
                        inside=inside.and.
     1                        pexr(npeb(eb),eblong(1,eb),
     1                        eblat(1,eb),xlong(i),ylat(j))
                  end do
            end if
            if (inside) then
                  if ((i.eq.1).or.(i.eq.nlong).or.
     1                  (j.eq.1).or.(j.eq.nlat)) then
            stop 'Edge points must be outside the modelling region'
                  else
                        if (.not.have(i-1,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i-1,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i-1,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j+1)
                              needp=.true.
                        end if
                        if (.not.have(i,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j+1)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j+1)
                              needp=.true.
                        end if
                  end if
            end if
            if ((i.eq.1).and.have(i,j).and.
     1            (.not.have(i+1,j))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i+1),ylat(j)
                  needp=.true.
            end if
            if ((i.eq.nlong).and.have(i,j).and.
     1            (.not.have(i-1,j))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i-1),ylat(j)
                  needp=.true.
            end if
            if ((i.gt.1).and.(i.lt.nlong).and.have(i,j).and.
     1            (.not.(have(i-1,j).or.have(i+1,j)))) then
                  write(*,*) 'Need values either at long,lat=',
     1                  xlong(i-1),ylat(j)
                  write(*,*) '  or at long,lat=',
     1                  xlong(i+1),ylat(j)
                  needp=.true.
            end if
            if ((j.eq.1).and.have(i,j).and.
     1            (.not.have(i,j+1))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i),ylat(j+1)
                  needp=.true.
            end if
            if ((j.eq.nlat).and.have(i,j).and.
     1            (.not.have(i,j-1))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i),ylat(j-1)
                  needp=.true.
            end if
            if ((j.gt.1).and.(j.lt.nlat).and.have(i,j).and.
     1            (.not.(have(i,j-1).or.have(i,j+1)))) then
                  write(*,*) 'Need values either at long,lat=',
     1                  xlong(i),ylat(j-1)
                  write(*,*) '  or at long,lat=',
     1                  xlong(i),ylat(j+1)
                  needp=.true.
            end if
      end do
      end do
      if (needp) stop 'Stress potential values need to be added'
c
      do j=1,nlat
      do i=1,nlong
            done(i,j)=.false.
            if (i.eq.1) then
                  if (have(i,j).and.have(i+1,j).and.
     1                  (.not.have(i+2,j))) then
                        xsxx(i,j)=(sxx(i+1,j)-sxx(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsyy(i,j)=(syy(i+1,j)-syy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxy(i,j)=(sxy(i+1,j)-sxy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (i.eq.nlong) then
                  if (have(i,j).and.have(i-1,j).and.
     1                  (.not.have(i-2,j))) then
                        xsxx(i,j)=(sxx(i,j)-sxx(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsyy(i,j)=(syy(i,j)-syy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxy(i,j)=(sxy(i,j)-sxy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (have(i-1,j).and.have(i,j).and.have(i+1,j)) then
                  if ((sxx(i,j)-sxx(i-1,j))
     1                  *(sxx(i+1,j)-sxx(i,j)).le.0.0d0) then
                        xsxx(i,j)=0.0d0
                  else
                        xsxx(i,j)=(sxx(i+1,j)-sxx(i-1,j))
     1                        /(xlong(i+1)-xlong(i-1))
                        xm=(sxx(i,j)-sxx(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xp=(sxx(i+1,j)-sxx(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        if (xsxx(i,j)/xm.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xm
                        if (xsxx(i,j)/xp.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xp
                  end if
                  if ((syy(i,j)-syy(i-1,j))
     1                  *(syy(i+1,j)-syy(i,j)).le.0.0d0) then
                        xsyy(i,j)=0.0d0
                  else
                        xsyy(i,j)=(syy(i+1,j)-syy(i-1,j))
     1                        /(xlong(i+1)-xlong(i-1))
                        xm=(syy(i,j)-syy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xp=(syy(i+1,j)-syy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        if (xsyy(i,j)/xm.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xm
                        if (xsyy(i,j)/xp.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xp
                  end if
                  if ((sxy(i,j)-sxy(i-1,j))
     1                  *(sxy(i+1,j)-sxy(i,j)).le.0.0d0) then
                        xsxy(i,j)=0.0d0
                  else
                        xsxy(i,j)=(sxy(i+1,j)-sxy(i-1,j))
     1                        /(xlong(i+1)-xlong(i-1))
                        xm=(sxy(i,j)-sxy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xp=(sxy(i+1,j)-sxy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        if (xsxy(i,j)/xm.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xm
                        if (xsxy(i,j)/xp.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xp
                  end if
                  done(i,j)=.true.
                  goto 30
            end if
            if (i.eq.2) then
                  if (have(i,j).and.have(i-1,j)) then
                        xsxx(i,j)=(sxx(i,j)-sxx(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsyy(i,j)=(syy(i,j)-syy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxy(i,j)=(sxy(i,j)-sxy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (i.eq.nlong-1) then
                  if (have(i,j).and.have(i+1,j)) then
                        xsxx(i,j)=(sxx(i+1,j)-sxx(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsyy(i,j)=(syy(i+1,j)-syy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxy(i,j)=(sxy(i+1,j)-sxy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (have(i,j).and.have(i-1,j).and.
     1            (.not.have(i-2,j))) then
                  xsxx(i,j)=(sxx(i,j)-sxx(i-1,j))
     1                  /(xlong(i)-xlong(i-1))
                  xsyy(i,j)=(syy(i,j)-syy(i-1,j))
     1                  /(xlong(i)-xlong(i-1))
                  xsxy(i,j)=(sxy(i,j)-sxy(i-1,j))
     1                  /(xlong(i)-xlong(i-1))
                  done(i,j)=.true.
                  goto 30
            end if
            if (have(i,j).and.have(i+1,j).and.
     1            (.not.have(i+2,j))) then
                  xsxx(i,j)=(sxx(i+1,j)-sxx(i,j))
     1                  /(xlong(i+1)-xlong(i))
                  xsyy(i,j)=(syy(i+1,j)-syy(i,j))
     1                  /(xlong(i+1)-xlong(i))
                  xsxy(i,j)=(sxy(i+1,j)-sxy(i,j))
     1                  /(xlong(i+1)-xlong(i))
                  done(i,j)=.true.
            end if
30    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            if (done(i,j).or.(.not.have(i,j))) goto 40
            if (i.eq.1) then
                  if (sxx(i+1,j)-sxx(i,j).eq.0.0d0) then
                        xsxx(i,j)=0.0d0
                  else
                        xp=(sxx(i+1,j)-sxx(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxx(i,j)=2.0d0*xp-xsxx(i+1,j)
                        if (xsxx(i,j)/xp.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xp
                  end if
                  if (syy(i+1,j)-syy(i,j).eq.0.0d0) then
                        xsyy(i,j)=0.0d0
                  else
                        xp=(syy(i+1,j)-syy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsyy(i,j)=2.0d0*xp-xsyy(i+1,j)
                        if (xsyy(i,j)/xp.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xp
                  end if
                  if (sxy(i+1,j)-sxy(i,j).eq.0.0d0) then
                        xsxy(i,j)=0.0d0
                  else
                        xp=(sxy(i+1,j)-sxy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxy(i,j)=2.0d0*xp-xsxy(i+1,j)
                        if (xsxy(i,j)/xp.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xp
                  end if
                  goto 40
            end if
            if (i.eq.nlong) then
                  if (sxx(i,j)-sxx(i-1,j).eq.0.0d0) then
                        xsxx(i,j)=0.0d0
                  else
                        xm=(sxx(i,j)-sxx(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxx(i,j)=2.0d0*xm-xsxx(i-1,j)
                        if (xsxx(i,j)/xm.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xm
                  end if
                  if (syy(i,j)-syy(i-1,j).eq.0.0d0) then
                        xsyy(i,j)=0.0d0
                  else
                        xm=(syy(i,j)-syy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsyy(i,j)=2.0d0*xm-xsyy(i-1,j)
                        if (xsyy(i,j)/xm.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xm
                  end if
                  if (sxy(i,j)-sxy(i-1,j).eq.0.0d0) then
                        xsxy(i,j)=0.0d0
                  else
                        xm=(sxy(i,j)-sxy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxy(i,j)=2.0d0*xm-xsxy(i-1,j)
                        if (xsxy(i,j)/xm.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xm
                  end if
                  goto 40
            end if
            if (done(i-1,j)) then
                  if (sxx(i,j)-sxx(i-1,j).eq.0.0d0) then
                        xsxx(i,j)=0.0d0
                  else
                        xm=(sxx(i,j)-sxx(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxx(i,j)=2.0d0*xm-xsxx(i-1,j)
                        if (xsxx(i,j)/xm.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xm
                  end if
                  if (syy(i,j)-syy(i-1,j).eq.0.0d0) then
                        xsyy(i,j)=0.0d0
                  else
                        xm=(syy(i,j)-syy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsyy(i,j)=2.0d0*xm-xsyy(i-1,j)
                        if (xsyy(i,j)/xm.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xm
                  end if
                  if (sxy(i,j)-sxy(i-1,j).eq.0.0d0) then
                        xsxy(i,j)=0.0d0
                  else
                        xm=(sxy(i,j)-sxy(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xsxy(i,j)=2.0d0*xm-xsxy(i-1,j)
                        if (xsxy(i,j)/xm.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xm
                  end if
                  goto 40
            end if
            if (done(i+1,j)) then
                  if (sxx(i+1,j)-sxx(i,j).eq.0.0d0) then
                        xsxx(i,j)=0.0d0
                  else
                        xp=(sxx(i+1,j)-sxx(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxx(i,j)=2.0d0*xp-xsxx(i+1,j)
                        if (xsxx(i,j)/xp.gt.1.5d0)
     1                        xsxx(i,j)=1.5d0*xp
                  end if
                  if (syy(i+1,j)-syy(i,j).eq.0.0d0) then
                        xsyy(i,j)=0.0d0
                  else
                        xp=(syy(i+1,j)-syy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsyy(i,j)=2.0d0*xp-xsyy(i+1,j)
                        if (xsyy(i,j)/xp.gt.1.5d0)
     1                        xsyy(i,j)=1.5d0*xp
                  end if
                  if (sxy(i+1,j)-sxy(i,j).eq.0.0d0) then
                        xsxy(i,j)=0.0d0
                  else
                        xp=(sxy(i+1,j)-sxy(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xsxy(i,j)=2.0d0*xp-xsxy(i+1,j)
                        if (xsxy(i,j)/xp.gt.1.5d0)
     1                        xsxy(i,j)=1.5d0*xp
                  end if
            end if
40    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            done(i,j)=.false.
            if (j.eq.1) then
                  if (have(i,j).and.have(i,j+1).and.
     1                  (.not.have(i,j+2))) then
                        ysxx(i,j)=(sxx(i,j+1)-sxx(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysyy(i,j)=(syy(i,j+1)-syy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxy(i,j)=(sxy(i,j+1)-sxy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (j.eq.nlat) then
                  if (have(i,j).and.have(i,j-1).and.
     1                  (.not.have(i,j-2))) then
                        ysxx(i,j)=(sxx(i,j)-sxx(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysyy(i,j)=(syy(i,j)-syy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxy(i,j)=(sxy(i,j)-sxy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (have(i,j-1).and.have(i,j).and.have(i,j+1)) then
                  if ((sxx(i,j)-sxx(i,j-1))
     1                  *(sxx(i,j+1)-sxx(i,j)).le.0.0d0) then
                        ysxx(i,j)=0.0d0
                  else
                        ysxx(i,j)=(sxx(i,j+1)-sxx(i,j-1))
     1                        /(ylat(j+1)-ylat(j-1))
                        ym=(sxx(i,j)-sxx(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yp=(sxx(i,j+1)-sxx(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        if (ysxx(i,j)/ym.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*ym
                        if (ysxx(i,j)/yp.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*yp
                  end if
                  if ((syy(i,j)-syy(i,j-1))
     1                  *(syy(i,j+1)-syy(i,j)).le.0.0d0) then
                        ysyy(i,j)=0.0d0
                  else
                        ysyy(i,j)=(syy(i,j+1)-syy(i,j-1))
     1                        /(ylat(j+1)-ylat(j-1))
                        ym=(syy(i,j)-syy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yp=(syy(i,j+1)-syy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        if (ysyy(i,j)/ym.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*ym
                        if (ysyy(i,j)/yp.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*yp
                  end if
                  if ((sxy(i,j)-sxy(i,j-1))
     1                  *(sxy(i,j+1)-sxy(i,j)).le.0.0d0) then
                        ysxy(i,j)=0.0d0
                  else
                        ysxy(i,j)=(sxy(i,j+1)-sxy(i,j-1))
     1                        /(ylat(j+1)-ylat(j-1))
                        ym=(sxy(i,j)-sxy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yp=(sxy(i,j+1)-sxy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        if (ysxy(i,j)/ym.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*ym
                        if (ysxy(i,j)/yp.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*yp
                  end if
                  done(i,j)=.true.
                  goto 50
            end if
            if (j.eq.2) then
                  if (have(i,j).and.have(i,j-1)) then
                        ysxx(i,j)=(sxx(i,j)-sxx(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysyy(i,j)=(syy(i,j)-syy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxy(i,j)=(sxy(i,j)-sxy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (j.eq.nlat-1) then
                  if (have(i,j).and.have(i,j+1)) then
                        ysxx(i,j)=(sxx(i,j+1)-sxx(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysyy(i,j)=(syy(i,j+1)-syy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxy(i,j)=(sxy(i,j+1)-sxy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (have(i,j).and.have(i,j-1).and.
     1            (.not.have(i,j-2))) then
                  ysxx(i,j)=(sxx(i,j)-sxx(i,j-1))
     1                  /(ylat(j)-ylat(j-1))
                  ysyy(i,j)=(syy(i,j)-syy(i,j-1))
     1                  /(ylat(j)-ylat(j-1))
                  ysxy(i,j)=(sxy(i,j)-sxy(i,j-1))
     1                  /(ylat(j)-ylat(j-1))
                  done(i,j)=.true.
                  goto 50
            end if
            if (have(i,j).and.have(i,j+1).and.
     1            (.not.have(i,j+2))) then
                  ysxx(i,j)=(sxx(i,j+1)-sxx(i,j))
     1                  /(ylat(j+1)-ylat(j))
                  ysyy(i,j)=(syy(i,j+1)-syy(i,j))
     1                  /(ylat(j+1)-ylat(j))
                  ysxy(i,j)=(sxy(i,j+1)-sxy(i,j))
     1                  /(ylat(j+1)-ylat(j))
                  done(i,j)=.true.
            end if
50    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            if (done(i,j).or.(.not.have(i,j))) goto 60
            if (j.eq.1) then
                  if (sxx(i,j+1)-sxx(i,j).eq.0.0d0) then
                        ysxx(i,j)=0.0d0
                  else
                        yp=(sxx(i,j+1)-sxx(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxx(i,j)=2.0d0*yp-ysxx(i,j+1)
                        if (ysxx(i,j)/yp.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*yp
                  end if
                  if (syy(i,j+1)-syy(i,j).eq.0.0d0) then
                        ysyy(i,j)=0.0d0
                  else
                        yp=(syy(i,j+1)-syy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysyy(i,j)=2.0d0*yp-ysyy(i,j+1)
                        if (ysyy(i,j)/yp.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*yp
                  end if
                  if (sxy(i,j+1)-sxy(i,j).eq.0.0d0) then
                        ysxy(i,j)=0.0d0
                  else
                        yp=(sxy(i,j+1)-sxy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxy(i,j)=2.0d0*yp-ysxy(i,j+1)
                        if (ysxy(i,j)/yp.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*yp
                  end if
                  goto 60
            end if
            if (j.eq.nlat) then
                  if (sxx(i,j)-sxx(i,j-1).eq.0.0d0) then
                        ysxx(i,j)=0.0d0
                  else
                        ym=(sxx(i,j)-sxx(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxx(i,j)=2.0d0*ym-ysxx(i,j-1)
                        if (ysxx(i,j)/ym.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*ym
                  end if
                  if (syy(i,j)-syy(i,j-1).eq.0.0d0) then
                        ysyy(i,j)=0.0d0
                  else
                        ym=(syy(i,j)-syy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysyy(i,j)=2.0d0*ym-ysyy(i,j-1)
                        if (ysyy(i,j)/ym.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*ym
                  end if
                  if (sxy(i,j)-sxy(i,j-1).eq.0.0d0) then
                        ysxy(i,j)=0.0d0
                  else
                        ym=(sxy(i,j)-sxy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxy(i,j)=2.0d0*ym-ysxy(i,j-1)
                        if (ysxy(i,j)/ym.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*ym
                  end if
                  goto 60
            end if
            if (done(i,j-1)) then
                  if (sxx(i,j)-sxx(i,j-1).eq.0.0d0) then
                        ysxx(i,j)=0.0d0
                  else
                        ym=(sxx(i,j)-sxx(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxx(i,j)=2.0d0*ym-ysxx(i,j-1)
                        if (ysxx(i,j)/ym.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*ym
                  end if
                  if (syy(i,j)-syy(i,j-1).eq.0.0d0) then
                        ysyy(i,j)=0.0d0
                  else
                        ym=(syy(i,j)-syy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysyy(i,j)=2.0d0*ym-ysyy(i,j-1)
                        if (ysyy(i,j)/ym.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*ym
                  end if
                  if (sxy(i,j)-sxy(i,j-1).eq.0.0d0) then
                        ysxy(i,j)=0.0d0
                  else
                        ym=(sxy(i,j)-sxy(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        ysxy(i,j)=2.0d0*ym-ysxy(i,j-1)
                        if (ysxy(i,j)/ym.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*ym
                  end if
                  goto 60
            end if
            if (done(i,j+1)) then
                  if (sxx(i,j+1)-sxx(i,j).eq.0.0d0) then
                        ysxx(i,j)=0.0d0
                  else
                        yp=(sxx(i,j+1)-sxx(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxx(i,j)=2.0d0*yp-ysxx(i,j+1)
                        if (ysxx(i,j)/yp.gt.1.5d0)
     1                        ysxx(i,j)=1.5d0*yp
                  end if
                  if (syy(i,j+1)-syy(i,j).eq.0.0d0) then
                        ysyy(i,j)=0.0d0
                  else
                        yp=(syy(i,j+1)-syy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysyy(i,j)=2.0d0*yp-ysyy(i,j+1)
                        if (ysyy(i,j)/yp.gt.1.5d0)
     1                        ysyy(i,j)=1.5d0*yp
                  end if
                  if (sxy(i,j+1)-sxy(i,j).eq.0.0d0) then
                        ysxy(i,j)=0.0d0
                  else
                        yp=(sxy(i,j+1)-sxy(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        ysxy(i,j)=2.0d0*yp-ysxy(i,j+1)
                        if (ysxy(i,j)/yp.gt.1.5d0)
     1                        ysxy(i,j)=1.5d0*yp
                  end if
            end if
60    continue
      end do
      end do
c
      do i=1,ngp
            if ((long(i).lt.xlong(1)).or.(lat(i).lt.ylat(1)))
     1            stop 'Grid point is outside function mesh'
            do j=1,nlong
                  if (long(i).le.xlong(j)) goto 70
            end do
            stop 'Grid point is outside function mesh'
70          continue
            do k=1,nlat
                  if (lat(i).le.ylat(k)) goto 80
            end do
            stop 'Grid point is outside function mesh'
80          continue
            if ((long(i).eq.xlong(j)).and.(lat(i).eq.ylat(k))) then
                  if (.not.have(j,k)) then
                        write(*,*) 'Need values at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Stress potential values need to be added'
                  end if
                  sxxpot(i)=sxx(j,k)
                  syypot(i)=syy(j,k)
                  sxypot(i)=sxy(j,k)
                  goto 90
            end if
            if (lat(i).eq.ylat(k)) then
                  if (.not.(have(j-1,k).and.have(j,k))) then
                        write(*,*) 'Need values both at long,lat=',
     1                        xlong(j-1),ylat(k)
                        write(*,*) '  and at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Stress potential values need to be added'
                  end if
                  dx=xlong(j)-xlong(j-1)
                  x0=(xlong(j)-long(i))/dx
                  x1=(long(i)-xlong(j-1))/dx
                  hx0=3.0d0*x0**2-2.0d0*x0**3
                  hx1=3.0d0*x1**2-2.0d0*x1**3
                  kx0=dx*x1*x0**2
                  kx1=-dx*x0*x1**2
                  sxxpot(i)=hx0*sxx(j-1,k)+hx1*sxx(j,k)
     1                  +kx0*xsxx(j-1,k)+kx1*xsxx(j,k)
                  syypot(i)=hx0*syy(j-1,k)+hx1*syy(j,k)
     1                  +kx0*xsyy(j-1,k)+kx1*xsyy(j,k)
                  sxypot(i)=hx0*sxy(j-1,k)+hx1*sxy(j,k)
     1                  +kx0*xsxy(j-1,k)+kx1*xsxy(j,k)
                  goto 90
            end if
            if (long(i).eq.xlong(j)) then
                  if (.not.(have(j,k-1).and.have(j,k))) then
                        write(*,*) 'Need values both at long,lat=',
     1                        xlong(j),ylat(k-1)
                        write(*,*) '  and at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Stress potential values need to be added'
                  end if
                  dy=ylat(k)-ylat(k-1)
                  y0=(ylat(k)-lat(i))/dy
                  y1=(lat(i)-ylat(k-1))/dy
                  hy0=3.0d0*y0**2-2.0d0*y0**3
                  hy1=3.0d0*y1**2-2.0d0*y1**3
                  ky0=dy*y1*y0**2
                  ky1=-dy*y0*y1**2
                  sxxpot(i)=hy0*sxx(j,k-1)+hy1*sxx(j,k)
     1                  +ky0*ysxx(j,k-1)+ky1*ysxx(j,k)
                  syypot(i)=hy0*syy(j,k-1)+hy1*syy(j,k)
     1                  +ky0*ysyy(j,k-1)+ky1*ysyy(j,k)
                  sxypot(i)=hy0*sxy(j,k-1)+hy1*sxy(j,k)
     1                  +ky0*ysxy(j,k-1)+ky1*ysxy(j,k)
                  goto 90
            end if
            if (.not.(have(j-1,k-1).and.have(j,k-1).and.
     1            have(j-1,k).and.have(j,k))) then
                  write(*,*) 'Need values at all four of long,lat=',
     1                  xlong(j-1),ylat(k-1)
                  write(*,*) '  long,lat=',
     1                  xlong(j),ylat(k-1)
                  write(*,*) '  long,lat=',
     1                  xlong(j-1),ylat(k)
                  write(*,*) '  and long,lat=',
     1                  xlong(j),ylat(k)
                  stop 'Stress potential values need to be added'
            end if
            dx=xlong(j)-xlong(j-1)
            x0=(xlong(j)-long(i))/dx
            x1=(long(i)-xlong(j-1))/dx
            hx0=3.0d0*x0**2-2.0d0*x0**3
            hx1=3.0d0*x1**2-2.0d0*x1**3
            kx0=dx*x1*x0**2
            kx1=-dx*x0*x1**2
            dy=ylat(k)-ylat(k-1)
            y0=(ylat(k)-lat(i))/dy
            y1=(lat(i)-ylat(k-1))/dy
            hy0=3.0d0*y0**2-2.0d0*y0**3
            hy1=3.0d0*y1**2-2.0d0*y1**3
            ky0=dy*y1*y0**2
            ky1=-dy*y0*y1**2
            sxxpot(i)=hx0*hy0*sxx(j-1,k-1)+hx1*hy0*sxx(j,k-1)
     1            +hx0*hy1*sxx(j-1,k)+hx1*hy1*sxx(j,k)
     2            +kx0*hy0*xsxx(j-1,k-1)+kx1*hy0*xsxx(j,k-1)
     3            +kx0*hy1*xsxx(j-1,k)+kx1*hy1*xsxx(j,k)
     4            +hx0*ky0*ysxx(j-1,k-1)+hx1*ky0*ysxx(j,k-1)
     5            +hx0*ky1*ysxx(j-1,k)+hx1*ky1*ysxx(j,k)
            syypot(i)=hx0*hy0*syy(j-1,k-1)+hx1*hy0*syy(j,k-1)
     1            +hx0*hy1*syy(j-1,k)+hx1*hy1*syy(j,k)
     2            +kx0*hy0*xsyy(j-1,k-1)+kx1*hy0*xsyy(j,k-1)
     3            +kx0*hy1*xsyy(j-1,k)+kx1*hy1*xsyy(j,k)
     4            +hx0*ky0*ysyy(j-1,k-1)+hx1*ky0*ysyy(j,k-1)
     5            +hx0*ky1*ysyy(j-1,k)+hx1*ky1*ysyy(j,k)
            sxypot(i)=hx0*hy0*sxy(j-1,k-1)+hx1*hy0*sxy(j,k-1)
     1            +hx0*hy1*sxy(j-1,k)+hx1*hy1*sxy(j,k)
     2            +kx0*hy0*xsxy(j-1,k-1)+kx1*hy0*xsxy(j,k-1)
     3            +kx0*hy1*xsxy(j-1,k)+kx1*hy1*xsxy(j,k)
     4            +hx0*ky0*ysxy(j-1,k-1)+hx1*ky0*ysxy(j,k-1)
     5            +hx0*ky1*ysxy(j-1,k)+hx1*ky1*ysxy(j,k)
90          continue
      end do
c
      return
      end
c
c
      SUBROUTINE getL(lun,ne,elong,elat,Lc,Lcc,Lcs,Ls,Lsc,Lss,
     1      neb,npeb,eblong,eblat)
c
      implicit none
      integer maxgp,maxe,maxeb,maxpr,maxdim
      parameter(maxgp=40000,maxe=2*maxgp,maxeb=10,maxpr=1001,
     1      maxdim=1001)
      integer lun,ne,neb
      integer nlong,nlat,i,p,j,nval,k,eb
      integer npeb(maxeb)
      logical needp,inside,pinr,pexr
      logical have(maxdim,maxdim),done(maxdim,maxdim)
      real*8 plong,plat,Lcp,Lccp,Lcsp,Lsp,Lscp,Lssp,xm,xp,ym,yp
      real*8 dx,x0,x1,hx0,hx1,kx0,kx1,dy,y0,y1,hy0,hy1,ky0,ky1
      real*8 ax00c,ax10c,ax01c,ax11c,ax00s,ax10s,ax01s,ax11s
      real*8 ay00c,ay10c,ay01c,ay11c,ay00s,ay10s,ay01s,ay11s
      real*8 p00c,p10c,p01c,p11c,p00s,p10s,p01s,p11s
      real*8 elong(maxe),elat(maxe)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
      real*8 eblong(maxpr,maxeb),eblat(maxpr,maxeb)
      real*8 xlong(maxdim),ylat(maxdim)
      real*8 Lcf(maxdim,maxdim),Lccf(maxdim,maxdim),
     1      Lcsf(maxdim,maxdim)
      real*8 Lsf(maxdim,maxdim),Lscf(maxdim,maxdim),
     1      Lssf(maxdim,maxdim)
      real*8 xLcf(maxdim,maxdim),xLsf(maxdim,maxdim)
      real*8 yLcf(maxdim,maxdim),yLsf(maxdim,maxdim)
c
      read(lun,*) nlong
      if (nlong.lt.3) then
            write(*,*) 'nlong=',nlong
            stop 'There must be at least 3 values of longitude'
      end if
      if (nlong.gt.maxdim) then
            write(*,*) 'nlong,maxdim=',nlong,maxdim
            stop 'Recompile with an increased maxdim value'
      end if
      do i=1,nlong
            read(lun,*) p,xlong(i)
      end do
      do i=2,nlong
            if (xlong(i).le.xlong(i-1)) then
                 write(*,*) 'i,xlong(i),xlong(i-1)=',
     1                 i,xlong(i),xlong(i-1)
            stop 'Longitudes must be in strictly increasing order'
            end if
      end do
      read(lun,*) nlat
      if (nlat.lt.3) then
            write(*,*) 'nlat=',nlat
            stop 'There must be at least 3 values of latitude'
      end if
      if (nlat.gt.maxdim) then
            write(*,*) 'nlat,maxdim=',nlat,maxdim
            stop 'Recompile with an increased maxdim value'
      end if
      do i=1,nlat
            read(lun,*) p,ylat(i)
      end do
      do i=2,nlat
            if (ylat(i).le.ylat(i-1)) then

                 write(*,*) 'i,ylat(i),ylat(i-1)=',
     1                 i,ylat(i),ylat(i-1)
            stop 'Latitudes must be in strictly increasing order'
            end if
      end do
c
      do j=1,nlat
      do i=1,nlong
            have(i,j)=.false.
      end do
      end do
      read(lun,*) nval
      do i=1,nval
            read(lun,*) p,plong,plat
            read(lun,*) Lcp,Lccp,Lcsp,Lsp,Lscp,Lssp
            do j=1,nlong
                  if (xlong(j).eq.plong) goto 10
            end do
            write(*,*) 'p,plong,plat=',p,plong,plat
            stop 'Could not find plong among longitudes'
10          continue
            do k=1,nlat
                  if (ylat(k).eq.plat) goto 20
            end do
            write(*,*) 'p,plong,plat=',p,plong,plat
            stop 'Could not find plat among latitudes'
20          continue
            have(j,k)=.true.
            Lcf(j,k)=Lcp
            Lccf(j,k)=Lccp
            Lcsf(j,k)=Lcsp
            Lsf(j,k)=Lsp
            Lscf(j,k)=Lscp
            Lssf(j,k)=Lssp
      end do
c
      needp=.false.
      do j=1,nlat
      do i=1,nlong
            inside=pinr(npeb(1),eblong(1,1),eblat(1,1),
     1                  xlong(i),ylat(j))
            if (neb.gt.1) then
                  do eb=2,neb
                        inside=inside.and.
     1                        pexr(npeb(eb),eblong(1,eb),
     1                        eblat(1,eb),xlong(i),ylat(j))
                  end do
            end if
            if (inside) then
                  if ((i.eq.1).or.(i.eq.nlong).or.
     1                  (j.eq.1).or.(j.eq.nlat)) then
            stop 'Edge points must be outside the modelling region'
                  else
                        if (.not.have(i-1,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j-1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j-1)
                              needp=.true.
                        end if
                        if (.not.have(i-1,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j)
                              needp=.true.
                        end if
                        if (.not.have(i-1,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i-1),ylat(j+1)
                              needp=.true.
                        end if
                        if (.not.have(i,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i),ylat(j+1)
                              needp=.true.
                        end if
                        if (.not.have(i+1,j+1)) then
                              write(*,*) 'Need values at long,lat=',
     1                              xlong(i+1),ylat(j+1)
                              needp=.true.
                        end if
                  end if
            end if
            if ((i.eq.1).and.have(i,j).and.
     1            (.not.have(i+1,j))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i+1),ylat(j)
                  needp=.true.
            end if
            if ((i.eq.nlong).and.have(i,j).and.
     1            (.not.have(i-1,j))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i-1),ylat(j)
                  needp=.true.
            end if
            if ((i.gt.1).and.(i.lt.nlong).and.have(i,j).and.
     1            (.not.(have(i-1,j).or.have(i+1,j)))) then
                  write(*,*) 'Need values either at long,lat=',
     1                  xlong(i-1),ylat(j)
                  write(*,*) '  or at long,lat=',
     1                  xlong(i+1),ylat(j)
                  needp=.true.
            end if
            if ((j.eq.1).and.have(i,j).and.
     1            (.not.have(i,j+1))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i),ylat(j+1)
                  needp=.true.
            end if
            if ((j.eq.nlat).and.have(i,j).and.
     1            (.not.have(i,j-1))) then
                  write(*,*) 'Need values at long,lat=',
     1                  xlong(i),ylat(j-1)
                  needp=.true.
            end if
            if ((j.gt.1).and.(j.lt.nlat).and.have(i,j).and.
     1            (.not.(have(i,j-1).or.have(i,j+1)))) then
                  write(*,*) 'Need values either at long,lat=',
     1                  xlong(i),ylat(j-1)
                  write(*,*) '  or at long,lat=',
     1                  xlong(i),ylat(j+1)
                  needp=.true.
            end if
      end do
      end do
      if (needp) stop 'Strain-rate capacity values need to be added'
c
      do j=1,nlat
      do i=1,nlong
            done(i,j)=.false.
            if (i.eq.1) then
                  if (have(i,j).and.have(i+1,j).and.
     1                  (.not.have(i+2,j))) then
                        xLcf(i,j)=(Lcf(i+1,j)-Lcf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLsf(i,j)=(Lsf(i+1,j)-Lsf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (i.eq.nlong) then
                  if (have(i,j).and.have(i-1,j).and.
     1                  (.not.have(i-2,j))) then
                        xLcf(i,j)=(Lcf(i,j)-Lcf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xLsf(i,j)=(Lsf(i,j)-Lsf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (have(i-1,j).and.have(i,j).and.have(i+1,j)) then
                  if ((Lcf(i,j)-Lcf(i-1,j))
     1                  *(Lcf(i+1,j)-Lcf(i,j)).le.0.0d0) then
                        xLcf(i,j)=0.0d0
                  else
                        xLcf(i,j)=(Lcf(i+1,j)-Lcf(i-1,j))
     1                        /(xlong(i+1)-xlong(i-1))
                        xm=(Lcf(i,j)-Lcf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xp=(Lcf(i+1,j)-Lcf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        if (xLcf(i,j)/xm.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xm
                        if (xLcf(i,j)/xp.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xp
                  end if
                  if ((Lsf(i,j)-Lsf(i-1,j))
     1                  *(Lsf(i+1,j)-Lsf(i,j)).le.0.0d0) then
                        xLsf(i,j)=0.0d0
                  else
                        xLsf(i,j)=(Lsf(i+1,j)-Lsf(i-1,j))
     1                        /(xlong(i+1)-xlong(i-1))
                        xm=(Lsf(i,j)-Lsf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xp=(Lsf(i+1,j)-Lsf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        if (xLsf(i,j)/xm.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xm
                        if (xLsf(i,j)/xp.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xp
                  end if
                  done(i,j)=.true.
                  goto 30
            end if
            if (i.eq.2) then
                  if (have(i,j).and.have(i-1,j)) then
                        xLcf(i,j)=(Lcf(i,j)-Lcf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xLsf(i,j)=(Lsf(i,j)-Lsf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (i.eq.nlong-1) then
                  if (have(i,j).and.have(i+1,j)) then
                        xLcf(i,j)=(Lcf(i+1,j)-Lcf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLsf(i,j)=(Lsf(i+1,j)-Lsf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        done(i,j)=.true.
                  end if
                  goto 30
            end if
            if (have(i,j).and.have(i-1,j).and.
     1            (.not.have(i-2,j))) then
                  xLcf(i,j)=(Lcf(i,j)-Lcf(i-1,j))
     1                  /(xlong(i)-xlong(i-1))
                  xLsf(i,j)=(Lsf(i,j)-Lsf(i-1,j))
     1                  /(xlong(i)-xlong(i-1))
                  done(i,j)=.true.
                  goto 30
            end if
            if (have(i,j).and.have(i+1,j).and.
     1            (.not.have(i+2,j))) then
                  xLcf(i,j)=(Lcf(i+1,j)-Lcf(i,j))
     1                  /(xlong(i+1)-xlong(i))
                  xLsf(i,j)=(Lsf(i+1,j)-Lsf(i,j))
     1                  /(xlong(i+1)-xlong(i))
                  done(i,j)=.true.
            end if
30    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            if (done(i,j).or.(.not.have(i,j))) goto 40
            if (i.eq.1) then
                  if (Lcf(i+1,j)-Lcf(i,j).eq.0.0d0) then
                        xLcf(i,j)=0.0d0
                  else
                        xp=(Lcf(i+1,j)-Lcf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLcf(i,j)=2.0d0*xp-xLcf(i+1,j)
                        if (xLcf(i,j)/xp.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xp
                  end if
                  if (Lsf(i+1,j)-Lsf(i,j).eq.0.0d0) then
                        xLsf(i,j)=0.0d0
                  else
                        xp=(Lsf(i+1,j)-Lsf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLsf(i,j)=2.0d0*xp-xLsf(i+1,j)
                        if (xLsf(i,j)/xp.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xp
                  end if
                  goto 40
            end if
            if (i.eq.nlong) then
                  if (Lcf(i,j)-Lcf(i-1,j).eq.0.0d0) then
                        xLcf(i,j)=0.0d0
                  else
                        xm=(Lcf(i,j)-Lcf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xLcf(i,j)=2.0d0*xm-xLcf(i-1,j)

                        if (xLcf(i,j)/xm.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xm
                  end if
                  if (Lsf(i,j)-Lsf(i-1,j).eq.0.0d0) then
                        xLsf(i,j)=0.0d0
                  else
                        xm=(Lsf(i,j)-Lsf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xLsf(i,j)=2.0d0*xm-xLsf(i-1,j)
                        if (xLsf(i,j)/xm.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xm
                  end if
                  goto 40
            end if
            if (done(i-1,j)) then
                  if (Lcf(i,j)-Lcf(i-1,j).eq.0.0d0) then
                        xLcf(i,j)=0.0d0
                  else
                        xm=(Lcf(i,j)-Lcf(i-1,j))
     1                        /(xlong(i)-xlong(i-1))
                        xLcf(i,j)=2.0d0*xm-xLcf(i-1,j)
                        if (xLcf(i,j)/xm.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xm
                  end if
                  if (Lsf(i,j)-Lsf(i-1,j).eq.0.0d0) then
                        xLsf(i,j)=0.0d0
                  else
                        xm=(Lsf(i,j)-Lsf(i-1,j))

     1                        /(xlong(i)-xlong(i-1))
                        xLsf(i,j)=2.0d0*xm-xLsf(i-1,j)
                        if (xLsf(i,j)/xm.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xm
                  end if
                  goto 40
            end if
            if (done(i+1,j)) then
                  if (Lcf(i+1,j)-Lcf(i,j).eq.0.0d0) then
                        xLcf(i,j)=0.0d0
                  else
                        xp=(Lcf(i+1,j)-Lcf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLcf(i,j)=2.0d0*xp-xLcf(i+1,j)
                        if (xLcf(i,j)/xp.gt.1.5d0)
     1                        xLcf(i,j)=1.5d0*xp
                  end if
                  if (Lsf(i+1,j)-Lsf(i,j).eq.0.0d0) then
                        xLsf(i,j)=0.0d0
                  else
                        xp=(Lsf(i+1,j)-Lsf(i,j))
     1                        /(xlong(i+1)-xlong(i))
                        xLsf(i,j)=2.0d0*xp-xLsf(i+1,j)
                        if (xLsf(i,j)/xp.gt.1.5d0)
     1                        xLsf(i,j)=1.5d0*xp
                  end if
            end if
40    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            done(i,j)=.false.
            if (j.eq.1) then
                  if (have(i,j).and.have(i,j+1).and.
     1                  (.not.have(i,j+2))) then
                        yLcf(i,j)=(Lcf(i,j+1)-Lcf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLsf(i,j)=(Lsf(i,j+1)-Lsf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (j.eq.nlat) then
                  if (have(i,j).and.have(i,j-1).and.
     1                  (.not.have(i,j-2))) then
                        yLcf(i,j)=(Lcf(i,j)-Lcf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLsf(i,j)=(Lsf(i,j)-Lsf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (have(i,j-1).and.have(i,j).and.have(i,j+1)) then
                  if ((Lcf(i,j)-Lcf(i,j-1))
     1                  *(Lcf(i,j+1)-Lcf(i,j)).le.0.0d0) then
                        yLcf(i,j)=0.0d0
                  else
                        yLcf(i,j)=(Lcf(i,j+1)-Lcf(i,j-1))
     1                        /(ylat(j+1)-ylat(j-1))
                        ym=(Lcf(i,j)-Lcf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yp=(Lcf(i,j+1)-Lcf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        if (yLcf(i,j)/ym.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*ym
                        if (yLcf(i,j)/yp.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*yp
                  end if
                  if ((Lsf(i,j)-Lsf(i,j-1))
     1                  *(Lsf(i,j+1)-Lsf(i,j)).le.0.0d0) then
                        yLsf(i,j)=0.0d0
                  else
                        yLsf(i,j)=(Lsf(i,j+1)-Lsf(i,j-1))
     1                        /(ylat(j+1)-ylat(j-1))
                        ym=(Lsf(i,j)-Lsf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yp=(Lsf(i,j+1)-Lsf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        if (yLsf(i,j)/ym.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*ym
                        if (yLsf(i,j)/yp.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*yp
                  end if
                  done(i,j)=.true.
                  goto 50
            end if
            if (j.eq.2) then
                  if (have(i,j).and.have(i,j-1)) then
                        yLcf(i,j)=(Lcf(i,j)-Lcf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLsf(i,j)=(Lsf(i,j)-Lsf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (j.eq.nlat-1) then
                  if (have(i,j).and.have(i,j+1)) then
                        yLcf(i,j)=(Lcf(i,j+1)-Lcf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLsf(i,j)=(Lsf(i,j+1)-Lsf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        done(i,j)=.true.
                  end if
                  goto 50
            end if
            if (have(i,j).and.have(i,j-1).and.
     1            (.not.have(i,j-2))) then
                  yLcf(i,j)=(Lcf(i,j)-Lcf(i,j-1))
     1                  /(ylat(j)-ylat(j-1))
                  yLsf(i,j)=(Lsf(i,j)-Lsf(i,j-1))
     1                  /(ylat(j)-ylat(j-1))
                  done(i,j)=.true.
                  goto 50
            end if
            if (have(i,j).and.have(i,j+1).and.
     1            (.not.have(i,j+2))) then
                  yLcf(i,j)=(Lcf(i,j+1)-Lcf(i,j))
     1                  /(ylat(j+1)-ylat(j))
                  yLsf(i,j)=(Lsf(i,j+1)-Lsf(i,j))
     1                  /(ylat(j+1)-ylat(j))
                  done(i,j)=.true.
            end if
50    continue
      end do
      end do
c
      do j=1,nlat
      do i=1,nlong
            if (done(i,j).or.(.not.have(i,j))) goto 60
            if (j.eq.1) then
                  if (Lcf(i,j+1)-Lcf(i,j).eq.0.0d0) then
                        yLcf(i,j)=0.0d0
                  else
                        yp=(Lcf(i,j+1)-Lcf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLcf(i,j)=2.0d0*yp-yLcf(i,j+1)
                        if (yLcf(i,j)/yp.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*yp
                  end if
                  if (Lsf(i,j+1)-Lsf(i,j).eq.0.0d0) then
                        yLsf(i,j)=0.0d0
                  else
                        yp=(Lsf(i,j+1)-Lsf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLsf(i,j)=2.0d0*yp-yLsf(i,j+1)
                        if (yLsf(i,j)/yp.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*yp
                  end if
                  goto 60
            end if
            if (j.eq.nlat) then
                  if (Lcf(i,j)-Lcf(i,j-1).eq.0.0d0) then
                        yLcf(i,j)=0.0d0
                  else
                        ym=(Lcf(i,j)-Lcf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLcf(i,j)=2.0d0*ym-yLcf(i,j-1)
                        if (yLcf(i,j)/ym.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*ym
                  end if
                  if (Lsf(i,j)-Lsf(i,j-1).eq.0.0d0) then
                        yLsf(i,j)=0.0d0
                  else
                        ym=(Lsf(i,j)-Lsf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLsf(i,j)=2.0d0*ym-yLsf(i,j-1)
                        if (yLsf(i,j)/ym.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*ym
                  end if
                  goto 60
            end if
            if (done(i,j-1)) then
                  if (Lcf(i,j)-Lcf(i,j-1).eq.0.0d0) then
                        yLcf(i,j)=0.0d0
                  else
                        ym=(Lcf(i,j)-Lcf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLcf(i,j)=2.0d0*ym-yLcf(i,j-1)
                        if (yLcf(i,j)/ym.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*ym
                  end if
                  if (Lsf(i,j)-Lsf(i,j-1).eq.0.0d0) then
                        yLsf(i,j)=0.0d0
                  else
                        ym=(Lsf(i,j)-Lsf(i,j-1))
     1                        /(ylat(j)-ylat(j-1))
                        yLsf(i,j)=2.0d0*ym-yLsf(i,j-1)
                        if (yLsf(i,j)/ym.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*ym
                  end if
                  goto 60
            end if
            if (done(i,j+1)) then
                  if (Lcf(i,j+1)-Lcf(i,j).eq.0.0d0) then
                        yLcf(i,j)=0.0d0
                  else
                        yp=(Lcf(i,j+1)-Lcf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLcf(i,j)=2.0d0*yp-yLcf(i,j+1)
                        if (yLcf(i,j)/yp.gt.1.5d0)
     1                        yLcf(i,j)=1.5d0*yp
                  end if
                  if (Lsf(i,j+1)-Lsf(i,j).eq.0.0d0) then
                        yLsf(i,j)=0.0d0
                  else
                        yp=(Lsf(i,j+1)-Lsf(i,j))
     1                        /(ylat(j+1)-ylat(j))
                        yLsf(i,j)=2.0d0*yp-yLsf(i,j+1)
                        if (yLsf(i,j)/yp.gt.1.5d0)
     1                        yLsf(i,j)=1.5d0*yp
                  end if
            end if
60    continue
      end do
      end do
c
      do i=1,ne
            if ((elong(i).lt.xlong(1)).or.(elat(i).lt.ylat(1)))
     1            stop 'Element point is outside function mesh'
            do j=1,nlong
                  if (elong(i).le.xlong(j)) goto 70
            end do
            stop 'Element point is outside function mesh'
70          continue
            do k=1,nlat
                  if (elat(i).le.ylat(k)) goto 80
            end do
            stop 'Element point is outside function mesh'
80          continue
            if ((elong(i).eq.xlong(j)).and.(elat(i).eq.ylat(k))) then
                  if (.not.have(j,k)) then
                        write(*,*) 'Need values at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Strain-rate capacity values need to be added'
                  end if
                  Lc(i)=Lcf(j,k)
                  Lcc(i)=Lccf(j,k)
                  Lcs(i)=Lcsf(j,k)
                  Ls(i)=Lsf(j,k)
                  Lsc(i)=Lscf(j,k)
                  Lss(i)=Lssf(j,k)
                  goto 90
            end if
            if (elat(i).eq.ylat(k)) then
                  if (.not.(have(j-1,k).and.have(j,k))) then
                        write(*,*) 'Need values both at long,lat=',
     1                        xlong(j-1),ylat(k)
                        write(*,*) '  and at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Strain-rate capacity values need to be added'
                  end if
                  dx=xlong(j)-xlong(j-1)
                  if (Lcf(j,k)-Lcf(j-1,k).eq.0.0d0) then
                        ax01c=0.0d0
                        ax11c=0.0d0
                  else
                        ax01c=dx*xLcf(j-1,k)/(Lcf(j,k)-Lcf(j-1,k))
                        ax11c=dx*xLcf(j,k)/(Lcf(j,k)-Lcf(j-1,k))
                  end if
                  if (Lsf(j,k)-Lsf(j-1,k).eq.0.0d0) then
                        ax01s=0.0d0
                        ax11s=0.0d0
                  else
                        ax01s=dx*xLsf(j-1,k)/(Lsf(j,k)-Lsf(j-1,k))
                        ax11s=dx*xLsf(j,k)/(Lsf(j,k)-Lsf(j-1,k))
                  end if
                  x0=(xlong(j)-elong(i))/dx
                  x1=(elong(i)-xlong(j-1))/dx
                  hx0=3.0d0*x0**2-2.0d0*x0**3
                  hx1=3.0d0*x1**2-2.0d0*x1**3
                  kx0=x1*x0**2
                  kx1=-x0*x1**2
                  p01c=hx0-ax01c*kx0-ax11c*kx1
                  p11c=hx1+ax01c*kx0+ax11c*kx1
                  p01s=hx0-ax01s*kx0-ax11s*kx1
                  p11s=hx1+ax01s*kx0+ax11s*kx1
                  Lc(i)=p01c*Lcf(j-1,k)+p11c*Lcf(j,k)
                  Lcc(i)=p01c*Lccf(j-1,k)+p11c*Lccf(j,k)
                  Lcs(i)=p01c*Lcsf(j-1,k)+p11c*Lcsf(j,k)
                  Ls(i)=p01s*Lsf(j-1,k)+p11s*Lsf(j,k)
                  Lsc(i)=p01s*Lscf(j-1,k)+p11s*Lscf(j,k)
                  Lss(i)=p01s*Lssf(j-1,k)+p11s*Lssf(j,k)
                  goto 90
            end if
            if (elong(i).eq.xlong(j)) then
                  if (.not.(have(j,k-1).and.have(j,k))) then
                        write(*,*) 'Need values both at long,lat=',
     1                        xlong(j),ylat(k-1)
                        write(*,*) '  and at long,lat=',
     1                        xlong(j),ylat(k)
                  stop 'Strain-rate capacity values need to be added'
                  end if
                  dy=ylat(k)-ylat(k-1)
                  if (Lcf(j,k)-Lcf(j,k-1).eq.0.0d0) then
                        ay10c=0.0d0
                        ay11c=0.0d0
                  else
                        ay10c=dy*yLcf(j,k-1)/(Lcf(j,k)-Lcf(j,k-1))
                        ay11c=dy*yLcf(j,k)/(Lcf(j,k)-Lcf(j,k-1))
                  end if
                  if (Lsf(j,k)-Lsf(j,k-1).eq.0.0d0) then
                        ay10s=0.0d0
                        ay11s=0.0d0
                  else
                        ay10s=dy*yLsf(j,k-1)/(Lsf(j,k)-Lsf(j,k-1))
                        ay11s=dy*yLsf(j,k)/(Lsf(j,k)-Lsf(j,k-1))
                  end if
                  y0=(ylat(k)-elat(i))/dy
                  y1=(elat(i)-ylat(k-1))/dy
                  hy0=3.0d0*y0**2-2.0d0*y0**3
                  hy1=3.0d0*y1**2-2.0d0*y1**3
                  ky0=y1*y0**2
                  ky1=-y0*y1**2
                  p10c=hy0-ay10c*ky0-ay11c*ky1
                  p11c=hy1+ay10c*ky0+ay11c*ky1
                  p10s=hy0-ay10s*ky0-ay11s*ky1
                  p11s=hy1+ay10s*ky0+ay11s*ky1
                  Lc(i)=p10c*Lcf(j,k-1)+p11c*Lcf(j,k)
                  Lcc(i)=p10c*Lccf(j,k-1)+p11c*Lccf(j,k)
                  Lcs(i)=p10c*Lcsf(j,k-1)+p11c*Lcsf(j,k)
                  Ls(i)=p10s*Lsf(j,k-1)+p11s*Lsf(j,k)
                  Lsc(i)=p10s*Lscf(j,k-1)+p11s*Lscf(j,k)
                  Lss(i)=p10s*Lssf(j,k-1)+p11s*Lssf(j,k)
                  goto 90
            end if
            if (.not.(have(j-1,k-1).and.have(j,k-1).and.
     1            have(j-1,k).and.have(j,k))) then
                  write(*,*) 'Need values at all four of long,lat=',
     1                  xlong(j-1),ylat(k-1)
                  write(*,*) '  long,lat=',
     1                  xlong(j),ylat(k-1)
                  write(*,*) '  long,lat=',
     1                  xlong(j-1),ylat(k)
                  write(*,*) '  and long,lat=',
     1                  xlong(j),ylat(k)
                  stop 'Strain-rate capacity values need to be added'
            end if
            dx=xlong(j)-xlong(j-1)
            if (Lcf(j,k-1)-Lcf(j-1,k-1).eq.0.0d0) then
                  ax00c=0.0d0
                  ax10c=0.0d0
            else
                  ax00c=dx*xLcf(j-1,k-1)/(Lcf(j,k-1)-Lcf(j-1,k-1))
                  ax10c=dx*xLcf(j,k-1)/(Lcf(j,k-1)-Lcf(j-1,k-1))
            end if
            if (Lcf(j,k)-Lcf(j-1,k).eq.0.0d0) then
                  ax01c=0.0d0
                  ax11c=0.0d0
            else
                  ax01c=dx*xLcf(j-1,k)/(Lcf(j,k)-Lcf(j-1,k))
                  ax11c=dx*xLcf(j,k)/(Lcf(j,k)-Lcf(j-1,k))
            end if
            if (Lsf(j,k-1)-Lsf(j-1,k-1).eq.0.0d0) then
                  ax00s=0.0d0
                  ax10s=0.0d0
            else
                  ax00s=dx*xLsf(j-1,k-1)/(Lsf(j,k-1)-Lsf(j-1,k-1))
                  ax10s=dx*xLsf(j,k-1)/(Lsf(j,k-1)-Lsf(j-1,k-1))
            end if
            if (Lsf(j,k)-Lsf(j-1,k).eq.0.0d0) then
                  ax01s=0.0d0
                  ax11s=0.0d0
            else
                  ax01s=dx*xLsf(j-1,k)/(Lsf(j,k)-Lsf(j-1,k))
                  ax11s=dx*xLsf(j,k)/(Lsf(j,k)-Lsf(j-1,k))
            end if
            x0=(xlong(j)-elong(i))/dx
            x1=(elong(i)-xlong(j-1))/dx
            hx0=3.0d0*x0**2-2.0d0*x0**3
            hx1=3.0d0*x1**2-2.0d0*x1**3
            kx0=x1*x0**2
            kx1=-x0*x1**2
            dy=ylat(k)-ylat(k-1)
            if (Lcf(j-1,k)-Lcf(j-1,k-1).eq.0.0d0) then
                  ay00c=0.0d0
                  ay01c=0.0d0
            else
                  ay00c=dy*yLcf(j-1,k-1)/(Lcf(j-1,k)-Lcf(j-1,k-1))
                  ay01c=dy*yLcf(j-1,k)/(Lcf(j-1,k)-Lcf(j-1,k-1))
            end if
            if (Lcf(j,k)-Lcf(j,k-1).eq.0.0d0) then
                  ay10c=0.0d0
                  ay11c=0.0d0
            else
                  ay10c=dy*yLcf(j,k-1)/(Lcf(j,k)-Lcf(j,k-1))
                  ay11c=dy*yLcf(j,k)/(Lcf(j,k)-Lcf(j,k-1))
            end if
            if (Lsf(j-1,k)-Lsf(j-1,k-1).eq.0.0d0) then
                  ay00s=0.0d0
                  ay01s=0.0d0
            else
                  ay00s=dy*yLsf(j-1,k-1)/(Lsf(j-1,k)-Lsf(j-1,k-1))
                  ay01s=dy*yLsf(j-1,k)/(Lsf(j-1,k)-Lsf(j-1,k-1))
            end if
            if (Lsf(j,k)-Lsf(j,k-1).eq.0.0d0) then
                  ay10s=0.0d0
                  ay11s=0.0d0
            else
                  ay10s=dy*yLsf(j,k-1)/(Lsf(j,k)-Lsf(j,k-1))
                  ay11s=dy*yLsf(j,k)/(Lsf(j,k)-Lsf(j,k-1))
            end if
            y0=(ylat(k)-elat(i))/dy
            y1=(elat(i)-ylat(k-1))/dy
            hy0=3.0d0*y0**2-2.0d0*y0**3
            hy1=3.0d0*y1**2-2.0d0*y1**3
            ky0=y1*y0**2
            ky1=-y0*y1**2
            p00c=hx0*hy0-ax00c*kx0*hy0-ax10c*kx1*hy0
     1            -ay00c*hx0*ky0-ay01c*hx0*ky1
            p10c=hx1*hy0+ax00c*kx0*hy0+ax10c*kx1*hy0
     1            -ay10c*hx1*ky0-ay11c*hx1*ky1
            p01c=hx0*hy1-ax01c*kx0*hy1-ax11c*kx1*hy1
     1            +ay00c*hx0*ky0+ay01c*hx0*ky1
            p11c=hx1*hy1+ax01c*kx0*hy1+ax11c*kx1*hy1
     1            +ay10c*hx1*ky0+ay11c*hx1*ky1
            p00s=hx0*hy0-ax00s*kx0*hy0-ax10s*kx1*hy0
     1            -ay00s*hx0*ky0-ay01s*hx0*ky1
            p10s=hx1*hy0+ax00s*kx0*hy0+ax10s*kx1*hy0
     1            -ay10s*hx1*ky0-ay11s*hx1*ky1
            p01s=hx0*hy1-ax01s*kx0*hy1-ax11s*kx1*hy1
     1            +ay00s*hx0*ky0+ay01s*hx0*ky1
            p11s=hx1*hy1+ax01s*kx0*hy1+ax11s*kx1*hy1
     1            +ay10s*hx1*ky0+ay11s*hx1*ky1
            Lc(i)=p00c*Lcf(j-1,k-1)+p10c*Lcf(j,k-1)
     1            +p01c*Lcf(j-1,k)+p11c*Lcf(j,k)
            Lcc(i)=p00c*Lccf(j-1,k-1)+p10c*Lccf(j,k-1)
     1            +p01c*Lccf(j-1,k)+p11c*Lccf(j,k)
            Lcs(i)=p00c*Lcsf(j-1,k-1)+p10c*Lcsf(j,k-1)
     1            +p01c*Lcsf(j-1,k)+p11c*Lcsf(j,k)
            Ls(i)=p00s*Lsf(j-1,k-1)+p10s*Lsf(j,k-1)
     1            +p01s*Lsf(j-1,k)+p11s*Lsf(j,k)
            Lsc(i)=p00s*Lscf(j-1,k-1)+p10s*Lscf(j,k-1)
     1            +p01s*Lscf(j-1,k)+p11s*Lscf(j,k)
            Lss(i)=p00s*Lssf(j-1,k-1)+p10s*Lssf(j,k-1)
     1            +p01s*Lssf(j-1,k)+p11s*Lssf(j,k)
90          continue
      end do
c
      return
      end
c
c
      FUNCTION pinr(npr,rlong,rlat,long,lat)
c
c This determines whether the point at (long,lat) is inside the region.
c
      implicit none
      logical pinr
      integer npr
      integer i,nlt,ngt,im
      real*8 long,lat,ilong
      real*8 rlong(npr),rlat(npr)
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
      SUBROUTINE getfL(rE,ne,elong,elat,Lc,Lcc,Lcs,Ls,Lsc,Lss,nfL,
     1      ffL,nflp,pfL,gpf,long,lat,yfLm,yfLp,fLcm,fLsm,fLcp,fLsp,
     2      fafLcm,fafLsm,fafLcp,fafLsp,azfLcm,azfLsm,azfLcp,azfLsp)
c
      implicit none
      integer maxsl,maxpl,maxf,maxgp,maxe
      parameter(maxsl=200,maxpl=maxsl+1,maxf=600,
     1      maxgp=40000,maxe=2*maxgp)
      integer ne,nfL
      integer i,k,j,f,p,gp
      integer ffL(maxf),nfLp(maxf),pfL(maxpl,maxf)
      integer gpf(0:maxsl,maxf)
      real*8 rE
      real*8 pi,aconv,along,alat,x,y,z,xn,yn,zn,xe,ye,ze
      real*8 w,wLc,wLcc,wLcs,wLs,wLsc,wLss,x1,y1,z1,x0,y0,z0
      real*8 xx,xy,xz,det,tx,ty,tz,px,py,pz,tr0,xr0,tr1,xr1
      real*8 tr,pr,xr,tn,pn,te,pe,azf,fx0,fx1,fx,fy,adj
      real*8 c0,s0,fac0,fas0,azc0,azs0,c1,s1,fac1,fas1,azc1,azs1
      real*8 cc0,cs0,sc0,ss0,cc1,cs1,sc1,ss1,wf,wd,p0,p1
      real*8 elong(maxe),elat(maxe)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
      real*8 long(maxgp),lat(maxgp)
      real*8 yfLm(maxpl,maxf),yfLp(maxpl,maxf)
      real*8 fLcm(maxpl,maxf),fLsm(maxpl,maxf),
     1      fLcp(maxpl,maxf),fLsp(maxpl,maxf)
      real*8 fafLcm(maxpl,maxf),fafLsm(maxpl,maxf),
     1      fafLcp(maxpl,maxf),fafLsp(maxpl,maxf)
      real*8 azfLcm(maxpl,maxf),azfLsm(maxpl,maxf),
     1      azfLcp(maxpl,maxf),azfLsp(maxpl,maxf)
c
      pi=4.0d0*datan(1.0d0)
      aconv=pi/180.0d0
c
      do i=1,ne
            along=aconv*elong(i)
            alat=aconv*elat(i)
            x=dcos(alat)*dcos(along)
            y=dcos(alat)*dsin(along)
            z=dsin(alat)
            xn=-dsin(alat)*dcos(along)
            yn=-dsin(alat)*dsin(along)
            zn=dcos(alat)
            xe=-dsin(along)
            ye=dcos(along)
            ze=0.0d0
            w=1.0d0
            wLc=Lc(i)
            wLcc=Lcc(i)
            wLcs=Lcs(i)
            wLs=Ls(i)
            wLsc=Lsc(i)
            wLss=Lss(i)
            do k=1,nfL
                  f=ffL(k)
                  p=pfL(1,k)
                  gp=gpf(p,f)
                  along=aconv*long(gp)
                  alat=aconv*lat(gp)
                  x1=dcos(alat)*dcos(along)
                  y1=dcos(alat)*dsin(along)
                  z1=dsin(alat)
                  do j=2,nfLp(k)
                        x0=x1
                        y0=y1
                        z0=z1
                        p=pfL(j,k)
                        gp=gpf(p,f)
                        along=aconv*long(gp)
                        alat=aconv*lat(gp)
                        x1=dcos(alat)*dcos(along)
                        y1=dcos(alat)*dsin(along)
                        z1=dsin(alat)
                        xx=x0+x1
                        xy=y0+y1
                        xz=z0+z1
                        det=dsqrt(xx**2+xy**2+xz**2)
                        xx=xx/det
                        xy=xy/det
                        xz=xz/det
                        tx=x1-x0
                        ty=y1-y0
                        tz=z1-z0
                        det=dsqrt(tx**2+ty**2+tz**2)
                        tx=tx/det
                        ty=ty/det
                        tz=tz/det
                        px=xy*tz-xz*ty
                        py=xz*tx-xx*tz
                        pz=xx*ty-xy*tx
                        tr0=tx*x0+ty*y0+tz*z0
                        xr0=xx*x0+xy*y0+xz*z0
                        tr1=tx*x1+ty*y1+tz*z1
                        xr1=xx*x1+xy*y1+xz*z1
                        tr=tx*x+ty*y+tz*z
                        pr=px*x+py*y+pz*z
                        xr=xx*x+xy*y+xz*z
                        if (1.0d0+xr.eq.0.0d0) goto 10
                        tn=tx*xn+ty*yn+tz*zn
                        pn=px*xn+py*yn+pz*zn
                        te=tx*xe+ty*ye
                        pe=px*xe+py*ye
                        if (dabs(tn-pe)+dabs(te+pn).eq.0.0d0)
     1                        goto 10
                        azf=datan2(te+pn,tn-pe)
                        fx0=2.0d0*rE*tr0/(1.0d0+xr0)
                        fx1=2.0d0*rE*tr1/(1.0d0+xr1)
                        fx=2.0d0*rE*tr/(1.0d0+xr)
                        fy=2.0d0*rE*pr/(1.0d0+xr)
                        fx1=fx1-fx0
                        fx=fx-fx0
                        if (fy.lt.0.0d0) then
                              adj=(yfLm(j-1,k)+yfLm(j,k))/pi
                              c0=fLcm(j-1,k)
                              s0=fLsm(j-1,k)
                              fac0=fafLcm(j-1,k)
                              fas0=fafLsm(j-1,k)
                              azc0=aconv*azfLcm(j-1,k)
                              azs0=aconv*azfLsm(j-1,k)
                              c1=fLcm(j,k)
                              s1=fLsm(j,k)
                              fac1=fafLcm(j,k)
                              fas1=fafLsm(j,k)
                              azc1=aconv*azfLcm(j,k)
                              azs1=aconv*azfLsm(j,k)
                        else
                              adj=(yfLp(j-1,k)+yfLp(j,k))/pi
                              c0=fLcp(j-1,k)
                              s0=fLsp(j-1,k)
                              fac0=fafLcp(j-1,k)
                              fas0=fafLsp(j-1,k)
                              azc0=aconv*azfLcp(j-1,k)
                              azs0=aconv*azfLsp(j-1,k)
                              c1=fLcp(j,k)
                              s1=fLsp(j,k)
                              fac1=fafLcp(j,k)
                              fas1=fafLsp(j,k)
                              azc1=aconv*azfLcp(j,k)
                              azs1=aconv*azfLsp(j,k)
                        end if
                        cc0=c0*fac0*dcos(2.0d0*(azf+azc0))
                        cs0=c0*fac0*dsin(2.0d0*(azf+azc0))
                        sc0=s0*fas0*dcos(4.0d0*(azf+azs0))
                        ss0=s0*fas0*dsin(4.0d0*(azf+azs0))
                        cc1=c1*fac1*dcos(2.0d0*(azf+azc1))
                        cs1=c1*fac1*dsin(2.0d0*(azf+azc1))
                        sc1=s1*fas1*dcos(4.0d0*(azf+azs1))
                        ss1=s1*fas1*dsin(4.0d0*(azf+azs1))
                        if (dabs(fy).lt.fx1*1.0d-8) then
                              wf=-adj*fy**2*(1/(fx1-fx)**3
     1                              +1/fx**3)/3.0d0
                        else
                              wf=adj*0.5d0*((datan((fx1-fx)/fy)
     1                              +datan(fx/fy))/fy
     2                              +(fx1-fx)/((fx1-fx)**2+fy**2)
     3                              +fx/(fx**2+fy**2))
                        end if
                        wd=adj*0.50d0*(1/(fx**2+fy**2)
     1                        -1/((fx1-fx)**2+fy**2))*fy**2
                        p0=(fx1-fx)/fx1
                        p1=fx/fx1
                        w=w+wf
                        wLc=wLc+wf*(p0*c0+p1*c1)+wd*(c1-c0)/fx1
                        wLcc=wLcc+wf*(p0*cc0+p1*cc1)+wd*(cc1-cc0)/fx1
                        wLcs=wLcs+wf*(p0*cs0+p1*cs1)+wd*(cs1-cs0)/fx1
                        wLs=wLs+wf*(p0*s0+p1*s1)+wd*(s1-s0)/fx1
                        wLsc=wLsc+wf*(p0*sc0+p1*sc1)+wd*(sc1-sc0)/fx1
                        wLss=wLss+wf*(p0*ss0+p1*ss1)+wd*(ss1-ss0)/fx1
10                      continue
                  end do
            end do
            Lc(i)=wLc/w
            Lcc(i)=wLcc/w
            Lcs(i)=wLcs/w
            Ls(i)=wLs/w
            Lsc(i)=wLsc/w
            Lss(i)=wLss/w
      end do
c
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c

