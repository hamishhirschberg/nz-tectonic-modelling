      SUBROUTINE writei(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1s,gp2s,s1e,s2e,s3e,nfs,sf,
     2      nvlgp,nvls,nvlf,gpvl,svl,fvl,nrbs,srb,
     3      ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,nece,eec,interp,
     4      rE,long,lat,sxxpot,syypot,sxypot,Lc,Lcc,Lcs,Ls,Lsc,Lss,
     5      Kc,Ks,ux,uy,uxm,uym,uxp,uyp,plat,plong,prate,
     6      volong,volat,oux,ouy,seoux,seouy,rouxuy,
     7      codut,codun,oduc,seoduc,rfo12,
     8      coexx,coeyy,coexy,oec,seoec,reo12,reo13,reo23,
     9      cdut,cdun,refduc,scduc,cexx,ceyy,cexy,refec,scec)
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     5      maxduc=2*maxf,maxec=100,maxece=500)
      integer ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      integer gp,s
      integer i,j,nj,vptype,vp,j2
      integer gp1s(maxs),gp2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer nfs(maxf),sf(maxfs,maxf)
      integer nvlgp(maxvl),nvls(maxvl),nvlf(maxvl)
      integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),srb(maxrbs,maxrb)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),nducs(maxduc),sduc(maxfs,maxduc)
      integer nece(maxec),eec(maxece,maxec)
      logical interp(maxvlj,maxvl)
      real*8 rE
      real*8 long(maxgp),lat(maxgp)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
      real*8 Kc(maxfs,maxf),Ks(maxfs,maxf)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 plat(maxrb),plong(maxrb),prate(maxrb)
      real*8 volong(maxvo),volat(maxvo),
     1      oux(maxvo),ouy(maxvo),seoux(maxvo),seouy(maxvo),
     2      rouxuy(maxvo)
      real*8 codut(2,maxfo),codun(2,maxfo),
     1      oduc(2,maxfo),seoduc(2,maxfo)
      real*8 rfo12(maxfo)
      real*8 coexx(3,maxeo),coeyy(3,maxeo),coexy(3,maxeo),
     1      oec(3,maxeo),seoec(3,maxeo)
      real*8 reo12(maxeo),reo13(maxeo),reo23(maxeo)
      real*8 cdut(maxfs,maxduc),cdun(maxfs,maxduc),
     1      refduc(maxfs,maxduc),scduc(maxfs,maxduc)
      real*8 cexx(maxece,maxec),ceyy(maxece,maxec),cexy(maxece,maxec),
     1      refec(maxece,maxec),scec(maxece,maxec)
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
            nj=nvlgp(i)+nvls(i)+nvlf(i)
            do j=1,nj
                  j2=j/2
                  if (j.eq.2*j2+1) then
                        if (fvl(j2,i).eq.0) then
                              vptype=1
                        else
                              vptype=3
                        end if
                  else
                        vptype=2
                  end if
                  if (interp(j,i)) vptype=vptype*(-1)
                  if (vptype.eq.1) then
                        gp=j/2
                        vp=gpvl(gp,i)
                        write(1,*) vptype,vp
                        write(1,*) ux(j,i),uy(j,i)
                  end if
                  if (vptype.eq.2) then
                        s=j/2
                        vp=svl(s,i)
                        write(1,*) vptype,vp
                        write(1,*) ux(j,i),uy(j,i)
                  end if
                  if (vptype.eq.3) then
                        gp=j/2
                        vp=gpvl(gp,i)
                        write(1,*) vptype,vp
                        write(1,*) fvl(gp,i),uxm(gp,i),uym(gp,i),
     1                        uxp(gp,i),uyp(gp,i)
                  end if
                  if (vptype.eq.-1) then
                        gp=j/2
                        vp=gpvl(gp,i)
                        write(1,*) vptype,vp
                  end if
                  if (vptype.eq.-2) then
                        s=j/2
                        vp=svl(s,i)
                        write(1,*) vptype,vp
                  end if
                  if (vptype.eq.-3) then
                        gp=j/2
                        vp=gpvl(gp,i)
                        write(1,*) vptype,vp
                        write(1,*) fvl(gp,i)
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
      close(1)
c
      return
      end
c
