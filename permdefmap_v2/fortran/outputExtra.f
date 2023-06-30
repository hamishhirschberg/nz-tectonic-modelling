      SUBROUTINE outex()
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,maxfp,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxefv,maxx,maxy,maxz
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     5      maxduc=2*maxf,maxec=100,maxece=500,
     6      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     7      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     8      maxpvl=maxsvl+maxvl,maxprb=maxsrb+maxrb,
     9      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     1      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     1      maxefv=12*(maxsf+maxsvl),
     2      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     3      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     4      +2*(maxpf+maxfT+maxXf)),
     5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     6      +maxfs*maxduc+maxece*maxec,
     7      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      integer npf,npvl,nprb,nsf,nsvl,nsrb,ntj
      integer nJvl,nfT,nTf,nTvl,nXf,nXvl,nefv
      integer nz,nx1,nx,nx2,nx0,ny,ny0,ny1
      integer i,j,k,l,gp1,gp2,gp3,s1,s2,s3,lun
      integer e1s(maxs),e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl),nvlj(maxvl)
      integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),srb(maxrbs,maxrb)
      integer gprb(0:maxrbs,maxrb)
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),nducs(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer nece(maxec),eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer nfp(maxpf),fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer vls(maxsvl),svls(maxsvl)
      integer nvlp(maxpvl),vlp(2,maxpvl),gpvlp(2,maxpvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer rbp(maxprb),gprbp(maxprb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer sz(maxz),kz(maxz),jz(maxz)
      integer kx(maxx),ix(maxx),jx(maxx),jcx(maxx),icx(maxx)
      integer envo(maxe),eneo(maxe),enec(maxe)
      integer fsnfo(maxfs,maxf),fsnduc(maxfs,maxf)
      logical extern(maxgp)
      real*8 rE
      real*8 sy00,s00,s10,sy01,s01,s11
      real*8 long(maxgp),lat(maxgp)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 volong(maxvo),volat(maxvo)
      real*8 oux(maxvo),ouy(maxvo)
      real*8 oduc(2,maxfo)
      real*8 oec(3,maxeo)
      real*8 area(maxe)
      real*8 exx(2,0:9,maxe),eyy(2,0:9,maxe),exy(2,0:9,maxe)
      real*8 sxx(2,0:9,maxe),syy(2,0:9,maxe),sxy(2,0:9,maxe)
      real*8 fx(maxe),fy(maxe)
      real*8 Lce(maxe),Lcce(maxe),Lcse(maxe),
     1      Lse(maxe),Lsce(maxe),Lsse(maxe)
      real*8 Kcfp(0:maxfs,maxf),Ksfp(0:maxfs,maxf)
      real*8 lenfs(maxfs,maxf),dufs(0:1,maxfs,maxf)
      real*8 ttfs(0:1,maxfs,maxf),tnfs(0:1,maxfs,maxf)
      real*8 Kcfs(maxfs,maxf),Ksfs(maxfs,maxf)
      real*8 Lcgp(maxgp),Lccgp(maxgp),Lcsgp(maxgp),
     1      Lsgp(maxgp),Lscgp(maxgp),Lssgp(maxgp)
      real*8 Lcf(0:maxfs,maxf),Lccf(0:maxfs,maxf),Lcsf(0:maxfs,maxf),
     1      Lsf(0:maxfs,maxf),Lscf(0:maxfs,maxf),Lssf(0:maxfs,maxf)
      real*8 Lctj(2,maxtj),Lcctj(2,maxtj),Lcstj(2,maxtj),
     1      Lstj(2,maxtj),Lsctj(2,maxtj),Lsstj(2,maxtj)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb),usrb(2,maxrbs,maxrb)
      real*8 seou(2,2,maxvo)
      real*8 seodu(2,2,maxfo)
      real*8 seoe(3,3,maxeo)
      real*8 z(maxz)
      real*8 y0(maxy),y(maxy),y1(maxy)
      real*8 xc(maxx),x0(maxx),x1(maxx),xd(maxx)
      real*8 ugpc(2,0:2,maxgp),uec(2,maxe)
      real*8 ufc(2,0:2,0:maxfs,maxf),uvlc(2,0:2,0:maxvls,maxvl)
      real*8 utjc(2,0:2,2,maxtj)
      real*8 uJvlc(2,0:2,3,maxJvl),ufTc(2,0:2,3,maxfT),
     1      uTfc(2,0:2,3,maxTf),uTvlc(2,0:2,3,maxTvl),
     2      uXfc(2,0:2,3,maxXf),uXvlc(2,0:2,3,maxXvl)
      real*8 dufc(2,0:1,0:maxfs,maxf)
      real*8 dufTc(2,0:1,maxfT),duXfc(2,0:1,maxXf)
      real*8 ugp0(2,0:2,maxgp),ue0(2,maxe)
      real*8 uf0(2,0:2,0:maxfs,maxf),uvl0(2,0:2,0:maxvls,maxvl)
      real*8 utj0(2,0:2,2,maxtj)
      real*8 uJvl0(2,0:2,3,maxJvl),ufT0(2,0:2,3,maxfT),
     1      uTf0(2,0:2,3,maxTf),uTvl0(2,0:2,3,maxTvl),
     2      uXf0(2,0:2,3,maxXf),uXvl0(2,0:2,3,maxXvl)
      real*8 duf0(2,0:1,0:maxfs,maxf)
      real*8 dufT0(2,0:1,maxfT),duXf0(2,0:1,maxXf)
      real*8 ugp1(2,0:2,maxgp),ue1(2,maxe)
      real*8 uf1(2,0:2,0:maxfs,maxf),uvl1(2,0:2,0:maxvls,maxvl)
      real*8 utj1(2,0:2,2,maxtj)
      real*8 uJvl1(2,0:2,3,maxJvl),ufT1(2,0:2,3,maxfT),
     1      uTf1(2,0:2,3,maxTf),uTvl1(2,0:2,3,maxTvl),
     2      uXf1(2,0:2,3,maxXf),uXvl1(2,0:2,3,maxXvl)
      real*8 duf1(2,0:1,0:maxfs,maxf)
      real*8 dufT1(2,0:1,maxfT),duXf1(2,0:1,maxXf)
      real*8 ugpd(2,0:2,maxgp),ued(2,maxe)
      real*8 ufd(2,0:2,0:maxfs,maxf),uvld(2,0:2,0:maxvls,maxvl)
      real*8 utjd(2,0:2,2,maxtj)
      real*8 uJvld(2,0:2,3,maxJvl),ufTd(2,0:2,3,maxfT),
     1      uTfd(2,0:2,3,maxTf),uTvld(2,0:2,3,maxTvl),
     2      uXfd(2,0:2,3,maxXf),uXvld(2,0:2,3,maxXvl)
      real*8 dufd(2,0:1,0:maxfs,maxf)
      real*8 dufTd(2,0:1,maxfT),duXfd(2,0:1,maxXf)
      real*8 elong(maxe),elat(maxe)
      real*8 slong(maxs),slat(maxs)
c
      open(1,file='output_control.bin',form='unformatted')
      read(1) ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      read(1) npf,npvl,nprb,nsf,nsvl,nsrb,ntj
      read(1) nJvl,nfT,nTf,nTvl,nXf,nXvl,nefv
      read(1) nz,nx
      read(1) (e1s(i),e2s(i),i=1,ns)
      read(1) (s1e(i),s2e(i),s3e(i),i=1,ne)
      read(1) (gp1e(i),gp2e(i),gp3e(i),i=1,ne)
      read(1) (nfs(i),i=1,nf)
      read(1) ((sf(j,i),j=1,nfs(i)),i=1,nf)
      read(1) ((gpf(j,i),j=0,nfs(i)),i=1,nf)
      read(1) (nvls(i),i=1,nvl)
      read(1) ((gpvl(j,i),j=0,nvls(i)),i=1,nvl)
      read(1) ((svl(j,i),j=1,nvls(i)),i=1,nvl)
      read(1) ((fvl(j,i),j=0,nvls(i)),i=1,nvl)
      read(1) (nrbs(i),i=1,nrb)
      read(1) ((srb(j,i),j=1,nrbs(i)),i=1,nrb)
      read(1) ((gprb(j,i),j=0,nrbs(i)),i=1,nrb)
      read(1) (evo(i),i=1,nvo)
      read(1) (ffo(i),sfo(i),nfoc(i),i=1,nfo)
      read(1) (eeo(i),neoc(i),i=1,neo)
      read(1) (fduc(i),nducs(i),i=1,nduc)
      read(1) ((sduc(j,i),j=1,nducs(i)),i=1,nduc)
      read(1) (ncduc(i),i=1,nduc)
      read(1) ((s1duc(j,i),s2duc(j,i),j=1,ncduc(i)),i=1,nduc)
      read(1) (nece(i),i=1,nec)
      read(1) ((eec(j,i),j=1,nece(i)),i=1,nec)
      read(1) (ncec(i),i=1,nec)
      read(1) ((e1ec(j,i),e2ec(j,i),j=1,ncec(i)),i=1,nec)
      read(1) (ipf(i),ipvl(i),iprb(i),i=1,ngp)
      read(1) (isf(i),isvl(i),isrb(i),i=1,ns)
      read(1) (fs(i),sfs(i),i=1,nsf)
      read(1) (nfp(i),i=1,npf)
      read(1) ((fp(j,i),gpfp(j,i),j=1,nfp(i)),i=1,npf)
      read(1) (jfp(i),tjfp(i),i=1,npf)
      read(1) (vls(i),svls(i),i=1,nsvl)
      read(1) (nvlp(i),i=1,npvl)
      read(1) ((vlp(j,i),gpvlp(j,i),j=1,nvlp(i)),i=1,npvl)
      read(1) (rbs(i),srbs(i),i=1,nsrb)
      read(1) (rbp(i),gprbp(i),i=1,nprb)
      read(1) (Jvlvlp(i),fTvlp(i),Tfvlp(i),i=1,npvl)
      read(1) (Tvlvlp(i),Xfvlp(i),Xvlvlp(i),i=1,npvl)
      read(1) (iefv(i),i=1,ne)
      read(1) (kgp1fv(i),kgp2fv(i),kgp3fv(i),i=1,nefv)
      read(1) (igp1fv(i),igp2fv(i),igp3fv(i),i=1,nefv)
      read(1) (jgp1fv(i),jgp2fv(i),jgp3fv(i),i=1,nefv)
      read(1) (rgp1fv(i),rgp2fv(i),rgp3fv(i),i=1,nefv)
      read(1) ((rgprb(j,i),j=0,nrbs(i)),i=1,nrb)
      read(1) (sz(i),kz(i),jz(i),i=1,nz)
      read(1) (kx(i),ix(i),jx(i),jcx(i),icx(i),i=1,nx)
      read(1) (extern(i),i=1,ngp)
      read(1) rE
      read(1) (long(i),lat(i),i=1,ngp)
      read(1) (sxxpot(i),syypot(i),sxypot(i),i=1,ngp)
      do i=1,nvl
            nvlj(i)=2*nvls(i)+1
      end do
      read(1) ((ux(j,i),uy(j,i),j=1,nvlj(i)),i=1,nvl)
      read(1) ((uxm(j,i),uym(j,i),j=0,nvls(i)),i=1,nvl)
      read(1) ((uxp(j,i),uyp(j,i),j=0,nvls(i)),i=1,nvl)
      read(1) (volong(i),volat(i),i=1,nvo)
      read(1) (oux(i),ouy(i),i=1,nvo)
      read(1) ((oduc(j,i),j=1,nfoc(i)),i=1,nfo)
      read(1) ((oec(j,i),j=1,neoc(i)),i=1,neo)
      read(1) (area(i),i=1,ne)
      read(1) (((exx(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (((eyy(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (((exy(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (((sxx(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (((syy(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (((sxy(k,j,i),k=1,2),j=0,8),i=1,ne)
      read(1) (fx(i),fy(i),i=1,ne)
      read(1) (Lce(i),Lcce(i),Lcse(i),i=1,ne)
      read(1) (Lse(i),Lsce(i),Lsse(i),i=1,ne)
      read(1) ((Kcfp(j,i),Ksfp(j,i),j=0,nfs(i)),i=1,nf)
      read(1) ((lenfs(j,i),j=1,nfs(i)),i=1,nf)
      read(1) (((dufs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      read(1) (((ttfs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      read(1) (((tnfs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      read(1) ((Kcfs(j,i),Ksfs(j,i),j=1,nfs(i)),i=1,nf)
      read(1) (Lcgp(i),Lccgp(i),Lcsgp(i),i=1,ngp)
      read(1) (Lsgp(i),Lscgp(i),Lssgp(i),i=1,ngp)
      read(1) ((Lcf(j,i),Lccf(j,i),Lcsf(j,i),j=0,nfs(i)),i=1,nf)
      read(1) ((Lsf(j,i),Lscf(j,i),Lssf(j,i),j=0,nfs(i)),i=1,nf)
      read(1) ((Lctj(j,i),Lcctj(j,i),Lcstj(j,i),j=1,2),i=1,ntj)
      read(1) ((Lstj(j,i),Lsctj(j,i),Lsstj(j,i),j=1,2),i=1,ntj)
      read(1) ((((ugprb(l,k,j,i),l=1,2),k=0,2),j=0,nrbs(i)),i=1,nrb)
      read(1) (((usrb(k,j,i),k=1,2),j=1,nrbs(i)),i=1,nrb)
      read(1) (((seou(k,j,i),k=1,2),j=1,2),i=1,nvo)
      read(1) (((seodu(k,j,i),k=1,nfoc(i)),j=1,nfoc(i)),i=1,nfo)
      read(1) (((seoe(k,j,i),k=1,neoc(i)),j=1,neoc(i)),i=1,neo)
c
      open(2,file='invert_output.bin',form='unformatted')
      read(2) nz,ny,ny0,ny1,nx,nx1,nx2,nx0
      read(2) sy00,s00,s10
      read(2) sy01,s01,s11
      read(2) (xc(i),i=1,nx)
      read(2) (z(i),i=1,nz)
      read(2) (x0(i),i=1,nx)
      read(2) (y0(i),i=1,ny0)
      read(2) (y(i),i=1,ny)
      read(2) (x1(i),i=1,nx)
      read(2) (y1(i),i=1,ny)
c
      do i=1,nx
            xd(i)=x1(i)-x0(i)
      end do
c
      call loadu(nx,kx,ix,jx,jcx,icx,xc,ugpc,uec,ufc,uvlc,
     1      utjc,uJvlc,ufTc,uTfc,uTvlc,uXfc,uXvlc,dufc,dufTc,duXfc)
      call loadu(nx,kx,ix,jx,jcx,icx,x0,ugp0,ue0,uf0,uvl0,
     1      utj0,uJvl0,ufT0,uTf0,uTvl0,uXf0,uXvl0,duf0,dufT0,duXf0)
      call loadu(nx,kx,ix,jx,jcx,icx,x1,ugp1,ue1,uf1,uvl1,
     1      utj1,uJvl1,ufT1,uTf1,uTvl1,uXf1,uXvl1,duf1,dufT1,duXf1)
      call loadu(nx,kx,ix,jx,jcx,icx,xd,ugpd,ued,ufd,uvld,
     1      utjd,uJvld,ufTd,uTfd,uTvld,uXfd,uXvld,dufd,dufTd,duXfd)
c
      do i=1,ne
            gp1=gp1e(i)
            gp2=gp2e(i)
            gp3=gp3e(i)
            s1=s1e(i)
            s2=s2e(i)
            s3=s3e(i)
            elong(i)=(long(gp1)+long(gp2)+long(gp3))/3.0d0
            elat(i)=(lat(gp1)+lat(gp2)+lat(gp3))/3.0d0
            slong(s1)=0.5d0*(long(gp2)+long(gp3))
            slat(s1)=0.5d0*(lat(gp2)+lat(gp3))
            slong(s2)=0.5d0*(long(gp3)+long(gp1))
            slat(s2)=0.5d0*(lat(gp3)+lat(gp1))
            slong(s3)=0.5d0*(long(gp1)+long(gp2))
            slat(s3)=0.5d0*(lat(gp1)+lat(gp2))
      end do
c
c~       call numobs(ne,nf,nfs,isf,sfs,nvo,nfo,neo,nduc,nec,evo,
c~      1      ffo,sfo,nfoc,eeo,neoc,fduc,sduc,ncduc,s1duc,s2duc,
c~      2      eec,ncec,e1ec,e2ec,envo,eneo,enec,fsnfo,fsnduc)
c
      lun=3
c
c~       open(lun,file='grid_geometry.log')
c~       call mesh(lun,ngp,ns,ne,nf,nvl,nrb,
c~      1      long,lat,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
c~      2      nfs,sf,gpf,nvls,svl,gpvl,fvl,nrbs,srb,gprb)
c~       write(lun,*)
c~       close(lun)
c
c~       open(lun,file='forces_and_rate_capacities.log')
c~       call forces(lun,nf,ne,nfs,sf,gpf,slong,slat,
c~      1      long,lat,lenfs,Kcfs,Ksfs,area,elong,elat,
c~      2      fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse)
c~       write(lun,*)
c~       close(lun)
c
c~       open(lun,file='fixed_velocity_values.log')
c~       call fixedu(lun,nvl,nrb,nvls,fvl,svl,gpvl,
c~      1      nrbs,srb,gprb,slong,slat,long,lat,
c~      2      ux,uy,uxm,uym,uxp,uyp,usrb,ugprb)
c~       write(lun,*)
c~       close(lun)
c
c~       open(lun,file='solution_x_values.log')
c~       call xvalue(lun,nx,kx,ix,jx,jcx,icx,xc,x0,x1,xd)
c~       write(lun,*)
c~       close(lun)
c
      open(lun,file='constraint_part_of_solution_in_elements_extra.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugpc,uec,ufc,uvlc,utjc,uJvlc,ufTc,uTfc,uTvlc,uXfc,uXvlc)
      write(lun,*)
      close(lun)
c
c~       if (nf.gt.0) then
c~       open(lun,file='constraint_part_of_solution_on_faults.log')
c~       call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
c~      2      dufc,dufTc,duXfc)
c~       write(lun,*)
c~       close(lun)
c~       end if
c
      open(lun,file='apriori_solution_in_elements_extra.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugp0,ue0,uf0,uvl0,utj0,uJvl0,ufT0,uTf0,uTvl0,uXf0,uXvl0)
      write(lun,*)
      close(lun)
c
c~       if (nf.gt.0) then
c~       open(lun,file='apriori_solution_on_faults.log')
c~       call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
c~      2      duf0,dufT0,duXf0)
c~       write(lun,*)
c~       close(lun)
c~       end if
c
      open(lun,file='aposteriori_solution_in_elements_extra.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugp1,ue1,uf1,uvl1,utj1,uJvl1,ufT1,uTf1,uTvl1,uXf1,uXvl1)
      write(lun,*)
      close(lun)
c
c~       if (nf.gt.0) then
c~       open(lun,file='aposteriori_solution_on_faults.log')
c~       call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
c~      2      duf1,dufT1,duXf1)
c~       write(lun,*)
c~       close(lun)
c~       end if
c
      open(lun,file='aposteriori_minus_apriori_in_elements_extra.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugpd,ued,ufd,uvld,utjd,uJvld,ufTd,uTfd,uTvld,uXfd,uXvld)
      write(lun,*)
      close(lun)
c
c~       if (nf.gt.0) then
c~       open(lun,file='aposteriori_minus_apriori_on_faults.log')
c~       call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
c~      2      dufd,dufTd,duXfd)
c~       write(lun,*)
c~       close(lun)
c~       end if
c
c~       open(lun,file='misfit_to_constraints.log')
c~       call zerror(lun,nz,e1s,e2s,isf,isvl,isrb,fs,sfs,
c~      1      vls,svls,rbs,srbs,sz,kz,jz,ux,uy,usrb,slong,slat,z)
c~       write(lun,*)
c~       close(lun)
c~ c
c~       open(lun,file='misfit_to_dynamics.log')
c~ c
c~       write(lun,*) 'In the apriori solution the total sum of squares'
c~       write(lun,*) ' for the dynamics part is',sy00
c~       write(lun,*) 'In preparation for the statistical aposteriori'
c~       write(lun,*) ' solution, using observables as well as the'
c~       write(lun,*) ' dynamics part, the dynamics was scaled so that'
c~       write(lun,*) ' this sum of squares became the number of'
c~       write(lun,*) ' degrees of freedom in the model',nx2
c~       write(lun,*) 'The residual sum of squares in the dynamics part'
c~       write(lun,*) ' of the apriori solution is',s00
c~       write(lun,*) 'After the scaling this became',
c~      1      s00*dfloat(nx2)/sy00
c~       write(lun,*) 'In the aposteriori solution the total sum of'
c~       write(lun,*) ' squares for the scaled dynamics part is',sy01
c~       write(lun,*) 'In the aposteriori solution the residual sum of'
c~       write(lun,*) ' squares for the scaled dynamics part increased'
c~       write(lun,*) ' through inclusion of observables to',s01
c~ c
c~       call ymatch(lun,ny0,ne,nf,nfs,sf,elong,elat,slong,slat,
c~      1      y0,y,y1)
c~       write(lun,*)
c~       close(lun)
c~ c
c~       open(lun,file='misfit_to_observations_apriori.log')
c~ c
c~       write(lun,*) 'The number of actual observables is',ny1
c~       write(lun,*) 'With the apriori solution the residual sum of'
c~       write(lun,*) ' squares for (unused) actual observables is',s10
c~       write(lun,*) 'Therefore, the total residual sum of squares'
c~       write(lun,*) ' before aposteriori minimisation was',
c~      1      s10+s00*dfloat(nx2)/sy00
c~ c
c~       call misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
c~      1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
c~      2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
c~      3      eec,ncec,e1ec,e2ec,y)
c~       write(lun,*)
c~       close(lun)
c~ c
c~       open(lun,file='misfit_to_observations_aposteriori.log')
c~ c
c~       write(lun,*) 'The number of actual observables is',ny1
c~       write(lun,*) 'In the aposteriori solution the residual sum of'
c~       write(lun,*) ' squares for the actual observables is',s11
c~       write(lun,*) 'So the total residual sum of squares reduced'
c~       write(lun,*) ' through inclusion of observables to',s01+s11
c~ c
c~       call misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
c~      1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
c~      2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
c~      3      eec,ncec,e1ec,e2ec,y1)
c~       write(lun,*)
c~       close(lun)
c
      close(1)
      close(2)
      return
      end
c
c
      SUBROUTINE loadu(nx,kx,ix,jx,jcx,icx,x,ugp,ue,uf,uvl,
     1      utj,uJvl,ufT,uTf,uTvl,uXf,uXvl,duf,dufT,duXf)
c
c This routine loads a solution vector x into elements and grid points.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,maxvl,maxvls,
     1      maxsf,maxsvl,maxpf,maxpvl,
     2      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     3      maxx
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     4      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     5      maxtj=80,maxJvl=10,maxfT=10,
     6      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     7      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     8      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     9      +2*(maxpf+maxfT+maxXf)))
      integer nx
      integer xi,k,i,j,jc,ic
      integer kx(maxx),ix(maxx),jx(maxx),jcx(maxx),icx(maxx)
      real*8 x(maxx)
      real*8 ugp(2,0:2,maxgp),ue(2,maxe)
      real*8 uf(2,0:2,0:maxfs,maxf),uvl(2,0:2,0:maxvls,maxvl)
      real*8 utj(2,0:2,2,maxtj)
      real*8 uJvl(2,0:2,3,maxJvl),ufT(2,0:2,3,maxfT),
     1      uTf(2,0:2,3,maxTf),uTvl(2,0:2,3,maxTvl),
     2      uXf(2,0:2,3,maxXf),uXvl(2,0:2,3,maxXvl)
      real*8 duf(2,0:1,0:maxfs,maxf)
      real*8 dufT(2,0:1,maxfT),duXf(2,0:1,maxXf)
c
      do xi=1,nx
            k=kx(xi)
            i=ix(xi)
            j=jx(xi)
            jc=jcx(xi)
            ic=icx(xi)
            if (k.eq.10) then
                  ue(ic,i)=x(xi)
                  goto 10
            end if
            if (k.eq.0) then
                  ugp(ic,jc,i)=x(xi)
                  goto 10
            end if
            if (k.eq.1) then
                  uf(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.21) then
                  duf(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.2) then
                  uvl(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.3) then
                  utj(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.4) then
                  uJvl(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.5) then
                  ufT(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.6) then
                  uTf(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.7) then
                  uTvl(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.8) then
                  uXf(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.9) then
                  uXvl(ic,jc,j,i)=x(xi)
                  goto 10
            end if
            if (k.eq.25) then
                  dufT(ic,jc,i)=x(xi)
                  goto 10
            end if
            if (k.eq.28) then
                  duXf(ic,jc,i)=x(xi)
            end if
10          continue
      end do
c
      return
      end
c
c
c~       SUBROUTINE numobs(ne,nf,nfs,isf,sfs,nvo,nfo,neo,nduc,nec,evo,
c~      1      ffo,sfo,nfoc,eeo,neoc,fduc,sduc,ncduc,s1duc,s2duc,
c~      2      eec,ncec,e1ec,e2ec,envo,eneo,enec,fsnfo,fsnduc)
c~ c
c~ c This routine loads the numbers of observations into element arrays
c~ c  and fault-side arrays.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxe,maxf,maxfs,
c~      1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
c~      2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
c~      3      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,maxfp,
c~      4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
c~      5      maxefv,maxx,maxy,maxz
c~       parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
c~      3      maxrb=10,maxrbs=200,
c~      4      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
c~      5      maxduc=2*maxf,maxec=100,maxece=500,
c~      6      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
c~      7      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
c~      8      maxpvl=maxsvl+maxvl,maxprb=maxsrb+maxrb,
c~      9      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
c~      1      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
c~      1      maxefv=12*(maxsf+maxsvl),
c~      2      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
c~      3      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
c~      4      +2*(maxpf+maxfT+maxXf)),
c~      5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
c~      6      +maxfs*maxduc+maxece*maxec,
c~      7      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
c~       integer ne,nf,nvo,nfo,neo,nduc,nec
c~       integer i,j,e,f,s
c~       integer nfs(maxf)
c~       integer evo(maxvo)
c~       integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
c~       integer eeo(maxeo),neoc(maxeo)
c~       integer fduc(maxduc),sduc(maxfs,maxduc)
c~       integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
c~       integer eec(maxece,maxec)
c~       integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
c~       integer isf(maxs)
c~       integer sfs(maxsf)
c~       integer envo(maxe),eneo(maxe),enec(maxe)
c~       integer fsnfo(maxfs,maxf),fsnduc(maxfs,maxf)
c~ c
c~       do i=1,ne
c~             envo(i)=0
c~             eneo(i)=0
c~             enec(i)=0
c~       end do
c~ c
c~       if (nf.gt.0) then
c~       do i=1,nf
c~             do j=1,nfs(i)
c~                   fsnfo(j,i)=0
c~                   fsnduc(j,i)=0
c~             end do
c~       end do
c~       end if
c~ c
c~       if (nvo.gt.0) then
c~       do i=1,nvo
c~             e=evo(i)
c~             envo(e)=envo(e)+1
c~       end do
c~       end if
c~ c
c~       if (nfo.gt.0) then
c~       do i=1,nfo
c~             f=ffo(i)
c~             j=sfs(isf(sfo(i)))
c~             fsnfo(j,f)=fsnfo(j,f)+nfoc(i)
c~       end do
c~       end if
c~ c
c~       if (neo.gt.0) then
c~       do i=1,neo
c~             e=eeo(i)
c~             eneo(e)=eneo(e)+neoc(i)
c~       end do
c~       end if
c~ c
c~       if (nduc.gt.0) then
c~       do i=1,nduc
c~             f=fduc(i)
c~             do j=1,ncduc(i)
c~                   s=sfs(isf(sduc(s1duc(j,i),i)))
c~                   fsnduc(s,f)=fsnduc(s,f)+1
c~                   s=sfs(isf(sduc(s2duc(j,i),i)))
c~                   fsnduc(s,f)=fsnduc(s,f)+1
c~             end do
c~       end do
c~       end if
c~ c
c~       if (nec.gt.0) then
c~       do i=1,nec
c~             do j=1,ncec(i)
c~                   e=eec(e1ec(j,i),i)
c~                   enec(e)=enec(e)+1
c~                   e=eec(e2ec(j,i),i)
c~                   enec(e)=enec(e)+1
c~             end do
c~       end do
c~       end if
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE mesh(lun,ngp,ns,ne,nf,nvl,nrb,
c~      1      long,lat,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
c~      2      nfs,sf,gpf,nvls,svl,gpvl,fvl,nrbs,srb,gprb)
c~ c
c~ c This routine outputs to the file with unit number 'lun' the geometry
c~ c  of the mesh, including grid points, element sides, elements, faults,
c~ c  velocity lines and rigid boundaries.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxe,maxf,maxfs,
c~      1      maxvl,maxvls,maxrb,maxrbs
c~       parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,
c~      3      maxrb=10,maxrbs=200)
c~       integer lun,ngp,ns,ne,nf,nvl,nrb
c~       integer i,j
c~       integer e1s(maxs),e2s(maxs)
c~       integer s1e(maxe),s2e(maxe),s3e(maxe)
c~       integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
c~       integer nfs(maxf),sf(maxfs,maxf)
c~       integer gpf(0:maxfs,maxf)
c~       integer nvls(maxvl),nvlj(maxvl)
c~       integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
c~      1      fvl(0:maxvls,maxvl)
c~       integer nrbs(maxrb),srb(maxrbs,maxrb)
c~       integer gprb(0:maxrbs,maxrb)
c~       real*8 long(maxgp),lat(maxgp)
c~ c
c~       write(lun,*) ngp,' Grid points'
c~       write(lun,*) 'Each line = index,long,lat'
c~       do i=1,ngp
c~             write(lun,*) i,long(i),lat(i)
c~       end do
c~ c
c~       write(lun,*) ns,' Element sides'
c~       write(lun,*) 'Each line = index,element1,element2'
c~       write(lun,*) 'Sides with element2 = 0 are at external boundaries'
c~       do i=1,ns
c~             write(lun,*) i,e1s(i),e2s(i)
c~       end do
c~ c
c~       write(lun,*) ne,' Elements'
c~       write(lun,*) 'First line = index,side1,side2,side3'
c~       write(lun,*) 'Second line = grid-point1,grid-point2,grid-point3'
c~       write(lun,*) 'Grid-point1 is opposite side1, and so on'
c~       do i=1,ne
c~             write(lun,*) i,s1e(i),s2e(i),s3e(i)
c~             write(lun,*) gp1e(i),gp2e(i),gp3e(i)
c~       end do
c~ c
c~       write(lun,*) nf,' Faults'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = fault,index,element-side'
c~       write(lun,*) 'Second = first-grid-point,second-grid-point'
c~       if (nf.gt.0) then
c~       do i=1,nf
c~             write(lun,*) i,nfs(i)
c~             do j=1,nfs(i)
c~                   write(lun,*) i,j,sf(j,i)
c~                   write(lun,*) gpf(j-1,i),gpf(j,i)
c~             end do
c~       end do
c~       end if
c~ c
c~       write(lun,*) nvl,' Velocity lines'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = v-line,index,element-side'
c~       write(lun,*) 'Second = first-grid-point,fault-at-grid-point'
c~       write(lun,*) 'Third = second-grid-point,fault-at-grid-point'
c~       write(lun,*) 'Where there is no fault fault-at-grid-point = 0'
c~       if (nvl.gt.0) then
c~       do i=1,nvl
c~             write(lun,*) i,nvls(i)
c~             do j=1,nvls(i)
c~                   write(lun,*) i,j,svl(j,i)
c~                   write(lun,*) gpvl(j-1,i),fvl(j-1,i)
c~                   write(lun,*) gpvl(j,i),fvl(j,i)
c~             end do
c~       end do
c~       end if
c~ c
c~       write(lun,*) nrb,' Rigid boundaries'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = rigid-b,index,element-side'
c~       write(lun,*) 'Second = first-grid-point,second-grid-point'
c~       if (nrb.gt.0) then
c~       do i=1,nrb
c~             write(lun,*) i,nrbs(i)
c~             do j=1,nrbs(i)
c~                   write(lun,*) i,j,srb(j,i)
c~                   write(lun,*) gprb(j-1,i),gprb(j,i)
c~             end do
c~       end do
c~       end if
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE forces(lun,nf,ne,nfs,sf,gpf,slong,slat,
c~      1      long,lat,lenfs,Kcfs,Ksfs,area,elong,elat,
c~      2      fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse)
c~ c
c~ c This routine outputs to the file with unit number 'lun' slip-rate
c~ c  capacity values for fault segments and force and strain-rate capacity
c~ c  values in elements. Faults are done first because of the complexity
c~ c  and shear volume of strain-rate capacity information.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxe,maxf,maxfs
c~       parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
c~      1      maxf=600,maxfs=100)
c~       integer lun,nf,ne
c~       integer i,j,s,gp1,gp2
c~       integer nfs(maxf),sf(maxfs,maxf)
c~       integer gpf(0:maxfs,maxf)
c~       real*8 aconv,Lconv,dx,dy,a,az,f1,f2,g1,g2,azc
c~       real*8 s11,s22,s12,e11,e22,e12,s0
c~       real*8 long(maxgp),lat(maxgp)
c~       real*8 area(maxe)
c~       real*8 fx(maxe),fy(maxe)
c~       real*8 Lce(maxe),Lcce(maxe),Lcse(maxe),
c~      1      Lse(maxe),Lsce(maxe),Lsse(maxe)
c~       real*8 lenfs(maxfs,maxf)
c~       real*8 Kcfs(maxfs,maxf),Ksfs(maxfs,maxf)
c~       real*8 elong(maxe),elat(maxe)
c~       real*8 slong(maxs),slat(maxs)
c~       real*8 L(3,3),D(3),V(3,3)
c~ c
c~       aconv=45.0d0/datan(1.0d0)
c~       Lconv=dsqrt(2.0d0)
c~ c
c~       write(lun,*) nf,' Faults'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = fault,index,long,lat'
c~       write(lun,*) 'Second = length,az'
c~       write(lun,*) 'Third = Kc+Ks,Kc,Ks'
c~       write(lun,*) 'The total capacity Kc+Ks applies for strike-slip'
c~       write(lun,*) ' and Kc alone for the horizontal part of dip-slip'
c~       if (nf.gt.0) then
c~       do i=1,nf
c~             write(lun,*) i,nfs(i)
c~             do j=1,nfs(i)
c~                   s=sf(j,i)
c~                   write(lun,*) i,j,slong(s),slat(s)
c~                   gp1=gpf(j-1,i)
c~                   gp2=gpf(j,i)
c~                   dx=long(gp2)-long(gp1)
c~                   dy=lat(gp2)-lat(gp1)
c~                   dx=dx*dcos(slat(s)/aconv)
c~                   a=datan2(dy,dx)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) lenfs(j,i),az
c~                   write(lun,*) Kcfs(j,i)+Ksfs(j,i),Kcfs(j,i),
c~      1                  Ksfs(j,i)
c~             end do
c~       end do
c~       end if
c~ c
c~       write(lun,*) ne,' Elements'
c~       write(lun,*) 'First line = index,area,long,lat'
c~       write(lun,*) 'Second = fx,fy'
c~       write(lun,*) 'Third = az,f1'
c~       write(lun,*) 'Fourth = Lc,Lcc,Lcs'
c~       write(lun,*) 'Fifth = Ls,Lsc,Lss'
c~       write(lun,*) 'Sixth = sqrt(Lcc**2+Lcs**2)/Lc,az'
c~       write(lun,*) 'Seventh = sqrt(Lsc**2+Lss**2)/Ls,az'
c~       write(lun,*) 'First eigenvalue line = index,L'
c~       write(lun,*) 'Second eigenvalue line = sxx,syy,sxy'
c~       write(lun,*) 'Third eigenvalue line = exx,eyy,exy'
c~       write(lun,*) 'Fourth eigenvalue line = az,s11,s22'
c~       write(lun,*) 'Fifth eigenvalue line = az,e11,e22'
c~       write(lun,*) 'For stress and strain rate the azimuth az is for'
c~       write(lun,*) ' the first principal values s11 and e11'
c~       do i=1,ne
c~             write(lun,*) i,area(i),elong(i),elat(i)
c~             write(lun,*) fx(i),fy(i)
c~             f1=fx(i)

c~             f2=fy(i)
c~             if ((f1.eq.0.0d0).and.(f2.eq.0.0d0)) then
c~                   a=0.0d0
c~             else
c~                   a=datan2(f2,f1)
c~             end if
c~             f1=dsqrt(f1**2+f2**2)
c~             az=90.0d0-a*aconv
c~             write(lun,*) az,f1
c~             write(lun,*) Lce(i),Lcce(i),Lcse(i)
c~             write(lun,*) Lse(i),Lsce(i),Lsse(i)
c~             g1=Lcce(i)
c~             g2=Lcse(i)
c~             if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
c~                   az=90.0d0
c~             else
c~                   a=0.5d0*datan2(g2,g1)
c~                   g1=dsqrt(g1**2+g2**2)
c~                   az=a*aconv
c~             end if
c~             if (az.lt.0.0d0) az=az+180.0d0
c~             write(lun,*) g1/Lce(i),az
c~             azc=az
c~             g1=Lsce(i)
c~             g2=Lsse(i)
c~             if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
c~                   az=90.0d0
c~             else
c~                   a=0.25d0*datan2(g2,g1)
c~                   g1=dsqrt(g1**2+g2**2)
c~                   az=a*aconv
c~             end if
c~             if (az.lt.azc-45.0d0) az=az+90.0d0
c~             if (az.gt.azc+45.0d0) az=az-90.0d0
c~             write(lun,*) g1/Lse(i),az
c~             L(1,1)=0.5d0*(Lce(i)+Lcce(i))+0.125d0*(Lse(i)-Lsce(i))
c~             L(2,2)=0.5d0*(Lce(i)-Lcce(i))+0.125d0*(Lse(i)-Lsce(i))
c~             L(3,3)=0.25d0*Lce(i)+0.125d0*(Lse(i)+Lsce(i))
c~             L(1,2)=-0.125d0*(Lse(i)-Lsce(i))
c~             L(1,3)=-0.25d0*Lcse(i)+0.125d0*Lsse(i)
c~             L(2,3)=-0.25d0*Lcse(i)-0.125d0*Lsse(i)
c~             L(1,3)=L(1,3)*Lconv
c~             L(2,3)=L(2,3)*Lconv
c~             L(3,3)=L(3,3)*Lconv**2
c~             L(2,1)=L(1,2)
c~             L(3,1)=L(1,3)
c~             L(3,2)=L(2,3)
c~             call jacobi(3,3,L,D,V)
c~             do j=1,3
c~                   write(lun,*) j,D(j)
c~                   s11=V(1,j)
c~                   s22=V(2,j)
c~                   s12=V(3,j)/Lconv
c~                   e11=D(j)*s11
c~                   e22=D(j)*s22
c~                   e12=D(j)*s12
c~                   write(lun,*) s11,s22,s12
c~                   write(lun,*) e11,e22,e12
c~                   s0=0.5d0*(s11+s22)
c~                   g1=0.5d0*(s11-s22)
c~                   g2=s12
c~                   if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
c~                         a=0.0d0
c~                   else
c~                         a=0.5d0*datan2(g2,g1)
c~                   end if
c~                   g1=dsqrt(g1**2+g2**2)
c~                   az=90.0d0-a*aconv
c~                   s11=s0+g1
c~                   s22=s0-g1
c~                   e11=D(j)*s11
c~                   e22=D(j)*s22
c~                   write(lun,*) az,s11,s22
c~                   write(lun,*) az,e11,e22
c~             end do
c~       end do
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE jacobi(n,nmax,L,D,V)
c~ c
c~ c This routine calculates eigenvalues and eigenvectors of a real
c~ c  symmetric matrix using the Jacobi transformation method.
c~ c
c~       implicit none
c~       integer n,nmax
c~       integer i,im,j,iter,ip,jm,jp,k
c~       real*8 s,Lav,Lmag,Lmax,Lmin,t,x,y,Lk
c~       real*8 L(nmax,nmax),D(nmax),V(nmax,nmax)
c~       real*8 W(nmax),Z(nmax)
c~ c
c~       s=0.0d0
c~       do i=1,n
c~             s=s+L(i,i)
c~       end do
c~       Lav=s/dfloat(n)
c~       s=0.0d0
c~       do i=2,n
c~             im=i-1
c~             do j=1,im
c~                   s=s+dabs(L(j,i))
c~             end do
c~       end do
c~       s=2.0d0*s
c~       do i=1,n
c~             D(i)=L(i,i)-Lav
c~             s=s+dabs(D(i))
c~       end do
c~       if (s.eq.0.0d0) then
c~             do i=1,n
c~                   do j=1,n
c~                         V(j,i)=0.0d0
c~                   end do
c~                   V(i,i)=1.0d0
c~                   D(i)=Lav
c~             end do
c~             return
c~       end if
c~       Lmag=s/dfloat(n)
c~ c
c~       do i=1,n
c~             do j=1,n
c~                   V(j,i)=0.0d0
c~             end do
c~             V(i,i)=1.0d0
c~             D(i)=D(i)/Lmag
c~             W(i)=D(i)
c~             Z(i)=0.0d0
c~       end do
c~       Lmax=0.0d0
c~       do i=2,n
c~             im=i-1
c~             do j=1,im
c~                   L(j,i)=L(j,i)/Lmag
c~                   if (dabs(L(j,i)).gt.Lmax) Lmax=dabs(L(j,i))
c~             end do
c~       end do
c~       iter=0
c~ c
c~ 10    iter=iter+1
c~       Lmin=Lmax
c~       do i=1,iter
c~             if (Lmin.lt.(1.0d-16)) goto 20
c~             Lmin=0.5d0*Lmin
c~       end do
c~ 20    continue
c~       do i=2,n
c~             im=i-1
c~             ip=i+1
c~             do j=1,im
c~                   jm=j-1
c~                   jp=j+1
c~                   if (dabs(L(j,i)).lt.Lmin) goto 30
c~                   if (dabs(L(j,i)).le.(1.0d-16)) then
c~                         L(j,i)=0.0d0
c~                         goto 30
c~                   end if
c~                   t=0.5d0*(W(j)-W(i))
c~                   if (t.lt.0.0d0) then
c~                         t=-L(j,i)/(dsqrt(t**2+L(j,i)**2)-t)
c~                   else
c~                         t=L(j,i)/(dsqrt(t**2+L(j,i)**2)+t)
c~                   end if
c~                   W(j)=W(j)+t*L(j,i)
c~                   W(i)=W(i)-t*L(j,i)
c~                   Z(j)=Z(j)+t*L(j,i)
c~                   Z(i)=Z(i)-t*L(j,i)
c~                   L(j,i)=0.0d0
c~                   x=1.0d0/dsqrt(1.0d0+t**2)
c~                   y=t*x
c~                   t=y/(1.0d0+x)
c~                   if (jm.ge.1) then
c~                         do k=1,jm
c~                               Lk=L(k,j)
c~                               L(k,j)=Lk+y*(L(k,i)-t*Lk)
c~                               L(k,i)=L(k,i)-y*(Lk+t*L(k,i))
c~                         end do
c~                   end if
c~                   if (jp.le.im) then
c~                         do k=jp,im
c~                               Lk=L(j,k)
c~                               L(j,k)=Lk+y*(L(k,i)-t*Lk)
c~                               L(k,i)=L(k,i)-y*(Lk+t*L(k,i))
c~                         end do
c~                   end if
c~                   if (ip.le.n) then
c~                         do k=ip,n
c~                               Lk=L(j,k)
c~                               L(j,k)=Lk+y*(L(i,k)-t*Lk)
c~                               L(i,k)=L(i,k)-y*(Lk+t*L(i,k))
c~                         end do
c~                   end if
c~                   do k=1,n
c~                         Lk=V(k,j)
c~                         V(k,j)=Lk+y*(V(k,i)-t*Lk)
c~                         V(k,i)=V(k,i)-y*(Lk+t*V(k,i))
c~                   end do
c~ 30                continue
c~             end do
c~       end do
c~       do i=1,n

c~             D(i)=D(i)+Z(i)
c~             W(i)=D(i)
c~             Z(i)=0.0d0
c~       end do
c~       Lmax=0.0d0
c~       do i=2,n
c~             im=i-1
c~             do j=1,im
c~                   if (dabs(L(j,i)).gt.Lmax) Lmax=dabs(L(j,i))
c~             end do
c~       end do
c~       if (Lmax.gt.1.0d-16) goto 10
c~ c
c~       do i=n,2,-1
c~             k=i
c~             Lk=D(i)
c~             im=i-1
c~             do j=im,1,-1
c~                   L(j,i)=L(i,j)
c~                   if (D(j).lt.Lk) then
c~                         k=j
c~                         Lk=D(j)
c~                   end if
c~             end do
c~             if (k.ne.i) then
c~                   D(k)=D(i)
c~                   D(i)=Lk
c~                   do j=1,n
c~                         Lk=V(j,k)
c~                         V(j,k)=V(j,i)
c~                         V(j,i)=Lk
c~                   end do
c~              end if
c~              D(i)=Lav+Lmag*D(i)
c~        end do
c~        D(1)=Lav+Lmag*D(1)
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE fixedu(lun,nvl,nrb,nvls,fvl,svl,gpvl,
c~      1      nrbs,srb,gprb,slong,slat,long,lat,
c~      2      ux,uy,uxm,uym,uxp,uyp,usrb,ugprb)
c~ c
c~ c This routine outputs to the file with unit number 'lun' the fixed
c~ c  velocity values on velocity lines and rigid boundaries.
c~ c
c~       implicit none
c~       integer maxgp,maxs,
c~      1      maxvl,maxvls,maxvlj,maxrb,maxrbs
c~       parameter(maxgp=40000,maxs=3*maxgp,
c~      1      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
c~      2      maxrb=10,maxrbs=200)
c~       integer lun,nvl,nrb
c~       integer i,nj,j,s,vlj,gp
c~       integer nvls(maxvl)
c~       integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
c~      1      fvl(0:maxvls,maxvl)
c~       integer nrbs(maxrb),srb(maxrbs,maxrb)
c~       integer gprb(0:maxrbs,maxrb)
c~       real*8 aconv,u1,u2,a,az
c~       real*8 long(maxgp),lat(maxgp)
c~       real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
c~      1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
c~      2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
c~       real*8 ugprb(2,0:2,0:maxrbs,maxrb),usrb(2,maxrbs,maxrb)
c~       real*8 slong(maxs),slat(maxs)
c~ c
c~       aconv=45.0d0/datan(1.0d0)
c~ c
c~       write(lun,*) nvl,' Velocity Lines'
c~       write(lun,*) 'First line = index,number-of-points'
c~       write(lun,*) 'First point line = v-line,index,long,lat'
c~       write(lun,*) 'Second = ux,uy'
c~       write(lun,*) 'Third = az,u1'
c~       write(lun,*) 'Each fault crossed counts as two points, with its'
c~       write(lun,*) ' negative side first and positive side second'
c~       if (nvl.gt.0) then
c~       do i=1,nvl
c~             nj=2*nvls(i)+1
c~             do j=0,nvls(i)
c~                   if (fvl(j,i).ne.0) nj=nj+1
c~             end do
c~             write(lun,*) i,nj
c~             nj=0
c~             do j=0,nvls(i)
c~                   if (j.gt.0) then
c~                         nj=nj+1
c~                         s=svl(j,i)
c~                         write(lun,*) i,nj,slong(s),slat(s)
c~                         vlj=2*j
c~                         u1=ux(vlj,i)
c~                         u2=uy(vlj,i)
c~                         write(lun,*) u1,u2
c~                         if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                               a=0.0d0
c~                         else
c~                               a=datan2(u2,u1)
c~                         end if
c~                         u1=dsqrt(u1**2+u2**2)
c~                         az=90.0d0-a*aconv
c~                         write(lun,*) az,u1
c~                   end if
c~                   if (fvl(j,i).eq.0) then
c~                         nj=nj+1
c~                         gp=gpvl(j,i)
c~                         write(lun,*) i,nj,long(gp),lat(gp)
c~                         vlj=2*j+1
c~                         u1=ux(vlj,i)
c~                         u2=uy(vlj,i)
c~                         write(lun,*) u1,u2
c~                         if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                               a=0.0d0
c~                         else
c~                               a=datan2(u2,u1)
c~                         end if
c~                         u1=dsqrt(u1**2+u2**2)
c~                         az=90.0d0-a*aconv
c~                         write(lun,*) az,u1
c~                   else
c~                         nj=nj+1
c~                         gp=gpvl(j,i)
c~                         write(lun,*) i,nj,long(gp),lat(gp)
c~                         u1=uxm(j,i)
c~                         u2=uym(j,i)
c~                         write(lun,*) u1,u2
c~                         if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                               a=0.0d0
c~                         else
c~                               a=datan2(u2,u1)
c~                         end if
c~                         u1=dsqrt(u1**2+u2**2)
c~                         az=90.0d0-a*aconv
c~                         write(lun,*) az,u1
c~                         nj=nj+1
c~                         write(lun,*) i,nj,long(gp),lat(gp)
c~                         u1=uxp(j,i)
c~                         u2=uyp(j,i)
c~                         write(lun,*) u1,u2
c~                         if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                               a=0.0d0
c~                         else
c~                               a=datan2(u2,u1)
c~                         end if
c~                         u1=dsqrt(u1**2+u2**2)
c~                         az=90.0d0-a*aconv
c~                         write(lun,*) az,u1
c~                   end if
c~             end do
c~       end do
c~       end if
c~ c
c~       write(lun,*) nrb,' Rigid boundaries'
c~       write(lun,*) 'First line = index,number-of-points'
c~       write(lun,*) 'First point line = v-line,index,long,lat'
c~       write(lun,*) 'Second = ux,uy'
c~       write(lun,*) 'Third = az,u1'
c~       if (nrb.gt.0) then
c~       do i=1,nrb
c~             nj=2*nrbs(i)+1
c~             write(lun,*) i,nj
c~             do j=0,nrbs(i)
c~                   if (j.gt.0) then
c~                         nj=2*j
c~                         s=srb(j,i)
c~                         write(lun,*) i,nj,slong(s),slat(s)
c~                         u1=usrb(1,j,i)
c~                         u2=usrb(2,j,i)
c~                         write(lun,*) u1,u2
c~                         if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                               a=0.0d0
c~                         else
c~                               a=datan2(u2,u1)
c~                         end if
c~                         u1=dsqrt(u1**2+u2**2)
c~                         az=90.0d0-a*aconv
c~                         write(lun,*) az,u1
c~                   end if
c~                   nj=2*j+1
c~                   gp=gprb(j,i)
c~                   write(lun,*) i,nj,long(gp),lat(gp)
c~                   u1=ugprb(1,0,j,i)
c~                   u2=ugprb(2,0,j,i)
c~                   write(lun,*) u1,u2
c~                   if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
c~                         a=0.0d0
c~                   else
c~                         a=datan2(u2,u1)
c~                   end if
c~                   u1=dsqrt(u1**2+u2**2)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) az,u1
c~             end do
c~       end do
c~       end if
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE xvalue(lun,nx,kx,ix,jx,jcx,icx,xc,x0,x1,xd)
c~ c
c~ c This routine outputs to the file with unit number 'lun' the raw
c~ c  solution vector x and its association with elements and grid points.
c~ c
c~       implicit none
c~       integer maxgp,maxf,maxfs,maxvl,maxvls,
c~      1      maxsf,maxsvl,maxpf,maxpvl,
c~      2      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
c~      3      maxx
c~       parameter(maxgp=40000,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,
c~      3      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
c~      4      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
c~      5      maxtj=80,maxJvl=10,maxfT=10,
c~      6      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
c~      7      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
c~      8      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
c~      9      +2*(maxpf+maxfT+maxXf)))
c~       integer lun,nx
c~       integer i
c~       integer kx(maxx),ix(maxx),jx(maxx),jcx(maxx),icx(maxx)
c~       real*8 xc(maxx),x0(maxx),x1(maxx),xd(maxx)
c~ c
c~       write(lun,*) nx,' Values of x'
c~       write(lun,*) 'First line = index,kind'
c~       write(lun,*) 'Second = i-index,j-index,derivative,component'
c~       write(lun,*) 'Third = xc,x0,x1,xd'
c~       write(lun,*) 'kind has the following values:'
c~       write(lun,*) ' 0 = u at grid point'
c~       write(lun,*) ' 1 = u on negative side of fault'
c~       write(lun,*) ' 2 = u on negative side of velocity line'
c~       write(lun,*) ' 3 = u at triple junction of faults'
c~       write(lun,*) ' 4 = u at joining of velocity lines'
c~       write(lun,*) ' 5 = u at truncation at a fault'
c~       write(lun,*) ' 6 = u at truncation of a fault'
c~       write(lun,*) ' 7 = u at truncation of a velocity line'
c~       write(lun,*) ' 8 = u at crossing of a fault'
c~       write(lun,*) ' 9 = u at crossing of two velocity lines'
c~       write(lun,*) ' 21 = du on a fault'
c~       write(lun,*) ' 25 = du at truncation at a fault'
c~       write(lun,*) ' 28 = du at crossing of a fault'
c~       write(lun,*) 'i-index = grid point, fault, etc'
c~       write(lun,*) 'j-index = fault or v-line side, region for others'
c~       write(lun,*) 'xc = constraint part of solution'
c~       write(lun,*) 'x0 = apriori solution'
c~       write(lun,*) 'x1 = aposteriori solution'
c~       write(lun,*) 'xd = aposteriori minus apriori'
c~       do i=1,nx
c~             write(lun,*) i,kx(i)
c~             write(lun,*) ix(i),jx(i),jcx(i),icx(i)
c~             write(lun,*) xc(i),x0(i),x1(i),xd(i)
c~       end do
c~ c
c~       return
c~       end
c
c
      SUBROUTINE evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugp,ue,uf,uvl,utj,uJvl,ufT,uTf,uTvl,uXf,uXvl)
c
c This routine outputs to the file with unit number 'lun' velocity,
c  strain-rate and stress values in elements for the solution, or
c  difference between solutions, in the ugp to uXvl vectors.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxsf,maxsvl,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     4      maxtj=80,maxJvl=10,maxfT=10,
     5      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     6      maxefv=12*(maxsf+maxsvl))
      integer lun,ne
      integer i,k,ic
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer envo(maxe),eneo(maxe),enec(maxe)
      real*8 rE
      real*8 w12
      real*8 long(maxgp),lat(maxgp)
      real*8 area(maxe)
      real*8 exx(2,0:9,maxe),eyy(2,0:9,maxe),exy(2,0:9,maxe)
      real*8 sxx(2,0:9,maxe),syy(2,0:9,maxe),sxy(2,0:9,maxe)
      real*8 ugp(2,0:2,maxgp),ue(2,maxe)
      real*8 uf(2,0:2,0:maxfs,maxf),uvl(2,0:2,0:maxvls,maxvl)
      real*8 utj(2,0:2,2,maxtj)
      real*8 uJvl(2,0:2,3,maxJvl),ufT(2,0:2,3,maxfT),
     1      uTf(2,0:2,3,maxTf),uTvl(2,0:2,3,maxTvl),
     2      uXf(2,0:2,3,maxXf),uXvl(2,0:2,3,maxXvl)
      real*8 elong(maxe),elat(maxe)
      real*8 u(2,0:9)
c
c~       aconv=45.0d0/datan(1.0d0)
c
      write(lun,*) ne,' Elements'
      write(lun,*) 'First line = index'
      write(lun,*) 'Second line = ux1,ux2,ux3'
      write(lun,*) 'Third = uy1,uy2,uy3'
      write(lun,*) 'Fourth = wxy'
      do i=1,ne
            write(lun,*) i
            call umat(rE,i,gp1e,gp2e,gp3e,long,lat,iefv,
     1            kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2            jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3            ugp,ue,uf,uvl,utj,uJvl,ufT,uTf,uTvl,uXf,uXvl,u)
            write(lun,*) u(1,0),u(1,3),u(1,6)
            write(lun,*) u(2,0),u(2,3),u(2,6)
            w12=0.0d0
            do k=0,8
                    w12=w12+(exy(1,k,i)+exx(2,k,i))*u(1,k)
                    w12=w12-exy(2,k,i)*u(2,k)
            end do
            write(lun,*) w12
      end do
c
      return
      end
c
c
      SUBROUTINE umat(rE,e,gp1e,gp2e,gp3e,long,lat,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      ugp,ue,uf,uvl,utj,uJvl,ufT,uTf,uTvl,uXf,uXvl,u)
c
c This routine assembles the velocity values at the nodes of an element.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxsf,maxsvl,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     4      maxtj=80,maxJvl=10,maxfT=10,
     5      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     6      maxefv=12*(maxsf+maxsvl))
      integer e
      integer gp1,gp2,gp3,ic,r,kgp,igp,jgp
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      real*8 rE
      real*8 pi,lconv,x1,y1,x2,y2,x3,y3
      real*8 long(maxgp),lat(maxgp)
      real*8 ugp(2,0:2,maxgp),ue(2,maxe)
      real*8 uf(2,0:2,0:maxfs,maxf),uvl(2,0:2,0:maxvls,maxvl)
      real*8 utj(2,0:2,2,maxtj)
      real*8 uJvl(2,0:2,3,maxJvl),ufT(2,0:2,3,maxfT),
     1      uTf(2,0:2,3,maxTf),uTvl(2,0:2,3,maxTvl),
     2      uXf(2,0:2,3,maxXf),uXvl(2,0:2,3,maxXvl)
      real*8 u(2,0:9)
c
      gp1=gp1e(e)
      gp2=gp2e(e)
      gp3=gp3e(e)
      if (iefv(e).eq.0) then
            do ic=1,2
                 u(ic,0)=ugp(ic,0,gp1)
                 u(ic,1)=ugp(ic,1,gp1)
                 u(ic,2)=ugp(ic,2,gp1)
                 u(ic,3)=ugp(ic,0,gp2)
                 u(ic,4)=ugp(ic,1,gp2)
                 u(ic,5)=ugp(ic,2,gp2)
                 u(ic,6)=ugp(ic,0,gp3)
                 u(ic,7)=ugp(ic,1,gp3)
                 u(ic,8)=ugp(ic,2,gp3)
            end do
      else
            r=rgp1fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       u(ic,0)=ugp(ic,0,gp1)
                       u(ic,1)=ugp(ic,1,gp1)
                       u(ic,2)=ugp(ic,2,gp1)
                  end do
                  goto 10
            end if
            kgp=kgp1fv(iefv(e))
            igp=igp1fv(iefv(e))
            jgp=jgp1fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       u(ic,0)=uf(ic,0,jgp,igp)
                       u(ic,1)=uf(ic,1,jgp,igp)
                       u(ic,2)=uf(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       u(ic,0)=uvl(ic,0,jgp,igp)
                       u(ic,1)=uvl(ic,1,jgp,igp)
                       u(ic,2)=uvl(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       u(ic,0)=utj(ic,0,r,igp)
                       u(ic,1)=utj(ic,1,r,igp)
                       u(ic,2)=utj(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       u(ic,0)=uJvl(ic,0,r,igp)
                       u(ic,1)=uJvl(ic,1,r,igp)
                       u(ic,2)=uJvl(ic,2,r,igp)

                  end do
                  goto 10
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       u(ic,0)=ufT(ic,0,r,igp)
                       u(ic,1)=ufT(ic,1,r,igp)
                       u(ic,2)=ufT(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       u(ic,0)=uTf(ic,0,r,igp)
                       u(ic,1)=uTf(ic,1,r,igp)
                       u(ic,2)=uTf(ic,2,r,igp)

                  end do
                  goto 10
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       u(ic,0)=uTvl(ic,0,r,igp)
                       u(ic,1)=uTvl(ic,1,r,igp)
                       u(ic,2)=uTvl(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       u(ic,0)=uXf(ic,0,r,igp)
                       u(ic,1)=uXf(ic,1,r,igp)
                       u(ic,2)=uXf(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       u(ic,0)=uXvl(ic,0,r,igp)
                       u(ic,1)=uXvl(ic,1,r,igp)
                       u(ic,2)=uXvl(ic,2,r,igp)
                  end do
            end if
10          continue
c
            r=rgp2fv(iefv(e))
            if (r.eq.0) then

                  do ic=1,2
                       u(ic,3)=ugp(ic,0,gp2)
                       u(ic,4)=ugp(ic,1,gp2)
                       u(ic,5)=ugp(ic,2,gp2)
                  end do
                  goto 20
            end if
            kgp=kgp2fv(iefv(e))
            igp=igp2fv(iefv(e))
            jgp=jgp2fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       u(ic,3)=uf(ic,0,jgp,igp)
                       u(ic,4)=uf(ic,1,jgp,igp)
                       u(ic,5)=uf(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       u(ic,3)=uvl(ic,0,jgp,igp)
                       u(ic,4)=uvl(ic,1,jgp,igp)
                       u(ic,5)=uvl(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       u(ic,3)=utj(ic,0,r,igp)
                       u(ic,4)=utj(ic,1,r,igp)
                       u(ic,5)=utj(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       u(ic,3)=uJvl(ic,0,r,igp)
                       u(ic,4)=uJvl(ic,1,r,igp)
                       u(ic,5)=uJvl(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       u(ic,3)=ufT(ic,0,r,igp)
                       u(ic,4)=ufT(ic,1,r,igp)
                       u(ic,5)=ufT(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       u(ic,3)=uTf(ic,0,r,igp)
                       u(ic,4)=uTf(ic,1,r,igp)
                       u(ic,5)=uTf(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       u(ic,3)=uTvl(ic,0,r,igp)
                       u(ic,4)=uTvl(ic,1,r,igp)
                       u(ic,5)=uTvl(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       u(ic,3)=uXf(ic,0,r,igp)
                       u(ic,4)=uXf(ic,1,r,igp)
                       u(ic,5)=uXf(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       u(ic,3)=uXvl(ic,0,r,igp)
                       u(ic,4)=uXvl(ic,1,r,igp)
                       u(ic,5)=uXvl(ic,2,r,igp)
                  end do
            end if
20          continue
c
            r=rgp3fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       u(ic,6)=ugp(ic,0,gp3)
                       u(ic,7)=ugp(ic,1,gp3)
                       u(ic,8)=ugp(ic,2,gp3)
                  end do
                  goto 30
            end if
            kgp=kgp3fv(iefv(e))
            igp=igp3fv(iefv(e))
            jgp=jgp3fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       u(ic,6)=uf(ic,0,jgp,igp)
                       u(ic,7)=uf(ic,1,jgp,igp)
                       u(ic,8)=uf(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       u(ic,6)=uvl(ic,0,jgp,igp)
                       u(ic,7)=uvl(ic,1,jgp,igp)
                       u(ic,8)=uvl(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       u(ic,6)=utj(ic,0,r,igp)
                       u(ic,7)=utj(ic,1,r,igp)
                       u(ic,8)=utj(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.4) then
                  do ic=1,2

                       u(ic,6)=uJvl(ic,0,r,igp)
                       u(ic,7)=uJvl(ic,1,r,igp)
                       u(ic,8)=uJvl(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       u(ic,6)=ufT(ic,0,r,igp)
                       u(ic,7)=ufT(ic,1,r,igp)
                       u(ic,8)=ufT(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       u(ic,6)=uTf(ic,0,r,igp)
                       u(ic,7)=uTf(ic,1,r,igp)
                       u(ic,8)=uTf(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       u(ic,6)=uTvl(ic,0,r,igp)
                       u(ic,7)=uTvl(ic,1,r,igp)
                       u(ic,8)=uTvl(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       u(ic,6)=uXf(ic,0,r,igp)
                       u(ic,7)=uXf(ic,1,r,igp)
                       u(ic,8)=uXf(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       u(ic,6)=uXvl(ic,0,r,igp)
                       u(ic,7)=uXvl(ic,1,r,igp)
                       u(ic,8)=uXvl(ic,2,r,igp)
                  end do
            end if
30          continue
      end if
c
      pi=4.0d0*datan(1.0d0)
      lconv=rE*pi/180.0d0
c
      x1=lconv*long(gp1)
      y1=lconv*lat(gp1)
      x2=lconv*long(gp2)
      y2=lconv*lat(gp2)
      x3=lconv*long(gp3)
      y3=lconv*lat(gp3)
      do ic=1,2
            ue(ic,e)=(u(ic,0)+u(ic,3)+u(ic,6))/3.0d0
     1            +(((x2-x1)+(x3-x1))*u(ic,1)
     2            +((y2-y1)+(y3-y1))*u(ic,2)
     1            +((x3-x2)+(x1-x2))*u(ic,4)
     2            +((y3-y2)+(y1-y2))*u(ic,5)
     1            +((x1-x3)+(x2-x3))*u(ic,7)
     2            +((y1-y3)+(y2-y3))*u(ic,8))/18.0d0
      end do
c
      return
      end
c
c
c~       SUBROUTINE fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
c~      2      duf,dufT,duXf)
c~ c
c~ c This routine outputs to the file with unit number 'lun' slip-rate
c~ c  and traction values in fault segments for the solution, or
c~ c  difference between solutions, in the duf to duXf vectors.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxf,maxfs,
c~      1      maxvl,maxvls,
c~      2      maxsvl,maxpvl,
c~      3      maxfT,maxXf
c~       parameter(maxgp=40000,maxs=3*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,
c~      3      maxsvl=maxvl*maxvls,maxpvl=maxsvl+maxvl,
c~      4      maxfT=10,maxXf=200)
c~       integer lun,nf
c~       integer i,j,s,gp1,gp2,k
c~       integer nfs(maxf),sf(maxfs,maxf)
c~       integer gpf(0:maxfs,maxf)
c~       integer ipvl(maxgp)
c~       integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
c~       integer fsnfo(maxfs,maxf),fsnduc(maxfs,maxf)
c~       real*8 aconv,dx,dy,dl,tx,ty,a,az,nx,ny
c~       real*8 dut,dun,tt,tn,du1,du2,sn1,sn2
c~       real*8 long(maxgp),lat(maxgp)
c~       real*8 lenfs(maxfs,maxf),dufs(0:1,maxfs,maxf)
c~       real*8 ttfs(0:1,maxfs,maxf),tnfs(0:1,maxfs,maxf)
c~       real*8 duf(2,0:1,0:maxfs,maxf)
c~       real*8 dufT(2,0:1,maxfT),duXf(2,0:1,maxXf)
c~       real*8 slong(maxs),slat(maxs)
c~       real*8 du(2,0:3)
c~ c
c~       aconv=45.0d0/datan(1.0d0)
c~ c
c~       write(lun,*) nf,' Faults'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = fault,index,long,lat'
c~       write(lun,*) 'Second = nfo,nduc,length,az'
c~       write(lun,*) 'Third = tx,ty,az'
c~       write(lun,*) 'Fourth = nx,ny,az'
c~       write(lun,*) 'Fifth = dut,dun (slip rates)'
c~       write(lun,*) 'Sixth = tt(=snt),tn(=snn) (tractions)'
c~       write(lun,*) 'Seventh = dux,duy'
c~       write(lun,*) 'Eighth = snx,sny'
c~       write(lun,*) 'Nineth = az,du1'
c~       write(lun,*) 'Tenth = az,sn1'
c~       do i=1,nf
c~             write(lun,*) i,nfs(i)
c~             do j=1,nfs(i)
c~                   s=sf(j,i)
c~                   write(lun,*) i,j,slong(s),slat(s)
c~                   gp1=gpf(j-1,i)
c~                   gp2=gpf(j,i)
c~                   dx=long(gp2)-long(gp1)
c~                   dy=lat(gp2)-lat(gp1)
c~                   dx=dx*dcos(slat(s)/aconv)
c~                   dl=dsqrt(dx**2+dy**2)
c~                   tx=dx/dl
c~                   ty=dy/dl
c~                   a=datan2(ty,tx)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) fsnfo(j,i),fsnduc(j,i),lenfs(j,i),az
c~                   write(lun,*) tx,ty,az
c~                   nx=-ty
c~                   ny=tx
c~                   a=datan2(ny,nx)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) nx,ny,az
c~                   call dumat(i,j,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1                  duf,dufT,duXf,du)
c~                   dut=0.0d0
c~                   dun=0.0d0
c~                   tt=0.0d0
c~                   tn=0.0d0
c~                   do k=0,1
c~                         dut=dut+dufs(k,j,i)*du(1,k)
c~                         dun=dun+dufs(k,j,i)*du(2,k)
c~                         tt=tt+ttfs(k,j,i)*du(1,k)
c~                         tn=tn+tnfs(k,j,i)*du(2,k)
c~                   end do
c~                   write(lun,*) dut,dun
c~                   write(lun,*) tt,tn
c~                   du1=dut*tx+dun*nx
c~                   du2=dut*ty+dun*ny
c~                   sn1=tt*tx+tn*nx
c~                   sn2=tt*ty+tn*ny
c~                   write(lun,*) du1,du2
c~                   write(lun,*) sn1,sn2
c~                   if ((du1.eq.0.0d0).and.(du2.eq.0.0d0)) then
c~                         a=0.0d0
c~                   else
c~                         a=datan2(du2,du1)
c~                   end if
c~                   du1=dsqrt(du1**2+du2**2)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) az,du1
c~                   if ((sn1.eq.0.0d0).and.(sn2.eq.0.0d0)) then
c~                         a=0.0d0
c~                   else


c~                         a=datan2(sn2,sn1)
c~                   end if
c~                   sn1=dsqrt(sn1**2+sn2**2)
c~                   az=90.0d0-a*aconv
c~                   write(lun,*) az,sn1
c~             end do
c~       end do
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE dumat(f,s,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
c~      1      duf,dufT,duXf,du)
c~ c
c~ c This assembles the slip-rate values at the nodes of a fault segment.
c~ c
c~       implicit none
c~       integer maxgp,maxf,maxfs,
c~      1      maxvl,maxvls,
c~      2      maxsvl,maxpvl,
c~      3      maxfT,maxXf
c~       parameter(maxgp=40000,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,
c~      3      maxsvl=maxvl*maxvls,maxpvl=maxsvl+maxvl,
c~      4      maxfT=10,maxXf=200)
c~       integer f,s
c~       integer gp1,gp2,ic,ip,fT,Xf
c~       integer gpf(0:maxfs,maxf)
c~       integer ipvl(maxgp)
c~       integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
c~       real*8 duf(2,0:1,0:maxfs,maxf)
c~       real*8 dufT(2,0:1,maxfT),duXf(2,0:1,maxXf)
c~       real*8 du(2,0:3)
c~ c
c~       gp1=gpf(s-1,f)
c~       gp2=gpf(s,f)
c~       do ic=1,2
c~             du(ic,0)=duf(ic,0,s-1,f)
c~       end do
c~       if (ipvl(gp2).eq.0) then
c~             do ic=1,2
c~                  du(ic,1)=duf(ic,0,s,f)
c~             end do
c~       else
c~             ip=ipvl(gp2)
c~             if (fTvlp(ip).ne.0) then
c~                   fT=fTvlp(ip)
c~                   do ic=1,2
c~                        du(ic,1)=dufT(ic,0,fT)
c~                   end do
c~                   goto 10
c~             end if
c~             if (Tfvlp(ip).ne.0) then
c~                   do ic=1,2
c~                        du(ic,1)=duf(ic,0,s,f)
c~                   end do
c~                   goto 10
c~             end if
c~             if (Xfvlp(ip).ne.0) then
c~                   Xf=Xfvlp(ip)
c~                   do ic=1,2
c~                        du(ic,1)=duXf(ic,0,Xf)
c~                   end do
c~             end if
c~ 10          continue
c~       end if
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE zerror(lun,nz,e1s,e2s,isf,isvl,isrb,fs,sfs,
c~      1      vls,svls,rbs,srbs,sz,kz,jz,ux,uy,usrb,slong,slat,z)
c~ c
c~ c This routine outputs to the file with unit number 'lun' the residuals
c~ c  in constructing the xc vector contribution that constraint z values
c~ c  make to the apriori and aposteriori solutions.
c~ c The other parts of the apriori and aposteriori solutions belong to a
c~ c  subspace of x vectors that is perpendicular to the set of possible xc
c~ c  vectors, and the numerical-precision errors in projecting onto that
c~ c  subspace will be comparable to the errors here in matching the z
c~ c  values.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxf,maxfs,
c~      1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
c~      2      maxsf,maxsvl,maxsrb,maxtj,maxz
c~       parameter(maxgp=40000,maxs=3*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
c~      3      maxrb=10,maxrbs=200,
c~      4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
c~      5      maxsrb=maxrb*maxrbs,maxtj=80,
c~      6      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
c~       integer lun,nz
c~       integer i,s,ic,f,j,vl,rb
c~       integer e1s(maxs),e2s(maxs)
c~       integer isf(maxs),isvl(maxs),isrb(maxs)
c~       integer fs(maxsf),sfs(maxsf)
c~       integer vls(maxsvl),svls(maxsvl)
c~       integer rbs(maxsrb),srbs(maxsrb)
c~       integer sz(maxz),kz(maxz),jz(maxz)
c~       real*8 zval
c~       real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl)
c~       real*8 usrb(2,maxrbs,maxrb)
c~       real*8 slong(maxs),slat(maxs)
c~       real*8 z(maxz)
c~ c
c~       write(lun,*) nz,' Constraints'
c~       write(lun,*) 'First line = index,side,long,lat'
c~       write(lun,*)
c~      1      'Second (Class 0) = class,sub-class,element-1,element-2'
c~       write(lun,*)
c~      1      'Second (Class 1) = class,sub-class,fault,segment'
c~       write(lun,*)
c~      1      'Second (Class 2) = class,sub-class,v-line,segment'
c~       write(lun,*)
c~      1      'Second (Class 3) = class,sub-class,rigid-b,segment'
c~       write(lun,*) 'Third = component,value,residual'
c~       do i=1,nz
c~             s=sz(i)
c~             ic=1+mod(i-1,2)
c~             write(lun,*) i,s,slong(s),slat(s)
c~             if (kz(i).eq.0) then
c~                   write(lun,*) kz(i),jz(i),e1s(s),e2s(s)
c~                   zval=0.0d0
c~             end if
c~             if (kz(i).eq.1) then
c~                   f=fs(isf(s))
c~                   j=sfs(isf(s))
c~                   write(lun,*) kz(i),jz(i),f,j
c~                   zval=0.0d0
c~             end if
c~             if (kz(i).eq.2) then
c~                   vl=vls(isvl(s))
c~                   j=svls(isvl(s))
c~                   write(lun,*) kz(i),jz(i),vl,j
c~                   if (ic.eq.1) zval=ux(2*j,vl)
c~                   if (ic.eq.2) zval=uy(2*j,vl)
c~             end if
c~             if (kz(i).eq.3) then
c~                   rb=rbs(isrb(s))
c~                   j=srbs(isrb(s))
c~                   write(lun,*) kz(i),jz(i),rb,j
c~                   zval=usrb(ic,j,rb)
c~             end if
c~             write(lun,*) ic,zval,z(i)
c~       end do
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE ymatch(lun,ny0,ne,nf,nfs,sf,elong,elat,slong,slat,
c~      1      y0,y,y1)
c~ c
c~ c This routine outputs to the file with unit number 'lun' residuals for
c~ c  the dynamics parts of the apriori and aposteriori solutions. The y0
c~ c  vector is the unscaled apriori solution, the y vector is the scaled
c~ c  apriori solution, with the same scaling as the aposteriori solution,
c~ c  and y1 vector is the aposteriori solution.
c~ c
c~ c The scaling of the aposteriori solution is designed to match the
c~ c  standardised statistical scaling of the observation residuals in the
c~ c  second parts of the y and y1 vectors. To ensure numerical stability
c~ c  the dynamics is over-sampled by a factor of 2 in each spatial
c~ c  dimension.
c~ c So, of the 12 dynamics residuals in each element there are 6 for each
c~ c  independent component of strain rate, given that one of the 3
c~ c  potential degrees of freedom (ie. exx, eyy and exy) per element is
c~ c  removed by strain-rate compatibility. This means that in the
c~ c  statistical scaling of the sum of squares for the aposteriori
c~ c  solution on average each of the 12 dynamics residuals contributes one
c~ c  sixth as much as an independent observation.
c~ c Similarly, of the 4 dynamics residuals for each fault segment there
c~ c  are 2 for each independent component of slip rate, and in the
c~ c  aposteriori analysis on average each of these 4 residuals contributes
c~ c  half as much as an independent observation.
c~ c Consequently, the y and y1 residuals associated with the dynamics
c~ c  should rarely if ever be much greater than 1.
c~ c
c~ c There are two possible reasons for the dynamics residuals being large.
c~ c  One is that the choice of spatial distribution for the stress-like
c~ c  potentials sxxpot, syypot and sxypot involves large
c~ c  physically-unrealisable contributions.
c~ c For example, adding a large spatially-uniform amount to both sxxpot
c~ c  and syypot does not change the forces that are acting, and therefore
c~ c  does not affect the dynamics solution. However, such a contribution
c~ c  does alter the dynamic equivalents of observations of strain rates
c~ c  and slip rates, but in a way that has no bearing on the solution.
c~ c In general, given a spatial distribution of values for any one of
c~ c  sxxpot, syypot and sxypot values can be chosen for the other two that
c~ c  result in there being no nett force.
c~ c The way to spot such a situation is where the dynamics residuals are
c~ c  large over much of the model.
c~ c
c~ c The other circumstance resulting in dynamics residuals being large is
c~ c  where there are large discrepencies between observations and apriori
c~ c  model predictions. Such large dynamics residuals show up only in the
c~ c  aposteriori solution, and are counter-balanced by the mismatch to
c~ c  observations being substantially smaller in the aposteriori solution
c~ c  than in the apriori solution. Though the sum of squares of the
c~ c  dynamics residuals increases in the aposteriori solution, the total
c~ c  sum of squares including the mismatch to observations decreases.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxe,maxf,maxfs,
c~      1      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
c~      2      maxsf,maxy
c~       parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
c~      3      maxduc=2*maxf,maxec=100,maxece=500,
c~      4      maxsf=maxf*maxfs,
c~      5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
c~      6      +maxfs*maxduc+maxece*maxec)
c~       integer lun,ny0,ne,nf
c~       integer iy,i,iq,ic,j,s,ih
c~       integer nfs(maxf),sf(maxfs,maxf)
c~       real*8 elong(maxe),elat(maxe)
c~       real*8 slong(maxs),slat(maxs)
c~       real*8 y0(maxy),y(maxy),y1(maxy)
c~ c
c~       iy=0
c~ c
c~       write(lun,*) ne,' Elements'
c~       write(lun,*) 'First line = index,long,lat'
c~       write(lun,*) 
c~      1      'Others = component+3*(sub-element),y0,y,y1-residuals'
c~       write(lun,*) 'exx=component-1,eyy=component-2,exy=component-3'
c~       do i=1,ne
c~             write(lun,*) i,elong(i),elat(i)
c~             do iq=0,3
c~             do ic=1,3
c~                   iy=iy+1
c~                   write(lun,*) ic+3*iq,y0(iy),y(iy),y1(iy)
c~             end do
c~             end do
c~       end do  
c~ c
c~       if (nf.gt.0) then
c~       write(lun,*) nf,' Faults'
c~       write(lun,*) 'First line = index,number-of-segments'
c~       write(lun,*) 'First segment line = fault,index,long,lat'
c~       write(lun,*) 
c~      1      'Others = component+2*(sub-segment-1),y0,y,y1-residuals'
c~       write(lun,*) 'dut=component-1,dun=component-2'
c~       do i=1,nf
c~             write(lun,*) i,nfs(i)
c~             do j=1,nfs(i)
c~                   s=sf(j,i)
c~                   write(lun,*) i,j,slong(s),slat(s)
c~                   do ih=1,2
c~                   do ic=1,2
c~                         iy=iy+1
c~                         write(lun,*) ic+2*(ih-1),y0(iy),y(iy),y1(iy)
c~                   end do
c~                   end do
c~             end do
c~       end do
c~       end if
c~ c
c~       if (iy.ne.ny0) stop 'Something is wrong with the y0 part of y'
c~ c
c~       return
c~       end
c
c
c~       SUBROUTINE misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
c~      1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
c~      2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
c~      3      eec,ncec,e1ec,e2ec,y)
c~ c
c~ c This routine outputs to the file with unit number 'lun' residuals for
c~ c  all observations. The input y vector can be for either the apriori or
c~ c  the aposteriori solution. On average the normalised y-residuals will
c~ c  have values that distribute about zero with magnitudes of order 1, if
c~ c  the fit to the observations is satisfactory.
c~ c
c~       implicit none
c~       integer maxgp,maxs,maxe,maxf,maxfs,
c~      1      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
c~      2      maxsf,maxy
c~       parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
c~      1      maxf=600,maxfs=100,
c~      2      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
c~      3      maxduc=2*maxf,maxec=100,maxece=500,
c~      4      maxsf=maxf*maxfs,
c~      5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
c~      6      +maxfs*maxduc+maxece*maxec)
c~       integer lun,ny0,ny,nvo,nfo,neo,nduc,nec
c~       integer iy,i,j,s1c,s2c,s1,s2,e1c,e2c,e1,e2
c~       integer evo(maxvo)
c~       integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
c~       integer eeo(maxeo),neoc(maxeo)
c~       integer fduc(maxduc),sduc(maxfs,maxduc)
c~       integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
c~       integer isf(maxs)
c~       integer sfs(maxsf)
c~       integer eec(maxece,maxec)
c~       integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
c~       real*8 ru1,ru2,rux,ruy,rdu1,rdu2,rduc1,rduc2
c~       real*8 re1,re2,re3,rec1,rec2,rec3
c~       real*8 volong(maxvo),volat(maxvo)
c~       real*8 oux(maxvo),ouy(maxvo)
c~       real*8 oduc(2,maxfo)
c~       real*8 oec(3,maxeo)
c~       real*8 seou(2,2,maxvo)
c~       real*8 seodu(2,2,maxfo)
c~       real*8 seoe(3,3,maxeo)
c~       real*8 y(maxy)
c~ c
c~       iy=ny0
c~ c
c~       if (nvo.gt.0) then
c~       write(lun,*) nvo,' Velocity Observations'
c~       write(lun,*) 'First line = index,element,longitude,latitude'
c~       write(lun,*) 'Others = component,value,residual,y-residual'
c~       do i=1,nvo
c~             write(lun,*) i,evo(i),volong(i),volat(i)
c~             iy=iy+1
c~             ru1=y(iy)
c~             iy=iy+1
c~             ru2=y(iy)
c~             rux=seou(1,1,i)*ru1
c~             ruy=seou(2,1,i)*ru1+seou(2,2,i)*ru2
c~             j=1
c~             write(lun,*) j,oux(i),rux,ru1
c~             j=2
c~             write(lun,*) j,ouy(i),ruy,ru2
c~       end do
c~       end if
c~ c
c~       if (nfo.gt.0) then
c~       write(lun,*) nfo,' Slip-rate Observations'
c~       write(lun,*) 'First line = index,fault,segment,number'
c~       write(lun,*) 'Others = component,value,residual,y-residual'
c~       do i=1,nfo
c~             write(lun,*) i,ffo(i),sfo(i),nfoc(i)
c~             iy=iy+1
c~             rdu1=y(iy)
c~             rduc1=seodu(1,1,i)*rdu1
c~             j=1
c~             write(lun,*) j,oduc(1,i),rduc1,rdu1
c~             if (nfoc(i).eq.2) then
c~                   iy=iy+1

c~                   rdu2=y(iy)
c~                   rduc2=seodu(2,1,i)*rdu1+seodu(2,2,i)*rdu2
c~                   j=2
c~                   write(lun,*) j,oduc(2,i),rduc2,rdu2
c~             end if
c~       end do
c~       end if
c~ c
c~       if (neo.gt.0) then
c~       write(lun,*) neo,' Strain-rate Observations'
c~       write(lun,*) 'First line = index,element,number'
c~       write(lun,*) 'Others = component,value,residual,y-residual'
c~       do i=1,neo
c~             write(lun,*) i,eeo(i),neoc(i)
c~             iy=iy+1
c~             re1=y(iy)
c~             rec1=seoe(1,1,i)*re1
c~             j=1
c~             write(lun,*) j,oec(1,i),rec1,re1
c~             if (neoc(i).gt.1) then
c~                   iy=iy+1
c~                   re2=y(iy)
c~                   rec2=seoe(2,1,i)*re1+seoe(2,2,i)*re2
c~                   j=2
c~                   write(lun,*) j,oec(2,i),rec2,re2
c~             end if
c~             if (neoc(i).eq.3) then
c~                   iy=iy+1
c~                   re3=y(iy)
c~                   rec3=seoe(3,1,i)*re1+seoe(3,2,i)*re2
c~      1                  +seoe(3,3,i)*re3
c~                   j=3
c~                   write(lun,*) j,oec(3,i),rec3,re3
c~             end if
c~       end do
c~       end if
c~ c
c~       if (nduc.gt.0) then
c~       write(lun,*) nduc,' Slip-rate Correlation Sets'
c~       write(lun,*) 'First line = index,fault,number-of-pairs'
c~       write(lun,*) 'Others = pair,segment-1,segment-2,y-residual'
c~       do i=1,nduc
c~             write(lun,*) i,fduc(i),ncduc(i)
c~             do j=1,ncduc(i)
c~                   s1c=s1duc(j,i)
c~                   s2c=s2duc(j,i)
c~                   s1=sfs(isf(sduc(s1c,i)))
c~                   s2=sfs(isf(sduc(s2c,i)))
c~                   iy=iy+1
c~                   write(lun,*) j,s1,s2,y(iy)
c~             end do
c~       end do
c~       end if
c~ c
c~       if (nec.gt.0) then
c~       write(lun,*) nec,' Strain-rate Correlation Sets'
c~       write(lun,*) 'First line = index,number-of-pairs'
c~       write(lun,*) 'Others = pair,element-1,element-2,y-residual'
c~       do i=1,nec
c~             write(lun,*) i,ncec(i)
c~             do j=1,ncec(i)
c~                   e1c=e1ec(j,i)
c~                   e2c=e2ec(j,i)
c~                   e1=eec(e1c,i)
c~                   e2=eec(e2c,i)
c~                   iy=iy+1
c~                   write(lun,*) j,e1,e2,y(iy)
c~             end do
c~       end do
c~       end if
c~ c
c~       if (iy.ne.ny) stop 'Something is wrong with the y1 part of y'
c~ c
c~       return
c~       end
c

