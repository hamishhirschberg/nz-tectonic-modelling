      SUBROUTINE output()
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
      call numobs(ne,nf,nfs,isf,sfs,nvo,nfo,neo,nduc,nec,evo,
     1      ffo,sfo,nfoc,eeo,neoc,fduc,sduc,ncduc,s1duc,s2duc,
     2      eec,ncec,e1ec,e2ec,envo,eneo,enec,fsnfo,fsnduc)
c
      lun=3
c
      open(lun,file='grid_geometry.log')
      call mesh(lun,ngp,ns,ne,nf,nvl,nrb,
     1      long,lat,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,svl,gpvl,fvl,nrbs,srb,gprb)
      write(lun,*)
      close(lun)
c
      open(lun,file='forces_and_rate_capacities.log')
      call forces(lun,nf,ne,nfs,sf,gpf,slong,slat,
     1      long,lat,lenfs,Kcfs,Ksfs,area,elong,elat,
     2      fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse)
      write(lun,*)
      close(lun)
c
      open(lun,file='fixed_velocity_values.log')
      call fixedu(lun,nvl,nrb,nvls,fvl,svl,gpvl,
     1      nrbs,srb,gprb,slong,slat,long,lat,
     2      ux,uy,uxm,uym,uxp,uyp,usrb,ugprb)
      write(lun,*)
      close(lun)
c
      open(lun,file='solution_x_values.log')
      call xvalue(lun,nx,kx,ix,jx,jcx,icx,xc,x0,x1,xd)
      write(lun,*)
      close(lun)
c
      open(lun,file='constraint_part_of_solution_in_elements.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugpc,uec,ufc,uvlc,utjc,uJvlc,ufTc,uTfc,uTvlc,uXfc,uXvlc)
      write(lun,*)
      close(lun)
c
      if (nf.gt.0) then
      open(lun,file='constraint_part_of_solution_on_faults.log')
      call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
     2      dufc,dufTc,duXfc)
      write(lun,*)
      close(lun)
      end if
c
      open(lun,file='apriori_solution_in_elements.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugp0,ue0,uf0,uvl0,utj0,uJvl0,ufT0,uTf0,uTvl0,uXf0,uXvl0)
      write(lun,*)
      close(lun)
c
      if (nf.gt.0) then
      open(lun,file='apriori_solution_on_faults.log')
      call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
     2      duf0,dufT0,duXf0)
      write(lun,*)
      close(lun)
      end if
c
      open(lun,file='aposteriori_solution_in_elements.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugp1,ue1,uf1,uvl1,utj1,uJvl1,ufT1,uTf1,uTvl1,uXf1,uXvl1)
      write(lun,*)
      close(lun)
c
      if (nf.gt.0) then
      open(lun,file='aposteriori_solution_on_faults.log')
      call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
     2      duf1,dufT1,duXf1)
      write(lun,*)
      close(lun)
      end if
c
      open(lun,file='aposteriori_minus_apriori_in_elements.log')
      call evalue(lun,rE,ne,gp1e,gp2e,gp3e,iefv,
     1      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3      envo,eneo,enec,long,lat,area,elong,elat,
     4      exx,eyy,exy,sxx,syy,sxy,
     5      ugpd,ued,ufd,uvld,utjd,uJvld,ufTd,uTfd,uTvld,uXfd,uXvld)
      write(lun,*)
      close(lun)
c
      if (nf.gt.0) then
      open(lun,file='aposteriori_minus_apriori_on_faults.log')
      call fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
     2      dufd,dufTd,duXfd)
      write(lun,*)
      close(lun)
      end if
c
      open(lun,file='misfit_to_constraints.log')
      call zerror(lun,nz,e1s,e2s,isf,isvl,isrb,fs,sfs,
     1      vls,svls,rbs,srbs,sz,kz,jz,ux,uy,usrb,slong,slat,z)
      write(lun,*)
      close(lun)
c
      open(lun,file='misfit_to_dynamics.log')
c
      write(lun,*) 'In the apriori solution the total sum of squares'
      write(lun,*) ' for the dynamics part is',sy00
      write(lun,*) 'In preparation for the statistical aposteriori'
      write(lun,*) ' solution, using observables as well as the'
      write(lun,*) ' dynamics part, the dynamics was scaled so that'
      write(lun,*) ' this sum of squares became the number of'
      write(lun,*) ' degrees of freedom in the model',nx2
      write(lun,*) 'The residual sum of squares in the dynamics part'
      write(lun,*) ' of the apriori solution is',s00
      write(lun,*) 'After the scaling this became',
     1      s00*dfloat(nx2)/sy00
      write(lun,*) 'In the aposteriori solution the total sum of'
      write(lun,*) ' squares for the scaled dynamics part is',sy01
      write(lun,*) 'In the aposteriori solution the residual sum of'
      write(lun,*) ' squares for the scaled dynamics part increased'
      write(lun,*) ' through inclusion of observables to',s01
c
      call ymatch(lun,ny0,ne,nf,nfs,sf,elong,elat,slong,slat,
     1      y0,y,y1)
      write(lun,*)
      close(lun)
c
      open(lun,file='misfit_to_observations_apriori.log')
c
      write(lun,*) 'The number of actual observables is',ny1
      write(lun,*) 'With the apriori solution the residual sum of'
      write(lun,*) ' squares for (unused) actual observables is',s10
      write(lun,*) 'Therefore, the total residual sum of squares'
      write(lun,*) ' before aposteriori minimisation was',
     1      s10+s00*dfloat(nx2)/sy00
c
      call misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
     1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
     2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
     3      eec,ncec,e1ec,e2ec,y)
      write(lun,*)
      close(lun)
c
      open(lun,file='misfit_to_observations_aposteriori.log')
c
      write(lun,*) 'The number of actual observables is',ny1
      write(lun,*) 'In the aposteriori solution the residual sum of'
      write(lun,*) ' squares for the actual observables is',s11
      write(lun,*) 'So the total residual sum of squares reduced'
      write(lun,*) ' through inclusion of observables to',s01+s11
c
      call misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
     1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
     2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
     3      eec,ncec,e1ec,e2ec,y1)
      write(lun,*)
      close(lun)
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
      SUBROUTINE numobs(ne,nf,nfs,isf,sfs,nvo,nfo,neo,nduc,nec,evo,
     1      ffo,sfo,nfoc,eeo,neoc,fduc,sduc,ncduc,s1duc,s2duc,
     2      eec,ncec,e1ec,e2ec,envo,eneo,enec,fsnfo,fsnduc)
c
c This routine loads the numbers of observations into element arrays
c  and fault-side arrays.
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
      integer ne,nf,nvo,nfo,neo,nduc,nec
      integer i,j,e,f,s
      integer nfs(maxf)
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      integer isf(maxs)
      integer sfs(maxsf)
      integer envo(maxe),eneo(maxe),enec(maxe)
      integer fsnfo(maxfs,maxf),fsnduc(maxfs,maxf)
c
      do i=1,ne
            envo(i)=0
            eneo(i)=0
            enec(i)=0
      end do
c
      if (nf.gt.0) then
      do i=1,nf
            do j=1,nfs(i)
                  fsnfo(j,i)=0
                  fsnduc(j,i)=0
            end do
      end do
      end if
c
      if (nvo.gt.0) then
      do i=1,nvo
            e=evo(i)
            envo(e)=envo(e)+1
      end do
      end if
c
      if (nfo.gt.0) then
      do i=1,nfo
            f=ffo(i)
            j=sfs(isf(sfo(i)))
            fsnfo(j,f)=fsnfo(j,f)+nfoc(i)
      end do
      end if
c
      if (neo.gt.0) then
      do i=1,neo
            e=eeo(i)
            eneo(e)=eneo(e)+neoc(i)
      end do
      end if
c
      if (nduc.gt.0) then
      do i=1,nduc
            f=fduc(i)
            do j=1,ncduc(i)
                  s=sfs(isf(sduc(s1duc(j,i),i)))
                  fsnduc(s,f)=fsnduc(s,f)+1
                  s=sfs(isf(sduc(s2duc(j,i),i)))
                  fsnduc(s,f)=fsnduc(s,f)+1
            end do
      end do
      end if
c
      if (nec.gt.0) then
      do i=1,nec
            do j=1,ncec(i)
                  e=eec(e1ec(j,i),i)
                  enec(e)=enec(e)+1
                  e=eec(e2ec(j,i),i)
                  enec(e)=enec(e)+1
            end do
      end do
      end if
c
      return
      end
c
c
      SUBROUTINE mesh(lun,ngp,ns,ne,nf,nvl,nrb,
     1      long,lat,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,svl,gpvl,fvl,nrbs,srb,gprb)
c
c This routine outputs to the file with unit number 'lun' the geometry
c  of the mesh, including grid points, element sides, elements, faults,
c  velocity lines and rigid boundaries.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxrb,maxrbs
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxrb=10,maxrbs=200)
      integer lun,ngp,ns,ne,nf,nvl,nrb
      integer i,j
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
      real*8 long(maxgp),lat(maxgp)
c
      write(lun,*) ngp,' Grid points'
      write(lun,*) 'Each line = index,long,lat'
      do i=1,ngp
            write(lun,*) i,long(i),lat(i)
      end do
c
      write(lun,*) ns,' Element sides'
      write(lun,*) 'Each line = index,element1,element2'
      write(lun,*) 'Sides with element2 = 0 are at external boundaries'
      do i=1,ns
            write(lun,*) i,e1s(i),e2s(i)
      end do
c
      write(lun,*) ne,' Elements'
      write(lun,*) 'First line = index,side1,side2,side3'
      write(lun,*) 'Second line = grid-point1,grid-point2,grid-point3'
      write(lun,*) 'Grid-point1 is opposite side1, and so on'
      do i=1,ne
            write(lun,*) i,s1e(i),s2e(i),s3e(i)
            write(lun,*) gp1e(i),gp2e(i),gp3e(i)
      end do
c
      write(lun,*) nf,' Faults'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = fault,index,element-side'
      write(lun,*) 'Second = first-grid-point,second-grid-point'
      if (nf.gt.0) then
      do i=1,nf
            write(lun,*) i,nfs(i)
            do j=1,nfs(i)
                  write(lun,*) i,j,sf(j,i)
                  write(lun,*) gpf(j-1,i),gpf(j,i)
            end do
      end do
      end if
c
      write(lun,*) nvl,' Velocity lines'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = v-line,index,element-side'
      write(lun,*) 'Second = first-grid-point,fault-at-grid-point'
      write(lun,*) 'Third = second-grid-point,fault-at-grid-point'
      write(lun,*) 'Where there is no fault fault-at-grid-point = 0'
      if (nvl.gt.0) then
      do i=1,nvl
            write(lun,*) i,nvls(i)
            do j=1,nvls(i)
                  write(lun,*) i,j,svl(j,i)
                  write(lun,*) gpvl(j-1,i),fvl(j-1,i)
                  write(lun,*) gpvl(j,i),fvl(j,i)
            end do
      end do
      end if
c
      write(lun,*) nrb,' Rigid boundaries'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = rigid-b,index,element-side'
      write(lun,*) 'Second = first-grid-point,second-grid-point'
      if (nrb.gt.0) then
      do i=1,nrb
            write(lun,*) i,nrbs(i)
            do j=1,nrbs(i)
                  write(lun,*) i,j,srb(j,i)
                  write(lun,*) gprb(j-1,i),gprb(j,i)
            end do
      end do
      end if
c
      return
      end
c
c
      SUBROUTINE forces(lun,nf,ne,nfs,sf,gpf,slong,slat,
     1      long,lat,lenfs,Kcfs,Ksfs,area,elong,elat,
     2      fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse)
c
c This routine outputs to the file with unit number 'lun' slip-rate
c  capacity values for fault segments and force and strain-rate capacity
c  values in elements. Faults are done first because of the complexity
c  and shear volume of strain-rate capacity information.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100)
      integer lun,nf,ne
      integer i,j,s,gp1,gp2
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      real*8 aconv,Lconv,dx,dy,a,az,f1,f2,g1,g2,azc
      real*8 s11,s22,s12,e11,e22,e12,s0
      real*8 long(maxgp),lat(maxgp)
      real*8 area(maxe)
      real*8 fx(maxe),fy(maxe)
      real*8 Lce(maxe),Lcce(maxe),Lcse(maxe),
     1      Lse(maxe),Lsce(maxe),Lsse(maxe)
      real*8 lenfs(maxfs,maxf)
      real*8 Kcfs(maxfs,maxf),Ksfs(maxfs,maxf)
      real*8 elong(maxe),elat(maxe)
      real*8 slong(maxs),slat(maxs)
      real*8 L(3,3),D(3),V(3,3)
c
      aconv=45.0d0/datan(1.0d0)
      Lconv=dsqrt(2.0d0)
c
      write(lun,*) nf,' Faults'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = fault,index,long,lat'
      write(lun,*) 'Second = length,az'
      write(lun,*) 'Third = Kc+Ks,Kc,Ks'
      write(lun,*) 'The total capacity Kc+Ks applies for strike-slip'
      write(lun,*) ' and Kc alone for the horizontal part of dip-slip'
      if (nf.gt.0) then
      do i=1,nf
            write(lun,*) i,nfs(i)
            do j=1,nfs(i)
                  s=sf(j,i)
                  write(lun,*) i,j,slong(s),slat(s)
                  gp1=gpf(j-1,i)
                  gp2=gpf(j,i)
                  dx=long(gp2)-long(gp1)
                  dy=lat(gp2)-lat(gp1)
                  dx=dx*dcos(slat(s)/aconv)
                  a=datan2(dy,dx)
                  az=90.0d0-a*aconv
                  write(lun,*) lenfs(j,i),az
                  write(lun,*) Kcfs(j,i)+Ksfs(j,i),Kcfs(j,i),
     1                  Ksfs(j,i)
            end do
      end do
      end if
c
      write(lun,*) ne,' Elements'
      write(lun,*) 'First line = index,area,long,lat'
      write(lun,*) 'Second = fx,fy'
      write(lun,*) 'Third = az,f1'
      write(lun,*) 'Fourth = Lc,Lcc,Lcs'
      write(lun,*) 'Fifth = Ls,Lsc,Lss'
      write(lun,*) 'Sixth = sqrt(Lcc**2+Lcs**2)/Lc,az'
      write(lun,*) 'Seventh = sqrt(Lsc**2+Lss**2)/Ls,az'
      write(lun,*) 'First eigenvalue line = index,L'
      write(lun,*) 'Second eigenvalue line = sxx,syy,sxy'
      write(lun,*) 'Third eigenvalue line = exx,eyy,exy'
      write(lun,*) 'Fourth eigenvalue line = az,s11,s22'
      write(lun,*) 'Fifth eigenvalue line = az,e11,e22'
      write(lun,*) 'For stress and strain rate the azimuth az is for'
      write(lun,*) ' the first principal values s11 and e11'
      do i=1,ne
            write(lun,*) i,area(i),elong(i),elat(i)
            write(lun,*) fx(i),fy(i)
            f1=fx(i)

            f2=fy(i)
            if ((f1.eq.0.0d0).and.(f2.eq.0.0d0)) then
                  a=0.0d0
            else
                  a=datan2(f2,f1)
            end if
            f1=dsqrt(f1**2+f2**2)
            az=90.0d0-a*aconv
            write(lun,*) az,f1
            write(lun,*) Lce(i),Lcce(i),Lcse(i)
            write(lun,*) Lse(i),Lsce(i),Lsse(i)
            g1=Lcce(i)
            g2=Lcse(i)
            if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
                  az=90.0d0
            else
                  a=0.5d0*datan2(g2,g1)
                  g1=dsqrt(g1**2+g2**2)
                  az=a*aconv
            end if
            if (az.lt.0.0d0) az=az+180.0d0
            write(lun,*) g1/Lce(i),az
            azc=az
            g1=Lsce(i)
            g2=Lsse(i)
            if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
                  az=90.0d0
            else
                  a=0.25d0*datan2(g2,g1)
                  g1=dsqrt(g1**2+g2**2)
                  az=a*aconv
            end if
            if (az.lt.azc-45.0d0) az=az+90.0d0
            if (az.gt.azc+45.0d0) az=az-90.0d0
            write(lun,*) g1/Lse(i),az
            L(1,1)=0.5d0*(Lce(i)+Lcce(i))+0.125d0*(Lse(i)-Lsce(i))
            L(2,2)=0.5d0*(Lce(i)-Lcce(i))+0.125d0*(Lse(i)-Lsce(i))
            L(3,3)=0.25d0*Lce(i)+0.125d0*(Lse(i)+Lsce(i))
            L(1,2)=-0.125d0*(Lse(i)-Lsce(i))
            L(1,3)=-0.25d0*Lcse(i)+0.125d0*Lsse(i)
            L(2,3)=-0.25d0*Lcse(i)-0.125d0*Lsse(i)
            L(1,3)=L(1,3)*Lconv
            L(2,3)=L(2,3)*Lconv
            L(3,3)=L(3,3)*Lconv**2
            L(2,1)=L(1,2)
            L(3,1)=L(1,3)
            L(3,2)=L(2,3)
            call jacobi(3,3,L,D,V)
            do j=1,3
                  write(lun,*) j,D(j)
                  s11=V(1,j)
                  s22=V(2,j)
                  s12=V(3,j)/Lconv
                  e11=D(j)*s11
                  e22=D(j)*s22
                  e12=D(j)*s12
                  write(lun,*) s11,s22,s12
                  write(lun,*) e11,e22,e12
                  s0=0.5d0*(s11+s22)
                  g1=0.5d0*(s11-s22)
                  g2=s12
                  if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
                        a=0.0d0
                  else
                        a=0.5d0*datan2(g2,g1)
                  end if
                  g1=dsqrt(g1**2+g2**2)
                  az=90.0d0-a*aconv
                  s11=s0+g1
                  s22=s0-g1
                  e11=D(j)*s11
                  e22=D(j)*s22
                  write(lun,*) az,s11,s22
                  write(lun,*) az,e11,e22
            end do
      end do
c
      return
      end
c
c
      SUBROUTINE jacobi(n,nmax,L,D,V)
c
c This routine calculates eigenvalues and eigenvectors of a real
c  symmetric matrix using the Jacobi transformation method.
c
      implicit none
      integer n,nmax
      integer i,im,j,iter,ip,jm,jp,k
      real*8 s,Lav,Lmag,Lmax,Lmin,t,x,y,Lk
      real*8 L(nmax,nmax),D(nmax),V(nmax,nmax)
      real*8 W(nmax),Z(nmax)
c
      s=0.0d0
      do i=1,n
            s=s+L(i,i)
      end do
      Lav=s/dfloat(n)
      s=0.0d0
      do i=2,n
            im=i-1
            do j=1,im
                  s=s+dabs(L(j,i))
            end do
      end do
      s=2.0d0*s
      do i=1,n
            D(i)=L(i,i)-Lav
            s=s+dabs(D(i))
      end do
      if (s.eq.0.0d0) then
            do i=1,n
                  do j=1,n
                        V(j,i)=0.0d0
                  end do
                  V(i,i)=1.0d0
                  D(i)=Lav
            end do
            return
      end if
      Lmag=s/dfloat(n)
c
      do i=1,n
            do j=1,n
                  V(j,i)=0.0d0
            end do
            V(i,i)=1.0d0
            D(i)=D(i)/Lmag
            W(i)=D(i)
            Z(i)=0.0d0
      end do
      Lmax=0.0d0
      do i=2,n
            im=i-1
            do j=1,im
                  L(j,i)=L(j,i)/Lmag
                  if (dabs(L(j,i)).gt.Lmax) Lmax=dabs(L(j,i))
            end do
      end do
      iter=0
c
10    iter=iter+1
      Lmin=Lmax
      do i=1,iter
            if (Lmin.lt.(1.0d-16)) goto 20
            Lmin=0.5d0*Lmin
      end do
20    continue
      do i=2,n
            im=i-1
            ip=i+1
            do j=1,im
                  jm=j-1
                  jp=j+1
                  if (dabs(L(j,i)).lt.Lmin) goto 30
                  if (dabs(L(j,i)).le.(1.0d-16)) then
                        L(j,i)=0.0d0
                        goto 30
                  end if
                  t=0.5d0*(W(j)-W(i))
                  if (t.lt.0.0d0) then
                        t=-L(j,i)/(dsqrt(t**2+L(j,i)**2)-t)
                  else
                        t=L(j,i)/(dsqrt(t**2+L(j,i)**2)+t)
                  end if
                  W(j)=W(j)+t*L(j,i)
                  W(i)=W(i)-t*L(j,i)
                  Z(j)=Z(j)+t*L(j,i)
                  Z(i)=Z(i)-t*L(j,i)
                  L(j,i)=0.0d0
                  x=1.0d0/dsqrt(1.0d0+t**2)
                  y=t*x
                  t=y/(1.0d0+x)
                  if (jm.ge.1) then
                        do k=1,jm
                              Lk=L(k,j)
                              L(k,j)=Lk+y*(L(k,i)-t*Lk)
                              L(k,i)=L(k,i)-y*(Lk+t*L(k,i))
                        end do
                  end if
                  if (jp.le.im) then
                        do k=jp,im
                              Lk=L(j,k)
                              L(j,k)=Lk+y*(L(k,i)-t*Lk)
                              L(k,i)=L(k,i)-y*(Lk+t*L(k,i))
                        end do
                  end if
                  if (ip.le.n) then
                        do k=ip,n
                              Lk=L(j,k)
                              L(j,k)=Lk+y*(L(i,k)-t*Lk)
                              L(i,k)=L(i,k)-y*(Lk+t*L(i,k))
                        end do
                  end if
                  do k=1,n
                        Lk=V(k,j)
                        V(k,j)=Lk+y*(V(k,i)-t*Lk)
                        V(k,i)=V(k,i)-y*(Lk+t*V(k,i))
                  end do
30                continue
            end do
      end do
      do i=1,n

            D(i)=D(i)+Z(i)
            W(i)=D(i)
            Z(i)=0.0d0
      end do
      Lmax=0.0d0
      do i=2,n
            im=i-1
            do j=1,im
                  if (dabs(L(j,i)).gt.Lmax) Lmax=dabs(L(j,i))
            end do
      end do
      if (Lmax.gt.1.0d-16) goto 10
c
      do i=n,2,-1
            k=i
            Lk=D(i)
            im=i-1
            do j=im,1,-1
                  L(j,i)=L(i,j)
                  if (D(j).lt.Lk) then
                        k=j
                        Lk=D(j)
                  end if
            end do
            if (k.ne.i) then
                  D(k)=D(i)
                  D(i)=Lk
                  do j=1,n
                        Lk=V(j,k)
                        V(j,k)=V(j,i)
                        V(j,i)=Lk
                  end do
             end if
             D(i)=Lav+Lmag*D(i)
       end do
       D(1)=Lav+Lmag*D(1)
c
      return
      end
c
c
      SUBROUTINE fixedu(lun,nvl,nrb,nvls,fvl,svl,gpvl,
     1      nrbs,srb,gprb,slong,slat,long,lat,
     2      ux,uy,uxm,uym,uxp,uyp,usrb,ugprb)
c
c This routine outputs to the file with unit number 'lun' the fixed
c  velocity values on velocity lines and rigid boundaries.
c
      implicit none
      integer maxgp,maxs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs
      parameter(maxgp=40000,maxs=3*maxgp,
     1      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     2      maxrb=10,maxrbs=200)
      integer lun,nvl,nrb
      integer i,nj,j,s,vlj,gp
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),srb(maxrbs,maxrb)
      integer gprb(0:maxrbs,maxrb)
      real*8 aconv,u1,u2,a,az
      real*8 long(maxgp),lat(maxgp)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb),usrb(2,maxrbs,maxrb)
      real*8 slong(maxs),slat(maxs)
c
      aconv=45.0d0/datan(1.0d0)
c
      write(lun,*) nvl,' Velocity Lines'
      write(lun,*) 'First line = index,number-of-points'
      write(lun,*) 'First point line = v-line,index,long,lat'
      write(lun,*) 'Second = ux,uy'
      write(lun,*) 'Third = az,u1'
      write(lun,*) 'Each fault crossed counts as two points, with its'
      write(lun,*) ' negative side first and positive side second'
      if (nvl.gt.0) then
      do i=1,nvl
            nj=2*nvls(i)+1
            do j=0,nvls(i)
                  if (fvl(j,i).ne.0) nj=nj+1
            end do
            write(lun,*) i,nj
            nj=0
            do j=0,nvls(i)
                  if (j.gt.0) then
                        nj=nj+1
                        s=svl(j,i)
                        write(lun,*) i,nj,slong(s),slat(s)
                        vlj=2*j
                        u1=ux(vlj,i)
                        u2=uy(vlj,i)
                        write(lun,*) u1,u2
                        if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                              a=0.0d0
                        else
                              a=datan2(u2,u1)
                        end if
                        u1=dsqrt(u1**2+u2**2)
                        az=90.0d0-a*aconv
                        write(lun,*) az,u1
                  end if
                  if (fvl(j,i).eq.0) then
                        nj=nj+1
                        gp=gpvl(j,i)
                        write(lun,*) i,nj,long(gp),lat(gp)
                        vlj=2*j+1
                        u1=ux(vlj,i)
                        u2=uy(vlj,i)
                        write(lun,*) u1,u2
                        if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                              a=0.0d0
                        else
                              a=datan2(u2,u1)
                        end if
                        u1=dsqrt(u1**2+u2**2)
                        az=90.0d0-a*aconv
                        write(lun,*) az,u1
                  else
                        nj=nj+1
                        gp=gpvl(j,i)
                        write(lun,*) i,nj,long(gp),lat(gp)
                        u1=uxm(j,i)
                        u2=uym(j,i)
                        write(lun,*) u1,u2
                        if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                              a=0.0d0
                        else
                              a=datan2(u2,u1)
                        end if
                        u1=dsqrt(u1**2+u2**2)
                        az=90.0d0-a*aconv
                        write(lun,*) az,u1
                        nj=nj+1
                        write(lun,*) i,nj,long(gp),lat(gp)
                        u1=uxp(j,i)
                        u2=uyp(j,i)
                        write(lun,*) u1,u2
                        if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                              a=0.0d0
                        else
                              a=datan2(u2,u1)
                        end if
                        u1=dsqrt(u1**2+u2**2)
                        az=90.0d0-a*aconv
                        write(lun,*) az,u1
                  end if
            end do
      end do
      end if
c
      write(lun,*) nrb,' Rigid boundaries'
      write(lun,*) 'First line = index,number-of-points'
      write(lun,*) 'First point line = v-line,index,long,lat'
      write(lun,*) 'Second = ux,uy'
      write(lun,*) 'Third = az,u1'
      if (nrb.gt.0) then
      do i=1,nrb
            nj=2*nrbs(i)+1
            write(lun,*) i,nj
            do j=0,nrbs(i)
                  if (j.gt.0) then
                        nj=2*j
                        s=srb(j,i)
                        write(lun,*) i,nj,slong(s),slat(s)
                        u1=usrb(1,j,i)
                        u2=usrb(2,j,i)
                        write(lun,*) u1,u2
                        if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                              a=0.0d0
                        else
                              a=datan2(u2,u1)
                        end if
                        u1=dsqrt(u1**2+u2**2)
                        az=90.0d0-a*aconv
                        write(lun,*) az,u1
                  end if
                  nj=2*j+1
                  gp=gprb(j,i)
                  write(lun,*) i,nj,long(gp),lat(gp)
                  u1=ugprb(1,0,j,i)
                  u2=ugprb(2,0,j,i)
                  write(lun,*) u1,u2
                  if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                        a=0.0d0
                  else
                        a=datan2(u2,u1)
                  end if
                  u1=dsqrt(u1**2+u2**2)
                  az=90.0d0-a*aconv
                  write(lun,*) az,u1
            end do
      end do
      end if
c
      return
      end
c
c
      SUBROUTINE xvalue(lun,nx,kx,ix,jx,jcx,icx,xc,x0,x1,xd)
c
c This routine outputs to the file with unit number 'lun' the raw
c  solution vector x and its association with elements and grid points.
c
      implicit none
      integer maxgp,maxf,maxfs,maxvl,maxvls,
     1      maxsf,maxsvl,maxpf,maxpvl,
     2      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     3      maxx
      parameter(maxgp=40000,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     4      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     5      maxtj=80,maxJvl=10,maxfT=10,
     6      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     7      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     8      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     9      +2*(maxpf+maxfT+maxXf)))
      integer lun,nx
      integer i
      integer kx(maxx),ix(maxx),jx(maxx),jcx(maxx),icx(maxx)
      real*8 xc(maxx),x0(maxx),x1(maxx),xd(maxx)
c
      write(lun,*) nx,' Values of x'
      write(lun,*) 'First line = index,kind'
      write(lun,*) 'Second = i-index,j-index,derivative,component'
      write(lun,*) 'Third = xc,x0,x1,xd'
      write(lun,*) 'kind has the following values:'
      write(lun,*) ' 0 = u at grid point'
      write(lun,*) ' 1 = u on negative side of fault'
      write(lun,*) ' 2 = u on negative side of velocity line'
      write(lun,*) ' 3 = u at triple junction of faults'
      write(lun,*) ' 4 = u at joining of velocity lines'
      write(lun,*) ' 5 = u at truncation at a fault'
      write(lun,*) ' 6 = u at truncation of a fault'
      write(lun,*) ' 7 = u at truncation of a velocity line'
      write(lun,*) ' 8 = u at crossing of a fault'
      write(lun,*) ' 9 = u at crossing of two velocity lines'
      write(lun,*) ' 21 = du on a fault'
      write(lun,*) ' 25 = du at truncation at a fault'
      write(lun,*) ' 28 = du at crossing of a fault'
      write(lun,*) 'i-index = grid point, fault, etc'
      write(lun,*) 'j-index = fault or v-line side, region for others'
      write(lun,*) 'xc = constraint part of solution'
      write(lun,*) 'x0 = apriori solution'
      write(lun,*) 'x1 = aposteriori solution'
      write(lun,*) 'xd = aposteriori minus apriori'
      do i=1,nx
            write(lun,*) i,kx(i)
            write(lun,*) ix(i),jx(i),jcx(i),icx(i)
            write(lun,*) xc(i),x0(i),x1(i),xd(i)
      end do
c
      return
      end
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
      real*8 aconv,e11,e22,e12,s11,s22,s12,u1,u2,a,az,s0,g1,g2
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
      aconv=45.0d0/datan(1.0d0)
c
      write(lun,*) ne,' Elements'
      write(lun,*) 'First line = index,long,lat'
      write(lun,*) 'Second line = nvo,neo,nec,area'
      write(lun,*) 'Third = ux,uy'
      write(lun,*) 'Fourth = exx,eyy,exy'
      write(lun,*) 'Fifth = sxx,syy,sxy'
      write(lun,*) 'Sixth = az,u1'
      write(lun,*) 'Seventh = az,e11,e22'
      write(lun,*) 'Eighth = az,s11,s22'
      write(lun,*) 'For strain rate and stress the azimuth az is for'
      write(lun,*) ' the first principal values e11 and s11'
      do i=1,ne
            write(lun,*) i,elong(i),elat(i)
            write(lun,*) envo(i),eneo(i),enec(i),area(i)
            call umat(rE,i,gp1e,gp2e,gp3e,long,lat,iefv,
     1            kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     2            jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     3            ugp,ue,uf,uvl,utj,uJvl,ufT,uTf,uTvl,uXf,uXvl,u)
            write(lun,*) ue(1,i),ue(2,i)
            e11=0.0d0
            e22=0.0d0
            e12=0.0d0
            s11=0.0d0
            s22=0.0d0
            s12=0.0d0
            do k=0,8
            do ic=1,2
                  e11=e11+exx(ic,k,i)*u(ic,k)
                  e22=e22+eyy(ic,k,i)*u(ic,k)
                  e12=e12+exy(ic,k,i)*u(ic,k)
                  s11=s11+sxx(ic,k,i)*u(ic,k)
                  s22=s22+syy(ic,k,i)*u(ic,k)
                  s12=s12+sxy(ic,k,i)*u(ic,k)
            end do
            end do
            write(lun,*) e11,e22,e12
            write(lun,*) s11,s22,s12
            u1=ue(1,i)
            u2=ue(2,i)
            if ((u1.eq.0.0d0).and.(u2.eq.0.0d0)) then
                  a=0.0d0
            else
                  a=datan2(u2,u1)
            end if
            u1=dsqrt(u1**2+u2**2)
            az=90.0d0-a*aconv
            write(lun,*) az,u1
            s0=0.5d0*(e11+e22)
            g1=0.5d0*(e11-e22)
            g2=e12
            if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
                  a=0.0d0
            else
                  a=0.5d0*datan2(g2,g1)
            end if
            g1=dsqrt(g1**2+g2**2)
            az=90.0d0-a*aconv
            e11=s0+g1
            e22=s0-g1
            write(lun,*) az,e11,e22
            s0=0.5d0*(s11+s22)
            g1=0.5d0*(s11-s22)
            g2=s12
            if ((g1.eq.0.0d0).and.(g2.eq.0.0d0)) then
                  a=0.0d0
            else
                  a=0.5d0*datan2(g2,g1)
            end if
            g1=dsqrt(g1**2+g2**2)
            az=90.0d0-a*aconv
            s11=s0+g1
            s22=s0-g1
            write(lun,*) az,s11,s22
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
      SUBROUTINE fvalue(lun,nf,nfs,sf,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      fsnfo,fsnduc,slong,slat,long,lat,lenfs,dufs,ttfs,tnfs,
     2      duf,dufT,duXf)
c
c This routine outputs to the file with unit number 'lun' slip-rate
c  and traction values in fault segments for the solution, or
c  difference between solutions, in the duf to duXf vectors.
c
      implicit none
      integer maxgp,maxs,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxsvl,maxpvl,
     3      maxfT,maxXf
      parameter(maxgp=40000,maxs=3*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsvl=maxvl*maxvls,maxpvl=maxsvl+maxvl,
     4      maxfT=10,maxXf=200)
      integer lun,nf
      integer i,j,s,gp1,gp2,k
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      integer ipvl(maxgp)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer fsnfo(maxfs,maxf),fsnduc(maxfs,maxf)
      real*8 aconv,dx,dy,dl,tx,ty,a,az,nx,ny
      real*8 dut,dun,tt,tn,du1,du2,sn1,sn2
      real*8 long(maxgp),lat(maxgp)
      real*8 lenfs(maxfs,maxf),dufs(0:1,maxfs,maxf)
      real*8 ttfs(0:1,maxfs,maxf),tnfs(0:1,maxfs,maxf)
      real*8 duf(2,0:1,0:maxfs,maxf)
      real*8 dufT(2,0:1,maxfT),duXf(2,0:1,maxXf)
      real*8 slong(maxs),slat(maxs)
      real*8 du(2,0:3)
c
      aconv=45.0d0/datan(1.0d0)
c
      write(lun,*) nf,' Faults'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = fault,index,long,lat'
      write(lun,*) 'Second = nfo,nduc,length,az'
      write(lun,*) 'Third = tx,ty,az'
      write(lun,*) 'Fourth = nx,ny,az'
      write(lun,*) 'Fifth = dut,dun (slip rates)'
      write(lun,*) 'Sixth = tt(=snt),tn(=snn) (tractions)'
      write(lun,*) 'Seventh = dux,duy'
      write(lun,*) 'Eighth = snx,sny'
      write(lun,*) 'Nineth = az,du1'
      write(lun,*) 'Tenth = az,sn1'
      do i=1,nf
            write(lun,*) i,nfs(i)
            do j=1,nfs(i)
                  s=sf(j,i)
                  write(lun,*) i,j,slong(s),slat(s)
                  gp1=gpf(j-1,i)
                  gp2=gpf(j,i)
                  dx=long(gp2)-long(gp1)
                  dy=lat(gp2)-lat(gp1)
                  dx=dx*dcos(slat(s)/aconv)
                  dl=dsqrt(dx**2+dy**2)
                  tx=dx/dl
                  ty=dy/dl
                  a=datan2(ty,tx)
                  az=90.0d0-a*aconv
                  write(lun,*) fsnfo(j,i),fsnduc(j,i),lenfs(j,i),az
                  write(lun,*) tx,ty,az
                  nx=-ty
                  ny=tx
                  a=datan2(ny,nx)
                  az=90.0d0-a*aconv
                  write(lun,*) nx,ny,az
                  call dumat(i,j,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1                  duf,dufT,duXf,du)
                  dut=0.0d0
                  dun=0.0d0
                  tt=0.0d0
                  tn=0.0d0
                  do k=0,1
                        dut=dut+dufs(k,j,i)*du(1,k)
                        dun=dun+dufs(k,j,i)*du(2,k)
                        tt=tt+ttfs(k,j,i)*du(1,k)
                        tn=tn+tnfs(k,j,i)*du(2,k)
                  end do
                  write(lun,*) dut,dun
                  write(lun,*) tt,tn
                  du1=dut*tx+dun*nx
                  du2=dut*ty+dun*ny
                  sn1=tt*tx+tn*nx
                  sn2=tt*ty+tn*ny
                  write(lun,*) du1,du2
                  write(lun,*) sn1,sn2
                  if ((du1.eq.0.0d0).and.(du2.eq.0.0d0)) then
                        a=0.0d0
                  else
                        a=datan2(du2,du1)
                  end if
                  du1=dsqrt(du1**2+du2**2)
                  az=90.0d0-a*aconv
                  write(lun,*) az,du1
                  if ((sn1.eq.0.0d0).and.(sn2.eq.0.0d0)) then
                        a=0.0d0
                  else


                        a=datan2(sn2,sn1)
                  end if
                  sn1=dsqrt(sn1**2+sn2**2)
                  az=90.0d0-a*aconv
                  write(lun,*) az,sn1
            end do
      end do
c
      return
      end
c
c
      SUBROUTINE dumat(f,s,gpf,ipvl,fTvlp,Tfvlp,Xfvlp,
     1      duf,dufT,duXf,du)
c
c This assembles the slip-rate values at the nodes of a fault segment.
c
      implicit none
      integer maxgp,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxsvl,maxpvl,
     3      maxfT,maxXf
      parameter(maxgp=40000,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxsvl=maxvl*maxvls,maxpvl=maxsvl+maxvl,
     4      maxfT=10,maxXf=200)
      integer f,s
      integer gp1,gp2,ic,ip,fT,Xf
      integer gpf(0:maxfs,maxf)
      integer ipvl(maxgp)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      real*8 duf(2,0:1,0:maxfs,maxf)
      real*8 dufT(2,0:1,maxfT),duXf(2,0:1,maxXf)
      real*8 du(2,0:3)
c
      gp1=gpf(s-1,f)
      gp2=gpf(s,f)
      do ic=1,2
            du(ic,0)=duf(ic,0,s-1,f)
      end do
      if (ipvl(gp2).eq.0) then
            do ic=1,2
                 du(ic,1)=duf(ic,0,s,f)
            end do
      else
            ip=ipvl(gp2)
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do ic=1,2
                       du(ic,1)=dufT(ic,0,fT)
                  end do
                  goto 10
            end if
            if (Tfvlp(ip).ne.0) then
                  do ic=1,2
                       du(ic,1)=duf(ic,0,s,f)
                  end do
                  goto 10
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do ic=1,2
                       du(ic,1)=duXf(ic,0,Xf)
                  end do
            end if
10          continue
      end if
c
      return
      end
c
c
      SUBROUTINE zerror(lun,nz,e1s,e2s,isf,isvl,isrb,fs,sfs,
     1      vls,svls,rbs,srbs,sz,kz,jz,ux,uy,usrb,slong,slat,z)
c
c This routine outputs to the file with unit number 'lun' the residuals
c  in constructing the xc vector contribution that constraint z values
c  make to the apriori and aposteriori solutions.
c The other parts of the apriori and aposteriori solutions belong to a
c  subspace of x vectors that is perpendicular to the set of possible xc
c  vectors, and the numerical-precision errors in projecting onto that
c  subspace will be comparable to the errors here in matching the z
c  values.
c
      implicit none
      integer maxgp,maxs,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxtj,maxz
      parameter(maxgp=40000,maxs=3*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxtj=80,
     6      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer lun,nz
      integer i,s,ic,f,j,vl,rb
      integer e1s(maxs),e2s(maxs)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer sz(maxz),kz(maxz),jz(maxz)
      real*8 zval
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl)
      real*8 usrb(2,maxrbs,maxrb)
      real*8 slong(maxs),slat(maxs)
      real*8 z(maxz)
c
      write(lun,*) nz,' Constraints'
      write(lun,*) 'First line = index,side,long,lat'
      write(lun,*)
     1      'Second (Class 0) = class,sub-class,element-1,element-2'
      write(lun,*)
     1      'Second (Class 1) = class,sub-class,fault,segment'
      write(lun,*)
     1      'Second (Class 2) = class,sub-class,v-line,segment'
      write(lun,*)
     1      'Second (Class 3) = class,sub-class,rigid-b,segment'
      write(lun,*) 'Third = component,value,residual'
      do i=1,nz
            s=sz(i)
            ic=1+mod(i-1,2)
            write(lun,*) i,s,slong(s),slat(s)
            if (kz(i).eq.0) then
                  write(lun,*) kz(i),jz(i),e1s(s),e2s(s)
                  zval=0.0d0
            end if
            if (kz(i).eq.1) then
                  f=fs(isf(s))
                  j=sfs(isf(s))
                  write(lun,*) kz(i),jz(i),f,j
                  zval=0.0d0
            end if
            if (kz(i).eq.2) then
                  vl=vls(isvl(s))
                  j=svls(isvl(s))
                  write(lun,*) kz(i),jz(i),vl,j
                  if (ic.eq.1) zval=ux(2*j,vl)
                  if (ic.eq.2) zval=uy(2*j,vl)
            end if
            if (kz(i).eq.3) then
                  rb=rbs(isrb(s))
                  j=srbs(isrb(s))
                  write(lun,*) kz(i),jz(i),rb,j
                  zval=usrb(ic,j,rb)
            end if
            write(lun,*) ic,zval,z(i)
      end do
c
      return
      end
c
c
      SUBROUTINE ymatch(lun,ny0,ne,nf,nfs,sf,elong,elat,slong,slat,
     1      y0,y,y1)
c
c This routine outputs to the file with unit number 'lun' residuals for
c  the dynamics parts of the apriori and aposteriori solutions. The y0
c  vector is the unscaled apriori solution, the y vector is the scaled
c  apriori solution, with the same scaling as the aposteriori solution,
c  and y1 vector is the aposteriori solution.
c
c The scaling of the aposteriori solution is designed to match the
c  standardised statistical scaling of the observation residuals in the
c  second parts of the y and y1 vectors. To ensure numerical stability
c  the dynamics is over-sampled by a factor of 2 in each spatial
c  dimension.
c So, of the 12 dynamics residuals in each element there are 6 for each
c  independent component of strain rate, given that one of the 3
c  potential degrees of freedom (ie. exx, eyy and exy) per element is
c  removed by strain-rate compatibility. This means that in the
c  statistical scaling of the sum of squares for the aposteriori
c  solution on average each of the 12 dynamics residuals contributes one
c  sixth as much as an independent observation.
c Similarly, of the 4 dynamics residuals for each fault segment there
c  are 2 for each independent component of slip rate, and in the
c  aposteriori analysis on average each of these 4 residuals contributes
c  half as much as an independent observation.
c Consequently, the y and y1 residuals associated with the dynamics
c  should rarely if ever be much greater than 1.
c
c There are two possible reasons for the dynamics residuals being large.
c  One is that the choice of spatial distribution for the stress-like
c  potentials sxxpot, syypot and sxypot involves large
c  physically-unrealisable contributions.
c For example, adding a large spatially-uniform amount to both sxxpot
c  and syypot does not change the forces that are acting, and therefore
c  does not affect the dynamics solution. However, such a contribution
c  does alter the dynamic equivalents of observations of strain rates
c  and slip rates, but in a way that has no bearing on the solution.
c In general, given a spatial distribution of values for any one of
c  sxxpot, syypot and sxypot values can be chosen for the other two that
c  result in there being no nett force.
c The way to spot such a situation is where the dynamics residuals are
c  large over much of the model.
c
c The other circumstance resulting in dynamics residuals being large is
c  where there are large discrepencies between observations and apriori
c  model predictions. Such large dynamics residuals show up only in the
c  aposteriori solution, and are counter-balanced by the mismatch to
c  observations being substantially smaller in the aposteriori solution
c  than in the apriori solution. Though the sum of squares of the
c  dynamics residuals increases in the aposteriori solution, the total
c  sum of squares including the mismatch to observations decreases.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     2      maxsf,maxy
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     3      maxduc=2*maxf,maxec=100,maxece=500,
     4      maxsf=maxf*maxfs,
     5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     6      +maxfs*maxduc+maxece*maxec)
      integer lun,ny0,ne,nf
      integer iy,i,iq,ic,j,s,ih
      integer nfs(maxf),sf(maxfs,maxf)
      real*8 elong(maxe),elat(maxe)
      real*8 slong(maxs),slat(maxs)
      real*8 y0(maxy),y(maxy),y1(maxy)
c
      iy=0
c
      write(lun,*) ne,' Elements'
      write(lun,*) 'First line = index,long,lat'
      write(lun,*) 
     1      'Others = component+3*(sub-element),y0,y,y1-residuals'
      write(lun,*) 'exx=component-1,eyy=component-2,exy=component-3'
      do i=1,ne
            write(lun,*) i,elong(i),elat(i)
            do iq=0,3
            do ic=1,3
                  iy=iy+1
                  write(lun,*) ic+3*iq,y0(iy),y(iy),y1(iy)
            end do
            end do
      end do  
c
      if (nf.gt.0) then
      write(lun,*) nf,' Faults'
      write(lun,*) 'First line = index,number-of-segments'
      write(lun,*) 'First segment line = fault,index,long,lat'
      write(lun,*) 
     1      'Others = component+2*(sub-segment-1),y0,y,y1-residuals'
      write(lun,*) 'dut=component-1,dun=component-2'
      do i=1,nf
            write(lun,*) i,nfs(i)
            do j=1,nfs(i)
                  s=sf(j,i)
                  write(lun,*) i,j,slong(s),slat(s)
                  do ih=1,2
                  do ic=1,2
                        iy=iy+1
                        write(lun,*) ic+2*(ih-1),y0(iy),y(iy),y1(iy)
                  end do
                  end do
            end do
      end do
      end if
c
      if (iy.ne.ny0) stop 'Something is wrong with the y0 part of y'
c
      return
      end
c
c
      SUBROUTINE misfit(lun,ny0,ny,nvo,nfo,neo,nduc,nec,
     1      evo,volong,volat,oux,ouy,seou,ffo,sfo,nfoc,oduc,seodu,
     2      eeo,neoc,oec,seoe,fduc,sduc,ncduc,s1duc,s2duc,isf,sfs,
     3      eec,ncec,e1ec,e2ec,y)
c
c This routine outputs to the file with unit number 'lun' residuals for
c  all observations. The input y vector can be for either the apriori or
c  the aposteriori solution. On average the normalised y-residuals will
c  have values that distribute about zero with magnitudes of order 1, if
c  the fit to the observations is satisfactory.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     2      maxsf,maxy
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     3      maxduc=2*maxf,maxec=100,maxece=500,
     4      maxsf=maxf*maxfs,
     5      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     6      +maxfs*maxduc+maxece*maxec)
      integer lun,ny0,ny,nvo,nfo,neo,nduc,nec
      integer iy,i,j,s1c,s2c,s1,s2,e1c,e2c,e1,e2
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer isf(maxs)
      integer sfs(maxsf)
      integer eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      real*8 ru1,ru2,rux,ruy,rdu1,rdu2,rduc1,rduc2
      real*8 re1,re2,re3,rec1,rec2,rec3
      real*8 volong(maxvo),volat(maxvo)
      real*8 oux(maxvo),ouy(maxvo)
      real*8 oduc(2,maxfo)
      real*8 oec(3,maxeo)
      real*8 seou(2,2,maxvo)
      real*8 seodu(2,2,maxfo)
      real*8 seoe(3,3,maxeo)
      real*8 y(maxy)
c
      iy=ny0
c
      if (nvo.gt.0) then
      write(lun,*) nvo,' Velocity Observations'
      write(lun,*) 'First line = index,element,longitude,latitude'
      write(lun,*) 'Others = component,value,residual,y-residual'
      do i=1,nvo
            write(lun,*) i,evo(i),volong(i),volat(i)
            iy=iy+1
            ru1=y(iy)
            iy=iy+1
            ru2=y(iy)
            rux=seou(1,1,i)*ru1
            ruy=seou(2,1,i)*ru1+seou(2,2,i)*ru2
            j=1
            write(lun,*) j,oux(i),rux,ru1
            j=2
            write(lun,*) j,ouy(i),ruy,ru2
      end do
      end if
c
      if (nfo.gt.0) then
      write(lun,*) nfo,' Slip-rate Observations'
      write(lun,*) 'First line = index,fault,segment,number'
      write(lun,*) 'Others = component,value,residual,y-residual'
      do i=1,nfo
            write(lun,*) i,ffo(i),sfo(i),nfoc(i)
            iy=iy+1
            rdu1=y(iy)
            rduc1=seodu(1,1,i)*rdu1
            j=1
            write(lun,*) j,oduc(1,i),rduc1,rdu1
            if (nfoc(i).eq.2) then
                  iy=iy+1

                  rdu2=y(iy)
                  rduc2=seodu(2,1,i)*rdu1+seodu(2,2,i)*rdu2
                  j=2
                  write(lun,*) j,oduc(2,i),rduc2,rdu2
            end if
      end do
      end if
c
      if (neo.gt.0) then
      write(lun,*) neo,' Strain-rate Observations'
      write(lun,*) 'First line = index,element,number'
      write(lun,*) 'Others = component,value,residual,y-residual'
      do i=1,neo
            write(lun,*) i,eeo(i),neoc(i)
            iy=iy+1
            re1=y(iy)
            rec1=seoe(1,1,i)*re1
            j=1
            write(lun,*) j,oec(1,i),rec1,re1
            if (neoc(i).gt.1) then
                  iy=iy+1
                  re2=y(iy)
                  rec2=seoe(2,1,i)*re1+seoe(2,2,i)*re2
                  j=2
                  write(lun,*) j,oec(2,i),rec2,re2
            end if
            if (neoc(i).eq.3) then
                  iy=iy+1
                  re3=y(iy)
                  rec3=seoe(3,1,i)*re1+seoe(3,2,i)*re2
     1                  +seoe(3,3,i)*re3
                  j=3
                  write(lun,*) j,oec(3,i),rec3,re3
            end if
      end do
      end if
c
      if (nduc.gt.0) then
      write(lun,*) nduc,' Slip-rate Correlation Sets'
      write(lun,*) 'First line = index,fault,number-of-pairs'
      write(lun,*) 'Others = pair,segment-1,segment-2,y-residual'
      do i=1,nduc
            write(lun,*) i,fduc(i),ncduc(i)
            do j=1,ncduc(i)
                  s1c=s1duc(j,i)
                  s2c=s2duc(j,i)
                  s1=sfs(isf(sduc(s1c,i)))
                  s2=sfs(isf(sduc(s2c,i)))
                  iy=iy+1
                  write(lun,*) j,s1,s2,y(iy)
            end do
      end do
      end if
c
      if (nec.gt.0) then
      write(lun,*) nec,' Strain-rate Correlation Sets'
      write(lun,*) 'First line = index,number-of-pairs'
      write(lun,*) 'Others = pair,element-1,element-2,y-residual'
      do i=1,nec
            write(lun,*) i,ncec(i)
            do j=1,ncec(i)
                  e1c=e1ec(j,i)
                  e2c=e2ec(j,i)
                  e1=eec(e1c,i)
                  e2=eec(e2c,i)
                  iy=iy+1
                  write(lun,*) j,e1,e2,y(iy)
            end do
      end do
      end if
c
      if (iy.ne.ny) stop 'Something is wrong with the y1 part of y'
c
      return
      end
c

