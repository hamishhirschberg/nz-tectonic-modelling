      SUBROUTINE setup()
c
c This version of the original Fortran code is intended for wrapping
c  into Python.
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
      integer gp1s(maxs),gp2s(maxs)
      integer e1s(maxs),e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      integer nvlgp(maxvl),nvls(maxvl),nvlf(maxvl)
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
      integer jfp(maxpf),tjfp(maxpf),gptj(maxtj)
      integer vls(maxsvl),svls(maxsvl)
      integer nvlp(maxpvl),vlp(2,maxpvl),gpvlp(2,maxpvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer rbp(maxprb),gprbp(maxprb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer gpJvl(maxJvl),vl1Jvl(maxJvl),vl2Jvl(maxJvl),
     1      gp1Jvl(maxJvl),gp2Jvl(maxJvl)
      integer gpfT(maxfT),f1fT(maxfT),vl2fT(maxfT),
     1      gp1fT(maxfT),gp2fT(maxfT)
      integer gpTf(maxTf),vl1Tf(maxTf),f2Tf(maxTf),
     1      gp1Tf(maxTf),gp2Tf(maxTf)
      integer gpTvl(maxTvl),vl1Tvl(maxTvl),vl2Tvl(maxTvl),
     1      gp1Tvl(maxTvl),gp2Tvl(maxTvl)
      integer gpXf(maxXf),vl1Xf(maxXf),f2Xf(maxXf),
     1      gp1Xf(maxXf),gp2Xf(maxXf)
      integer gpXvl(maxXvl),vl1Xvl(maxXvl),vl2Xvl(maxXvl),
     1      gp1Xvl(maxXvl),gp2Xvl(maxXvl)
      integer negp(maxgp)
      integer nesf(0:1,maxfs,maxf),negpf(0:1,0:maxfs,maxf)
      integer nesvl(0:1,maxvls,maxvl),negpvl(0:1,0:maxvls,maxvl)
      integer netj(0:2,maxtj)
      integer neJvl(0:3,maxJvl),nefT(0:3,maxfT),neTf(0:3,maxTf),
     1      neTvl(0:3,maxTvl),neXf(0:3,maxXf),neXvl(0:3,maxXvl)
      integer iefv(maxe)
      integer ks1fv(maxefv),ks2fv(maxefv),ks3fv(maxefv)
      integer is1fv(maxefv),is2fv(maxefv),is3fv(maxefv)
      integer js1fv(maxefv),js2fv(maxefv),js3fv(maxefv)
      integer rs1fv(maxefv),rs2fv(maxefv),rs3fv(maxefv)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rsrb(maxrbs,maxrb),rgprb(0:maxrbs,maxrb)
      integer gprank(maxgp),gplist(maxgp)
      integer smingp(maxgp),smaxgp(maxgp)
      integer srank(maxs),slist(maxs)
      integer smins(maxs),smaxs(maxs)
      integer erank(maxe),elist(maxe)
      integer smine(maxe),smaxe(maxe)
      integer z1(maxs),zn(maxs)
      integer sz(maxz),kz(maxz),jz(maxz)
      integer zmin(maxz),zmax(maxz)
      integer csqlen(maxz)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer smaxx(maxx),kx(maxx),ix(maxx),jx(maxx),
     1      jcx(maxx),icx(maxx)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      logical extern(maxgp)
      logical p0zero(maxf),pnzero(maxf)
      logical interp(maxvlj,maxvl)
      logical fillf0(maxvl),fillfn(maxvl)
      real*8 rE
      real*8 long(maxgp),lat(maxgp)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
      real*8 Kc(maxfs,maxf),Ks(maxfs,maxf)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 dut(0:maxfs,maxf),dun(0:maxfs,maxf)
      real*8 plat(maxrb),plong(maxrb),prate(maxrb)
      real*8 volong(maxvo),volat(maxvo),
     1      oux(maxvo),ouy(maxvo),seoux(maxvo),seouy(maxvo),
     2      rouxuy(maxvo)
      real*8 w1vo(maxvo),w2vo(maxvo),w3vo(maxvo)
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
      real*8 apgpf(0:maxfs,maxf),amgpf(0:maxfs,maxf)
      real*8 apgpvl(0:maxvls,maxvl),amgpvl(0:maxvls,maxvl)
      real*8 apgprb(0:maxrbs,maxrb),amgprb(0:maxrbs,maxrb)
      real*8 area0(0:3,maxe),area(maxe)
      real*8 esub(0:3,3,2,0:9,maxe)
      real*8 exx(2,0:9,maxe),eyy(2,0:9,maxe),exy(2,0:9,maxe)
      real*8 sxx(2,0:9,maxe),syy(2,0:9,maxe),sxy(2,0:9,maxe)
      real*8 fx(maxe),fy(maxe)
      real*8 Lce(maxe),Lcce(maxe),Lcse(maxe),
     1      Lse(maxe),Lsce(maxe),Lsse(maxe)
      real*8 len0(2,maxfs,maxf)
      real*8 pindu(2,0:1,maxfs,maxf),pinu(2,2,2,0:5,maxfs,maxf)
      real*8 pindu0(2,2,maxf),pindun(2,2,maxf)
      real*8 Kcfp(0:maxfs,maxf),Ksfp(0:maxfs,maxf)
      real*8 sedut(2,maxfs,maxf),sedun(2,maxfs,maxf)
      real*8 dusub(2,2,0:1,maxfs,maxf),dupot(2,2,maxfs,maxf)
      real*8 lenfs(maxfs,maxf),dufs(0:1,maxfs,maxf)
      real*8 ttfs(0:1,maxfs,maxf),tnfs(0:1,maxfs,maxf)
      real*8 Kcfs(maxfs,maxf),Ksfs(maxfs,maxf)
      real*8 Lcgp(maxgp),Lccgp(maxgp),Lcsgp(maxgp),
     1      Lsgp(maxgp),Lscgp(maxgp),Lssgp(maxgp)
      real*8 Lcf(0:maxfs,maxf),Lccf(0:maxfs,maxf),Lcsf(0:maxfs,maxf),
     1      Lsf(0:maxfs,maxf),Lscf(0:maxfs,maxf),Lssf(0:maxfs,maxf)
      real*8 Lctj(2,maxtj),Lcctj(2,maxtj),Lcstj(2,maxtj),
     1      Lstj(2,maxtj),Lsctj(2,maxtj),Lsstj(2,maxtj)
      real*8 epot(0:3,3,maxe),seesub(0:3,3,3,maxe)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb),usrb(2,maxrbs,maxrb)
      real*8 pinurb(0:5,maxrbs,maxrb)
      real*8 pinuvl(0:5,maxvls,maxvl)
      real*8 seou(2,2,maxvo),ou(2,maxvo),uvo(2,2,0:9,maxvo)
      real*8 seodu(2,2,maxfo),odu(2,maxfo),ducfo(2,2,0:1,maxfo)
      real*8 seoe(3,3,maxeo),oe(3,maxeo),eceo(3,2,0:9,maxeo)
      real*8 duc(maxfs,maxduc),dus1c(2,0:1,maxfs,maxduc),
     1      dus2c(2,0:1,maxfs,maxduc)
      real*8 ec(maxece,maxec),ee1c(2,0:9,maxece,maxec),
     1      ee2c(2,0:9,maxece,maxec)
      real*8 cz(0:25,maxz),z(maxz)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
c ngp = number of grid points
c ns = number of sides of elements
c ne = number of elements
c nf = number of faults
c nvl = number of velocity lines
c nrb = number of rigid boundaries
c nvo = number of velocity observations
c nfo = number of fault slip-rate observations
c neo = number of strain-rate observations
c nduc = number of sets of slip-rate correlations
c nec = number of sets of strain-rate correlations
c
c npf = number of grid points on faults
c npvl = number of grid points on velocity lines
c nprb = number of grid points on rigid boundaries
c nsf = number of element sides on faults
c nsvl = number of element sides on velocity lines
c nsrb = number of element sides on rigid boundaries
c
c ntj = number of triple junctions of faults
c nJvl = number of points where ends of velocity lines join
c nfT = number of truncations of velocity lines at faults
c nTf = number of truncations of faults at exterior lines
c nTvl = number of truncations of velocity lines at velocity lines
c nXf = number of points where faults cross velocity lines
c nXvl = number of points where two velocity lines cross
c
c nefv = number of elements with complex points on faults or v-lines
c
c nz = number of constraints in the system
c nx1 = number of variables assigned to constraints (equal to nz)
c nx = total number of variables in the system
c nx2 = number of variables to be solved for (excluding constraints)
c nx0 = number of variables with prescribed values
c ny = total number of apriori and actual observables to be fitted
c ny0 = number of apriori observable-like inputs, for the dynamic-like
c   apriori solution
c ny1 = number of actual observables, for the statistical aposteriori
c   analysis
c
c gp1s(i) = grid point 1 on element side i
c gp2s(i) = grid point 2 on element side i
c e1s(i) = element 1 with element side i
c e2s(i) = element 2 with element side i
c
c s1e(i) = side 1 of element i
c s2e(i) = side 2 of element i
c s3e(i) = side 3 of element i
c gp1e(i) = grid point 1 of element i
c gp2e(i) = grid point 2 of element i
c gp3e(i) = grid point 3 of element i
c
c nfs(i) = number of element sides on fault i
c sf(j,i) = index to element sides of side j on fault i
c gpf(j,i) = index to grid points of point j on fault i
c
c nvlgp(i) = number of grid points not on faults on velocity line i
c nvls(i) = number of element sides on velocity line i
c nvlf(i) = number of faults on velocity line i
c gpvl(j,i) = index to grid points of point j on velocity line i
c svl(j,i) = index to element sides of side j on velocity line i
c fvl(j,i) = index to faults of point j on velocity line i
c
c nrbs(i) = number of element sides on rigid boundary i
c srb(j,i) = index to element sides of side j on rigid boundary i
c gprb(j,i) = index to grid points of point j on rigid boundary i
c
c evo(i) = element containing velocity observation i
c
c ffo(i) = fault with slip-rate observation i
c sfo(i) = element side with slip-rate observation i
c nfoc(i) = number of coefficient vectors for slip-rate observation i
c
c eeo(i) = element with strain-rate observation i
c neoc(i) = number of coefficient vectors for strain-rate observation i
c
c fduc(i) = fault with slip-rate correlations i
c nducs(i) = number of element sides for slip-rate correlations i
c sduc(j,i) = element side j for slip-rate correlations i
c ncduc(i) = number of correlations in slip-rate set i
c s1duc(j,i) = element side 1 for correlation j in slip-rate set i
c s2duc(j,i) = element side 2 for correlation j in slip-rate set i
c
c nece(i) = number of elements for strain-rate correlations i
c eec(j,i) = element j for strain-rate correlations i
c ncec(i) = number of correlations in strain-rate set i
c e1ec(j,i) = element 1 for correlation j in strain-rate set i
c e2ec(j,i) = element 2 for correlation j in strain-rate set i
c
c ipf(i) = index to points on faults of grid point i
c ipvl(i) = index to points on velocity lines of grid point i
c iprb(i) = index to points on rigid boundaries of grid point i
c
c isf(i) = index to sides on faults of element side i
c isvl(i) = index to sides on velocity lines of element side i
c isrb(i) = index to sides on rigid boundaries of element side i
c
c fs(is) = fault for side 'is' in set on faults
c sfs(is) = side on fault fs(is) for side 'is' in set on faults
c nfp(ip) = number of faults for point ip in set on faults
c fp(j,ip) = fault j for point ip in set on faults
c gpfp(j,ip) = fault point on fault j for point ip in set on faults
c jfp(ip) = index to not-ending fault for point ip in set on faults
c tjfp(ip) = triple junction for point ip in set on faults
c
c gptj(i) = grid point at triple junction i of faults
c
c vls(is) = velocity line for side 'is' in set on velocity lines
c svls(is) = side on v-line vls(is) for side 'is' in set on v-lines
c nvlp(ip) = number of velocity lines for point ip in set on v-lines
c vlp(j,ip) = velocity line j for point ip in set on velocity lines
c gpvlp(j,ip) = v-line point on v-line j for point ip in set on v-lines
c
c rbs(is) = rigid boundary for side 'is' in set on rigid boundaries
c srbs(is) = side on rigid-b rbs(is) for side 'is' in set on rigid-bs
c rbp(ip) = rigid boundary for point ip in set on rigid-boundaries
c gprbp(ip) = rigid-b point on rigid-b for point ip in set on rigid-bs
c
c Jvlvlp(ip) = join of ends of v-lines for point ip in set on v-lines
c fTvlp(ip) = truncation at a fault for point ip in set on v-lines
c Tfvlp(ip) = truncation of a fault for point ip in set on v-lines
c Tvlvlp(ip) = truncation at a v-line for point ip in set on v-lines
c Xfvlp(ip) = crossing of a fault for point ip in set on v-lines
c Xvlvlp(ip) = crossing of v-lines for point ip in set on v-lines
c
c gpJvl(i) = grid point at join i of ends of velocity lines
c vl1Jvl(i) = velocity line at join i of ends of velocity lines
c vl2Jvl(i) = velocity line at join i of ends of velocity lines
c gp1Jvl(i) = point on 1st v-line at join i of ends of velocity lines
c gp2Jvl(i) = point on 2nd v-line at join i of ends of velocity lines
c
c gpfT(i) = grid point at truncation i at a fault
c f1fT(i) = fault at truncation i at a fault
c vl2fT(i) = truncating velocity line at truncation i at a fault
c gp1fT(i) = point on fault at truncation i at a fault
c gp2fT(i) = point on velocity line at truncation i at a fault
c
c gpTf(i) = grid point at truncation i of a fault
c vl1Tf(i) = (exterior) velocity line at truncation i of a fault
c f2Tf(i) = fault at truncation i of a fault
c gp1Tf(i) = point on velocity line at truncation i of a fault
c gp2Tf(i) = point on fault at truncation i of a fault
c
c gpTvl(i) = grid point at truncation i at a velocity line
c vl1Tvl(i) = not-ending velocity line at truncation i at a v-line
c vl2Tvl(i) = truncating velocity line at truncation i at a v-line
c gp1Tvl(i) = point on not-ending v-line at truncation i at a v-line
c gp2Tvl(i) = point on truncating v-line at truncation i at a v-line
c
c gpXf(i) = grid point at crossing i of a fault
c vl1Xf(i) = velocity line at crossing i of a fault
c f2Xf(i) = fault at crossing i of a fault
c gp1Xf(i) = point on velocity line at crossing i of a fault
c gp2Xf(i) = point on fault at crossing i of a fault
c
c gpXvl(i) = grid point at crossing i of velocity lines
c vl1Xvl(i) = velocity line at crossing i of velocity lines
c vl2Xvl(i) = velocity line at crossing i of velocity lines
c gp1Xvl(i) = point on 1st velocity line at crossing i of v-lines
c gp2Xvl(i) = point on 2nd velocity line at crossing i of v-lines
c
c negp(i) = number of elements at grid point i (is 0 if i is complex)
c
c nesf(r,j,i) = number of elements in region r at side j on fault i
c negpf(r,j,i) = number of elements in region r at point j on fault i
c nesvl(r,j,i) = number of elements in region r at side j on v-line i
c negpvl(r,j,i) = number of elements in region r at point j on v-line i
c
c netj(r,i) = number of elements in region r at fault triple junction i
c neJvl(r,i) = number of elements in region r at v-line joining i
c nefT(r,i) = number of elements in region r at truncation i at a fault
c neTf(r,i) = number of elements in region r at truncation i of a fault
c neTvl(r,i) = number of elements in region r at v-line truncation i
c neXf(r,i) = number of elements in region r at crossing i of a fault
c neXvl(r,i) = number of elements in region r at v-line crossing i
c
c iefv(i) = index of element i in set with complex grid points
c
c ks1fv(ie) = type of side 1 for complex element ie
c ks2fv(ie) = type of side 2 for complex element ie
c ks3fv(ie) = type of side 3 for complex element ie
c is1fv(ie) = primary index for type of side 1 for complex element ie
c is2fv(ie) = primary index for type of side 2 for complex element ie
c is3fv(ie) = primary index for type of side 3 for complex element ie
c js1fv(ie) = second index for type of side 1 for complex element ie
c js2fv(ie) = second index for type of side 2 for complex element ie
c js3fv(ie) = second index for type of side 3 for complex element ie
c rs1fv(ie) = region of side 1 for complex element ie
c rs2fv(ie) = region of side 2 for complex element ie
c rs3fv(ie) = region of side 3 for complex element ie
c
c kgp1fv(ie) = type of point 1 for complex element ie
c kgp2fv(ie) = type of point 2 for complex element ie
c kgp3fv(ie) = type of point 3 for complex element ie
c igp1fv(ie) = primary index for type of point 1 for complex element ie
c igp2fv(ie) = primary index for type of point 2 for complex element ie
c igp3fv(ie) = primary index for type of point 3 for complex element ie
c jgp1fv(ie) = second index for type of point 1 for complex element ie
c jgp2fv(ie) = second index for type of point 2 for complex element ie
c jgp3fv(ie) = second index for type of point 3 for complex element ie
c rgp1fv(ie) = region of point 1 for complex element ie
c rgp2fv(ie) = region of point 2 for complex element ie
c rgp3fv(ie) = region of point 3 for complex element ie
c
c rsrb(j,i) = region for side j of rigid boundary i
c rgprb(j,i) = region for point j of rigid boundary i
c
c gprank(i) = ranking of grid point i in the arrangement of variables
c gplist(i) = grid point that is i-th ranked in that list
c smingp(i) = side connected to grid point i with the minimum ranking
c smaxgp(i) = side connected to grid point i with the maximum ranking
c
c srank(i) = ranking of side i in the arrangement of variables
c slist(i) = side that is i-th ranked in that list
c smins(i) = side connected to side i with the minimum ranking
c smaxs(i) = side connected to side i with the maximum ranking
c
c erank(i) = ranking of element i in the arrangement of variables
c elist(i) = element that is i-th ranked in that list
c smine(i) = side connected to element i with the minimum ranking
c smaxe(i) = side connected to element i with the maximum ranking
c
c z1(i) = first constraint associated with side i
c zn(i) = last constraint associated with side i
c
c sz(i) = element side for constraint i
c kz(i) = class of constraint for constraint i
c jz(i) = sub-class of constraint for constraint i
c
c zmin(i) = first constraint that connects to constraint i
c zmax(i) = last constraint that connects to constraint i
c csqlen(i) = location of the i-th diagonal element in the stored part
c   of the lower triangle of the sparse symmetric constraint matrix for
c   Cholesky decomposition
c
c xgpu(ic,jc,i) = location in the vector of variables of u component ic
c   with derivative index jc at grid point i
c xeu(ic,i) = location in the vector of variables of u component ic at
c   the  midpoint of element i
c
c xfu(ic,jc,j,i) = location in the vector of variables of u component ic
c   with derivative index jc at the j-th point on minus side of fault i
c xvlu(ic,jc,j,i) = location in the vector of variables of u component
c   ic and derivative jc at the j-th point on minus side of v-line i
c xtju(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at triple junction i
c xJvlu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at joining i of v-lines
c xfTu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at truncation i at a fault
c xTfu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at truncation i of a fault
c xTvlu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at truncation i of a v-line
c xXfu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at crossing i of a fault
c xXvlu(ic,jc,r,i) = location in the vector of variables of u component
c   ic and derivative jc for region r at crossing i of v-lines
c
c xfdu(ic,jc,j,i) = location in the vector of variables of du component
c   ic with derivative index jc at the j-th point on fault i
c xfTdu(ic,jc,i) = location in the vector of variables of the extra du
c   component ic with derivative index jc at truncation i at a fault
c xXfdu(ic,jc,i) = location in the vector of variables of the extra du
c   component ic with derivative index jc at crossing i of a fault
c
c smaxx(i) = last side in the ranked list connected with variable i
c kx(i) = type of point for variable i
c ix(i) = primary index of point for variable i
c jx(i) = secondary index of point for variable i
c jcx(i) = derivative index for variable i
c icx(i) = component index for variable i
c
c xz(j,i) = index to the variable in the x-array for the j-th
c   contribution to constraint i in the z-array
c x1maxz(i) = highest index in xz for x1 variables for constraint i
c x2maxz(i) = highest index in xz for x2 variables for constraint i
c x0maxz(i) = highest index in xz for x0 variables for constraint i
c
c xy(j,i) = index to the variable in the x-array for the j-th
c   contribution to observable i in the y-array
c x1maxy(i) = highest index in xy for x1 variables for observable i
c x2maxy(i) = highest index in xy for x2 variables for observable i
c x0maxy(i) = highest index in xy for x0 variables for observable i
c
c extern(i) = true if grid point i is on an external boundary
c p0zero(i) = true if first point on fault i has zero slip-rate
c pnzero(i) = true if last point on fault i has zero slip-rate
c interp(j,i) = true if jth velocity values on v-line i are to be found
c fillf0(i) = true if v-line i starts at a fault needing v-values
c fillfn(i) = true if v-line i ends at a fault needing v-values
c
c rE = radius of the Earth
c
c long(i) = longitude (in degrees) of grid point i
c lat(i) = latitude (in degrees) of grid point i
c sxxpot(i) = xx-component of stress-like potential at grid point i
c syypot(i) = yy-component of stress-like potential at grid point i
c sxypot(i) = xy-component of stress-like potential at grid point i
c
c Lc(i) = Lc strain-rate capacity for element i
c Lcc(i) = Lcc strain-rate capacity for element i
c Lcs(i) = Lcs strain-rate capacity for element i
c Ls(i) = Ls strain-rate capacity for element i
c Lsc(i) = Lsc strain-rate capacity for element i
c Lss(i) = Lss strain-rate capacity for element i
c
c Kc(j,i) = Kc slip-rate capacity for side j on fault i
c Ks(j,i) = Ks slip-rate capacity for side j on fault i
c
c ux(j,i) = x-velocity value at grid-point or side j on velocity line i
c uy(j,i) = y-velocity value at grid-point or side j on velocity line i
c uxm(j,i) = x-velocity on minus side of fault at point j on v-line i
c uym(j,i) = y-velocity on minus side of fault at point j on v-line i
c uxp(j,i) = x-velocity on plus side of fault at point j on v-line i
c uyp(j,i) = y-velocity on plus side of fault at point j on v-line i
c
c dut(j,i) = tangential slip-rate value at point j on fault i
c dun(j,i) = normal slip-rate value at point j on fault i
c
c plat(i) = latitude (degrees) of rotation pole for rigid boundary i
c plong(i) = longitude (degrees) of rotation pole for rigid boundary i
c prate(i) = rotation rate (in strain-rate units) of rigid boundary i
c
c volong(i) = longitude (degrees) of velocity observation i
c volat(i) = latitude (degrees) of velocity observation i
c oux(i) = x-velocity for velocity observation i
c ouy(i) = y-velocity for velocity observation i
c seoux(i) = standard error of x-velocity for velocity observation i
c seouy(i) = standard error of y-velocity for velocity observation i
c rouxuy(i) = correlation coefficient of velocities for v-observation i
c
c w1vo(i) = weight of grid point 1 in element containing v-observation i
c w2vo(i) = weight of grid point 2 in element containing v-observation i
c w3vo(i) = weight of grid point 3 in element containing v-observation i
c
c codut(j,i) = strike-slip factor for value j of slip-rate observation i
c codun(j,i) = dip-slip factor for value j of slip-rate observation i
c oduc(j,i) = value for component j of slip-rate observation i
c seoduc(j,i) = standard error of value j of slip-rate observation i
c rfo12(i) = j-1&2 correlation coefficient of slip-rate observation i
c
c coexx(j,i) = exx factor for value j of strain-rate observation i
c coeyy(j,i) = eyy factor for value j of strain-rate observation i
c coexy(j,i) = exy factor for value j of strain-rate observation i
c oec(j,i) = value for component j of strain-rate observation i
c seoec(j,i) = standard error of value j of strain-rate observation i
c reo12(i) = j-1&2 correlation coefficient of strain-rate observation i
c reo13(i) = j-1&3 correlation coefficient of strain-rate observation i
c reo23(i) = j-2&3 correlation coefficient of strain-rate observation i
c
c cdut(j,i) = strike-slip factor for jth side in slip-rate correlation i
c cdun(j,i) = dip-slip factor for jth side in slip-rate correlation i
c refduc(j,i) = reference value for jth side in slip-rate correlation i
c scduc(j,i) = scaling factor for jth side in slip-rate correlation i
c
c cexx(j,i) = exx factor for jth element in strain-rate correlation i
c ceyy(j,i) = eyy factor for jth element in strain-rate correlation i
c cexy(j,i) = exy factor for jth element in strain-rate correlation i
c refec(j,i) = reference value for jth element in strain-r correlation i
c scec(j,i) = scaling factor for jth element in strain-r correlation i
c
c apgpf(j,i) = positive direction (radians) from point j on fault i
c amgpf(j,i) = negative direction (radians) from point j on fault i
c apgpvl(j,i) = positive direction (radians) from point j on v-line i
c amgpvl(j,i) = negative direction (radians) from point j on v-line i
c apgprb(j,i) = positive direction (radians) from point j on rigid-b i
c amgprb(j,i) = negative direction (radians) from point j on rigid-b i
c
c area0(j,i) = area of sub-element j in element i
c area(i) = area of element i
c
c esub(j,k,ic,jc,i) = normalised contribution to average strain-rate
c   component k in sub-element j from component ic of velocity field
c   contribution jc in element i
c exx(ic,jc,i) = contribution to average value of exx in element i from
c   component ic of velocity field contribution jc
c eyy(ic,jc,i) = contribution to average value of eyy in element i from
c   component ic of velocity field contribution jc
c exy(ic,jc,i) = contribution to average value of exy in element i from
c   component ic of velocity field contribution jc
c
c sxx(ic,jc,i) = contribution to L-matrix-weighted average value of sxx
c   in element i from component ic of velocity field contribution jc
c syy(ic,jc,i) = contribution to L-matrix-weighted average value of syy
c   in element i from component ic of velocity field contribution jc
c sxy(ic,jc,i) = contribution to L-matrix-weighted average value of sxy
c   in element i from component ic of velocity field contribution jc
c
c fx(i) = average value of body force component fx in element i
c fy(i) = average value of body force component fy in element i
c
c Lce(i) = average Lc strain-rate capacity in element i
c Lcce(i) = average Lcc strain-rate capacity in element i
c Lcse(i) = average Lcs strain-rate capacity in element i
c Lse(i) = average Ls strain-rate capacity in element i
c Lsce(i) = average Lsc strain-rate capacity in element i
c Lsse(i) = average Lss strain-rate capacity in element i
c
c len0(k,j,i) = length of sub-segment k of segment j on fault i
c
c pindu(k,jc,j,i) = contribution to slip-rate average in sub-segment k
c   from slip-rate contribution jc in segment j on fault i
c pinu(k,kc,ic,jc,j,i) = matching contribution to average of (t,n)
c   slip-rate component kc in sub-segment k from (x,y) component ic of
c   velocity contribution jc on plus side of segment j on fault i
c pindu0(kc,ic,i) = contribution to (x,y) slip-rate component kc from
c   (t,n) slip-rate component ic at the first point on fault i
c pindun(kc,ic,i) = contribution to (x,y) slip-rate component kc from
c   (t,n) slip-rate component ic at the last point on fault i
c
c Kcfp(j,i) = Kc value at grid point j on fault i
c Ksfp(j,i) = Ks value at grid point j on fault i
c sedut(k,j,i) = normalising factor for the tangential component of
c   average slip-rate on sub-segment k of segment j on fault i
c sedun(k,j,i) = normalising factor for the fault-normal component of
c   average slip-rate on sub-segment k of segment j on fault i
c
c dusub(k,ic,jc,j,i) = normalised contribution to average value of
c   slip-rate component ic in sub-segment k from slip-rate contribution
c   jc in segment j on fault i
c dupot(k,ic,j,i) = normalised average value of slip-rate potential
c   component ic in sub-segment k of segment j on fault i
c lenfs(j,i) = length of segment j on fault i
c dufs(jc,j,i) = contribution to average value of slip-rate on segment j
c   of fault i from slip-rate contribution jc
c ttfs(jc,j,i) = contribution to K-weighted average value of tangential
c   traction on segment j of fault i from slip-rate contribution jc
c tnfs(jc,j,i) = contribution to K-weighted average value of normal
c   traction on segment j of fault i from slip-rate contribution jc
c Kcfs(j,i) = average Kc slip-rate capacity for segment j on fault i
c Ksfs(j,i) = average Ks slip-rate capacity for segment j on fault i
c
c Lcgp(i) = Lc value at grid point i
c Lccgp(i) = Lcc value at grid point i
c Lcsgp(i) = Lcs value at grid point i
c Lsgp(i) = Ls value at grid point i
c Lscgp(i) = Lsc value at grid point i
c Lssgp(i) = Lss value at grid point i
c
c Lcf(j,i) = Lc value on minus side of fault i at fault grid point j
c Lccf(j,i) = Lcc value on minus side of fault i at fault grid point j
c Lcsf(j,i) = Lcs value on minus side of fault i at fault grid point j
c Lsf(j,i) = Ls value on minus side of fault i at fault grid point j
c Lscf(j,i) = Lsc value on minus side of fault i at fault grid point j
c Lssf(j,i) = Lss value on minus side of fault i at fault grid point j
c
c Lctj(r,i) = Lc value in region r at triple junction of faults i
c Lcctj(r,i) = Lcc value in region r at triple junction of faults i
c Lcstj(r,i) = Lcs value in region r at triple junction of faults i
c Lstj(r,i) = Ls value in region r at triple junction of faults i
c Lsctj(r,i) = Lsc value in region r at triple junction of faults i
c Lsstj(r,i) = Lss value in region r at triple junction of faults i
c
c epot(j,k,i) = normalised average value of strain-rate potential
c   component k in sub-element j of element i
c seesub(j,e,k,i) = element of normalising matrix relating normalised
c   strain-rate component k and (xx,yy,xy) component e in sub-element j
c   of element i
c
c ugprb(ic,jc,j,i) = value of jc derivative of velocity component ic at
c   grid point j on rigid boundary i
c usrb(ic,j,i) = value of velocity component ic at midpoint of segment j
c   on rigid boundary i
c pinurb(jc,j,i) = contribution to velocity at midpoint of segment j on
c   rigid boundary i from velocity field contribution jc
c pinuvl(jc,j,i) = contribution to velocity at midpoint of segment j on
c   velocity line i from velocity field contribution jc
c
c seou(ic,kc,i) = element of standard error matrix relating normalised
c   component kc and (x,y) component ic of velocity observation i
c ou(kc,i) = value of normalised component kc of velocity observation i
c uvo(kc,ic,jc,i) = contribution to normalised component kc of velocity
c   observation i from component ic of velocity field contribution jc
c
c seodu(ic,kc,i) = element of standard error matrix relating normalised
c   component kc and component ic of fault slip-rate observation i
c odu(kc,i) = normalised component kc value of slip-rate observation i
c ducfo(kc,ic,jc,i) = contribution to normalised slip-rate component kc
c   of observation i from component ic of slip-rate contribution jc
c
c seoe(ic,kc,i) = element of standard error matrix relating normalised
c   component kc and component ic of fault strain-rate observation i
c oe(kc,i) = normalised component kc value of strain-rate observation i
c eceo(kc,ic,jc,i) = contribution to normalised strain-rate component kc
c   of observation i from component ic of velocity field contribution jc
c
c duc(j,i) = value of normalised difference between pair j of adjacent
c   segments for fault-slip correlation i
c dus1c(ic,jc,j,i) = contribution to the normalised difference from
c   component ic of slip-rate contribution jc in the segment making the
c   positive contribution for pair j in fault-slip correlation i
c dus2c(ic,jc,j,i) = contribution to the normalised difference from
c   component ic of slip-rate contribution jc in the segment making the
c   negative contribution for pair j in fault-slip correlation i
c
c ec(j,i) = value of normalised difference between pair j of adjacent
c   elements for strain-rate correlation i
c ee1c(ic,jc,j,i) = contribution to the normalised difference from
c   component ic of velocity field contribution jc in the element making
c   the positive contribution for pair j in strain-rate correlation i
c ee2c(ic,jc,j,i) = contribution to the normalised difference from
c   component ic of velocity field contribution jc in the element making
c   the negative contribution for pair j in strain-rate correlation i
c
c cz(j,i) = coefficient multiplying the variable in the x-array making
c   the j-th contribution to constraint i in the z-array
c z(i) = constraint value i
c ay(j,i) = coefficient multiplying the variable in the x-array making
c   the j-th contribution to normalised observable i in the y-array
c y(i) = normalised observable value i
c x(i) = velocity or slip-rate field variable i
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      call input(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1s,gp2s,s1e,s2e,s3e,
     2      nfs,sf,nvlgp,nvls,nvlf,gpvl,svl,fvl,nrbs,srb,
     3      ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,nece,eec,interp,
     4      rE,long,lat,sxxpot,syypot,sxypot,Lc,Lcc,Lcs,Ls,Lsc,Lss,
     5      Kc,Ks,ux,uy,uxm,uym,uxp,uyp,plat,plong,prate,
     6      volong,volat,oux,ouy,seoux,seouy,rouxuy,
     7      codut,codun,oduc,seoduc,rfo12,
     8      coexx,coeyy,coexy,oec,seoec,reo12,reo13,reo23,
     9      cdut,cdun,refduc,scduc,cexx,ceyy,cexy,refec,scec)
c
      call check(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1s,gp2s,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,gpvl,svl,fvl,nrbs,srb,gprb,
     3      evo,ffo,sfo,eeo,fduc,nducs,sduc,ncduc,s1duc,s2duc,
     4      nece,eec,ncec,e1ec,e2ec,npf,npvl,nprb,nsf,nsvl,nsrb,
     5      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,
     6      ntj,gptj,vls,svls,nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,
     7      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     8      nJvl,gpJvl,vl1Jvl,vl2Jvl,gp1Jvl,gp2Jvl,
     9      nfT,gpfT,f1fT,vl2fT,gp1fT,gp2fT,
     1      nTf,gpTf,vl1Tf,f2Tf,gp1Tf,gp2Tf,
     1      nTvl,gpTvl,vl1Tvl,vl2Tvl,gp1Tvl,gp2Tvl,
     2      nXf,gpXf,vl1Xf,f2Xf,gp1Xf,gp2Xf,
     3      nXvl,gpXvl,vl1Xvl,vl2Xvl,gp1Xvl,gp2Xvl,
     4      extern,p0zero,pnzero,interp,fillf0,fillfn,
     5      rE,long,lat,volong,volat,w1vo,w2vo,w3vo,
     6      apgpf,amgpf,apgpvl,amgpvl,apgprb,amgprb)
c
      call findr(ngp,ns,ne,nf,nvl,nrb,
     1      s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,gpvl,svl,fvl,nrbs,srb,gprb,
     3      npf,npvl,nprb,nsf,nsvl,nsrb,
     4      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,
     5      ntj,gptj,vls,svls,nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,
     6      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     7      nJvl,gpJvl,vl1Jvl,vl2Jvl,gp1Jvl,gp2Jvl,
     8      nfT,gpfT,f1fT,vl2fT,gp1fT,gp2fT,
     9      nTf,gpTf,vl1Tf,f2Tf,gp1Tf,gp2Tf,
     1      nTvl,gpTvl,vl1Tvl,vl2Tvl,gp1Tvl,gp2Tvl,
     1      nXf,gpXf,vl1Xf,f2Xf,gp1Xf,gp2Xf,
     2      nXvl,gpXvl,vl1Xvl,vl2Xvl,gp1Xvl,gp2Xvl,
     3      negp,nesf,negpf,nesvl,negpvl,netj,
     4      neJvl,nefT,neTf,neTvl,neXf,neXvl,
     5      nefv,iefv,ks1fv,ks2fv,ks3fv,is1fv,is2fv,is3fv,
     6      js1fv,js2fv,js3fv,rs1fv,rs2fv,rs3fv,
     7      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     8      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,rsrb,rgprb,
     9      long,lat,apgpf,amgpf,apgpvl,amgpvl,apgprb,amgprb,
     2      e2s,extern)
c
      call build(ngp,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1e,gp2e,gp3e,nfs,gpf,nvls,gpvl,fvl,nrbs,gprb,evo,
     2      ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,ncduc,s1duc,s2duc,
     3      nece,eec,ncec,e1ec,e2ec,npf,npvl,nprb,ipf,ipvl,iprb,
     4      isf,sfs,nfp,fp,gpfp,jfp,tjfp,ntj,gptj,nvlp,vlp,gpvlp,
     5      rbp,gprbp,nefv,iefv,kgp1fv,kgp2fv,kgp3fv,
     6      igp1fv,igp2fv,igp3fv,rgp1fv,rgp2fv,rgp3fv,rgprb,
     7      p0zero,pnzero,interp,fillf0,fillfn,
     8      rE,long,lat,sxxpot,syypot,sxypot,Lc,Lcc,Lcs,Ls,Lsc,Lss,
     9      Kc,Ks,ux,uy,uxm,uym,uxp,uyp,dut,dun,plat,plong,prate,
     1      oux,ouy,seoux,seouy,rouxuy,w1vo,w2vo,w3vo,
     1      codut,codun,oduc,seoduc,rfo12,coexx,coeyy,coexy,
     2      oec,seoec,reo12,reo13,reo23,cdut,cdun,refduc,scduc,
     3      cexx,ceyy,cexy,refec,scec,area0,area,esub,exx,eyy,exy,
     4      sxx,syy,sxy,fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse,
     5      len0,pindu,pinu,pindu0,pindun,Kcfp,Ksfp,
     6      sedut,sedun,dusub,dupot,lenfs,dufs,ttfs,tnfs,Kcfs,Ksfs,
     7      Lcgp,Lccgp,Lcsgp,Lsgp,Lscgp,Lssgp,Lcf,Lccf,Lcsf,
     8      Lsf,Lscf,Lssf,Lctj,Lcctj,Lcstj,Lstj,Lsctj,Lsstj,
     9      epot,seesub,ugprb,usrb,pinurb,pinuvl,
     2      seou,ou,uvo,seodu,odu,ducfo,seoe,oe,eceo,
     1      duc,dus1c,dus2c,ec,ee1c,ee2c)
c
      call order(ngp,ns,ne,nf,nvl,ntj,gp1s,gp2s,e1s,e2s,
     1      s1e,s2e,s3e,gp1e,gp2e,gp3e,nfs,gpf,nvls,gpvl,gprb,
     2      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,fp,gpfp,jfp,tjfp,
     3      vls,svls,rbs,srbs,nJvl,nfT,nTf,nTvl,nXf,nXvl,
     4      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,iefv,
     5      rs1fv,rs2fv,rs3fv,rgp1fv,rgp2fv,rgp3fv,rgprb,
     6      p0zero,pnzero,long,lat,nz,nx1,nx,nx2,nx0,
     7      gprank,gplist,smingp,smaxgp,srank,slist,smins,smaxs,
     8      erank,elist,smine,smaxe,z1,zn,sz,kz,jz,zmin,zmax,
     9      csqlen,xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,
     1      xTvlu,xXfu,xXvlu,xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     1      ux,uy,uxm,uym,uxp,uyp,dut,dun,ugprb,x)
c
      call makem(ns,ne,nx1,nx2,nz,ny,nf,ny0,nvo,nfo,neo,nduc,nec,
     1      ny1,elist,gp1e,gp2e,gp3e,s1e,s2e,s3e,isf,isvl,isrb,e1s,e2s,
     2      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3      iefv,kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,jgp1fv,
     4      jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,xz,cz,z,x,
     5      x1maxz,x2maxz,x0maxz,xfdu,xfTdu,xXfdu,long,lat,ipf,ipvl,
     6      iprb,Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,tjfp,fs,sfs,
     7      gpf,rbp,gprbp,rgprb,nfs,p0zero,pnzero,pindu0,pindun,pindu,
     8      pinu,vls,svls,gpvl,nvls,pinuvl,ux,uy,rbs,srbs,gprb,
     9      fp,gpfp,jfp,pinurb,usrb,epot,esub,xy,ay,y,x1maxy,x2maxy,
     1      x0maxy,dusub,dupot,evo,ou,uvo,ffo,sfo,nfoc,odu,ducfo,eeo,
     1      neoc,oe,eceo,ncduc,s1duc,s2duc,fduc,sduc,duc,dus1c,dus2c,
     2      ncec,e1ec,e2ec,eec,ec,ee1c,ee2c)
c
      call output(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      npf,npvl,nprb,nsf,nsvl,nsrb,ntj,nJvl,nfT,nTf,nTvl,
     2      nXf,nXvl,nefv,nz,nx1,nx,nx2,nx0,ny,ny0,ny1,e1s,e2s,
     3      s1e,s2e,s3e,gp1e,gp2e,gp3e,nfs,sf,gpf,nvls,gpvl,svl,fvl,
     4      nrbs,srb,gprb,evo,ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,
     5      ncduc,s1duc,s2duc,nece,eec,ncec,e1ec,e2ec,ipf,ipvl,iprb,
     6      isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,vls,svls,
     7      nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,Jvlvlp,fTvlp,Tfvlp,
     8      Tvlvlp,Xfvlp,Xvlvlp,iefv,kgp1fv,kgp2fv,kgp3fv,igp1fv,
     9      igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     1      rgprb,sz,kz,jz,kx,ix,jx,jcx,icx,zmin,csqlen,xz,x1maxz,
     1      x2maxz,x0maxz,xy,x2maxy,x0maxy,extern,rE,long,lat,
     2      sxxpot,syypot,sxypot,ux,uy,uxm,uym,uxp,uyp,volong,volat,
     3      oux,ouy,oduc,oec,area,exx,eyy,exy,sxx,syy,sxy,fx,fy,
     4      Lce,Lcce,Lcse,Lse,Lsce,Lsse,Kcfp,Ksfp,lenfs,dufs,
     5      ttfs,tnfs,Kcfs,Ksfs,Lcgp,Lccgp,Lcsgp,Lsgp,Lscgp,Lssgp,
     6      Lcf,Lccf,Lcsf,Lsf,Lscf,Lssf,Lctj,Lcctj,Lcstj,
     7      Lstj,Lsctj,Lsstj,ugprb,usrb,seou,seodu,seoe,cz,z,ay,y,x)
c
      return
      end
c
c In the following x and y respectively denote longitude and latitude
c   components, with the positive x and y directions being east and

c   north. For fault slip rates the components in the horizontal plane
c   are dut = tangential (i.e. strike-slip, right-lateral is positive)
c   and dun = normal (i.e. dip-slip, extension is positive). Vertical
c   components satisfy ezz = -(exx+eyy) in the case of strain rate and
c   duz = dun*tan(dip) in the case of fault slip.
c
c For the dynamic-like apriori model and the approach in general to be
c   valid, the region being considered must be thin enough for
c   horizontal projections of fault planes to be viewed as zero-width
c   lines on the horizontal plane.
c
c The dynamic-like apriori model is constructed with the ingredients
c   being slip-rate capacity on faults, strain-rate capacity and
c   dimensionless stress-like potentials, combined with velocity
c   boundary conditions and optionally velocities specified
c   along other lines.
c
c Slip-rate capacity on faults:
c   Kc = kappa*cos(dip)**2,
c   Ks = kappa*sin(dip)**2.
c   kappa is a relative measure of slip-rate capacity with the
c    dimensions of slip rate, and the dip angle is measured from
c    horizontal. Both Kc and Ks must be greater than zero, so dips of
c    precisely 0 (horizontal) and 90 degrees (vertical) are not allowed.
c   Slip at grid points at ends of faults is automatically zero, except
c    where three faults meet or where a single fault intersects an
c    external boundary or the boundary of any internal area that is not
c    part of the triangulation. To match this, Kc and Ks are set to zero
c    automatically at these points.
c   Elsewhere averaging of values in adjoining segments is performed to
c    give point values at the ends of segments along each fault, with
c    weights proportional to segment length. The values of Kc and Ks
c    used in the modelling within each line segment are linearly
c    interpolated from the point values. The averaging preserves the
c    line integral of slip-rate capacity.
c
c Strain-rate capacity:
c   Lc = Sum_of[neta*cos(dip)**2],
c   Lcc = Sum_of[(neta*cos(dip)**2)*cos(2*strike)],
c   Lcs = Sum_of[(neta*cos(dip)**2)*sin(2*strike)],
c   Ls = Sum_of[neta*sin(dip)**2],
c   Lsc = Sum_of[(neta*sin(dip)**2)*cos(4*strike)],
c   Lss = Sum_of[(neta*sin(dip)**2)*sin(4*strike)].
c   The sums are over the equivalent of fracture orientations within an
c    area, with neta corresponding to the product of kappa and fracture
c    length per unit area. Lc, Lcc, Lcs, Ls, Lsc and Lss have dimensions
c    of strain rate, the dip angle for each fracture orientation is
c    measured from horizontal, and the associated strike azimuth is
c    measured clockwise from north. Both Lc and Ls must be greater than
c    zero, and Lc**2 and Ls**2 must be greater than the sums
c    Lcc**2+Lcs**2 and Lsc**2+Lss**2 respectively. Lcc, Lcs, Lsc and Lss
c    can be positive, negative or zero. The conditions
c    Lc > sqrt(Lcc**2+Lcs**2) and Ls > sqrt(Lsc**2+Lss**2) preclude the
c    possibility of just one fracture azimuth, as well as precluding all
c    dips from being either 0 or 90 degrees. Setting Lcc, Lcs, Lsc and
c    Lss to zero gives isotropic behaviour in the horizontal plane, and
c    setting Ls = 4*Lc with Lcc, Lcs, Lsc and Lss set to zero
c    corresponds to 3-dimensional isotropy with Poisson's ratio 0.5.
c    For situations with one fraction orientation dominating the
c    following rules of thumb should apply:
c       Lsc=Ls*(Lcc**2-Lcs**2)/Lc**2,
c       Lss=Ls*(2*Lcc*Lcs)/Lc**2.
c   At rigid boundary lines all 6 components of strain-rate capacity are
c    automatically set to zero.
c   Elsewhere averaging of values in the elements that meet at each grid
c    point is performed to give point values there, with weights
c    proportional to element area. The values used in the modelling
c    within each element are linearly interpolated from the grid point
c    values. The averaging preserves the area integral of strain-rate
c    capacity.
c   The averaging is performed independently on each side of a fault,
c    with the consequence that strain-rate capacity can be discontinuous
c    at the fault. Elsewhere strain-rate capacity is continuous.
c
c Dimensionless stress-like potentials sxxpot, syypot and sxypot:
c   Non-zero values of sxxpot-syypot and sxypot (except at the equator)
c    and spatial difference in potential values (anywhere) combine with
c    velocity boundary conditions in dictating strain-rate patterns.
c   Non-zero values should be used sparingly and with caution, and are
c    intended for where velocity boundary conditions alone are unable to
c    predict major features of the strain-rate pattern.
c   In principle, variations in potential should be compatible in sign
c    and relative magnitude with actual physical forces. In flat-Earth
c    versions of true horizontal force-balance equations corresponding
c    physical forces are of the simple differential form
c       fx ~ -(dsxxpot/dx+dsxypot/dy) and fy ~ -(dsxypot/dy+dsyypot/dy).
c    In the spherical case the relationship between fx and fy and
c    sxxpot, syypot and sxypot is also of this form, except that
c    dsxxpot/dx becomes
c       sec(lat)*dsxxpot/dlong-2*tan(lat)*sxypot
c    and dsxypot/dx becomes
c       sec(lat)*dsxypot/dlong+tan(lat)*(sxxpot-syypot).
c   The following applies when sxxpot, syypot and sxypot are uniformly
c    zero and the kappa and neta tensors (constructed using the 2
c    components of slip-rate capacity and the 6 components of
c    strain-rate capacity) are assigned spatially compatible values,
c    with the neta tensor being close in magnitude to the strain rate.
c    Then in the apriori solutions computed prior to observational data
c    being incorporated dimensionless stress-like quantites with values
c    uniformly of order unity are obtained by multiplying strain rates
c    by the inverse of the neta tensor.
c   In general when the values of sxxpot, syypot and sxypot are
c    multiplied by the neta tensor itself strain-rate-observation-like
c    quantities exxpot, eyypot, exypot are produced. Similarly,
c    slip-rate-observation-like quantities duxpot, duypot are produced
c    when sxxpot, syypot and sxypot are converted into traction-like
c    quantities acting on the sides of faults and these are multiplied
c    by the kappa tensor. When duxpot and duypot are zero traction-like
c    quantities obtained by multiplying slip rates by the inverse of the
c    kappa tensor will be of order unity if the kappa tensor is close in
c    magnitude to the slip rate.
c   The overall mismatch in apriori solutions between the computed
c    strain rates and slip rates and the exxpot, eyypot, exypot and
c    duxpot, duypot values is apportioned spatially according to the
c    neta and kappa tensor values and compatibility constraints.
c   A final point to indicate the nature of the potentials is that when
c    sxypot is zero and sxxpot and syypot are equal with a non-zero
c    value that is spatially uniform, solutions for slip rates and
c    strain rates will be the same (to the precision of the
c    calculations) as solutions obtained when zero values are used for
c    all three of sxxpot, syypot and sxypot. In other words, adding a
c    spatially constant amount to both sxxpot and syypot doesn't affect
c    the results.
c   Within each element and along its sides values are obtained by
c    interpolation of the grid-point values.
c
c Velocity specified along lines:
c   Velocities must be specified along external boundary lines and the
c    boundaries of any internal areas that are not part of the
c    triangulation. This rule of specifying velocities includes any
c    rigid boundaries, where the specification is by way of
c    rotation-rate vectors, rather than through direct input of
c    velocity. A possible exception, which won't be implemented for the
c    time being, is to determine rotation-rate vectors as part of the
c    inversion process for individual rigid boundary lines that form
c    closed loops.
c   Velocities can be specified also along internal lines, with a use of
c    this capability being to determine appropriate values for the
c    stress-like potentials sxxpot, syypot and sxypot. Let us suppose
c    that the input internal velocity values are known to be
c    approximately correct but differ from the values predicted by an
c    apriori model without the velocities included. In the revised model
c    (with the velocities added) artifical discontinuities in
c    traction-like quantities (calculated using the neta tensor and the
c    new strain-rate solution) will be introduced at the internal lines
c    where the velocity values are enforced. The magnitudes of the
c    discontinuities in the traction-like quantities indicate how
c    different average values of the stress-like potentials in regions
c    either side of each line have to be to eliminate the artifical
c    discontinuities.
c
c Parameterisation of velocity components ux and uy and slip-rate
c  components dut and dun:
c
c Within each element each component of velocity is specified as a cubic
c  Hermite polynomial, with continuous value and continuous x and y
c  derivatives at each of the three grid points and the value at the
c  midpoint of the element as a further parameter, giving the 10
c  parameters in total needed to define the 2D cubic function.
c
c On each fault segment each component of slip rate is specified as a
c  cubic spline, with continuous values and derivatives at the two grid
c  points.
c
c Derivative constraints:
c

c In general at the midpoint of each of the three sides of each element
c  the normal derivative of each component of velocity is required to be
c  continuous. This results in both the x and y derivatives being
c  continuous at all points along the side, not just at the grid points.
c  The only sides where this constraint is not applied are at faults and
c  at lines where velocities are specified, including external
c  boundaries.
c
c Taking into account that there are approximately half as many grid
c  points as elements in a grid, and approximately one and half times as
c  many sides, this constraint leaves effectively one value of each
c  component of velocity (or equivalently two components of strain rate,
c  with the third component given by strain-rate compatibility) free in
c  each element. (In comparison, linear triangular elements, with only
c  the two components of velocity specified at each grid point, have
c  effectively only one free component of velocity per element and
c  velocity derivatives are not continuous.)
c
c On each fault segment the third derivative of each component of slip
c  rate is required to be zero, except in segments at zero-slip ends of
c  faults. This results in there being one value of slip rate free for
c  each component in each fault segment.
c
c Other constraints:
c
c At grid points and the midpoints of segments on rigid boundary lines
c  and general lines where velocities are specified the values of both
c  components of horizontal velocity are constrained to have the
c  specified values. In the case of a fault being intersected the
c  velocities on both sides of the fault are constrained, and the
c  slip rate at the grid point in question is constrained to be equal to
c  the difference in the velocities. When the grid point is internal to
c  the triangulation the latter condition replaces the requirement that
c  the derivative of each component of slip rate be continuous.
c
c On faults the difference in velocity between the two sides is
c  constrained to be equal to the slip rates. This is done by matching
c  the average values in each half-segment.
c
c Transformation of the dynamic-like apriori solution for combination
c  with statistical observational constraints:
c
c With fine-enough gridding the total sum of squares SS(dynamic) for the
c  dynamic-like apriori solution will be effectively independent of the
c  grid-point spacing, in the same way as corresponding sums of squares
c  are for solutions to true dynamics problems. In contrast the number
c  of degrees of freedom N(dof) in the grid, once the contraints above
c  have been applied, will be roughly linearly proportional to the
c  number of points. Observational constraints relating to average
c  values of strain rate in elements and to slip-rate averages for fault
c  segments are similarly grid-dependent.
c
c Before the apriori solution is combined with observations, to produce
c  a statistical aposteriori solution, each contribution to SS(dynamic)
c  is multiplied by the normalisation factor N(dof)/SS(dynamic) to
c  convert the sum of squares into a normalised statistical-like one
c  with value N(dof). Then rules of Bayesian Statistics are used to
c  integrate sums of squares derived from the observations with the
c  apriori information. In this products of kappa values times lengths
c  of fault segments multiplied by N(dof)/SS(dynamic) perform the roles
c  of inverses of apriori variances for slip rates, and products of neta
c  values times areas of elements multiplied by N(dof)/SS(dynamic)
c  perform the roles of inverses of apriori variances for strain rates.
c
c The aposteriori solution is purely statistical in nature, without a
c  dynamical analogue. Even so, if the inputs to the apriori solution
c  can be tweaked so that the apriori and aposteriori solutions agree,
c  or equivalently the apriori solution matches the observations to
c  within standard errors, then clearly a dynamic-like solution has been
c  constructed that is in accord with the observations. In general
c  differences between the aposteriori and apriori solutions will
c  indicate where inputs to the apriori solution have to be changed to
c  achieve this.
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE input(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
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
      integer gp,s,e,f,vl,rb,vo,fo,eo,duc,ec
      integer i,j,nj,vptype,vp
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
cf2py intent(out) ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,gp1s,gp2s,s1e,s2e,s3e,nfs,sf,nvlgp,nvls,nvlf,gpvl,svl,fvl,nrbs,srb,ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,nece,eec,interp,re,long_bn,lat,sxxpot,syypot,sxypot,lc,lcc,lcs,ls,lsc,lss,kc,ks,ux,uy,uxm,uym,uxp,uyp,plat,plong,prate,volong,volat,oux,ouy,seoux,seouy,rouxuy,codut,codun,oduc,seoduc,rfo12,coexx,coeyy,coexy,oec,seoec,reo12,reo13,reo23,cdut,cdun,refduc,scduc,cexx,ceyy,cexy,refec,scec
c
      open(1,file='setup_input.dat')
c
c Geometry input and apriori model specification:
c
c Radius of the Earth:
      read(1,*) rE
c   A consistent set of units must be used throughout. The value input
c    for rE defines the distance scale. The velocity scale must be the
c    same for velocities and slip rates, and the unit value for strain
c    rate and rotation rate must be equal to one unit of velocity per
c    unit of distance.
c   No scale is required for the stress-like potentials, given that they
c    are dimensionless. As explained above, stress-like quantities
c    obtained by dividing strain rates by the neta tensor and slip rates
c    by the kappa tensor will have values of order unity when the neta
c    and kappa values are close in magnitude to the strain rates and the
c    slip rates respectively.
c   Longitude (long) and latitude (lat) values are in degrees.
c
c Grid points:
      read(1,*) ngp
      if (ngp.gt.maxgp) then
            write(*,*) 'ngp,maxgp=',ngp,maxgp
            stop 'Recompile with an increased maxgp value'
      end if
      do i=1,ngp
            read(1,*) gp,long(i),lat(i)
            if (gp.ne.i) then
                  write(*,*) 'i,gp=',i,gp
                  stop 'Identifying number gp is not equal to i'
            end if
            read(1,*) sxxpot(i),syypot(i),sxypot(i)
      end do
c   The grid-point identifying number gp is required to be the same as
c    the i-value, from 1 to ngp.
c
c Sides of elements (knowing these from the start saves significantly on
c  coding and helps in keeping track of faults and other lines):
      read(1,*) ns
      if (ns.gt.maxs) then
            write(*,*) 'ns,maxs=',ns,maxs
            stop 'Recompile with an increased maxs value'
      end if
      do i=1,ns
            read(1,*) s,gp1s(i),gp2s(i)
            if (s.ne.i) then
                  write(*,*) 'i,s=',i,s
                  stop 'Identifying number s is not equal to i'
            end if
      end do
c   The side identifying number s is required to be the same as the
c    i-value, from 1 to ns.
c   The identifying numbers gp1s and gp2s for the two grid points can be
c    input with either one first.
c
c Elements:
c   Angles between sides of elements cannot be less than 15 degrees (use
c    Ruppert's algorithm to add extra grid points that rectify this if
c    necessary). The only exceptions allowed are in corners where faults
c    meet other faults or rigid boundary lines at more acute angles.
c   No element can have a rigid boundary line on more than one side.
      read(1,*) ne
      if (ne.gt.maxe) then
            write(*,*) 'ne,maxe=',ne,maxe
            stop 'Recompile with an increased maxe value'
      end if
      do i=1,ne
            read(1,*) e,s1e(i),s2e(i),s3e(i)
            if (e.ne.i) then
                  write(*,*) 'i,e=',i,e
                  stop 'Identifying number e is not equal to i'
            end if
            read(1,*) Lc(i),Lcc(i),Lcs(i),Ls(i),Lsc(i),Lss(i)
            if (Lc(i).le.dsqrt(Lcc(i)**2+Lcs(i)**2)) then
                  write(*,*) 'i,Lc,Lcc,Lcs=',i,Lc(i),Lcc(i),Lcs(i)
            stop 'Lc must be greater than sqrt(Lcc**2+Lcs**2)'
            end if
            if (Ls(i).le.dsqrt(Lsc(i)**2+Lss(i)**2)) then
                  write(*,*) 'i,Ls,Lsc,Lss=',i,Ls(i),Lsc(i),Lss(i)
            stop 'Ls must be greater than sqrt(Lsc**2+Lss**2)'
            end if
      end do
c   The element identifying number e is required to be the same as the
c    i-value, from 1 to ne.
c   The identifying numbers s1e, s2e and s3e for the three sides can be
c    input in any order.
c
c Faults (internally within the program new sets of grid points and
c  sides of elements are generated for each fault, so that there is one
c  set of velocity parameters for the elements on each side of the fault
c  and a third set of parameters is for slip on the fault):
c   Faults cannot cross or have line segments in common, and no grid
c    point can be repeated on the same fault.
c   A fault that ends at a grid point in common with another fault is
c    considered to terminate (i.e. have zero slip) there, except for the
c    case where three faults meet, which is recognised by the three
c    faults having a common end point.
c   Each fault must have at least one line segment where the fault does
c    not terminate (i.e. where neither grid point has zero slip).
c   nfs is the number of line segments (or equivalently sides of
c    elements, with identifying numbers sf) along each fault, and these
c    must be input sequentially along the fault.
c   The sign convention used to distinguish between the two sides of
c    each fault is that the left side of the fault looking from the
c    last line segment towards the first line segment is the negative
c    side, with the right side being the positive side.
      read(1,*) nf
      if (nf.gt.maxf) then
            write(*,*) 'nf,maxf=',nf,maxf
            stop 'Recompile with an increased maxf value'
      end if
      if (nf.gt.0) then
      do i=1,nf
            read(1,*) f,nfs(i)
            if (f.ne.i) then
                  write(*,*) 'i,f=',i,f
                  stop 'Identifying number f is not equal to i'
            end if
            if (nfs(i).le.1) then
                  write(*,*) 'i,nfs(i)=',i,nfs(i)
                  stop 'nfs must be greater than 1'
            end if
            if (nfs(i).gt.maxfs) then
                  write(*,*) 'i,nfs(i),maxfs=',i,nfs(i),maxfs
                  stop 'Recompile with an increased maxfs value'
            end if
            do j=1,nfs(i)
                  read(1,*) sf(j,i),Kc(j,i),Ks(j,i)
                  if ((Kc(j,i).le.0.0d0).or.(Ks(j,i).le.0.0d0))
     1                  then
                        write(*,*) 'i,j,Kc,Ks=',i,j,Kc(j,i),
     1                        Ks(j,i)
                  stop 'Kc and Ks must be greater than zero'
                  end if
            end do
      end do
      end if
c   The fault identifying number f is required to be the same as the
c    i-value, from 1 to nf.
c
c Lines where velocities are input, which must include external
c  boundaries that are not rigid boundary lines (and the boundaries of
c  any internal areas that are not part of the triangulation count here
c  as being external boundaries):
c   These lines can cross faults (at grid points), except where three
c    faults meet, but cannot have line segments in common with faults.
c   Any line passing through a grid point where a fault terminates (i.e.
c    has zero slip) is not considered to cross that fault.
c   Similarly to what is done with faults, for each line where
c    velocities are input new sets of grid points and sides of elements
c    are generated internally within the program, so that there is one
c    set of velocity parameters for each side of a line that is interior
c    to the triangulation. This allows normal derivatives to be
c    discontinuous. The sign convention is the same as for faults.
c   At most two lines where velocities are input can meet or cross at a
c    grid point, and only one line where velocities are input can cross
c    a fault at a grid point where the fault does not terminate.
c   No grid point can be repeated on the same line.
c   At points in common with rigid boundary lines the values are
c    discarded, and the input for the rigid boundary line is used,
c    except for the case of a fault coinciding with a rigid boundary
c    line, where the input velocity is retained on the opposite side of
c    the fault to the rigid boundary line.
c   nvlgp, is the number of grid points on each line excluding those on
c    faults, nvls is the number of line segments (i.e. sides of
c    elements) along the line, and nvlf is the number of faults crossed
c    by the line.
c   Each line must start and end at grid points, so nvls must be equal
c    to nvlgp+nvlf-1.
c   The points must be input sequentially.
c   vptype is a flag for the nature of each point where velocity values
c    are specified and has the values 1 = grid point, 2 = midpoint of a
c    line segment, and 3 = fault. In the cases 1 and 3 vp is the
c    identifying number of a grid point, whereas in the case 2 it is the
c    identifying number of a side.
c   If the value of vptype is preceded by a minus sign no velocity is
c    input for that point, and if the point is not on either another
c    line where velocities are specified or a rigid boundary line the
c    program solves the corresponding 1D problem to interpolate for
c    appropriate values.
c   The option of having the program interpolate requires velocity
c    values to be independently specified at the end points of the
c    section of line where the interpolation is performed. These values
c    can be either input directly, input indirectly if the end point is
c    on a rigid boundary line, or determined by the program if the point
c    is on another line where values are interpolated.
c   The end points of a section of line where interpolation is performed
c    must be grid points, and the program interpolates along the lines
c    (i.e. solves the 1D problem) in the order in which the lines are
c    input.
c   So the option of having values for an end point specified as part of
c    interpolation along another line applies only if the other line is
c    input ahead of the line where the values at the end point are to be
c    used.
c   In accord with this and how intersections with rigid boundaries are
c    handled, when two lines where velocities are input cross or meet
c    the velocity values that are used at the intersection point are
c    always those for the velocity line that is input first.
c   In solving the 1D problem, velocities and slip rates are treated as
c    being locally symmetric about the line and the Kc and Ks values at
c    faults and neta tensor values elsewhere are used to partition the
c    strain rates required by the end values of velocity and the
c    stress-like potentials along the line.
c   For a fault (with identifying number fvl) uxm and uym are the values
c    for the minus (i.e. negative) side of the fault according to the
c    convention above, and uxp and uyp are the values for the positive
c    side.
c   Any fault that intersects an external boundary but is not identified
c    as a fault in the velocity input is considered to terminate at the
c    boundary
      read(1,*) nvl
      if (nvl.gt.maxvl) then
            write(*,*) 'nvl,maxvl=',nvl,maxvl
            stop 'Recompile with an increased maxvl value'
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            read(1,*) vl,nvlgp(i),nvls(i),nvlf(i)
            if (vl.ne.i) then
                  write(*,*) 'i,vl=',i,vl
                  stop 'Identifying number vl is not equal to i'
            end if
            if (nvls(i).le.1) then
                  write(*,*) 'i,nvls(i)=',i,nvls(i)
                  stop 'nvls must be greater than 1'
            end if
            if (nvls(i).ne.nvlgp(i)+nvlf(i)-1) then
                  write(*,*) 'i,nvlgp(i),nvls(i),nvlf(i)=',i,
     1                  nvlgp(i),nvls(i),nvlf(i)
                  stop 'nvls must be equal to than nvlgp+nvlf-1'
            end if
            if (nvls(i).gt.maxvls) then
                  write(*,*) 'i,ns,maxvls=',i,ns,maxvls
                  stop 'Recompile with an increased maxvls value'
            end if
            nj=nvlgp(i)+nvls(i)+nvlf(i)
            if (nj.gt.maxvlj) then
                  write(*,*) 'i,nj,maxvlj=',i,nj,maxvlj
                  stop 'Recompile with an increased maxvlj value'
            end if
            do j=1,nj
                  read(1,*) vptype,vp
                  if (vptype.eq.0) then
                        write(*,*) 'i,j,vptype=',i,j,vptype
                        stop 'vptype value is not valid'
                  end if
                  if (vptype.eq.1) then
                        gp=j/2
                        if (j.ne.2*gp+1) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For grid point velocity input j must be odd'
                        end if
                        gpvl(gp,i)=vp
                        fvl(gp,i)=0
                        read(1,*) ux(j,i),uy(j,i)
                        interp(j,i)=.false.
                  end if
                  if (vptype.eq.2) then
                        s=j/2
                        if (j.ne.2*s) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For velocity input on a side j must be even'
                        end if
                        svl(s,i)=vp
                        read(1,*) ux(j,i),uy(j,i)
                        interp(j,i)=.false.
                  end if
                  if (vptype.eq.3) then
                        gp=j/2
                        if (j.ne.2*gp+1) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For velocity input at a fault j must be odd'
                        end if
                        gpvl(gp,i)=vp
                        read(1,*) fvl(gp,i),uxm(gp,i),uym(gp,i),
     1                        uxp(gp,i),uyp(gp,i)
                        interp(j,i)=.false.
                  end if
                  if (vptype.gt.3) then
                        write(*,*) 'i,j,vptype=',i,j,vptype
                        stop 'vptype value is not valid'
                  end if
                  if (vptype.eq.-1) then
                        gp=j/2
                        if (j.ne.2*gp+1) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For grid point velocity input j must be odd'
                        end if
                        gpvl(gp,i)=vp
                        fvl(gp,i)=0
                        interp(j,i)=.true.
                  end if
                  if (vptype.eq.-2) then
                        s=j/2
                        if (j.ne.2*s) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For velocity input on a side j must be even'
                        end if
                        svl(s,i)=vp
                        interp(j,i)=.true.
                  end if
                  if (vptype.eq.-3) then
                        gp=j/2
                        if (j.ne.2*gp+1) then
                              write(*,*) 'i,j,vptype=',i,j,
     1                              vptype
            stop 'For velocity input at a fault j must be odd'
                        end if
                        gpvl(gp,i)=vp
                        read(1,*) fvl(gp,i)
                        interp(j,i)=.true.
                  end if
                  if (vptype.lt.-3) then
                        write(*,*) 'i,j,vptype=',i,j,vptype
                        stop 'vptype value is not valid'
                  end if
            end do
      end do
      end if
c   The velocity-line identifying number vl is required to be the same
c    as the i-value, from 1 to nvl, and numerous other checks are
c    performed.
c
c Rigid boundary lines (which can have line segments in common with
c  faults, but any fault that has an end point on a rigid boundary is
c  considered to terminate at that point, other than when the end point
c  is also at an end of the rigid boundary line, where the
c  considerations that apply are those above for lines where velocities
c  are input):
c   Rigid boundary lines cannot have points in common, and each rigid
c    boundary line must be at an external boundary or the boundary of an
c    internal area that is not part of the triangulation.
c   A rigid boundary line can be a closed loop, with the same grid point
c    at both ends. Except for such cases, no grid point can be repeated
c    on the same rigid boundary.
c   nrbs is the number of line segments on each rigid boundary, and the
c    line segments (with identifying numbers srb) are input in
c    sequential order.
c   The convention for rotation rate (prate) is that positive values are
c    anti-clockwise rotation about the pole at (plat,plong).
c   For the time being I won't implement the possibility of having a
c    rigid boundary with unspecified rotation pole and rotation rate.
      read(1,*) nrb
      if (nrb.gt.maxrb) then
            write(*,*) 'nrb,maxrb=',nrb,maxrb
            stop 'Recompile with an increased maxrb value'
      end if
      if (nrb.gt.0) then
      do i=1,nrb
            read(1,*) rb,nrbs(i),plat(i),plong(i),prate(i)
            if (rb.ne.i) then
                  write(*,*) 'i,rb=',i,rb
                  stop 'Identifying number rb is not equal to i'
            end if
            if (nrbs(i).gt.maxrbs) then
                  write(*,*) 'i,nrbs(i),maxrbs=',i,nrbs(i),maxrbs
                  stop 'Recompile with an increased maxrbs value'
            end if
            do j=1,nrbs(i)
                  read(1,*) srb(j,i)
            end do
      end do
      end if
c   The rigid-boundary-line identifying number rb is required to be the
c    same as the i-value, from 1 to nrb.
c
c Observation input:
c   There are presently two classes of observational input catered for.
c
c Velocities at points, average slip rates on fault segments, and
c  average strain rates in elements:
c   This first group relates to individual points, fault segments and
c    elements, and in the case of fault segments and elements the
c    statistical data provided can be for a partial set of the slip-rate
c    or strain-rate components.
c   Most commonly the use of partial input will be where combinations of
c    the components of slip rate or strain rate are known to have
c    effectively zero values, while little is known of the values of
c    other combinations of the components. Adding values with large
c    standard errors for the combinations that little is known about
c    would add effectively nothing to the combined apriori and
c    observational sum of squares, as in that case the contribution from
c    the apriori solution would dominate.
c
c Velocities at points:
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
      end if
c   The velocity-observation identifying number vo is required to be the
c    same as the i-value, from 1 to nvo.
c   seoux and seouy are the standard errors for the observed east oux
c    and north ouy components of velocity, and rouxuy is their
c    correlation coefficient.
c
c Average values of slip rate on fault segments:
      read(1,*) nfo
      if (nfo.gt.maxfo) then
            write(*,*) 'nfo,maxfo=',nfo,maxfo
            stop 'nfo is impossibly large'
      end if
      if (nfo.gt.0) then
      do i=1,nfo
            read(1,*) fo,ffo(i),sfo(i),nfoc(i)
            if (fo.ne.i) then
                  write(*,*) 'i,fo=',i,fo
                  stop 'Identifying number fo is not equal to i'
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
c   The fault-segment-observation identifying number fo is required to
c    be the same as the i-value, from 1 to nfo.
c   ffo and sfo are the identifying numbers of the fault and the line
c    segment (or equivalently side of the adjoining elements).
c   nfoc is the number of observed components (1 or 2), and for each
c    component codut and codun are the contributions from the tangential
c    and normal components dut and dun such that the observed value oduc
c    is codut*dut+codun*dun. seoduc is the standard error of oduc, and
c    when there are two components rfo12 is their correlation
c     coefficient.
c
c Average values of strain rate in elements:
      read(1,*) neo
      if (neo.gt.maxeo) then
            write(*,*) 'neo,maxeo=',neo,maxeo
            stop 'neo is impossibly large'
      end if
      if (neo.gt.0) then
      do i=1,neo
            read(1,*) eo,eeo(i),neoc(i)
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
      end do
      end if
c   The element-observation identifying number eo is required to be the
c    same as the i-value, from 1 to neo.
c   eeo is the identifying number of the element.
c   neoc is the number of observed components (1, 2 or 3), and for each
c    component coexx, coeyy and coexy are the contributions from the
c    three components of strain rate such that the observed value oec is
c    coexx*exx+coeyy*eyy+coexy*exy. seoec is the standard error of oec,
c    and when there are two or three components reo12, reo13, reo23 are
c    their correlation coefficients.
c
c Observational constraints on differences between adjoining fault
c  segments and between adjoining elements:
c   This second class of observational input is designed to enforce
c    observed correlations between components of slip rate in adjoining
c    fault segments and between components of strain rate in adjoining
c    elements.
c   The constraints are input set by set, where a set consists of a
c    single component in each of at least two fault segments or
c    elements. For each pair of these fault segments or elements that
c    share a common point, in the case of fault segments, or side, in
c    the case of elements, a contribution of the following form is added
c    to the joint apriori and observational sum of squares used to
c    obtain the aposteriori solution:
c       [(value1-reference1)/scale1-(value2-reference2)/scale2]**2.
c    Here value1 is the value of the component in one of the fault
c    segments or elements, and value2 is the value of the associated
c    component in the other fault segment or element. The reference
c    values reference1 and reference2 are like observed values in the
c    sense that the contribution to the sum of squares is zero when
c    value1 = reference1 and value2 = reference2, and the scale factors
c    scale1 and scale2 are like standard errors.
c   What the contributions from each set attempt to do is make the term
c    (value-reference)/scale the same in all the fault segments or
c    elements in the set.
c
c   A use of such constraints is to specify an observed functional form
c    for the values of a component of slip rate along a fault. This is
c    achieved by setting the reference values and scale factors
c    proportional to the function required.
c
c Differences between components of slip rate on fault segments:
      read(1,*) nduc
      if (nduc.gt.maxduc) then
            write(*,*) 'nduc,maxduc=',nduc,maxduc
            stop 'Recompile with an increased maxduc value'
      end if
      if (nduc.gt.0) then
      do i=1,nduc
            read(1,*) duc,fduc(i),nducs(i)
            if (duc.ne.i) then
                  write(*,*) 'i,duc=',i,duc
                  stop 'Identifying number duc is not equal to i'
            end if
            if (nducs(i).gt.maxfs) then
                  write(*,*) 'i,nducs(i),maxfs=',i,nducs(i),maxfs
                  stop 'Recompile with an increased maxfs value'
            end if
            do j=1,nducs(i)
                  read(1,*) sduc(j,i),cdut(j,i),cdun(j,i),
     1                  refduc(j,i),scduc(j,i)
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
c   The fault-slip-component identifying number duc for each set is
c    required to be the same as the i-value, from 1 to nduc.
c   For each set fduc and sduc are the identifying numbers of the fault
c    and each of the line segments (or equivalently sides of elements)
c    in the set.
c   nducs is the number of line segments, and for each line segment cdut
c    and cdun are the contributions from the tangential and normal
c    slip rates dut and dun such that the value of the component being
c    considered in that line segment is cdut*dut+cdun*dun. refduc is the
c    reference value for the component and scduc is its scale factor.
c
c Differences between components of strain rate in elements:
      read(1,*) nec
      if (nec.gt.maxec) then
            write(*,*) 'nec,maxec=',nec,maxec
            stop 'Recompile with an increased maxec value'
      end if
      if (nec.gt.0) then
      do i=1,nec
            read(1,*) ec,nece(i)
            if (ec.ne.i) then
                  write(*,*) 'i,ec=',i,ec
                  stop 'Identifying number ec is not equal to i'
            end if
            if (nece(i).gt.maxece) then
                  write(*,*) 'i,nece(i),maxece=',i,nece(i),maxece
                  stop 'Recompile with an increased maxece value'
            end if
            do j=1,nece(i)
                  read(1,*) eec(j,i),cexx(j,i),ceyy(j,i),
     1                  cexy(j,i),refec(j,i),scec(j,i)
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
      end do
      end if
c   The strain-rate-component identifying number ec for each set is
c    required to be the same as the i-value, from 1 to nec.
c   For each set eec is the identifying number of each of the elements
c    in the set.
c   nece is the number of elements, and for each element cexx, ceyy and
c    cexy are the contributions from the strain rates exx, eyy and exy
c    such that the value of the component being considered in that
c    element is cexx*exx+ceyy*eyy+cexy*exy. refec is the reference value
c    for the component and scec is its scale factor.
c
      close(1)
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE check(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1s,gp2s,e1s,e2s,s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,gpvl,svl,fvl,nrbs,srb,gprb,
     3      evo,ffo,sfo,eeo,fduc,nducs,sduc,ncduc,s1duc,s2duc,
     4      nece,eec,ncec,e1ec,e2ec,npf,npvl,nprb,nsf,nsvl,nsrb,
     5      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,
     6      ntj,gptj,vls,svls,nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,
     7      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     8      nJvl,gpJvl,vl1Jvl,vl2Jvl,gp1Jvl,gp2Jvl,
     9      nfT,gpfT,f1fT,vl2fT,gp1fT,gp2fT,
     1      nTf,gpTf,vl1Tf,f2Tf,gp1Tf,gp2Tf,
     1      nTvl,gpTvl,vl1Tvl,vl2Tvl,gp1Tvl,gp2Tvl,
     2      nXf,gpXf,vl1Xf,f2Xf,gp1Xf,gp2Xf,
     3      nXvl,gpXvl,vl1Xvl,vl2Xvl,gp1Xvl,gp2Xvl,
     4      extern,p0zero,pnzero,interp,fillf0,fillfn,
     5      rE,long,lat,volong,volat,w1vo,w2vo,w3vo,
     6      apgpf,amgpf,apgpvl,amgpvl,apgprb,amgprb)
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,maxfp,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl
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
     1      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10)
      integer ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      integer npf,npvl,nprb,nsf,nsvl,nsrb,ntj
      integer nJvl,nfT,nTf,nTvl,nXf,nXvl
      integer gp,s,e,f,vl
      integer i,j,nj
      integer im,middle,ip,free,flag,s1,s2,s3
      integer s1c,s2c,e1c,e2c,e3c
      integer gp1s(maxs),gp2s(maxs)
      integer e1s(maxs),e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),srb(maxrbs,maxrb)
      integer gprb(0:maxrbs,maxrb)
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo)
      integer eeo(maxeo)
      integer fduc(maxduc),nducs(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer nece(maxec),eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer nfp(maxpf),fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf),gptj(maxtj)
      integer vls(maxsvl),svls(maxsvl)
      integer nvlp(maxpvl),vlp(2,maxpvl),gpvlp(2,maxpvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer rbp(maxprb),gprbp(maxprb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer gpJvl(maxJvl),vl1Jvl(maxJvl),vl2Jvl(maxJvl),
     1      gp1Jvl(maxJvl),gp2Jvl(maxJvl)
      integer gpfT(maxfT),f1fT(maxfT),vl2fT(maxfT),
     1      gp1fT(maxfT),gp2fT(maxfT)
      integer gpTf(maxTf),vl1Tf(maxTf),f2Tf(maxTf),
     1      gp1Tf(maxTf),gp2Tf(maxTf)
      integer gpTvl(maxTvl),vl1Tvl(maxTvl),vl2Tvl(maxTvl),
     1      gp1Tvl(maxTvl),gp2Tvl(maxTvl)
      integer gpXf(maxXf),vl1Xf(maxXf),f2Xf(maxXf),
     1      gp1Xf(maxXf),gp2Xf(maxXf)
      integer gpXvl(maxXvl),vl1Xvl(maxXvl),vl2Xvl(maxXvl),
     1      gp1Xvl(maxXvl),gp2Xvl(maxXvl)
      logical found
      logical extern(maxgp)
      logical p0zero(maxf),pnzero(maxf)
      logical interp(maxvlj,maxvl)
      logical fillf0(maxvl),fillfn(maxvl)
      logical scross
      real*8 rE
      real*8 xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1
      real*8 cosmax,x1,y1,x2,y2,x3,y3,d1,d2,d3,cos1,cos2,cos3
      real*8 long(maxgp),lat(maxgp)
      real*8 volong(maxvo),volat(maxvo)
      real*8 w1vo(maxvo),w2vo(maxvo),w3vo(maxvo)
      real*8 apgpf(0:maxfs,maxf),amgpf(0:maxfs,maxf)
      real*8 apgpvl(0:maxvls,maxvl),amgpvl(0:maxvls,maxvl)
      real*8 apgprb(0:maxrbs,maxrb),amgprb(0:maxrbs,maxrb)
      external scross,finde
c
      if (rE.le.0d0) then
            write(*,*) 'rE=',rE
            stop 'Radius of the Earth must be greater than zero'
      end if
c
      do i=2,ngp
            im=i-1
            do gp=1,im
                  if ((long(i).eq.long(gp)).and.
     1                  (lat(i).eq.lat(gp))) then
                        write(*,*) 'i,gp=',i,gp
                        write(*,*) 'longitudes=',long(i),
     1                        long(gp)
                        write(*,*) 'latitudes=',lat(i),
     1                        lat(gp)
                        stop 'Grid points must be distinct'
                  end if
            end do
      end do
c
      do i=2,ns
            im=i-1
            do s=1,im
                  if ((gp1s(i).eq.gp1s(s)).and.
     1                  (gp2s(i).eq.gp2s(s))) then
                        write(*,*) 'i,s=',i,s
                        write(*,*) 'gp1s(i),gp1s(s)=',gp1s(i),
     1                        gp1s(s)
                        write(*,*) 'gp2s(i),gp2s(s)=',gp2s(i),
     1                        gp2s(s)
                        stop 'Sides must be distinct'
                  end if
                  if ((gp1s(i).eq.gp2s(s)).and.
     1                  (gp2s(i).eq.gp1s(s))) then
                        write(*,*) 'i,s=',i,s
                        write(*,*) 'gp1s(i),gp2s(s)=',gp1s(i),
     1                        gp2s(s)
                        write(*,*) 'gp2s(i),gp1s(s)=',gp2s(i),
     1                        gp1s(s)
                        stop 'Sides must be distinct'
                  end if
                  xa0=long(gp1s(i))
                  ya0=lat(gp1s(i))
                  xa1=long(gp2s(i))
                  ya1=lat(gp2s(i))
                  xb0=long(gp1s(s))
                  yb0=lat(gp1s(s))
                  xb1=long(gp2s(s))
                  yb1=lat(gp2s(s))
                  if (scross(xa0,ya0,xa1,ya1,xb0,yb0,xb1,yb1))
     1                  then
                        write(*,*) 'i,s=',i,s
                        stop 'Sides must not cross'
                  end if
            end do
      end do
      do i=1,ns
            if (gp1s(i).eq.gp2s(i)) then
                  write(*,*) 'i,gp1s(i),gp2s(i)=',i,gp1s(i),
     1                  gp2s(i)
                  stop 'Grid points on sides must be distinct'
            end if
            e1s(i)=0
            e2s(i)=0
            isf(i)=0
            isvl(i)=0
            isrb(i)=0
      end do
      do i=1,ngp
            found=.false.
            do s=1,ns
                  if ((gp1s(s).eq.i).or.(gp2s(s).eq.i))
     1                  found=.true.
            end do
            if (.not.found) then


                  write(*,*) 'i=',i
                  stop 'Grid points must belong to sides'
            end if
            extern(i)=.false.
            ipf(i)=0
            ipvl(i)=0
            iprb(i)=0
      end do
c
      do i=2,ne
            im=i-1
            do e=1,im
                  if ((s1e(i).eq.s1e(e)).and.
     1                  (s2e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s1e(e)=',s1e(i),
     1                        s1e(e)
                        write(*,*) 's2e(i),s2e(e)=',s2e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s1e(e)).and.
     1                  (s2e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s1e(e)=',s1e(i),
     1                        s1e(e)
                        write(*,*) 's2e(i),s3e(e)=',s2e(i),
     1                        s3e(e)

            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s2e(e)).and.
     1                  (s2e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s2e(e)=',s1e(i),
     1                        s2e(e)
                        write(*,*) 's2e(i),s3e(e)=',s2e(i),
     1                        s3e(e)
            stop 'Elements can have at most one common side'
                  end if
c
                  if ((s1e(i).eq.s3e(e)).and.
     1                  (s2e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s3e(e)=',s1e(i),
     1                        s3e(e)
                        write(*,*) 's2e(i),s2e(e)=',s2e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s3e(e)).and.
     1                  (s2e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s3e(e)=',s1e(i),
     1                        s3e(e)
                        write(*,*) 's2e(i),s1e(e)=',s2e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s2e(e)).and.
     1                  (s2e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s2e(e)=',s1e(i),
     1                        s2e(e)
                        write(*,*) 's2e(i),s1e(e)=',s2e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
c
                  if ((s1e(i).eq.s1e(e)).and.
     1                  (s3e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s1e(e)=',s1e(i),
     1                        s1e(e)
                        write(*,*) 's3e(i),s2e(e)=',s3e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s1e(e)).and.
     1                  (s3e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s1e(e)=',s1e(i),
     1                        s1e(e)
                        write(*,*) 's3e(i),s3e(e)=',s3e(i),
     1                        s3e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s2e(e)).and.
     1                  (s3e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s2e(e)=',s1e(i),
     1                        s2e(e)
                        write(*,*) 's3e(i),s3e(e)=',s3e(i),
     1                        s3e(e)
            stop 'Elements can have at most one common side'
                  end if
c
                  if ((s1e(i).eq.s3e(e)).and.
     1                  (s3e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s3e(e)=',s1e(i),
     1                        s3e(e)
                        write(*,*) 's3e(i),s2e(e)=',s3e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s3e(e)).and.
     1                  (s3e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s3e(e)=',s1e(i),
     1                        s3e(e)
                        write(*,*) 's3e(i),s1e(e)=',s3e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s1e(i).eq.s2e(e)).and.
     1                  (s3e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's1e(i),s2e(e)=',s1e(i),
     1                        s2e(e)
                        write(*,*) 's3e(i),s1e(e)=',s3e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
c
                  if ((s2e(i).eq.s1e(e)).and.
     1                  (s3e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s1e(e)=',s2e(i),
     1                        s1e(e)
                        write(*,*) 's3e(i),s2e(e)=',s3e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s2e(i).eq.s1e(e)).and.
     1                  (s3e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s1e(e)=',s2e(i),
     1                        s1e(e)
                        write(*,*) 's3e(i),s3e(e)=',s3e(i),
     1                        s3e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s2e(i).eq.s2e(e)).and.
     1                  (s3e(i).eq.s3e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s2e(e)=',s2e(i),
     1                        s2e(e)
                        write(*,*) 's3e(i),s3e(e)=',s3e(i),
     1                        s3e(e)
            stop 'Elements can have at most one common side'
                  end if
c
                  if ((s2e(i).eq.s3e(e)).and.
     1                  (s3e(i).eq.s2e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s3e(e)=',s2e(i),
     1                        s3e(e)
                        write(*,*) 's3e(i),s2e(e)=',s3e(i),
     1                        s2e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s2e(i).eq.s3e(e)).and.
     1                  (s3e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s3e(e)=',s2e(i),
     1                        s3e(e)
                        write(*,*) 's3e(i),s1e(e)=',s3e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
                  if ((s2e(i).eq.s2e(e)).and.
     1                  (s3e(i).eq.s1e(e))) then
                        write(*,*) 'i,e=',i,e
                        write(*,*) 's2e(i),s2e(e)=',s2e(i),
     1                        s2e(e)
                        write(*,*) 's3e(i),s1e(e)=',s3e(i),
     1                        s1e(e)
            stop 'Elements can have at most one common side'
                  end if
            end do
      end do
      do i=1,ne
            if (s1e(i).eq.s2e(i)) then
                  write(*,*) 'i,s1e(i),s2e(i)=',i,s1e(i),
     1                  s2e(i)
                  stop 'Sides of elements must be distinct'
            end if
            if (s1e(i).eq.s3e(i)) then
                  write(*,*) 'i,s1e(i),s3e(i)=',i,s1e(i),
     1                  s3e(i)
                  stop 'Sides of elements must be distinct'
            end if
            if (s2e(i).eq.s3e(i)) then
                  write(*,*) 'i,s2e(i),s3e(i)=',i,s2e(i),
     1                  s3e(i)
                  stop 'Sides of elements must be distinct'
            end if
            gp3e(i)=0
            if (gp1s(s1e(i)).eq.gp1s(s2e(i))) gp3e(i)=gp1s(s1e(i))
            if (gp1s(s1e(i)).eq.gp2s(s2e(i))) gp3e(i)=gp1s(s1e(i))
            if (gp2s(s1e(i)).eq.gp1s(s2e(i))) gp3e(i)=gp2s(s1e(i))
            if (gp2s(s1e(i)).eq.gp2s(s2e(i))) gp3e(i)=gp2s(s1e(i))
            if (gp3e(i).eq.0) then
                  write(*,*) 'i,s1e(i),s2e(i)=',i,s1e(i),s2e(i)
            stop 'Sides of elements must have a common grid point'
            end if
            gp2e(i)=0
            if (gp1s(s1e(i)).eq.gp1s(s3e(i))) gp2e(i)=gp1s(s1e(i))
            if (gp1s(s1e(i)).eq.gp2s(s3e(i))) gp2e(i)=gp1s(s1e(i))
            if (gp2s(s1e(i)).eq.gp1s(s3e(i))) gp2e(i)=gp2s(s1e(i))
            if (gp2s(s1e(i)).eq.gp2s(s3e(i))) gp2e(i)=gp2s(s1e(i))
            if (gp2e(i).eq.0) then
                  write(*,*) 'i,s1e(i),s3e(i)=',i,s1e(i),s3e(i)
            stop 'Sides of elements must have a common grid point'
            end if
            gp1e(i)=0
            if (gp1s(s2e(i)).eq.gp1s(s3e(i))) gp1e(i)=gp1s(s2e(i))
            if (gp1s(s2e(i)).eq.gp2s(s3e(i))) gp1e(i)=gp1s(s2e(i))
            if (gp2s(s2e(i)).eq.gp1s(s3e(i))) gp1e(i)=gp2s(s2e(i))
            if (gp2s(s2e(i)).eq.gp2s(s3e(i))) gp1e(i)=gp2s(s2e(i))
            if (gp1e(i).eq.0) then
                  write(*,*) 'i,s2e(i),s3e(i)=',i,s2e(i),s3e(i)
            stop 'Sides of elements must have a common grid point'
            end if
            if (e1s(s1e(i)).eq.0) then
                  e1s(s1e(i))=i
            else
                  if (e2s(s1e(i)).eq.0) then
                        e2s(s1e(i))=i
                  else
                        write(*,*) 'i,s1e(i),e1s,e2s=',
     1                       1,s1e(i),e1s(s1e(i)),
     2                        e2s(s1e(i))
            stop 'Sides cannot belong to more than two elements'
                  end if
            end if
            if (e1s(s2e(i)).eq.0) then
                  e1s(s2e(i))=i
            else
                  if (e2s(s2e(i)).eq.0) then
                        e2s(s2e(i))=i
                  else
                        write(*,*) 'i,s2e(i),e1s,e2s=',
     1                       1,s2e(i),e1s(s2e(i)),
     2                        e2s(s2e(i))
            stop 'Sides cannot belong to more than two elements'
                  end if
            end if
            if (e1s(s3e(i)).eq.0) then
                  e1s(s3e(i))=i
            else
                  if (e2s(s3e(i)).eq.0) then
                        e2s(s3e(i))=i
                  else
                        write(*,*) 'i,s3e(i),e1s,e2s=',
     1                       1,s3e(i),e1s(s3e(i)),
     2                        e2s(s3e(i))
            stop 'Sides cannot belong to more than two elements'
                  end if
            end if
      end do
      do i=1,ns
            if (e1s(i).eq.0) then
                  write(*,*) 'i=',i
                  stop 'Sides must belong to at least one element'
            end if
            if (e2s(i).eq.0) then
                  extern(gp1s(i))=.true.
                  extern(gp2s(i))=.true.
            end if
      end do
c
c If e2s(i) equals 0 then side i is at the boundary of the triangulation
c
      ntj=0
      if (nf.gt.0) then
      nsf=0
      npf=0
      do i=1,nf
            if (nfs(i).eq.1) then
                  gpf(0,i)=gp1s(sf(1,i))
                  gpf(1,i)=gp2s(sf(1,i))
            else
                  do s=2,nfs(i)
                        j=s-1
                        gpf(j,i)=0
                        if (gp1s(sf(j,i)).eq.gp1s(sf(s,i)))
     1                        gpf(j,i)=gp1s(sf(j,i))
                        if (gp1s(sf(j,i)).eq.gp2s(sf(s,i)))
     1                        gpf(j,i)=gp1s(sf(j,i))
                        if (gp2s(sf(j,i)).eq.gp1s(sf(s,i)))
     1                        gpf(j,i)=gp2s(sf(j,i))
                        if (gp2s(sf(j,i)).eq.gp2s(sf(s,i)))
     1                        gpf(j,i)=gp2s(sf(j,i))
                        if (gpf(j,i).eq.0) then
                              write(*,*) 'i,j,s=',i,j,s
            stop 'On fault i segments j and s have no common point'
                        end if
                  end do
                  if (gpf(1,i).eq.gp1s(sf(1,i)))
     1                  gpf(0,i)=gp2s(sf(1,i))
                  if (gpf(1,i).eq.gp2s(sf(1,i)))
     1                  gpf(0,i)=gp1s(sf(1,i))
                  j=nfs(i)-1
                  if (gpf(j,i).eq.gp1s(sf(nfs(i),i)))
     1                  gpf(nfs(i),i)=gp2s(sf(nfs(i),i))
                  if (gpf(j,i).eq.gp2s(sf(nfs(i),i)))
     1                  gpf(nfs(i),i)=gp1s(sf(nfs(i),i))
            end if
            do j=1,nfs(i)
                  if (isf(sf(j,i)).eq.0) then
                        nsf=nsf+1
                        if (nsf.gt.maxsf) then
                              write(*,*) 'nsf,maxsf=',nsf,
     1                              maxsf
                        stop 'Recompile with an increased maxsf value'
                        end if
                        isf(sf(j,i))=nsf
                        fs(nsf)=i
                        sfs(nsf)=j
                  else
                        write(*,*) 'i,j,sf,fs=',
     1                        i,j,sf(j,i),
     2                        fs(isf(sf(j,i)))
                  stop 'Side sf cannot be on two faults i and fs'
                  end if
                  gp=j-1
                  xa0=long(gpf(gp,i))
                  ya0=lat(gpf(gp,i))
                  xa1=long(gpf(j,i))
                  ya1=lat(gpf(j,i))
                  apgpf(gp,i)=datan2(xa1-xa0,ya1-ya0)
                  amgpf(j,i)=datan2(xa0-xa1,ya0-ya1)
            end do
            amgpf(0,i)=apgpf(0,i)
            apgpf(nfs(i),i)=amgpf(nfs(i),i)
            do j=0,nfs(i)
                  if (ipf(gpf(j,i)).eq.0) then
                        npf=npf+1
                        if (npf.gt.maxpf) then
                              write(*,*) 'npf,maxpf=',npf,
     1                              maxpf
                        stop 'Recompile with an increased maxpf value'
                        end if
                        ipf(gpf(j,i))=npf
                        nfp(npf)=1
                        fp(1,npf)=i
                        gpfp(1,npf)=j
                  else
                        ip=ipf(gpf(j,i))
                        do f=1,nfp(ip)
                              if (fp(f,ip).eq.i) then
                                    write(*,*) 'i,j=',i,j
            stop 'A grid point cannot be repeated on the same fault'
                              end if
                        end do
                        nfp(ip)=nfp(ip)+1
                        if (nfp(ip).gt.maxfp) then
                              write(*,*) 'nfp,maxfp=',
     1                              nfp(ip),maxfp
                        stop 'Recompile with an increased maxfp value'
                        end if
                        fp(nfp(ip),ip)=i
                        gpfp(nfp(ip),ip)=j
                  end if
            end do
            p0zero(i)=.true.
            pnzero(i)=.true.
      end do
      do i=1,ngp
            if (ipf(i).ne.0) then
                  ip=ipf(i)
                  jfp(ip)=0
                  middle=0
                  do j=1,nfp(ip)
                        if ((gpfp(j,ip).lt.nfs(fp(j,ip)))
     1                        .and.(gpfp(j,ip).gt.0)) then
                              jfp(ip)=j
                              middle=middle+1
                        end if
                  end do
                  if (middle.gt.1) then
                        write(*,*) 'i,nfp=',i,nfp(ip)
      stop 'A grid point can be in the middle of at most one fault'
                  end if
                  tjfp(ip)=0
                  if ((nfp(ip).eq.3).and.(middle.eq.0)) then
                        ntj=ntj+1
                        if (ntj.gt.maxtj) then
                              write(*,*) 'ntj,maxtj=',ntj,maxtj
                  stop 'Recompile with an increased maxtj value'
                        end if
                        tjfp(ip)=ntj
                        gptj(ntj)=i
                        do j=1,3

                              if (gpfp(j,ip).eq.0)
     1                              p0zero(fp(j,ip))=.false.
                              if (gpfp(j,ip).eq.nfs(fp(j,ip)))
     1                              pnzero(fp(j,ip))=.false.
                        end do
                  end if
            end if
      end do
      end if
c
      if (nvl.gt.0) then
      nsvl=0
      npvl=0
      do i=1,nvl
            if (nvls(i).eq.1) then
                  if (((gpvl(0,i).ne.gp1s(svl(1,i))).or.
     1                  (gpvl(1,i).ne.gp2s(svl(1,i)))).and.
     2                  ((gpvl(0,i).ne.gp2s(svl(1,i))).or.
     3                  (gpvl(1,i).ne.gp1s(svl(1,i))))) then
                        write(*,*) 'i,nvls=',i,nvls(i)
      stop 'Grid points do not match on the one side of v-line i'
                  end if
            else
                  do s=2,nvls(i)
                        j=s-1
                        if ((gp1s(svl(j,i)).eq.gp1s(svl(s,i))).and.
     1                        (gpvl(j,i).ne.gp1s(svl(j,i)))) then
                              write(*,*) 'i,j,s=',i,j,s
      stop 'Grid point j not on sides j and s on velocity line i'
                        end if
                        if ((gp1s(svl(j,i)).eq.gp2s(svl(s,i))).and.
     1                        (gpvl(j,i).ne.gp1s(svl(j,i)))) then
                              write(*,*) 'i,j,s=',i,j,s
      stop 'Grid point j not on sides j and s on velocity line i'
                        end if
                        if ((gp2s(svl(j,i)).eq.gp1s(svl(s,i))).and.
     1                        (gpvl(j,i).ne.gp2s(svl(j,i)))) then
                              write(*,*) 'i,j,s=',i,j,s
      stop 'Grid point j not on sides j and s on velocity line i'
                        end if
                        if ((gp2s(svl(j,i)).eq.gp2s(svl(s,i))).and.
     1                        (gpvl(j,i).ne.gp2s(svl(j,i)))) then
                              write(*,*) 'i,j,s=',i,j,s
      stop 'Grid point j not on sides j and s on velocity line i'
                        end if
                  end do
                  if ((gpvl(1,i).eq.gp1s(svl(1,i))).and.
     1                  (gpvl(0,i).ne.gp2s(svl(1,i)))) then
                        write(*,*) 'i=',i
      stop 'Grid point 0 not on side 1 on velocity line i'
                  end if
                  if ((gpvl(1,i).eq.gp2s(svl(1,i))).and.
     1                  (gpvl(0,i).ne.gp1s(svl(1,i)))) then
                        write(*,*) 'i=',i
      stop 'Grid point 0 not on side 1 on velocity line i'
                  end if
                  j=nvls(i)-1
                  if ((gpvl(j,i).eq.gp1s(svl(nvls(i),i))).and.
     1                  (gpvl(nvls(i),i).ne.gp2s(svl(nvls(i),i)))) then
                        write(*,*) 'i,nvls=',i,nvls(i)
      stop 'Grid point nvls not on side nvls on velocity line i'
                  end if
                  if ((gpvl(j,i).eq.gp2s(svl(nvls(i),i))).and.
     1                  (gpvl(nvls(i),i).ne.gp1s(svl(nvls(i),i)))) then
                        write(*,*) 'i,nvls=',i,nvls(i)
      stop 'Grid point nvls not on side nvls on velocity line i'
                  end if
            end if
            do j=1,nvls(i)
                  if (isf(svl(j,i)).ne.0) then
                        write(*,*) 'i,j,svl=',i,j,svl(j,i)
      stop 'Velocity lines and faults cannot have segments in common'
                  end if
                  if (isvl(svl(j,i)).eq.0) then
                        nsvl=nsvl+1
                        if (nsvl.gt.maxsvl) then
                              write(*,*) 'nsvl,maxsvl=',nsvl,
     1                              maxsvl
                        stop 'Recompile with an increased maxsvl value'
                        end if
                        isvl(svl(j,i))=nsvl
                        vls(nsvl)=i
                        svls(nsvl)=j
                  else
                        write(*,*) 'i,j,svl,vls=',
     1                        i,j,svl(j,i),
     2                        vls(isvl(svl(j,i)))
            stop 'Side svl cannot be on two velocity lines i and vls'
                  end if
                  gp=j-1
                  xa0=long(gpvl(gp,i))
                  ya0=lat(gpvl(gp,i))
                  xa1=long(gpvl(j,i))
                  ya1=lat(gpvl(j,i))
                  apgpvl(gp,i)=datan2(xa1-xa0,ya1-ya0)
                  amgpvl(j,i)=datan2(xa0-xa1,ya0-ya1)
            end do
            amgpvl(0,i)=apgpvl(0,i)
            apgpvl(nvls(i),i)=amgpvl(nvls(i),i)
            do j=0,nvls(i)
                  if (ipvl(gpvl(j,i)).eq.0) then
                        gp=gpvl(j,i)
                        if (ipf(gp).ne.0) then
                              ip=ipf(gp)
                              if (tjfp(ip).ne.0) then
                                    write(*,*) 'i,j,gp=',
     1                                    i,j,gp
      stop 'A velocity line cannot include a junction of three faults'
                              end if
                              if (jfp(ip).ne.0) then
                                    f=jfp(ip)
                                    if (fvl(j,i).ne.fp(f,ip)) then
                                          write(*,*)
     1                                          'i,j,fvl,fp=',
     2                                           i,j,fvl(j,i),
     3                                           fp(f,ip)
      stop 'Fault fp on velocity line i has been input as fault fvl'
                                    end if
                              end if
                              if ((jfp(ip).eq.0).and.
     1                              (nfp(ip).gt.1).and.
     2                              (fvl(j,i).ne.0)) then
                                          write(*,*)
     1                                          'i,j,fvl=',i,j,
     2                                          fvl(j,i)
      stop 'All faults are terminating so vptype cannot be 3 or -3'
                              end if
                              if ((jfp(ip).eq.0).and.
     1                              (nfp(ip).eq.1).and.
     2                              (extern(gp))) then
                                    if ((fvl(j,i).ne.fp(1,ip))
     1                                    .and.(fvl(j,i).ne.0)) then
                                          write(*,*)
     1                                         'i,j,fvl,fp=',
     2                                          i,j,fvl(j,i),
     3                                          fp(1,ip)
      stop 'Fault fp on velocity line i has been input as fault fvl'
                                    end if
                                    if (fvl(j,i).eq.fp(1,ip)) then
                                          jfp(ip)=1
                                          if (gpfp(1,ip).eq.0)
     1                                          p0zero(fp(1,ip))=
     2                                                .false.
                                          if (gpfp(1,ip).eq.
     1                                          nfs(fp(1,ip)))
     2                                          pnzero(fp(1,ip))=
     3                                                .false.
                                    end if
                              end if
                              if ((jfp(ip).eq.0).and.
     1                              (nfp(ip).eq.1).and.
     2                              (.not.extern(gp))) then
                                    if (fvl(j,i).ne.0) then
                                          write(*,*)
     1                                          'i,j,fvl=',i,j,
     2                                          fvl(j,i)
      stop 'The fault is terminating so vptype cannot be 3 or -3'
                                    end if
                              end if
                        else
                              if (fvl(j,i).ne.0) then
                                    write(*,*)
     1                                    'i,j,fvl=',i,j,
     2                                    fvl(j,i)
      stop 'There is no fault at this grid point on the velocity line'
                              end if
                        end if
                        npvl=npvl+1
                        if (npvl.gt.maxpvl) then
                              write(*,*) 'npvl,maxpvl=',
     1                              npvl,maxpvl
                        stop 'Recompile with an increased maxpvl value'
                        end if
                        ipvl(gpvl(j,i))=npvl
                        nvlp(npvl)=1
                        vlp(1,npvl)=i
                        gpvlp(1,npvl)=j
                  else
                        ip=ipvl(gpvl(j,i))
                        do vl=1,nvlp(ip)
                              if (vlp(vl,ip).eq.i) then
                                    write(*,*) 'i,j=',i,j
      stop 'A grid point cannot be repeated on the same velocity line'
                              end if
                        end do
                        if (ipf(gpvl(j,i)).ne.0) then
                              ip=ipf(gpvl(j,i))
                              if (jfp(ip).ne.0) then
                                    write(*,*) 'i,j,gp=',
     1                                    i,j,gpvl(j,i)
      stop 'Two velocity lines cannot share a grid point with a fault'
                              end if
                              if (fvl(j,i).ne.0) then
                                    write(*,*)
     1                                    'i,j,fvl=',i,j,
     2                                    fvl(j,i)
      stop 'All faults are terminating so vptype cannot be 3 or -3'
                              end if
                        else
                              if (fvl(j,i).ne.0) then
                                    write(*,*)
     1                                    'i,j,fvl=',i,j,
     2                                    fvl(j,i)
      stop 'There is no fault at this grid point on the velocity line'
                              end if
                        end if
                        ip=ipvl(gpvl(j,i))
                        nvlp(ip)=nvlp(ip)+1
                        if (nvlp(ip).gt.2) then
                              write(*,*) 'i,j,gp,nvlp=',
     1                              i,j,gpvl(j,i),nvlp(ip)
               stop 'A grid point can be on at most two velocity lines'
                        end if
                        interp(2*j+1,i)=.false.
                        vlp(2,ip)=i
                        gpvlp(2,ip)=j
                  end if
            end do
            nj=2*nvls(i)+1
            do j=2,nj
                  s=2*(j/2)
                  gp=2*((j-1)/2)+1
                  if ((.not.interp(s,i)).and.interp(gp,i)) then
                        write(*,*) 'i,j,s,gp=',i,j,s,gp
                        if (s.gt.gp) then
      stop 'vptype cannot change here to being positive on the side'
                        else
      stop 'vptype cannot change here from being positive on the side'
                        end if
                  end if
            end do
      end do
      end if
      if (nf.gt.0) then
      do i=1,nf
            free=nfs(i)
            if (p0zero(i)) free=free-1
            if (pnzero(i)) free=free-1
            if (free.lt.1) then
                  write(*,*) 'i,nfs=',i,nfs(i)
      stop 'Each fault must have at least one non-terminating segment' 
            end if
      end do
      end if
c
      if (nrb.gt.0) then
      nsrb=0
      nprb=0
      do i=1,nrb
            if (nrbs(i).eq.1) then
                  gprb(0,i)=gp1s(srb(1,i))
                  gprb(1,i)=gp2s(srb(1,i))
            else
                  do s=2,nrbs(i)
                        j=s-1
                        gprb(j,i)=0
                        if (gp1s(srb(j,i)).eq.gp1s(srb(s,i)))
     1                        gprb(j,i)=gp1s(srb(j,i))
                        if (gp1s(srb(j,i)).eq.gp2s(srb(s,i)))
     1                        gprb(j,i)=gp1s(srb(j,i))
                        if (gp2s(srb(j,i)).eq.gp1s(srb(s,i)))
     1                        gprb(j,i)=gp2s(srb(j,i))
                        if (gp2s(srb(j,i)).eq.gp2s(srb(s,i)))
     1                        gprb(j,i)=gp2s(srb(j,i))
                        if (gprb(j,i).eq.0) then
                              write(*,*) 'i,j,s=',i,j,s
      stop 'On rigid boundary i segments j and s have no common point'
                        end if
                  end do
                  if (gprb(1,i).eq.gp1s(srb(1,i)))
     1                  gprb(0,i)=gp2s(srb(1,i))
                  if (gprb(1,i).eq.gp2s(srb(1,i)))
     1                  gprb(0,i)=gp1s(srb(1,i))
                  j=nrbs(i)-1
                  if (gprb(j,i).eq.gp1s(srb(nrbs(i),i)))
     1                  gprb(nrbs(i),i)=gp2s(srb(nrbs(i),i))
                  if (gprb(j,i).eq.gp2s(srb(nrbs(i),i)))
     1                  gprb(nrbs(i),i)=gp1s(srb(nrbs(i),i))
            end if
            do j=1,nrbs(i)
                  if (e2s(srb(j,i)).ne.0) then
                        write(*,*) 'i,j,e2s=',i,j,e2s(srb(j,i))
      stop 'No side of a rigid boundary can belong to two elements'
                  end if
                  if (isrb(srb(j,i)).eq.0) then
                        nsrb=nsrb+1
                        if (nsrb.gt.maxsrb) then
                              write(*,*) 'nsrb,maxsrb=',nsrb,
     1                              maxsrb
                        stop 'Recompile with an increased maxsrb value'
                        end if
                        isrb(srb(j,i))=nsrb
                        rbs(nsrb)=i
                        srbs(nsrb)=j
                  else
                        write(*,*) 'i,j,srb,rbs=',
     1                        i,j,srb(j,i),
     2                        rbs(isrb(srb(j,i)))
      stop 'Side srb cannot be on two rigid boundares i and rbs'
                  end if
                  gp=j-1
                  xa0=long(gprb(gp,i))
                  ya0=lat(gprb(gp,i))
                  xa1=long(gprb(j,i))
                  ya1=lat(gprb(j,i))
                  apgprb(gp,i)=datan2(xa1-xa0,ya1-ya0)
                  amgprb(j,i)=datan2(xa0-xa1,ya0-ya1)
            end do
            amgprb(0,i)=apgprb(0,i)
            apgprb(nrbs(i),i)=amgprb(nrbs(i),i)
            do j=0,nrbs(i)
                  if (ipvl(gprb(j,i)).ne.0) then
                        ip=ipvl(gprb(j,i))
                        middle=0
                        do vl=1,nvlp(ip)
                              if ((gpvlp(vl,ip).lt.
     1                              nvls(vlp(vl,ip)))
     2                              .and.(gpvlp(vl,ip)
     3                              .gt.0))
     4                              middle=1
                        end do
                        if (middle.eq.1) then
                              write(*,*) 'i,j,gp=',i,j,
     1                              gprb(j,i)
      stop 'Rigid boundaries can only be at ends of velocity lines'
                        end if
                  end if
                  if (iprb(gprb(j,i)).eq.0) then
                        nprb=nprb+1
                        if (nprb.gt.maxprb) then
                              write(*,*) 'nprb,maxprb=',
     1                              nprb,maxprb
                        stop 'Recompile with an increased maxprb value'
                        end if
                        iprb(gprb(j,i))=nprb
                        rbp(nprb)=i
                        gprbp(nprb)=j
                  else
                        if ((rbp(iprb(gprb(j,i))).eq.i).and.
     1                        ((gprbp(iprb(gprb(j,i))).ne.0)
     2                        .or.(j.ne.nrbs(i)))) then
                              write(*,*) 'i,j,gp=',i,j,gprb(j,i)
      stop 'Only the end points can be the same on a rigid boundary'
                        end if
                        if (rbp(iprb(gprb(j,i))).ne.i) then
                              write(*,*) 'i,j,gp=',i,j,gprb(j,i)
            stop 'A grid point can be on at most one rigid boundary'
                        end if
                  end if
            end do
      end do
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            fillf0(i)=.false.
            fillfn(i)=.false.
            nj=2*nvls(i)+1
            if (interp(1,i)) then
                  gp=gpvl(0,i)
                  if (iprb(gp).eq.0) then
                        write(*,*) 'i=',i
      stop 'vptype must be positive at the start of this velocity line'
                  end if
                  if (ipf(gp).ne.0) then
                        ip=ipf(gp)
                        if (jfp(ip).ne.0) then
                             fillf0(i)=.true.
                        end if
                  end if
            end if
            interp(1,i)=.false.
            if (interp(nj,i)) then
                  gp=gpvl(nvls(i),i)
                  if (iprb(gp).eq.0) then
                        write(*,*) 'i=',i
      stop 'vptype must be positive at the end of this velocity line'
                  end if
                  if (ipf(gp).ne.0) then
                        ip=ipf(gp)
                        if (jfp(ip).ne.0) then
                             fillfn(i)=.true.
                        end if
                  end if
            end if
            interp(nj,i)=.false.
      end do
      end if
c
      nJvl=0
      nfT=0
      nTf=0
      nTvl=0
      nXf=0
      nXvl=0
      do i=1,ngp
            if (ipvl(i).ne.0) then
                  ip=ipvl(i)
                  Jvlvlp(ip)=0
                  fTvlp(ip)=0
                  Tfvlp(ip)=0
                  Tvlvlp(ip)=0
                  Xfvlp(ip)=0
                  Xvlvlp(ip)=0
                  if ((ipf(i).ne.0).and.(iprb(i).ne.0)) then
                        ip=ipf(i)
                        middle=0
                        do j=1,nfp(ip)
                              if ((gpfp(j,ip).lt.nfs(fp(j,ip)))
     1                              .and.(gpfp(j,ip).gt.0)) then
                                    middle=1
                              end if
                        end do
                        if ((middle.eq.0).and.(jfp(ip).eq.1)) then
                              nTf=nTf+1
                              if (nTf.gt.maxTf) then
                                    write(*,*) 'nTf,maxTf=',nTf,
     1                                    maxTf
                        stop 'Recompile with an increased maxTf value'
                              end if
                              Tfvlp(ipvl(i))=nTf
                              gpTf(nTf)=i
                              vl1Tf(nTf)=vlp(1,ipvl(i))
                              f2Tf(nTf)=fp(1,ip)
                              gp1Tf(nTf)=gpvlp(1,ipvl(i))
                              gp2Tf(nTf)=gpfp(1,ip)
                        end if
                        if (middle.eq.1) then
                              nXf=nXf+1
                              if (nXf.gt.maxXf) then
                                    write(*,*) 'nXf,maxXf=',nXf,
     1                                    maxXf
                        stop 'Recompile with an increased maxXf value'
                              end if
                              Xfvlp(ipvl(i))=nXf
                              gpXf(nXf)=i
                              vl1Xf(nXf)=vlp(1,ipvl(i))
                              f2Xf(nXf)=fp(jfp(ip),ip)
                              gp1Xf(nXf)=gpvlp(1,ipvl(i))
                              gp2Xf(nXf)=gpfp(jfp(ip),ip)
                              xa0=amgpvl(gp1Xf(nXf),vl1Xf(nXf))
                              xa1=apgpvl(gp1Xf(nXf),vl1Xf(nXf))
                              if (gp1Xf(nXf).eq.0)
     1                              xa0=amgprb(gprbp(iprb(i)),
     2                                    rbp(iprb(i)))
                              if (gp1Xf(nXf).eq.nvls(ipvl(i)))
     1                              xa1=apgprb(gprbp(iprb(i)),
     2                                    rbp(iprb(i)))
                              ya0=amgpf(gp2Xf(nXf),f2Xf(nXf))
                              ya1=apgpf(gp2Xf(nXf),f2Xf(nXf))
                              if (xa0.lt.xa1) then
                                    xb0=xa0
                                    xb1=xa1
                              else
                                    xb0=xa1
                                    xb1=xa0
                              end if
                              if (ya0.lt.ya1) then
                                    yb0=ya0
                                    yb1=ya1
                              else
                                    yb0=ya1
                                    yb1=ya0
                              end if
                              if (.not.(((xb0.lt.yb0).and.
     1                              (yb0.lt.xb1).and.
     2                              (xb1.lt.yb1)).or.
     3                              ((yb0.lt.xb0).and.
     4                              (xb0.lt.yb1).and.
     5                              (yb1.lt.xb1)))) then
                                    write(*,*) 'i,vl,f=',i,
     1                                    vl1Xf(nXf),f2Xf(nXf)
      stop 'A fault cannot touch but can cross a velocity/rigid combo'
                              end if
                        end if
                  end if
                  if ((ipf(i).ne.0).and.(iprb(i).eq.0)) then
                        ip=ipf(i)
                        middle=0
                        do j=1,nfp(ip)
                              if ((gpfp(j,ip).lt.nfs(fp(j,ip)))
     1                              .and.(gpfp(j,ip).gt.0)) then
                                    middle=1
                              end if
                        end do
                        if ((middle.eq.0).and.(jfp(ip).eq.1)) then
                              nTf=nTf+1
                              if (nTf.gt.maxTf) then
                                    write(*,*) 'nTf,maxTf=',nTf,
     1                                    maxTf
                        stop 'Recompile with an increased maxTf value'
                              end if
                              Tfvlp(ipvl(i))=nTf
                              gpTf(nTf)=i
                              vl1Tf(nTf)=vlp(1,ipvl(i))
                              f2Tf(nTf)=fp(1,ip)
                              gp1Tf(nTf)=gpvlp(1,ipvl(i))
                              gp2Tf(nTf)=gpfp(1,ip)
                        end if
                        if (middle.eq.1) then
                              if ((gpvlp(1,ipvl(i)).eq.0).or.
     1                              (gpvlp(1,ipvl(i)).eq.
     2                              nvls(vlp(1,ipvl(i))))) then
                                    nfT=nfT+1
                                    if (nfT.gt.maxfT) then
                                          write(*,*) 'nfT,maxfT=',nfT,
     1                                          maxfT
                        stop 'Recompile with an increased maxfT value'
                                    end if
                                    fTvlp(ipvl(i))=nfT
                                    gpfT(nfT)=i
                                    f1fT(nfT)=fp(jfp(ip),ip)
                                    vl2fT(nfT)=vlp(1,ipvl(i))
                                    gp1fT(nfT)=gpfp(jfp(ip),ip)

                                    gp2fT(nfT)=gpvlp(1,ipvl(i))
                              else
                                    nXf=nXf+1
                                    if (nXf.gt.maxXf) then
                                          write(*,*) 'nXf,maxXf=',nXf,
     1                                          maxXf
                        stop 'Recompile with an increased maxXf value'
                                    end if
                                    Xfvlp(ipvl(i))=nXf
                                    gpXf(nXf)=i
                                    vl1Xf(nXf)=vlp(1,ipvl(i))
                                    f2Xf(nXf)=fp(jfp(ip),ip)
                                    gp1Xf(nXf)=gpvlp(1,ipvl(i))
                                    gp2Xf(nXf)=gpfp(jfp(ip),ip)
                                    xa0=amgpvl(gp1Xf(nXf),vl1Xf(nXf))
                                    xa1=apgpvl(gp1Xf(nXf),vl1Xf(nXf))
                                    ya0=amgpf(gp2Xf(nXf),f2Xf(nXf))
                                    ya1=apgpf(gp2Xf(nXf),f2Xf(nXf))
                                    if (xa0.lt.xa1) then
                                          xb0=xa0
                                          xb1=xa1
                                    else
                                          xb0=xa1
                                          xb1=xa0
                                    end if
                                    if (ya0.lt.ya1) then
                                          yb0=ya0
                                          yb1=ya1
                                    else
                                          yb0=ya1
                                          yb1=ya0
                                    end if
                                    if (.not.(((xb0.lt.yb0).and.
     1                                    (yb0.lt.xb1).and.
     2                                    (xb1.lt.yb1)).or.
     3                                    ((yb0.lt.xb0).and.
     4                                    (xb0.lt.yb1).and.
     5                                    (yb1.lt.xb1)))) then
                                          write(*,*) 'i,vl,f=',i,
     1                                          vl1Xf(nXf),f2Xf(nXf)
      stop 'A velocity line and a fault cannot touch but can cross'
                                    end if
                              end if
                        end if
                  end if
                  if ((ipf(i).eq.0).and.(iprb(i).ne.0)) then
                        ip=ipvl(i)
                        if (nvlp(ip).eq.2) then
                              nJvl=nJvl+1
                              if (nJvl.gt.maxJvl) then
                                    write(*,*) 'nJvl,maxJvl=',nJvl,
     1                                    maxJvl
                        stop 'Recompile with an increased maxJvl value'
                              end if
                              Jvlvlp(ip)=nJvl
                              gpJvl(nJvl)=i
                              vl1Jvl(nJvl)=vlp(1,ip)
                              vl2Jvl(nJvl)=vlp(2,ip)
                              gp1Jvl(nJvl)=gpvlp(1,ip)
                              gp2Jvl(nJvl)=gpvlp(2,ip)
                        end if
                  end if
                  if ((ipf(i).eq.0).and.(iprb(i).eq.0)) then
                        ip=ipvl(i)
                        if (nvlp(ip).eq.2) then
                              if (((gpvlp(1,ip).eq.0).or.
     1                              (gpvlp(1,ip).eq.nvls(vlp(1,ip))))
     2                              .and.((gpvlp(2,ip).eq.0).or.
     3                              (gpvlp(2,ip).eq.nvls(vlp(2,ip)))))
     4                              then
                                    nJvl=nJvl+1
                                    if (nJvl.gt.maxJvl) then
                                          write(*,*) 'nJvl,maxJvl=',
     1                                          nJvl,maxJvl
                        stop 'Recompile with an increased maxJvl value'
                                    end if
                                    Jvlvlp(ip)=nJvl
                                    gpJvl(nJvl)=i
                                    vl1Jvl(nJvl)=vlp(1,ip)
                                    vl2Jvl(nJvl)=vlp(2,ip)
                                    gp1Jvl(nJvl)=gpvlp(1,ip)
                                    gp2Jvl(nJvl)=gpvlp(2,ip)
                              end if
                              if (((gpvlp(1,ip).ne.0).and.
     1                              (gpvlp(1,ip).ne.nvls(vlp(1,ip))))
     2                              .and.((gpvlp(2,ip).eq.0).or.
     3                              (gpvlp(2,ip).eq.nvls(vlp(2,ip)))))
     4                              then
                                    nTvl=nTvl+1
                                    if (nTvl.gt.maxTvl) then
                                          write(*,*) 'nTvl,maxTvl=',
     1                                          nTvl,maxTvl
                        stop 'Recompile with an increased maxTvl value'
                                    end if
                                    Tvlvlp(ip)=nTvl
                                    gpTvl(nTvl)=i
                                    vl1Tvl(nTvl)=vlp(1,ip)
                                    vl2Tvl(nTvl)=vlp(2,ip)
                                    gp1Tvl(nTvl)=gpvlp(1,ip)
                                    gp2Tvl(nTvl)=gpvlp(2,ip)
                              end if
                              if (((gpvlp(1,ip).eq.0).or.
     1                              (gpvlp(1,ip).eq.nvls(vlp(1,ip))))
     2                              .and.((gpvlp(2,ip).ne.0).and.
     3                              (gpvlp(2,ip).ne.nvls(vlp(2,ip)))))
     4                              then
                                    nTvl=nTvl+1
                                    if (nTvl.gt.maxTvl) then
                                          write(*,*) 'nTvl,maxTvl=',
     1                                          nTvl,maxTvl
                        stop 'Recompile with an increased maxTvl value'
                                    end if
                                    Tvlvlp(ip)=nTvl
                                    gpTvl(nTvl)=i
                                    vl1Tvl(nTvl)=vlp(2,ip)
                                    vl2Tvl(nTvl)=vlp(1,ip)
                                    gp1Tvl(nTvl)=gpvlp(2,ip)
                                    gp2Tvl(nTvl)=gpvlp(1,ip)
                              end if
                              if (((gpvlp(1,ip).ne.0).and.
     1                              (gpvlp(1,ip).ne.nvls(vlp(1,ip))))
     2                              .and.((gpvlp(2,ip).ne.0).and.
     3                              (gpvlp(2,ip).ne.nvls(vlp(2,ip)))))
     4                              then
                                    nXvl=nXvl+1
                                    if (nXvl.gt.maxXvl) then
                                          write(*,*) 'nXvl,maxXvl=',
     1                                          nXvl,maxXvl
                        stop 'Recompile with an increased maxXvl value'
                                    end if
                                    Xvlvlp(ip)=nXvl
                                    gpXvl(nXvl)=i
                                    vl1Xvl(nXvl)=vlp(1,ip)
                                    vl2Xvl(nXvl)=vlp(2,ip)
                                    gp1Xvl(nXvl)=gpvlp(1,ip)
                                    gp2Xvl(nXvl)=gpvlp(2,ip)
                                    xa0=amgpvl(gpvlp(1,ip),vlp(1,ip))
                                    xa1=apgpvl(gpvlp(1,ip),vlp(1,ip))
                                    ya0=amgpvl(gpvlp(2,ip),vlp(2,ip))
                                    ya1=apgpvl(gpvlp(2,ip),vlp(2,ip))
                                    if (xa0.lt.xa1) then
                                          xb0=xa0
                                          xb1=xa1
                                    else
                                          xb0=xa1
                                          xb1=xa0
                                    end if
                                    if (ya0.lt.ya1) then
                                          yb0=ya0
                                          yb1=ya1
                                    else
                                          yb0=ya1
                                          yb1=ya0
                                    end if
                                    if (.not.(((xb0.lt.yb0).and.
     1                                    (yb0.lt.xb1).and.
     2                                    (xb1.lt.yb1)).or.
     3                                    ((yb0.lt.xb0).and.
     4                                    (xb0.lt.yb1).and.
     5                                    (yb1.lt.xb1)))) then
                                          write(*,*) 'i,vl1,vl2=',i,
     1                                          vlp(1,ip),vlp(2,ip)
            stop 'Two velocity lines cannot touch but can cross'
                                    end if
                              end if
                        end if
                  end if
            end if
      end do
c
      do i=1,ns
            if (e2s(i).eq.0) then
                  if ((isvl(i).eq.0).and.(isrb(i).eq.0)) then
                        write(*,*) 'i=',i
      stop 'Boundaries must be on velocity lines or rigid boundaries'
                  end if
            end if
      end do
      do i=1,ne
            free=0
            if (isrb(s1e(i)).eq.0) free=free+1
            if (isrb(s2e(i)).eq.0) free=free+1
            if (isrb(s3e(i)).eq.0) free=free+1
            if (free.lt.2) then
                  write(*,*) 'i,s1e,s2e,s3e=',i,s1e(i),s2e(i),
     1                  s3e(i)
      stop 'A rigid boundary can be on at most one side of an element'
            end if
      end do
c
      cosmax=dcos(1.0d0*datan(1.0d0)/45.0d0)
      do i=1,ne
            x1=long(gp1e(i))
            y1=lat(gp1e(i))
            x2=long(gp2e(i))
            y2=lat(gp2e(i))
            x3=long(gp3e(i))
            y3=lat(gp3e(i))
            d1=dsqrt((x3-x2)**2+(y3-y2)**2)
            d2=dsqrt((x3-x1)**2+(y3-y1)**2)
            d3=dsqrt((x2-x1)**2+(y2-y1)**2)
            cos1=(d2**2+d3**2-d1**2)/(2.0d0*d2*d3)
            cos2=(d1**2+d3**2-d2**2)/(2.0d0*d1*d3)
            cos3=(d1**2+d2**2-d3**2)/(2.0d0*d1*d2)
            if ((cos1.gt.cosmax).and.
     1             ((isf(s2e(i))+isrb(s2e(i)).eq.0).or.
     2             (isf(s3e(i))+isrb(s3e(i)).eq.0))) then
                   write(*,*) 'i,s2e,s3e=',i,s2e(i),s3e(i)
      stop 'Angle between these element sides is less than 15 degrees'
            end if
            if ((cos2.gt.cosmax).and.
     1             ((isf(s1e(i))+isrb(s1e(i)).eq.0).or.
     2             (isf(s3e(i))+isrb(s3e(i)).eq.0))) then
                   write(*,*) 'i,s1e,s3e=',i,s1e(i),s3e(i)
      stop 'Angle between these element sides is less than 15 degrees'
            end if
            if ((cos3.gt.cosmax).and.
     1             ((isf(s1e(i))+isrb(s1e(i)).eq.0).or.
     2             (isf(s2e(i))+isrb(s2e(i)).eq.0))) then
                   write(*,*) 'i,s1e,s2e=',i,s1e(i),s2e(i)
      stop 'Angle between these element sides is less than 15 degrees'
            end if
            if ((iprb(gp1e(i)).ne.0).and.
     1            (iprb(gp2e(i)).ne.0).and.
     2            (iprb(gp3e(i)).ne.0)) then
                  write(*,*) 'gp1,iprb=',gp1e(i),iprb(gp1e(i))
                  write(*,*) 'gp2,iprb=',gp2e(i),iprb(gp2e(i))
                  write(*,*) 'gp3,iprb=',gp3e(i),iprb(gp3e(i))
            write(*,*) 'All three grid points are on rigid boundaries'
            stop 'This element will have zero strain-rate capacity'
            end if
      end do

c
      write(*,*) 'Well done, the geometry and apriori model are valid'
c
      if (nvo.gt.0) then
      e=1
      do i=1,nvo
            call finde(volong(i),volat(i),e,w1vo(i),w2vo(i),w3vo(i),
     1            flag,ngp,long,lat,ns,e1s,e2s,isf,ne,gp1e,gp2e,gp3e,
     2            s1e,s2e,s3e)
            if (flag.eq.1) then
                  write(*,*) 'i,volong,volat=',i,volong(i),volat(i)
                  stop 'Velocity observation cannot be on a fault'
            end if
            if (flag.eq.2) then
                  write(*,*) 'i,volong,volat=',i,volong(i),volat(i)
                  stop 'Velocity observation is outside the grid'
            end if
            evo(i)=e
      end do
      end if
c
      if (nfo.gt.0) then
      do i=1,nfo
            if (isf(sfo(i)).eq.0) then
                  write(*,*) 'i,sfo=',i,sfo(i)
                  stop 'Side sfo is not on a fault'
            end if
            if (fs(isf(sfo(i))).ne.ffo(i)) then
                  write(*,*) 'i,ffo,sfo=',i,ffo(i),sfo(i)
                  stop 'Side sfo is not on fault ffo'
            end if
      end do
      end if
c
      if (neo.gt.0) then
      do i=1,neo
            if ((eeo(i).le.0).or.(eeo(i).gt.ne)) then
                  write(*,*) 'i,eeo=',i,eeo(i)
                  stop 'Element eeo is not in the grid'
            end if
      end do
      end if
c
      if (nduc.gt.0) then
      do i=1,nduc
            do j=1,nducs(i)
                  if (isf(sduc(j,i)).eq.0) then
                        write(*,*) 'i,j,sduc=',i,j,sduc(j,i)
                        stop 'Side sduc is not on a fault'
                  end if
                  if (fs(isf(sduc(j,i))).ne.fduc(i)) then
                        write(*,*) 'i,j,fduc,sduc=',i,j,fduc(i),
     1                        sduc(j,i)
                        stop 'Side sduc is not on fault fduc'
                  end if
            end do
            ncduc(i)=0
            do j=1,nducs(i)
                  s1=sfs(isf(sduc(j,i)))-1
                  s2=sfs(isf(sduc(j,i)))+1
                  s1c=0
                  s2c=0
                  do s=1,nducs(i)
                        if (sfs(isf(sduc(s,i))).eq.s1)
     1                        s1c=s
                        if (sfs(isf(sduc(s,i))).eq.s2)
     1                        s2c=s
                  end do
                  if (s1c+s2c.eq.0) then
                        write(*,*) 'i,j,fduc,sduc=',i,j,fduc(i),
     1                        sduc(j,i)
            stop 'Side sduc is not connected to other sides in set i'
                  end if
                  if (s1c.gt.j) then
                        ncduc(i)=ncduc(i)+1
                        s1duc(ncduc(i),i)=j
                        s2duc(ncduc(i),i)=s1c
                  end if
                  if (s2c.gt.j) then
                        ncduc(i)=ncduc(i)+1
                        s1duc(ncduc(i),i)=j
                        s2duc(ncduc(i),i)=s2c
                  end if
            end do
      end do
      end if
c
      if (nec.gt.0) then
      do i=1,nec
            do j=1,nece(i)
                  if ((eec(j,i).le.0).or.(eec(j,i).gt.ne)) then
                        write(*,*) 'i,j,eec=',i,j,eec(j,i)
                        stop 'Element eec is not in the grid'
                  end if
            end do
            ncec(i)=0
            do j=1,nece(i)
                  s1=s1e(eec(j,i))
                  s2=s2e(eec(j,i))
                  s3=s3e(eec(j,i))
                  e1c=0
                  e2c=0
                  e3c=0
                  do e=1,nece(i)
                        if ((s1e(eec(e,i)).eq.s1).or.
     1                        (s2e(eec(e,i)).eq.s1).or.
     2                        (s3e(eec(e,i)).eq.s1))
     3                        e1c=e
                        if ((s1e(eec(e,i)).eq.s2).or.
     1                        (s2e(eec(e,i)).eq.s2).or.
     2                        (s3e(eec(e,i)).eq.s2))
     3                        e2c=e
                        if ((s1e(eec(e,i)).eq.s3).or.
     1                        (s2e(eec(e,i)).eq.s3).or.
     2                        (s3e(eec(e,i)).eq.s3))
     3                        e3c=e
                  end do
                  if (e1c+e2c+e3c.eq.0) then
                        write(*,*) 'i,j,eec=',i,j,eec(j,i)
      stop 'Element eec is not connected to other elements in set i'
                  end if
                  if (e1c.gt.j) then
                        ncec(i)=ncec(i)+1
                        if (ncec(i).gt.maxece) then
                              write(*,*) 'i,ncec,maxece=',i,
     1                              ncec(i),maxece
                  stop 'Recompile with an increased maxece value'
                        end if
                        e1ec(ncec(i),i)=j
                        e2ec(ncec(i),i)=e1c
                  end if
                  if (e2c.gt.j) then
                        ncec(i)=ncec(i)+1
                        if (ncec(i).gt.maxece) then
                              write(*,*) 'i,ncec,maxece=',i,
     1                              ncec(i),maxece
                  stop 'Recompile with an increased maxece value'
                        end if
                        e1ec(ncec(i),i)=j
                        e2ec(ncec(i),i)=e2c
                  end if
                  if (e3c.gt.j) then
                        ncec(i)=ncec(i)+1
                        if (ncec(i).gt.maxece) then
                              write(*,*) 'i,ncec,maxece=',i,
     1                              ncec(i),maxece
                  stop 'Recompile with an increased maxece value'
                        end if
                        e1ec(ncec(i),i)=j
                        e2ec(ncec(i),i)=e3c
                  end if
            end do
      end do
      end if
c
      write(*,*) 'Well done again, the observational input is valid'
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
      SUBROUTINE finde(x,y,e,w1,w2,w3,flag,ngp,xgp,ygp,ns,e1s,e2s,isf,
     1      ne,gp1e,gp2e,gp3e,s1e,s2e,s3e)
c
c This attempts to find the element e containing the position (x,y),
c  starting from an initial guess. When successful the returned values
c  w1,w2,w3 are the geometrical weights, between 0 and 1 to within the
c  distance precision prec (in degrees), such that 
c     x = w1*xgp(gp1e(e))+w2*xgp(gp2e(e))+w3*xgp(gp3e(e)) and
c     y = w1*ygp(gp1e(e))+w2*ygp(gp2e(e))+w3*ygp(gp3e(e)).
c The flag returns one of three values: 0 = search results are okay,
c  1 = search successful but (x,y) is within the distance prec of a
c  fault on one of the sides of the element, and 2 = element not found
c  because the point is outside the grid.
c Initially the search is performed starting from the supplied value of
c  e and moving successively to the adjoining element that is most in
c  the direction of the point (x,y). This would always work if the grid
c  had no boundary. At boundaries the possibility exists of having to
c  back track, which isn't allowed in this routine as it could lead to
c  infinite looping.
c If the quick approach fails, all the elements are tested until one is
c  found containing the point, to within the distance precision.
c
      implicit none
      integer e,flag,ngp,ns,ne
      integer e1s(ns),e2s(ns),isf(ns)
      integer gp1e(ne),gp2e(ne),gp3e(ne),s1e(ne),s2e(ne),s3e(ne)
      integer e0,e1,s1a,s2a,s3a
      logical okay
      real*8 x,y,w1,w2,w3
      real*8 xgp(ngp),ygp(ngp)
      real*8 prec,x1,y1,x2,y2,x3,y3,xprod,d1,d2,d3,d1a,d2a,d3a
c
cf2py intent(out) w1,w2,w3,flag
cf2py intent(in,out) e
c
      prec=1.0d-6
      e0=0
10    x1=xgp(gp3e(e))-xgp(gp2e(e))
      y1=ygp(gp3e(e))-ygp(gp2e(e))
      x2=xgp(gp1e(e))-xgp(gp3e(e))
      y2=ygp(gp1e(e))-ygp(gp3e(e))
      x3=xgp(gp2e(e))-xgp(gp1e(e))
      y3=ygp(gp2e(e))-ygp(gp1e(e))
      xprod=x2*y3-y2*x3
      w1=(x1*(y-ygp(gp3e(e)))-y1*(x-xgp(gp3e(e))))/xprod
      w2=(x2*(y-ygp(gp1e(e)))-y2*(x-xgp(gp1e(e))))/xprod
      w3=(x3*(y-ygp(gp2e(e)))-y3*(x-xgp(gp2e(e))))/xprod
      d1=w1*dabs(xprod)/dsqrt(x1**2+y1**2)
      d2=w2*dabs(xprod)/dsqrt(x2**2+y2**2)
      d3=w3*dabs(xprod)/dsqrt(x3**2+y3**2)
      s1a=s1e(e)
      d1a=d1
      if (d2.lt.d1a) then
            s2a=s1a
            d2a=d1a
            s1a=s2e(e)
            d1a=d2
      else
            s2a=s2e(e)
            d2a=d2
      end if
      if (d3.lt.d1a) then
            s3a=s2a
            d3a=d2a
            s2a=s1a
            d2a=d1a
            s1a=s3e(e)
            d1a=d3
      else
            if (d3.lt.d2a) then
                  s3a=s2a
                  d3a=d2a
                  s2a=s3e(e)
                  d2a=d3
            else
                  s3a=s3e(e)
                  d3a=d3
            end if
      end if
      okay=(d1a.ge.-prec)
      if (okay) then
            okay=((isf(s1a).eq.0).or.(d1a.gt.prec)).and.
     1            ((isf(s2a).eq.0).or.(d2a.gt.prec))
            if (okay) then
                  flag=0
            else
                  flag=1
            end if
            return
      end if
      e1=e1s(s1a)
      if (e1.eq.e) e1=e2s(s1a)
      if ((e1.ne.0).and.(e1.ne.e0)) then
            e0=e
            e=e1
            goto 10
      end if
      e1=e1s(s2a)
      if (e1.eq.e) e1=e2s(s2a)
      if ((e1.ne.0).and.(e1.ne.e0)) then
            e0=e
            e=e1
            goto 10
      end if
      e1=e1s(s3a)
      if (e1.eq.e) e1=e2s(s3a)
      if ((e1.ne.0).and.(e1.ne.e0)) then
            e0=e
            e=e1
            goto 10
      end if
      do e=1,ne
            x1=xgp(gp3e(e))-xgp(gp2e(e))
            y1=ygp(gp3e(e))-ygp(gp2e(e))
            x2=xgp(gp1e(e))-xgp(gp3e(e))
            y2=ygp(gp1e(e))-ygp(gp3e(e))

            x3=xgp(gp2e(e))-xgp(gp1e(e))
            y3=ygp(gp2e(e))-ygp(gp1e(e))
            xprod=x2*y3-y2*x3
            w1=(x1*(y-ygp(gp3e(e)))-y1*(x-xgp(gp3e(e))))/xprod
            w2=(x2*(y-ygp(gp1e(e)))-y2*(x-xgp(gp1e(e))))/xprod
            w3=(x3*(y-ygp(gp2e(e)))-y3*(x-xgp(gp2e(e))))/xprod
            d1=w1*dabs(xprod)/dsqrt(x1**2+y1**2)
            d2=w2*dabs(xprod)/dsqrt(x2**2+y2**2)
            d3=w3*dabs(xprod)/dsqrt(x3**2+y3**2)
            s1a=s1e(e)
            d1a=d1
            if (d2.lt.d1a) then
                  s2a=s1a
                  d2a=d1a
                  s1a=s2e(e)
                  d1a=d2
            else
                  s2a=s2e(e)
                  d2a=d2
            end if
            if (d3.lt.d1a) then
                  s3a=s2a
                  d3a=d2a
                  s2a=s1a
                  d2a=d1a
                  s1a=s3e(e)
                  d1a=d3
            else
                  if (d3.lt.d2a) then
                        s3a=s2a
                        d3a=d2a
                        s2a=s3e(e)
                        d2a=d3
                  else
                        s3a=s3e(e)
                        d3a=d3
                  end if
            end if
            okay=(d1a.ge.-prec)
            if (okay) then
                  okay=((isf(s1a).eq.0).or.(d1a.gt.prec)).and.
     1                  ((isf(s2a).eq.0).or.(d2a.gt.prec))
                  if (okay) then
                        flag=0
                  else
                        flag=1
                  end if
                  return
            end if
      end do
      e=ne
      flag=2
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE findr(ngp,ns,ne,nf,nvl,nrb,
     1      s1e,s2e,s3e,gp1e,gp2e,gp3e,
     2      nfs,sf,gpf,nvls,gpvl,svl,fvl,nrbs,srb,gprb,
     3      npf,npvl,nprb,nsf,nsvl,nsrb,
     4      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,
     5      ntj,gptj,vls,svls,nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,
     6      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     7      nJvl,gpJvl,vl1Jvl,vl2Jvl,gp1Jvl,gp2Jvl,
     8      nfT,gpfT,f1fT,vl2fT,gp1fT,gp2fT,
     9      nTf,gpTf,vl1Tf,f2Tf,gp1Tf,gp2Tf,
     1      nTvl,gpTvl,vl1Tvl,vl2Tvl,gp1Tvl,gp2Tvl,
     1      nXf,gpXf,vl1Xf,f2Xf,gp1Xf,gp2Xf,
     2      nXvl,gpXvl,vl1Xvl,vl2Xvl,gp1Xvl,gp2Xvl,
     3      negp,nesf,negpf,nesvl,negpvl,netj,
     4      neJvl,nefT,neTf,neTvl,neXf,neXvl,
     5      nefv,iefv,ks1fv,ks2fv,ks3fv,is1fv,is2fv,is3fv,
     6      js1fv,js2fv,js3fv,rs1fv,rs2fv,rs3fv,
     7      kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     8      jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,rsrb,rgprb,
     9      long,lat,apgpf,amgpf,apgpvl,amgpvl,apgprb,amgprb,
     2      e2s,extern)
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,maxprb=maxsrb+maxrb,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl))
      integer ngp,ns,ne,nf,nvl,nrb
      integer npf,npvl,nprb,nsf,nsvl,nsrb,ntj
      integer nJvl,nfT,nTf,nTvl,nXf,nXvl,nefv
      integer i,j,ip,f1,f2,f3,gp1,gp2,gp3,vl1,vl2,f,s,gp,r,vl
      integer rgp1f,rgp2f,rgp3f,rgp1vl,rgp2vl,rgp3vl
      integer tj,Jvl,fT,Tf,Tvl,Xf,Xvl,is,jp
      integer e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf),sf(maxfs,maxf)
      integer gpf(0:maxfs,maxf)
      integer nvlgp(maxvl),nvls(maxvl),nvlf(maxvl)
      integer gpvl(0:maxvls,maxvl),svl(maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),srb(maxrbs,maxrb)
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer nfp(maxpf),fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf),gptj(maxtj)
      integer vls(maxsvl),svls(maxsvl)
      integer nvlp(maxpvl),vlp(2,maxpvl),gpvlp(2,maxpvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer rbp(maxprb),gprbp(maxprb)

      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer gpJvl(maxJvl),vl1Jvl(maxJvl),vl2Jvl(maxJvl),
     1      gp1Jvl(maxJvl),gp2Jvl(maxJvl)
      integer gpfT(maxfT),f1fT(maxfT),vl2fT(maxfT),
     1      gp1fT(maxfT),gp2fT(maxfT)
      integer gpTf(maxTf),vl1Tf(maxTf),f2Tf(maxTf),
     1      gp1Tf(maxTf),gp2Tf(maxTf)
      integer gpTvl(maxTvl),vl1Tvl(maxTvl),vl2Tvl(maxTvl),
     1      gp1Tvl(maxTvl),gp2Tvl(maxTvl)
      integer gpXf(maxXf),vl1Xf(maxXf),f2Xf(maxXf),
     1      gp1Xf(maxXf),gp2Xf(maxXf)
      integer gpXvl(maxXvl),vl1Xvl(maxXvl),vl2Xvl(maxXvl),
     1      gp1Xvl(maxXvl),gp2Xvl(maxXvl)
      integer negp(maxgp)
      integer nesf(0:1,maxfs,maxf),negpf(0:1,0:maxfs,maxf)
      integer nesvl(0:1,maxvls,maxvl),negpvl(0:1,0:maxvls,maxvl)
      integer netj(0:2,maxtj)
      integer neJvl(0:3,maxJvl),nefT(0:3,maxfT),neTf(0:3,maxTf),
     1      neTvl(0:3,maxTvl),neXf(0:3,maxXf),neXvl(0:3,maxXvl)
      integer iefv(maxe)
      integer ks1fv(maxefv),ks2fv(maxefv),ks3fv(maxefv)
      integer is1fv(maxefv),is2fv(maxefv),is3fv(maxefv)
      integer js1fv(maxefv),js2fv(maxefv),js3fv(maxefv)
      integer rs1fv(maxefv),rs2fv(maxefv),rs3fv(maxefv)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rsrb(maxrbs,maxrb),rgprb(0:maxrbs,maxrb)
      logical fvline,online,wanted,found
      logical extern(maxgp)
      real*8 a1,a2,a3,pi,prec,dx,dy,as,agp1,agp2
      real*8 long(maxgp),lat(maxgp)
      real*8 apgpf(0:maxfs,maxf),amgpf(0:maxfs,maxf)
      real*8 apgpvl(0:maxvls,maxvl),amgpvl(0:maxvls,maxvl)
      real*8 apgprb(0:maxrbs,maxrb),amgprb(0:maxrbs,maxrb)
c
c This rountine finds the regions at complex grid points and lines that
c  elements belong to, and finishes by doing the same for rigid 
c  boundaries at those points and lines.
c For each element in the set with complex points the indices ks1fv,
c  ks2fv, ks3fv, kgp1fv, kgp2fv, kgp3fv indicate the type of complexity
c  at the sides and corner points of the element:
c    0 = ordinary side or point,
c    1 = fault,
c    2 = velocity line,
c    3 = triple junction of faults,
c    4 = point where ends of two velocity lines are joined,
c    5 = point where a velocity line truncates internally at a fault,
c    6 = point where a fault truncates externally at a velocity line,
c    7 = point where a velocity line truncates at another velocity line,
c    8 = point where a velocity line is crossed by a fault,
c    9 = point where two velocity lines cross.
c In cases 6 and 8 a rigid boundary at the point counts as the extension
c  of the velocity line, given that only ends of velocity lines can be
c  at rigid boundaries. Other instances where a rigid boundary can be
c  present in a complex case are 1, 3 and 4.
c Points are treated as being ordinary at zero-slip ends of faults, at
c  isolated ends of velocity lines, and at rigid boundaries, except for
c  cases 1, 3, 4, 6 and 8.
c Sides can be only of type 0, 1 or 2.
c
c The convention for the numbering of regions is that the positive side
c  of a line (to the right when looking from the end of the line to the
c  beginning of the line) is 0 and the negative side is 1.
c For cases 4, 5, 6, 7, 8 and 9 involving two lines the numbering is
c  the number for the first line plus 2 times the number for the second
c  line:
c    0 = (0,0) = (+,+),
c    1 = (1,0) = (-,+),
c    2 = (0,1) = (+,-),
c    3 = (1,1) = (-,-).
c For triple junctions of faults region 0 is between faults 1 and 2,
c  region 1 is between faults 2 and 3, and region 2 is between faults 3
c  and 1. The faults are reordered in this routine so that the fault
c  directions from the triple junction satisfy a3 < a2 < a1. This
c  results in the correspondence between regions and the positive and
c  negative sides of each fault being as follows:
c for fault 1 [+,-] = [0,2] if the fault begins at the triple junction,
c for fault 2 [+,-] = [1,0] if the fault begins at the triple junction,
c for fault 3 [+,-] = [2,1] if the fault begins at the triple junction,
c for fault 1 [+,-] = [2,0] if the fault ends at the triple junction,
c for fault 2 [+,-] = [0,1] if the fault ends at the triple junction,
c for fault 3 [+,-] = [1,2] if the fault ends at the triple junction.
c
c The arrays negp, nesf, negpf, nesvl, negpvl, netj, neJvl, nefT, neTf,
c  neTvl, neXf, neXvl record the number of elements in each region for
c  each type of point or side. For sides of faults and velocity lines
c  there will be 1 element on each side of the line, except at external
c  boundaries, where there is only one element.
c
c The angles apgpf, amgpf, apgpvl, amgpvl, apgprb, amgprb indicate the
c  positive (towards the end) and negative (towards the beginning)
c  directions at each grid point on a line. They were calculated in
c  the previous subroutine and are used here in identifying the regions.
c In the previous subroutine the negative direction values at the
c  beginning of lines are set equal to the positive direction values
c  there and the positive direction values at the end of lines are set
c  equal to the negative direction values. This is for convenience, so
c  that either value can be used, which makes coding simpler for case 3
c  triple junctions.
c
c For instances where the negative-direction angle amgp* is less than
c  the positive-direction angle amgp*, the positive side of the line is
c  at angles between amgp* and apgp*, whereas the negative side is at
c  angles either less than amgp* or greater than apgp*.
c Conversely, where the negative-direction angle amgp* is greater than
c  the positive-direction angle apgp*, the negative side of the line is
c  at angles between apgp* and amgp*, whereas the positive side is at
c  angles either greater than amgp* or less than apgp*.
c
c Here for cases 4, 5, 6 and 7 in general, and for case 8 at an external
c  boundary (which is allowed only if the external line is a rigid
c  boundary), the other line is used to provide a proper continuation
c  direction at the end of each line that is truncating.
c For instances of 5, 6 and 7 without rigid boundaries and external
c  instances of 8 there is a choice of two directions on the other line,
c  and one of the four regions (+,+), (-,+), (+,-) and (-,-) does not
c  exist, either inside or outside the grid. The convention adopted is
c  that the opposite side of the other line is part of the positive side
c  of the line that is truncating. Consequently, the region that doesn't
c  exist is one of the of the two involving the negative side of the
c  line that is truncating, and in all instances the other region
c  involving the negative side of the line that is truncating will be
c  populated by elements - that is, the region that doesn't exist is
c  easily identified.
c Other instances where regions don't exist are case 4 in general and
c  case 6 at a rigid boundary. In these cases at most two regions can be
c  populated by elements and they come in pairs: (+,+) and (-,-) if the
c  end of one line is the beginning of the other, and (-,+) and (+,-) if
c  the truncations of both lines are the same (i.e. both beginning or
c  both ending). Again, it is easy to identify which pair of regions
c  doesn't exist from which regions are populated by elements.
c
c In some of the multitude of cases tested for below two angles can be
c  equal. To allow for rounding error in calculating the angles a small

c  adjustment prec is added or subtracted, as appropriate. The preset
c  value for prec is 1.0d-12. There shouldn't be any need for a larger
c  value. The way the code is written is such that a good compiler will
c  always give an equal value each time equal angles are calculated,
c  meaning that prec could be set to zero. There are checks below
c  indicating if there are any clear problems with the value of prec,
c  and rather than recompiling with a different value the sensible
c  option would be to find a better compiler.
c
      do i=1,ngp
            negp(i)=0
      end do
      if (nf.gt.0) then
      do i=1,nf
            do j=1,nfs(i)
                  nesf(0,j,i)=0
                  nesf(1,j,i)=0
            end do
            do j=0,nfs(i)
                  negpf(0,j,i)=0
                  negpf(1,j,i)=0
            end do
      end do
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            do j=1,nvls(i)
                  nesvl(0,j,i)=0
                  nesvl(1,j,i)=0
            end do
            do j=0,nvls(i)
                  negpvl(0,j,i)=0
                  negpvl(1,j,i)=0
            end do
      end do
      end if
      if (ntj.gt.0) then
      do i=1,ntj
            ip=ipf(gptj(i))
            f1=fp(1,ip)
            f2=fp(2,ip)
            f3=fp(3,ip)
            gp1=gpfp(1,ip)
            gp2=gpfp(2,ip)
            gp3=gpfp(3,ip)
            a1=apgpf(gp1,f1)
            a2=apgpf(gp2,f2)
            a3=apgpf(gp3,f3)
            if (a1.lt.a3) then
                  fp(1,ip)=f3
                  fp(3,ip)=f1
                  gpfp(1,ip)=gp3
                  gpfp(3,ip)=gp1
                  f1=fp(1,ip)
                  f3=fp(3,ip)
                  gp1=gpfp(1,ip)
                  gp3=gpfp(3,ip)
                  a1=apgpf(gp1,f1)
                  a3=apgpf(gp3,f3)
            end if
            if (a1.lt.a2) then
                  fp(1,ip)=f2
                  fp(2,ip)=f1
                  gpfp(1,ip)=gp2
                  gpfp(2,ip)=gp1
            end if
            if (a2.lt.a3) then
                  fp(2,ip)=f3
                  fp(3,ip)=f2
                  gpfp(2,ip)=gp3
                  gpfp(3,ip)=gp2
            end if
            netj(0,i)=0
            netj(1,i)=0
            netj(2,i)=0
      end do
      end if
      if (nJvl.gt.0) then
      do i=1,nJvl
            vl1=vl1Jvl(i)
            vl2=vl2Jvl(i)
            gp1=gp1Jvl(i)
            gp2=gp2Jvl(i)
            if ((gp1.eq.0).and.(gp2.eq.0)) then
                  amgpvl(gp1,vl1)=apgpvl(gp2,vl2)
                  amgpvl(gp2,vl2)=apgpvl(gp1,vl1)
            end if
            if ((gp1.gt.0).and.(gp2.eq.0)) then
                  apgpvl(gp1,vl1)=apgpvl(gp2,vl2)
                  amgpvl(gp2,vl2)=amgpvl(gp1,vl1)
            end if
            if ((gp1.eq.0).and.(gp2.gt.0)) then
                  amgpvl(gp1,vl1)=amgpvl(gp2,vl2)
                  apgpvl(gp2,vl2)=apgpvl(gp1,vl1)
            end if
            if ((gp1.gt.0).and.(gp2.gt.0)) then
                  apgpvl(gp1,vl1)=amgpvl(gp2,vl2)
                  apgpvl(gp2,vl2)=amgpvl(gp1,vl1)
            end if
            neJvl(0,i)=0
            neJvl(1,i)=0
            neJvl(2,i)=0
            neJvl(3,i)=0
      end do
      end if
      if (nfT.gt.0) then
      do i=1,nfT
            f1=f1fT(i)
            vl2=vl2fT(i)
            gp1=gp1fT(i)
            gp2=gp2fT(i)
            if (amgpf(gp1,f1).lt.apgpf(gp1,f1)) then
                  a1=amgpf(gp1,f1)
                  a2=apgpf(gp1,f1)
            else
                  a1=apgpf(gp1,f1)
                  a2=amgpf(gp1,f1)
            end if
            if (gp2.eq.0) then
                  a3=apgpvl(gp2,vl2)
                  if ((a3.lt.a1).or.(a3.gt.a2)) then
                        amgpvl(gp2,vl2)=a1
                  else
                        amgpvl(gp2,vl2)=a2
                  end if
            end if
            if (gp2.gt.0) then
                  a3=amgpvl(gp2,vl2)
                  if ((a3.lt.a1).or.(a3.gt.a2)) then
                        apgpvl(gp2,vl2)=a2
                  else
                        apgpvl(gp2,vl2)=a1
                  end if
            end if
            nefT(0,i)=0
            nefT(1,i)=0
            nefT(2,i)=0
            nefT(3,i)=0
      end do
      end if
      if (nTf.gt.0) then
      do i=1,nTf
            vl1=vl1Tf(i)
            f2=f2Tf(i)
            gp1=gp1Tf(i)
            gp2=gp2Tf(i)
            if (iprb(gpTf(i)).ne.0) then
                  if ((gp1.eq.0).and.(gp2.eq.0)) then
                        amgpvl(gp1,vl1)=apgpf(gp2,f2)
                        amgpf(gp2,f2)=apgpvl(gp1,vl1)
                  end if
                  if ((gp1.gt.0).and.(gp2.eq.0)) then
                        apgpvl(gp1,vl1)=apgpf(gp2,f2)
                        amgpf(gp2,f2)=amgpvl(gp1,vl1)
                  end if
                  if ((gp1.eq.0).and.(gp2.gt.0)) then
                        amgpvl(gp1,vl1)=amgpf(gp2,f2)
                        apgpf(gp2,f2)=apgpvl(gp1,vl1)
                  end if
                  if ((gp1.gt.0).and.(gp2.gt.0)) then
                        apgpvl(gp1,vl1)=amgpf(gp2,f2)
                        apgpf(gp2,f2)=amgpvl(gp1,vl1)
                  end if
            end if
            if (iprb(gpTf(i)).eq.0) then
                  if (amgpvl(gp1,vl1).lt.apgpvl(gp1,vl1)) then
                        a1=amgpvl(gp1,vl1)
                        a2=apgpvl(gp1,vl1)
                  else
                        a1=apgpvl(gp1,vl1)
                        a2=amgpvl(gp1,vl1)
                  end if
                  if (gp2.eq.0) then
                        a3=apgpf(gp2,f2)
                        if ((a3.lt.a1).or.(a3.gt.a2)) then
                              amgpf(gp2,f2)=a1
                        else
                              amgpf(gp2,f2)=a2
                        end if
                  end if
                  if (gp2.gt.0) then
                        a3=amgpf(gp2,f2)
                        if ((a3.lt.a1).or.(a3.gt.a2)) then
                              apgpf(gp2,f2)=a2
                        else
                              apgpf(gp2,f2)=a1
                        end if
                  end if
            end if
            neTf(0,i)=0
            neTf(1,i)=0
            neTf(2,i)=0
            neTf(3,i)=0
      end do
      end if
      if (nTvl.gt.0) then
      do i=1,nTvl
            vl1=vl1Tvl(i)
            vl2=vl2Tvl(i)
            gp1=gp1Tvl(i)
            gp2=gp2Tvl(i)
            if (amgpvl(gp1,vl1).lt.apgpvl(gp1,vl1)) then
                  a1=amgpvl(gp1,vl1)
                  a2=apgpvl(gp1,vl1)
            else
                  a1=apgpvl(gp1,vl1)
                  a2=amgpvl(gp1,vl1)
            end if
            if (gp2.eq.0) then
                  a3=apgpvl(gp2,vl2)
                  if ((a3.lt.a1).or.(a3.gt.a2)) then
                        amgpvl(gp2,vl2)=a1
                  else
                        amgpvl(gp2,vl2)=a2
                  end if
            end if
            if (gp2.gt.0) then
                  a3=amgpvl(gp2,vl2)
                  if ((a3.lt.a1).or.(a3.gt.a2)) then
                        apgpvl(gp2,vl2)=a2
                  else
                        apgpvl(gp2,vl2)=a1
                  end if
            end if
            neTvl(0,i)=0
            neTvl(1,i)=0
            neTvl(2,i)=0
            neTvl(3,i)=0
      end do
      end if
      if (nXf.gt.0) then
      do i=1,nXf
            if (iprb(gpXf(i)).ne.0) then
                  vl1=vl1Xf(i)
                  f2=f2Xf(i)
                  gp1=gp1Xf(i)
                  gp2=gp2Xf(i)
                  if (amgpf(gp2,f2).lt.apgpf(gp2,f2)) then
                        a1=amgpf(gp2,f2)
                        a2=apgpf(gp2,f2)
                  else
                        a1=apgpf(gp2,f2)
                        a2=amgpf(gp2,f2)
                  end if
                  if (gp1.eq.0) then
                        a3=apgpvl(gp1,vl1)

                        if ((a3.lt.a1).or.(a3.gt.a2)) then
                              amgpvl(gp1,vl1)=a1
                        else
                              amgpvl(gp1,vl1)=a2
                        end if
                  end if
                  if (gp1.gt.0) then
                        a3=amgpvl(gp1,vl1)
                        if ((a3.lt.a1).or.(a3.gt.a2)) then
                              apgpvl(gp1,vl1)=a2

                        else
                              apgpvl(gp1,vl1)=a1
                        end if
                  end if
            end if
            neXf(0,i)=0
            neXf(1,i)=0
            neXf(2,i)=0
            neXf(3,i)=0
      end do
      end if
      if (nXvl.gt.0) then
      do i=1,nXvl
            neXvl(0,i)=0
            neXvl(1,i)=0
            neXvl(2,i)=0
            neXvl(3,i)=0
      end do
      end if
c
      pi=4.0d0*datan(1.0d0)
      prec=1.0d-12
      nefv=0
      do i=1,ne
            negp(gp1e(i))=negp(gp1e(i))+1
            negp(gp2e(i))=negp(gp2e(i))+1
            negp(gp3e(i))=negp(gp3e(i))+1
            iefv(i)=0
            fvline=.false.
c
            if (ipf(gp1e(i)).ne.0) then
                  ip=ipf(gp1e(i))
                  if (jfp(ip).ne.0) fvline=.true.
                  if (tjfp(ip).ne.0) fvline=.true.
            end if
            if (ipvl(gp1e(i)).ne.0) then
                  ip=ipvl(gp1e(i))
                  if ((gpvlp(1,ip).gt.0).and.
     1                  (gpvlp(1,ip).lt.nvls(vlp(1,ip))))
     2                  fvline=.true.
                  if (Jvlvlp(ip).ne.0) fvline=.true.
                  if (fTvlp(ip).ne.0) fvline=.true.
                  if (Tfvlp(ip).ne.0) fvline=.true.
                  if (Tvlvlp(ip).ne.0) fvline=.true.
                  if (Xfvlp(ip).ne.0) fvline=.true.
                  if (Xvlvlp(ip).ne.0) fvline=.true.
            end if
            if (ipf(gp2e(i)).ne.0) then
                  ip=ipf(gp2e(i))
                  if (jfp(ip).ne.0) fvline=.true.
                  if (tjfp(ip).ne.0) fvline=.true.
            end if
            if (ipvl(gp2e(i)).ne.0) then
                  ip=ipvl(gp2e(i))
                  if ((gpvlp(1,ip).gt.0).and.
     1                  (gpvlp(1,ip).lt.nvls(vlp(1,ip))))
     2                  fvline=.true.
                  if (Jvlvlp(ip).ne.0) fvline=.true.
                  if (fTvlp(ip).ne.0) fvline=.true.
                  if (Tfvlp(ip).ne.0) fvline=.true.
                  if (Tvlvlp(ip).ne.0) fvline=.true.
                  if (Xfvlp(ip).ne.0) fvline=.true.
                  if (Xvlvlp(ip).ne.0) fvline=.true.
            end if
            if (ipf(gp3e(i)).ne.0) then
                  ip=ipf(gp3e(i))
                  if (jfp(ip).ne.0) fvline=.true.
                  if (tjfp(ip).ne.0) fvline=.true.
            end if
            if (ipvl(gp3e(i)).ne.0) then
                  ip=ipvl(gp3e(i))
                  if ((gpvlp(1,ip).gt.0).and.
     1                  (gpvlp(1,ip).lt.nvls(vlp(1,ip))))
     2                  fvline=.true.
                  if (Jvlvlp(ip).ne.0) fvline=.true.
                  if (fTvlp(ip).ne.0) fvline=.true.
                  if (Tfvlp(ip).ne.0) fvline=.true.
                  if (Tvlvlp(ip).ne.0) fvline=.true.
                  if (Xfvlp(ip).ne.0) fvline=.true.
                  if (Xvlvlp(ip).ne.0) fvline=.true.
            end if
c
            if (fvline) then
                  nefv=nefv+1
                  if (nefv.gt.maxefv) then
                        write(*,*) 'nefv,maxefv=',nefv,maxefv
                  stop 'Recompile with an increased maxefv value'
                  end if
                  iefv(i)=nefv
                  ks1fv(nefv)=0
                  ks2fv(nefv)=0
                  ks3fv(nefv)=0
                  rs1fv(nefv)=0
                  rs2fv(nefv)=0
                  rs3fv(nefv)=0
                  kgp1fv(nefv)=0
                  kgp2fv(nefv)=0
                  kgp3fv(nefv)=0
                  rgp1fv(nefv)=0
                  rgp2fv(nefv)=0
                  rgp3fv(nefv)=0
c
c Sides on faults
c
                  if (isf(s1e(i)).ne.0) then
                        ks1fv(nefv)=1
                        is=isf(s1e(i))
                        f=fs(is)
                        s=sfs(is)
                        is1fv(nefv)=f
                        js1fv(nefv)=s
                        if (s.eq.nfs(f)) then
                              j=s-1
                              online=gpf(j-1,f).eq.gp1e(i)
                        else
                              j=s
                              online=gpf(j+1,f).eq.gp1e(i)
                        end if 
                        a1=amgpf(j,f)
                        a2=apgpf(j,f)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs1fv(nefv)=1
                                    else
                                          rs1fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs1fv(nefv)=0
                                    else
                                          rs1fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpf(j,f)
                              dx=long(gp1e(i))-long(gp)
                              dy=lat(gp1e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs1fv(nefv)=1
                                    else
                                          rs1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rs1fv(nefv)=0
                                    else
                                          rs1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs1fv(nefv)
                        nesf(r,s,f)=nesf(r,s,f)+1
                  end if
c
                  if (isf(s2e(i)).ne.0) then
                        ks2fv(nefv)=1
                        is=isf(s2e(i))
                        f=fs(is)
                        s=sfs(is)
                        is2fv(nefv)=f
                        js2fv(nefv)=s
                        if (s.eq.nfs(f)) then
                              j=s-1
                              online=gpf(j-1,f).eq.gp2e(i)
                        else
                              j=s
                              online=gpf(j+1,f).eq.gp2e(i)
                        end if 
                        a1=amgpf(j,f)
                        a2=apgpf(j,f)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs2fv(nefv)=1
                                    else
                                          rs2fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs2fv(nefv)=0
                                    else
                                          rs2fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpf(j,f)
                              dx=long(gp2e(i))-long(gp)
                              dy=lat(gp2e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs2fv(nefv)=1
                                    else
                                          rs2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rs2fv(nefv)=0
                                    else
                                          rs2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs2fv(nefv)
                        nesf(r,s,f)=nesf(r,s,f)+1
                  end if
c
                  if (isf(s3e(i)).ne.0) then
                        ks3fv(nefv)=1
                        is=isf(s3e(i))
                        f=fs(is)
                        s=sfs(is)
                        is3fv(nefv)=f
                        js3fv(nefv)=s
                        if (s.eq.nfs(f)) then
                              j=s-1
                              online=gpf(j-1,f).eq.gp3e(i)
                        else
                              j=s
                              online=gpf(j+1,f).eq.gp3e(i)
                        end if 
                        a1=amgpf(j,f)
                        a2=apgpf(j,f)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs3fv(nefv)=1
                                    else
                                          rs3fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs3fv(nefv)=0
                                    else
                                          rs3fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpf(j,f)
                              dx=long(gp3e(i))-long(gp)
                              dy=lat(gp3e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs3fv(nefv)=1
                                    else
                                          rs3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rs3fv(nefv)=0
                                    else
                                          rs3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs3fv(nefv)
                        nesf(r,s,f)=nesf(r,s,f)+1
                  end if
c
c Sides on velocity lines
c
                  if (isvl(s1e(i)).ne.0) then
                        ks1fv(nefv)=2
                        is=isvl(s1e(i))
                        vl=vls(is)
                        s=svls(is)
                        is1fv(nefv)=vl
                        js1fv(nefv)=s
                        if (s.eq.nvls(vl)) then
                              j=s-1
                              online=gpvl(j-1,vl).eq.gp1e(i)
                        else
                              j=s
                              online=gpvl(j+1,vl).eq.gp1e(i)
                        end if 
                        a1=amgpvl(j,vl)
                        a2=apgpvl(j,vl)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs1fv(nefv)=1
                                    else
                                          rs1fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs1fv(nefv)=0
                                    else
                                          rs1fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpvl(j,vl)
                              dx=long(gp1e(i))-long(gp)
                              dy=lat(gp1e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs1fv(nefv)=1
                                    else
                                          rs1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rs1fv(nefv)=0
                                    else
                                          rs1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs1fv(nefv)
                        nesvl(r,s,vl)=nesvl(r,s,vl)+1
                  end if
c
                  if (isvl(s2e(i)).ne.0) then
                        ks2fv(nefv)=2
                        is=isvl(s2e(i))
                        vl=vls(is)
                        s=svls(is)
                        is2fv(nefv)=vl
                        js2fv(nefv)=s
                        if (s.eq.nvls(vl)) then
                              j=s-1
                              online=gpvl(j-1,vl).eq.gp2e(i)
                        else
                              j=s
                              online=gpvl(j+1,vl).eq.gp2e(i)
                        end if 
                        a1=amgpvl(j,vl)
                        a2=apgpvl(j,vl)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs2fv(nefv)=1
                                    else
                                          rs2fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs2fv(nefv)=0
                                    else
                                          rs2fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpvl(j,vl)
                              dx=long(gp2e(i))-long(gp)
                              dy=lat(gp2e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs2fv(nefv)=1
                                    else
                                          rs2fv(nefv)=0
                                    end if
                              else

                                    if ((a3.lt.a2).or.

     1                                    (a3.gt.a1)) then
                                          rs2fv(nefv)=0
                                    else
                                          rs2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs2fv(nefv)
                        nesvl(r,s,vl)=nesvl(r,s,vl)+1
                  end if
c
                  if (isvl(s3e(i)).ne.0) then
                        ks3fv(nefv)=2
                        is=isvl(s3e(i))
                        vl=vls(is)
                        s=svls(is)
                        is3fv(nefv)=vl
                        js3fv(nefv)=s
                        if (s.eq.nvls(vl)) then
                              j=s-1
                              online=gpvl(j-1,vl).eq.gp3e(i)
                        else
                              j=s
                              online=gpvl(j+1,vl).eq.gp3e(i)
                        end if 
                        a1=amgpvl(j,vl)
                        a2=apgpvl(j,vl)
                        if (online) then
                              if (a1.lt.a2) then
                                    if (a1.lt.a2-pi) then
                                          rs3fv(nefv)=1
                                    else
                                          rs3fv(nefv)=0
                                    end if
                              else
                                    if (a1.gt.a2+pi) then
                                          rs3fv(nefv)=0
                                    else
                                          rs3fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (.not.online) then
                              gp=gpvl(j,vl)
                              dx=long(gp3e(i))-long(gp)
                              dy=lat(gp3e(i))-lat(gp)
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rs3fv(nefv)=1
                                    else
                                          rs3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rs3fv(nefv)=0
                                    else
                                          rs3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rs3fv(nefv)
                        nesvl(r,s,vl)=nesvl(r,s,vl)+1
                  end if
c
c Grid points on faults (including v-line cases but not at zero-slip
c   ends or triple junctions)
c
                  wanted=.false.
                  if (ipf(gp1e(i)).ne.0) then
                        ip=ipf(gp1e(i))
                        wanted=(jfp(ip).ne.0).and.
     1                        (tjfp(ip).eq.0)
                  end if
                  if (wanted) then
                        kgp1fv(nefv)=1
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        igp1fv(nefv)=f
                        jgp1fv(nefv)=j
                        online=.false.
                        if (ks2fv(nefv).eq.1) then
                              if (is2fv(nefv).eq.f) then
                                    rgp1fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks3fv(nefv).eq.1) then
                              if (is3fv(nefv).eq.f) then
                                    rgp1fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpf(j,f)
                              a2=apgpf(j,f)
                              dx=long(gp2e(i))+long(gp3e(i))
     1                              -2.0d0*long(gp1e(i))
                              dy=lat(gp2e(i))+lat(gp3e(i))
     1                              -2.0d0*lat(gp1e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp1fv(nefv)=1
                                    else
                                          rgp1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp1fv(nefv)=0
                                    else
                                          rgp1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp1fv(nefv)
                        if (ipvl(gp1e(i)).eq.0)
     1                        negpf(r,j,f)=negpf(r,j,f)+1
                        rgp1f=r
                        negp(gp1e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipf(gp2e(i)).ne.0) then
                        ip=ipf(gp2e(i))
                        wanted=(jfp(ip).ne.0).and.
     1                        (tjfp(ip).eq.0)
                  end if
                  if (wanted) then
                        kgp2fv(nefv)=1
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        igp2fv(nefv)=f
                        jgp2fv(nefv)=j
                        online=.false.
                        if (ks3fv(nefv).eq.1) then
                              if (is3fv(nefv).eq.f) then
                                    rgp2fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks1fv(nefv).eq.1) then
                              if (is1fv(nefv).eq.f) then
                                    rgp2fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpf(j,f)
                              a2=apgpf(j,f)
                              dx=long(gp3e(i))+long(gp1e(i))
     1                              -2.0d0*long(gp2e(i))
                              dy=lat(gp3e(i))+lat(gp1e(i))
     1                              -2.0d0*lat(gp2e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp2fv(nefv)=1
                                    else
                                          rgp2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp2fv(nefv)=0
                                    else
                                          rgp2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp2fv(nefv)
                        if (ipvl(gp2e(i)).eq.0)
     1                        negpf(r,j,f)=negpf(r,j,f)+1
                        rgp2f=r
                        negp(gp2e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipf(gp3e(i)).ne.0) then
                        ip=ipf(gp3e(i))
                        wanted=(jfp(ip).ne.0).and.
     1                        (tjfp(ip).eq.0)
                  end if
                  if (wanted) then
                        kgp3fv(nefv)=1
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        igp3fv(nefv)=f
                        jgp3fv(nefv)=j
                        online=.false.
                        if (ks1fv(nefv).eq.1) then
                              if (is1fv(nefv).eq.f) then
                                    rgp3fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks2fv(nefv).eq.1) then
                              if (is2fv(nefv).eq.f) then
                                    rgp3fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpf(j,f)
                              a2=apgpf(j,f)
                              dx=long(gp1e(i))+long(gp2e(i))
     1                              -2.0d0*long(gp3e(i))
                              dy=lat(gp1e(i))+lat(gp2e(i))
     1                              -2.0d0*lat(gp3e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp3fv(nefv)=1
                                    else
                                          rgp3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp3fv(nefv)=0
                                    else
                                          rgp3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp3fv(nefv)
                        if (ipvl(gp3e(i)).eq.0)
     1                        negpf(r,j,f)=negpf(r,j,f)+1
                        rgp3f=r
                        negp(gp3e(i))=0
                  end if
c
c Grid points on velocity lines (including multiple-line cases but not
c   single-velocity-line terminations)
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        wanted=((j.gt.0).and.(j.lt.nvls(vl))).or.
     1                        (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip)
     3                        .ne.0)
                  end if
                  if (wanted) then
                        kgp1fv(nefv)=2
                        igp1fv(nefv)=vl
                        jgp1fv(nefv)=j
                        online=.false.
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp2e(i))+long(gp3e(i))
     1                              -2.0d0*long(gp1e(i))
                              dy=lat(gp2e(i))+lat(gp3e(i))
     1                              -2.0d0*lat(gp1e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp1fv(nefv)=1
                                    else
                                          rgp1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp1fv(nefv)=0
                                    else
                                          rgp1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp1fv(nefv)
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        negpvl(r,j,vl)=negpvl(r,j,vl)+1
                        rgp1vl=r
                        negp(gp1e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        wanted=((j.gt.0).and.(j.lt.nvls(vl))).or.
     1                        (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip)
     3                        .ne.0)
                  end if
                  if (wanted) then
                        kgp2fv(nefv)=2
                        igp2fv(nefv)=vl
                        jgp2fv(nefv)=j
                        online=.false.
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp3e(i))+long(gp1e(i))
     1                              -2.0d0*long(gp2e(i))
                              dy=lat(gp3e(i))+lat(gp1e(i))
     1                              -2.0d0*lat(gp2e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp2fv(nefv)=1
                                    else
                                          rgp2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp2fv(nefv)=0
                                    else
                                          rgp2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp2fv(nefv)
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        negpvl(r,j,vl)=negpvl(r,j,vl)+1
                        rgp2vl=r
                        negp(gp2e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        wanted=((j.gt.0).and.(j.lt.nvls(vl))).or.
     1                        (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip)
     3                        .ne.0)
                  end if
                  if (wanted) then
                        kgp3fv(nefv)=2
                        igp3fv(nefv)=vl
                        jgp3fv(nefv)=j
                        online=.false.
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp1e(i))+long(gp2e(i))
     1                              -2.0d0*long(gp3e(i))
                              dy=lat(gp1e(i))+lat(gp2e(i))
     1                              -2.0d0*lat(gp3e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp3fv(nefv)=1
                                    else
                                          rgp3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp3fv(nefv)=0
                                    else
                                          rgp3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp3fv(nefv)
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        negpvl(r,j,vl)=negpvl(r,j,vl)+1
                        rgp3vl=r
                        negp(gp3e(i))=0
                  end if
c
c Grid points at junctions of three faults
c
                  wanted=.false.
                  if (ipf(gp1e(i)).ne.0) then
                        ip=ipf(gp1e(i))
                        wanted=(tjfp(ip).ne.0)
                  end if
                  if (wanted) then
                        tj=tjfp(ip)
                        kgp1fv(nefv)=3
                        igp1fv(nefv)=tj
                        f1=fp(1,ip)
                        f2=fp(2,ip)
                        f3=fp(3,ip)
                        gp1=gpfp(1,ip)
                        gp2=gpfp(2,ip)
                        gp3=gpfp(3,ip)
                        a1=apgpf(gp1,f1)
                        a2=apgpf(gp2,f2)
                        a3=apgpf(gp3,f3)
                        dx=long(gp2e(i))+long(gp3e(i))
     1                        -2.0d0*long(gp1e(i))
                        dy=lat(gp2e(i))+lat(gp3e(i))
     1                        -2.0d0*lat(gp1e(i))
                        as=datan2(dx,dy)
                        if ((a2.lt.as).and.(as.le.a1))
     1                        rgp1fv(nefv)=0
                        if ((a3.lt.as).and.(as.le.a2))
     1                        rgp1fv(nefv)=1
                        if ((a1.lt.as).or.(as.le.a3))
     1                        rgp1fv(nefv)=2
                        r=rgp1fv(nefv)
                        netj(r,tj)=netj(r,tj)+1
                        negp(gp1e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipf(gp2e(i)).ne.0) then
                        ip=ipf(gp2e(i))
                        wanted=(tjfp(ip).ne.0)
                  end if
                  if (wanted) then
                        tj=tjfp(ip)
                        kgp2fv(nefv)=3
                        igp2fv(nefv)=tj
                        f1=fp(1,ip)
                        f2=fp(2,ip)
                        f3=fp(3,ip)
                        gp1=gpfp(1,ip)
                        gp2=gpfp(2,ip)
                        gp3=gpfp(3,ip)
                        a1=apgpf(gp1,f1)
                        a2=apgpf(gp2,f2)
                        a3=apgpf(gp3,f3)
                        dx=long(gp3e(i))+long(gp1e(i))
     1                        -2.0d0*long(gp2e(i))
                        dy=lat(gp3e(i))+lat(gp1e(i))
     1                        -2.0d0*lat(gp2e(i))
                        as=datan2(dx,dy)
                        if ((a2.lt.as).and.(as.le.a1))
     1                        rgp2fv(nefv)=0
                        if ((a3.lt.as).and.(as.le.a2))
     1                        rgp2fv(nefv)=1
                        if ((a1.lt.as).or.(as.le.a3))
     1                        rgp2fv(nefv)=2
                        r=rgp2fv(nefv)
                        netj(r,tj)=netj(r,tj)+1
                        negp(gp2e(i))=0
                  end if
c
                  wanted=.false.
                  if (ipf(gp3e(i)).ne.0) then
                        ip=ipf(gp3e(i))
                        wanted=(tjfp(ip).ne.0)
                  end if
                  if (wanted) then
                        tj=tjfp(ip)
                        kgp3fv(nefv)=3
                        igp3fv(nefv)=tj
                        f1=fp(1,ip)
                        f2=fp(2,ip)
                        f3=fp(3,ip)
                        gp1=gpfp(1,ip)
                        gp2=gpfp(2,ip)
                        gp3=gpfp(3,ip)
                        a1=apgpf(gp1,f1)
                        a2=apgpf(gp2,f2)
                        a3=apgpf(gp3,f3)
                        dx=long(gp1e(i))+long(gp2e(i))
     1                        -2.0d0*long(gp3e(i))
                        dy=lat(gp1e(i))+lat(gp2e(i))
     1                        -2.0d0*lat(gp3e(i))
                        as=datan2(dx,dy)
                        if ((a2.lt.as).and.(as.le.a1))
     1                        rgp3fv(nefv)=0
                        if ((a3.lt.as).and.(as.le.a2))
     1                        rgp3fv(nefv)=1
                        if ((a1.lt.as).or.(as.le.a3))
     1                        rgp3fv(nefv)=2
                        r=rgp3fv(nefv)
                        netj(r,tj)=netj(r,tj)+1
                        negp(gp3e(i))=0
                  end if
c
c Grid points where two velocity lines are joined
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(Jvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Jvl=Jvlvlp(ip)
                        kgp1fv(nefv)=4
                        igp1fv(nefv)=Jvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp2e(i))+long(gp3e(i))
     1                              -2.0d0*long(gp1e(i))
                              dy=lat(gp2e(i))+lat(gp3e(i))
     1                              -2.0d0*lat(gp1e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp1fv(nefv)=1
                                    else
                                          rgp1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp1fv(nefv)=0
                                    else
                                          rgp1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp1vl+2*rgp1fv(nefv)
                        rgp1fv(nefv)=r
                        neJvl(r,Jvl)=neJvl(r,Jvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(Jvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Jvl=Jvlvlp(ip)
                        kgp2fv(nefv)=4
                        igp2fv(nefv)=Jvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp3e(i))+long(gp1e(i))
     1                              -2.0d0*long(gp2e(i))
                              dy=lat(gp3e(i))+lat(gp1e(i))
     1                              -2.0d0*lat(gp2e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp2fv(nefv)=1
                                    else
                                          rgp2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp2fv(nefv)=0
                                    else
                                          rgp2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp2vl+2*rgp2fv(nefv)
                        rgp2fv(nefv)=r
                        neJvl(r,Jvl)=neJvl(r,Jvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(Jvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Jvl=Jvlvlp(ip)
                        kgp3fv(nefv)=4
                        igp3fv(nefv)=Jvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp1e(i))+long(gp2e(i))
     1                              -2.0d0*long(gp3e(i))
                              dy=lat(gp1e(i))+lat(gp2e(i))
     1                              -2.0d0*lat(gp3e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp3fv(nefv)=1
                                    else
                                          rgp3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp3fv(nefv)=0
                                    else
                                          rgp3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp3vl+2*rgp3fv(nefv)
                        rgp3fv(nefv)=r
                        neJvl(r,Jvl)=neJvl(r,Jvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(fTvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        fT=fTvlp(ip)
                        kgp1fv(nefv)=5
                        igp1fv(nefv)=fT
                        r=rgp1f+2*rgp1vl
                        rgp1fv(nefv)=r
                        nefT(r,fT)=nefT(r,fT)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                        ip=ipf(gp1e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp1f,j,f)=0
                  end if
c
c Grid points where a velocity line terminates at a fault
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(fTvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        fT=fTvlp(ip)
                        kgp2fv(nefv)=5
                        igp2fv(nefv)=fT
                        r=rgp2f+2*rgp2vl
                        rgp2fv(nefv)=r
                        nefT(r,fT)=nefT(r,fT)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                        ip=ipf(gp2e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp2f,j,f)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(fTvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        fT=fTvlp(ip)
                        kgp3fv(nefv)=5
                        igp3fv(nefv)=fT
                        r=rgp3f+2*rgp3vl
                        rgp3fv(nefv)=r
                        nefT(r,fT)=nefT(r,fT)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                        ip=ipf(gp3e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp3f,j,f)=0
                  end if
c
c Grid points where a fault truncates at an exterior velocity line
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(Tfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tf=Tfvlp(ip)
                        kgp1fv(nefv)=6
                        igp1fv(nefv)=Tf
                        r=rgp1vl+2*rgp1f
                        rgp1fv(nefv)=r
                        neTf(r,Tf)=neTf(r,Tf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                        ip=ipf(gp1e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp1f,j,f)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(Tfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tf=Tfvlp(ip)
                        kgp2fv(nefv)=6
                        igp2fv(nefv)=Tf
                        r=rgp2vl+2*rgp2f
                        rgp2fv(nefv)=r
                        neTf(r,Tf)=neTf(r,Tf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                        ip=ipf(gp2e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp2f,j,f)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(Tfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tf=Tfvlp(ip)
                        kgp3fv(nefv)=6
                        igp3fv(nefv)=Tf
                        r=rgp3vl+2*rgp3f
                        rgp3fv(nefv)=r
                        neTf(r,Tf)=neTf(r,Tf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                        ip=ipf(gp3e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp3f,j,f)=0
                  end if
c
c Grid points where a velocity line terminates at another velocity line
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(Tvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tvl=Tvlvlp(ip)
                        kgp1fv(nefv)=7
                        igp1fv(nefv)=Tvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp2e(i))+long(gp3e(i))
     1                              -2.0d0*long(gp1e(i))
                              dy=lat(gp2e(i))+lat(gp3e(i))
     1                              -2.0d0*lat(gp1e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp1fv(nefv)=1
                                    else
                                          rgp1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp1fv(nefv)=0
                                    else
                                          rgp1fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (vl2Tvl(Tvl).eq.vl) r=rgp1vl+2*rgp1fv(nefv)
                        if (vl1Tvl(Tvl).eq.vl) r=rgp1fv(nefv)+2*rgp1vl
                        rgp1fv(nefv)=r
                        neTvl(r,Tvl)=neTvl(r,Tvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(Tvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tvl=Tvlvlp(ip)
                        kgp2fv(nefv)=7
                        igp2fv(nefv)=Tvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp3e(i))+long(gp1e(i))
     1                              -2.0d0*long(gp2e(i))
                              dy=lat(gp3e(i))+lat(gp1e(i))
     1                              -2.0d0*lat(gp2e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp2fv(nefv)=1
                                    else
                                          rgp2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp2fv(nefv)=0
                                    else
                                          rgp2fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (vl2Tvl(Tvl).eq.vl) r=rgp2vl+2*rgp2fv(nefv)
                        if (vl1Tvl(Tvl).eq.vl) r=rgp2fv(nefv)+2*rgp2vl
                        rgp2fv(nefv)=r
                        neTvl(r,Tvl)=neTvl(r,Tvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(Tvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Tvl=Tvlvlp(ip)
                        kgp3fv(nefv)=7
                        igp3fv(nefv)=Tvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp1e(i))+long(gp2e(i))
     1                              -2.0d0*long(gp3e(i))
                              dy=lat(gp1e(i))+lat(gp2e(i))
     1                              -2.0d0*lat(gp3e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp3fv(nefv)=1
                                    else
                                          rgp3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp3fv(nefv)=0
                                    else
                                          rgp3fv(nefv)=1
                                    end if
                              end if
                        end if
                        if (vl2Tvl(Tvl).eq.vl) r=rgp3vl+2*rgp3fv(nefv)
                        if (vl1Tvl(Tvl).eq.vl) r=rgp3fv(nefv)+2*rgp3vl
                        rgp3fv(nefv)=r
                        neTvl(r,Tvl)=neTvl(r,Tvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                  end if
c
c Grid points where a velocity line crosses a fault
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(Xfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xf=Xfvlp(ip)
                        kgp1fv(nefv)=8
                        igp1fv(nefv)=Xf
                        r=rgp1vl+2*rgp1f
                        rgp1fv(nefv)=r
                        neXf(r,Xf)=neXf(r,Xf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                        ip=ipf(gp1e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp1f,j,f)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(Xfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xf=Xfvlp(ip)
                        kgp2fv(nefv)=8
                        igp2fv(nefv)=Xf
                        r=rgp2vl+2*rgp2f
                        rgp2fv(nefv)=r
                        neXf(r,Xf)=neXf(r,Xf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                        ip=ipf(gp2e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp2f,j,f)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(Xfvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xf=Xfvlp(ip)
                        kgp3fv(nefv)=8
                        igp3fv(nefv)=Xf
                        r=rgp3vl+2*rgp3f
                        rgp3fv(nefv)=r
                        neXf(r,Xf)=neXf(r,Xf)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                        ip=ipf(gp3e(i))
                        f=fp(jfp(ip),ip)
                        j=gpfp(jfp(ip),ip)
                        negpf(rgp3f,j,f)=0
                  end if
c
c Grid points where two velocity lines cross
c
                  wanted=.false.
                  if (ipvl(gp1e(i)).ne.0) then
                        ip=ipvl(gp1e(i))
                        wanted=(Xvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xvl=Xvlvlp(ip)
                        kgp1fv(nefv)=9
                        igp1fv(nefv)=Xvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp1fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp2e(i))+long(gp3e(i))
     1                              -2.0d0*long(gp1e(i))
                              dy=lat(gp2e(i))+lat(gp3e(i))
     1                              -2.0d0*lat(gp1e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp1fv(nefv)=1
                                    else
                                          rgp1fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp1fv(nefv)=0
                                    else
                                          rgp1fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp1vl+2*rgp1fv(nefv)
                        rgp1fv(nefv)=r
                        neXvl(r,Xvl)=neXvl(r,Xvl)+1
                        vl=vlp(1,ip)







                        j=gpvlp(1,ip)
                        negpvl(rgp1vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp2e(i)).ne.0) then
                        ip=ipvl(gp2e(i))
                        wanted=(Xvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xvl=Xvlvlp(ip)
                        kgp2fv(nefv)=9
                        igp2fv(nefv)=Xvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks3fv(nefv).eq.2) then
                              if (is3fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs3fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp2fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp3e(i))+long(gp1e(i))
     1                              -2.0d0*long(gp2e(i))
                              dy=lat(gp3e(i))+lat(gp1e(i))
     1                              -2.0d0*lat(gp2e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp2fv(nefv)=1
                                    else
                                          rgp2fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp2fv(nefv)=0
                                    else
                                          rgp2fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp2vl+2*rgp2fv(nefv)
                        rgp2fv(nefv)=r
                        neXvl(r,Xvl)=neXvl(r,Xvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp2vl,j,vl)=0
                  end if
c
                  wanted=.false.
                  if (ipvl(gp3e(i)).ne.0) then
                        ip=ipvl(gp3e(i))
                        wanted=(Xvlvlp(ip).ne.0)
                  end if
                  if (wanted) then
                        Xvl=Xvlvlp(ip)
                        kgp3fv(nefv)=9
                        igp3fv(nefv)=Xvl
                        vl=vlp(2,ip)
                        j=gpvlp(2,ip)
                        online=.false.
                        if (ks1fv(nefv).eq.2) then
                              if (is1fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs1fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (ks2fv(nefv).eq.2) then
                              if (is2fv(nefv).eq.vl) then
                                    rgp3fv(nefv)=rs2fv(nefv)
                                    online=.true.
                              end if
                        end if
                        if (.not.online) then
                              a1=amgpvl(j,vl)
                              a2=apgpvl(j,vl)
                              dx=long(gp1e(i))+long(gp2e(i))
     1                              -2.0d0*long(gp3e(i))
                              dy=lat(gp1e(i))+lat(gp2e(i))
     1                              -2.0d0*lat(gp3e(i))
                              a3=datan2(dx,dy)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          rgp3fv(nefv)=1
                                    else
                                          rgp3fv(nefv)=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          rgp3fv(nefv)=0
                                    else
                                          rgp3fv(nefv)=1
                                    end if
                              end if
                        end if
                        r=rgp3vl+2*rgp3fv(nefv)
                        rgp3fv(nefv)=r
                        neXvl(r,Xvl)=neXvl(r,Xvl)+1
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        negpvl(rgp3vl,j,vl)=0
                  end if
            end if
      end do
c
c Do some checking
c
      do i=1,ngp
            if (negp(i).eq.0) then
                  if ((ipf(i).eq.0).and.(ipvl(i).eq.0)) then
                        write(*,*) 'i=',i
                        stop 'No elements at grid point'
                  end if
                  if ((ipf(i).ne.0).and.(ipvl(i).eq.0)) then
                        ip=ipf(i)
                        if ((jfp(ip).eq.0).and.
     1                        (tjfp(ip).eq.0)) then
                              write(*,*) 'i,ipf=',i,ip
                              stop 'No elements at grid point'
                        end if
                  end if
                  if (ipvl(i).ne.0) then
                        ip=ipvl(i)
                        vl=vlp(1,ip)
                        j=gpvlp(1,ip)
                        if (((j.eq.0).or.(j.eq.nvls(vl))).and.
     1                        (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip)
     3                        .eq.0)) then
                              write(*,*) 'i,ipvl=',i,ip
                              stop 'No elements at grid point'
                        end if
                  end if
            end if
      end do
      if (nf.gt.0) then
      do i=1,nf
            do j=1,nfs(i)
                  if ((nesf(0,j,i).eq.0).and.
     1                  (isrb(sf(j,i)).eq.0)) then
                        r=0
                        write(*,*) 'i,j,r=',i,j,r
                        stop 'No element on side of fault'
                  end if
                  if ((nesf(1,j,i).eq.0).and.
     1                  (isrb(sf(j,i)).eq.0)) then
                        r=1
                        write(*,*) 'i,j,r=',i,j,r
                        stop 'No element on side of fault'
                  end if
            end do
            do j=2,nfs(i)
                  if ((negpf(0,j-1,i).eq.0).and.
     1                  (ipvl(gpf(j-1,i))
     2                  +iprb(gpf(j-1,i)).eq.0)) then
                        r=0
                        write(*,*) 'i,j,r=',i,j-1,r
                        stop 'No elements at grid point on fault'
                  end if
                  if ((negpf(1,j-1,i).eq.0).and.
     1                  (ipvl(gpf(j-1,i))
     2                  +iprb(gpf(j-1,i)).eq.0)) then
                        r=1
                        write(*,*) 'i,j,r=',i,j-1,r
                        stop 'No elements at grid point on fault'
                  end if
            end do
      end do
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            do j=1,nvls(i)
                  if ((nesvl(0,j,i).eq.0).and.
     1                  (e2s(svl(j,i)).ne.0)) then
                        r=0
                        write(*,*) 'i,j,r=',i,j,r
                        stop 'No element on side of v-line'
                  end if
                  if ((nesvl(1,j,i).eq.0).and.
     1                  (e2s(svl(j,i)).ne.0)) then
                        r=1
                        write(*,*) 'i,j,r=',i,j,r
                        stop 'No element on side of v-line'
                  end if
            end do
            do j=2,nvls(i)
                  ip=ipvl(gpvl(j-1,i))
                  if ((negpvl(0,j-1,i).eq.0).and.
     1                  (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     3                  .and.(.not.extern(gpvl(j-1,i)))) then
                        r=0
                        write(*,*) 'i,j,r=',i,j-1,r
                        stop 'No elements at grid point on v-line'
                  end if
                  if ((negpvl(1,j-1,i).eq.0).and.
     1                  (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     3                  .and.(.not.extern(gpvl(j-1,i)))) then
                        r=1
                        write(*,*) 'i,j,r=',i,j-1,r
                        stop 'No elements at grid point on v-line'
                  end if
            end do
      end do
      end if
      if (ntj.gt.0) then
      do i=1,ntj
            if ((netj(0,i).eq.0).and.(iprb(gptj(i)).eq.0)) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at triple junction'
            end if
            if ((netj(1,i).eq.0).and.(iprb(gptj(i)).eq.0)) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at triple junction'
            end if
            if ((netj(2,i).eq.0).and.(iprb(gptj(i)).eq.0)) then
                  r=2
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at triple junction'
            end if
      end do
      end if
      if (nJvl.gt.0) then
      do i=1,nJvl
            if (neJvl(0,i)+neJvl(3,i)
     1            +neJvl(1,i)+neJvl(2,i).eq.0) then
                  write(*,*) 'i=',i
                  stop 'No elements at joining of v-lines'
            end if
            if ((neJvl(0,i)+neJvl(3,i).gt.0).and.
     1            (neJvl(1,i)+neJvl(2,i).gt.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled zone at joining of v-lines'
            end if
            vl1=vl1Jvl(i)
            gp1=gp1Jvl(i)
            if ((neJvl(0,i).eq.0).and.(neJvl(3,i).gt.0).and.
     1            (.not.extern(gpvl(gp1,vl1)))) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at joining of v-lines'
            end if
            if ((neJvl(1,i).eq.0).and.(neJvl(2,i).gt.0).and.
     1            (.not.extern(gpvl(gp1,vl1)))) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at joining of v-lines'
            end if
            if ((neJvl(2,i).eq.0).and.(neJvl(1,i).gt.0).and.
     1            (.not.extern(gpvl(gp1,vl1)))) then
                  r=2
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at joining of v-lines'
            end if
            if ((neJvl(3,i).eq.0).and.(neJvl(0,i).gt.0).and.
     1            (.not.extern(gpvl(gp1,vl1)))) then
                  r=3
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at joining of v-lines'
            end if
      end do
      end if
      if (nfT.gt.0) then
      do i=1,nfT
            if ((nefT(2,i).eq.0).and.(nefT(3,i).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements in region at truncation at a fault'
            end if
            if ((nefT(2,i).gt.0).and.(nefT(3,i).gt.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled region at truncation at a fault'
            end if
            if (nefT(0,i).eq.0) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation at a fault'
            end if
            if (nefT(1,i).eq.0) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation at a fault'
            end if
      end do
      end if
      if (nTf.gt.0) then
      do i=1,nTf
            gp=gpTf(i)
            if ((neTf(0,i).eq.0).and.(neTf(1,i).eq.0).and.
     1            (iprb(gp).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(2,i).eq.0).and.(neTf(3,i).eq.0).and.
     1            (iprb(gp).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(0,i).gt.0).and.(neTf(1,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled region at truncation of a fault'
            end if
            if ((neTf(2,i).gt.0).and.(neTf(3,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled region at truncation of a fault'
            end if
            if ((neTf(0,i).eq.0).and.(neTf(2,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(1,i).eq.0).and.(neTf(3,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(2,i).eq.0).and.(neTf(0,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  r=2
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(3,i).eq.0).and.(neTf(1,i).gt.0).and.
     1            (iprb(gp).eq.0)) then
                  r=3
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation of a fault'
            end if
            if ((neTf(0,i)+neTf(3,i)
     1            +neTf(1,i)+neTf(2,i).eq.0).and.
     1            (iprb(gp).ne.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements at truncation of a fault'
            end if
            if ((neTf(0,i)+neTf(3,i).gt.0).and.
     1            (neTf(1,i)+neTf(2,i).gt.0).and.
     1            (iprb(gp).ne.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled zone at truncation of a fault'
            end if
      end do
      end if
      if (nTvl.gt.0) then
      do i=1,nTvl
            if ((neTvl(2,i).eq.0).and.(neTvl(3,i).eq.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements in region at truncation at a v-line'
            end if
            if ((neTvl(2,i).gt.0).and.(neTvl(3,i).gt.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled region at truncation at a v-line'
            end if
            if (neTvl(0,i).eq.0) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation at a v-line'
            end if
            if (neTvl(1,i).eq.0) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at truncation at a v-line'
            end if
      end do
      end if
      if (nXf.gt.0) then
      do i=1,nXf
            gp=gpXf(i)
            if ((neXf(0,i).eq.0).and.(neXf(1,i).gt.0)) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of a fault'
            end if
            if ((neXf(1,i).eq.0).and.(iprb(gp).eq.0)) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of a fault'
            end if
            if ((neXf(2,i).eq.0).and.(neXf(3,i).gt.0)) then
                  r=2
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of a fault'
            end if
            if ((neXf(3,i).eq.0).and.(iprb(gp).eq.0)) then
                  r=3
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of a fault'
            end if
            if ((neXf(1,i).eq.0).and.(neXf(3,i).eq.0).and.
     1            (iprb(gp).ne.0)) then
                  write(*,*) 'i=',i
                  stop 'No elements in region at crossing of a fault'
            end if
            if ((neXf(1,i).gt.0).and.(neXf(3,i).gt.0).and.
     1            (iprb(gp).ne.0)) then
                  write(*,*) 'i=',i
                  stop 'Wrong filled region at crossing of a fault'
            end if
      end do
      end if
      if (nXvl.gt.0) then
      do i=1,nXvl
            if (neXvl(0,i).eq.0) then
                  r=0
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of v-lines'
            end if
            if (neXvl(1,i).eq.0) then
                  r=1
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of v-lines'
            end if
            if (neXvl(2,i).eq.0) then
                  r=2
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of v-lines'
            end if
            if (neXvl(3,i).eq.0) then
                  r=3
                  write(*,*) 'i,r=',i,r
                  stop 'No elements in region at crossing of v-lines'
            end if
      end do
      end if
c
c RIGID BOUNDARIES
c
      do i=1,nrb
            do j=1,nrbs(i)
                  s=srb(j,i)
                  if (isf(s).eq.0) rsrb(j,i)=0
                  if (isf(s).ne.0) then
                        is=isf(s)
                        if (nesf(0,sfs(is),fs(is)).eq.0)
     1                        rsrb(j,i)=0
                        if (nesf(1,sfs(is),fs(is)).eq.0)
     1                        rsrb(j,i)=1
                  end if
            end do
            do j=0,nrbs(i)
                  gp=gprb(j,i)
c
c Simple grid points on rigid boundaries
c
                  if (negp(gp).ne.0) rgprb(j,i)=0
c
c Grid points on rigid boundaries that are also on faults but not
c   velocity lines
c
                  if ((negp(gp).eq.0).and.(ipf(gp).ne.0).and.
     1                  (ipvl(gp).eq.0)) then
                        ip=ipf(gp)
c
c Non-triple-junction cases
c
                        if (tjfp(ip).eq.0) then
                              f=fp(jfp(ip),ip)
                              jp=gpfp(jfp(ip),ip)
                              if (negpf(0,jp,f).eq.0)
     1                              rgprb(j,i)=0
                              if (negpf(1,jp,f).eq.0)
     1                              rgprb(j,i)=1
c
                              if ((negpf(0,jp,f).ne.0).and.
     1                              (negpf(1,jp,f).ne.0)) then
                                    a1=amgpf(jp,f)
                                    a2=apgpf(jp,f)
                                    if (amgprb(j,i).lt.apgprb(j,i))
     1                                    then
                                          agp1=amgprb(j,i)
                                          agp2=apgprb(j,i)
                                    else
                                          agp1=apgprb(j,i)
                                          agp2=amgprb(j,i)
                                    end if
                                    found=.false.
                                    agp1=agp1+prec
                                    agp2=agp2-prec
                                    if (a1.lt.a2) then
                                          if ((a1.le.agp1).and.
     1                                          (agp2.le.a2)) then
                                                rgprb(j,i)=0
                                                found=.true.
                                          end if
                                          if ((a2.le.agp1).or.
     1                                          (agp2.le.a1)) then
                                                rgprb(j,i)=1
                                                found=.true.
                                          end if
                                    else
                                          if ((a2.le.agp1).and.
     1                                          (agp2.le.a1)) then
                                                rgprb(j,i)=1
                                                found=.true.
                                          end if
                                          if ((a1.le.agp1).or.
     1                                          (agp2.le.a2)) then
                                                rgprb(j,i)=0
                                                found=.true.
                                          end if
                                    end if
                                    if (.not.found) then
                                          write(*,*) 
     1                                    'prec,a1,a2,agp1,agp2=',
     2                                          prec,a1,a2,agp1,agp2
                  stop 'Recompile with an increased value of prec'
                                    end if
                              end if
                        end if
c
c Grid points on rigid boundaries that are also at triple junctions
c
                        if (tjfp(ip).ne.0) then
                              tj=tjfp(ip)
                              f1=fp(1,ip)
                              f2=fp(2,ip)
                              f3=fp(3,ip)
                              gp1=gpfp(1,ip)
                              gp2=gpfp(2,ip)
                              gp3=gpfp(3,ip)
                              a1=apgpf(gp1,f1)
                              a2=apgpf(gp2,f2)
                              a3=apgpf(gp3,f3)
                              if (amgprb(j,i).lt.apgprb(j,i)) then
                                    agp1=amgprb(j,i)
                                    agp2=apgprb(j,i)
                              else
                                    agp1=apgprb(j,i)
                                    agp2=amgprb(j,i)
                              end if
                              found=.false.
                              agp1=agp1+prec
                              agp2=agp2-prec
                              if ((a2.le.agp1).and.(agp2.le.a1))
     1                              then
                                    rgprb(j,i)=0
                                    found=.true.
                              end if
                              if ((a3.le.agp1).and.(agp2.le.a2))
     1                              then
                                    rgprb(j,i)=1
                                    found=.true.
                              end if
                              if ((a1.le.agp1).or.(agp2.le.a3))
     1                              then
                                    rgprb(j,i)=2
                                    found=.true.
                              end if
                              if (.not.found) then
                                    write(*,*) 
     1                                    'prec,a1,a2,a3,agp1,agp2=',
     2                                    prec,a1,a2,a3,agp1,agp2
                  stop 'Recompile with an increased value of prec'
                              end if
                        end if
                  end if
c
c Grid points on rigid boundaries and also at ends of velocity lines
c
                  if ((negp(gp).eq.0).and.(ipvl(gp).ne.0)) then
                        ip=ipvl(gp)
c
c Simple case of a single velocity line
c
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        rgprb(j,i)=0
c
c Grid points on rigid boundaries where two velocity lines are joined
c
                        if (Jvlvlp(ip).ne.0) then
                              Jvl=Jvlvlp(ip)
                              vl=vl1Jvl(Jvl)
                              jp=gp1Jvl(Jvl)
                              a1=amgpvl(jp,vl)
                              a2=apgpvl(jp,vl)
                              a3=apgprb(j,i)
                              if (a1.lt.a2) then
                                    if ((a3.lt.a1).or.
     1                                    (a3.gt.a2)) then
                                          r=1
                                    else
                                          r=0
                                    end if
                              else
                                    if ((a3.lt.a2).or.
     1                                    (a3.gt.a1)) then
                                          r=0
                                    else
                                          r=1
                                    end if
                              end if
                              if (r.eq.0) then
                                    if (neJvl(3,Jvl).ne.0)
     1                                    rgprb(j,i)=0
                                    if (neJvl(1,Jvl).ne.0)
     1                                    rgprb(j,i)=2
                              end if
                              if (r.eq.1) then
                                    if (neJvl(2,Jvl).ne.0)
     1                                    rgprb(j,i)=1
                                    if (neJvl(0,Jvl).ne.0)
     1                                    rgprb(j,i)=3
                              end if
                        end if
c
c Grid points on rigid boundaries where a fault truncates at the end of
c   a velocity line
c
                        if (Tfvlp(ip).ne.0) then
                              Tf=Tfvlp(ip)
                              vl=vl1Tf(Tf)
                              jp=gp1Tf(Tf)
                              a1=amgpvl(jp,vl)
                              a2=apgpvl(jp,vl)
                              a3=apgprb(j,i)
                              agp1=a3+prec
                              agp2=a3-prec
                              if (a1.lt.a2) then
                                    if ((a1.le.agp1).and.
     1                                    (agp2.le.a2)) then
                                          if (neTf(3,Tf).ne.0)
     1                                          rgprb(j,i)=0
                                          if (neTf(1,Tf).ne.0)
     1                                          rgprb(j,i)=2
                                    end if
                                    if ((a2.le.agp1).or.
     1                                    (agp2.le.a1)) then
                                          if (neTf(2,Tf).ne.0)
     1                                          rgprb(j,i)=1
                                          if (neTf(0,Tf).ne.0)
     1                                          rgprb(j,i)=3
                                    end if
                              else
                                    if ((a1.le.agp1).or.
     1                                    (agp2.le.a2)) then
                                          if (neTf(3,Tf).ne.0)
     1                                          rgprb(j,i)=0
                                          if (neTf(1,Tf).ne.0)
     1                                          rgprb(j,i)=2
                                    end if
                                    if ((a2.le.agp1).and.
     1                                    (agp2.le.a1)) then
                                          if (neTf(2,Tf).ne.0)
     1                                          rgprb(j,i)=1
                                          if (neTf(0,Tf).ne.0)
     1                                          rgprb(j,i)=3
                                    end if
                              end if
                        end if
c
c Grid points on rigid boundaries where the end of a velocity line
c   crosses a fault
c
                        if (Xfvlp(ip).ne.0) then
                              Xf=Xfvlp(ip)
                              if (neXf(3,Xf).eq.0) rgprb(j,i)=2
                              if (neXf(1,Xf).eq.0) rgprb(j,i)=0
                        end if
                  end if
            end do
      end do
c
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE build(ngp,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      gp1e,gp2e,gp3e,nfs,gpf,nvls,gpvl,fvl,nrbs,gprb,evo,
     2      ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,ncduc,s1duc,s2duc,
     3      nece,eec,ncec,e1ec,e2ec,npf,npvl,nprb,ipf,ipvl,iprb,
     4      isf,sfs,nfp,fp,gpfp,jfp,tjfp,ntj,gptj,nvlp,vlp,gpvlp,
     5      rbp,gprbp,nefv,iefv,kgp1fv,kgp2fv,kgp3fv,
     6      igp1fv,igp2fv,igp3fv,rgp1fv,rgp2fv,rgp3fv,rgprb,
     7      p0zero,pnzero,interp,fillf0,fillfn,
     8      rE,long,lat,sxxpot,syypot,sxypot,Lc,Lcc,Lcs,Ls,Lsc,Lss,
     9      Kc,Ks,ux,uy,uxm,uym,uxp,uyp,dut,dun,plat,plong,prate,
     1      oux,ouy,seoux,seouy,rouxuy,w1vo,w2vo,w3vo,
     1      codut,codun,oduc,seoduc,rfo12,coexx,coeyy,coexy,
     2      oec,seoec,reo12,reo13,reo23,cdut,cdun,refduc,scduc,
     3      cexx,ceyy,cexy,refec,scec,area0,area,esub,exx,eyy,exy,
     4      sxx,syy,sxy,fx,fy,Lce,Lcce,Lcse,Lse,Lsce,Lsse,
     5      len0,pindu,pinu,pindu0,pindun,Kcfp,Ksfp,
     6      sedut,sedun,dusub,dupot,lenfs,dufs,ttfs,tnfs,Kcfs,Ksfs,
     7      Lcgp,Lccgp,Lcsgp,Lsgp,Lscgp,Lssgp,Lcf,Lccf,Lcsf,
     8      Lsf,Lscf,Lssf,Lctj,Lcctj,Lcstj,Lstj,Lsctj,Lsstj,
     9      epot,seesub,ugprb,usrb,pinurb,pinuvl,
     2      seou,ou,uvo,seodu,odu,ducfo,seoe,oe,eceo,
     1      duc,dus1c,dus2c,ec,ee1c,ee2c)
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,maxfp,
     4      maxtj,maxefv
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     5      maxduc=2*maxf,maxec=100,maxece=500,
     6      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     7      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     8      maxpvl=maxsvl+maxvl,maxprb=maxsrb+maxrb,
     9      maxfp=6,maxtj=80,maxefv=12*(maxsf+maxsvl))
      integer ngp,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      integer npf,npvl,nprb,nsf,nsvl,nsrb,ntj,nefv
      integer i,gp1,gp2,gp3,j,iq,ic,ih,k,ie,ip,gp,rb,f,tj,r,vl,j0,jn
      integer s1c,s2c,s1,s2,e1c,e2c,e1,e2
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf),gpf(0:maxfs,maxf)
      integer nvls(maxvl),gpvl(0:maxvls,maxvl),
     1      fvl(0:maxvls,maxvl)
      integer nrbs(maxrb),gprb(0:maxrbs,maxrb)
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),nducs(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer nece(maxec),eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs)
      integer sfs(maxsf)
      integer nfp(maxpf),fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf),gptj(maxtj)
      integer nvlp(maxpvl),vlp(2,maxpvl),gpvlp(2,maxpvl)
      integer rbp(maxprb),gprbp(maxprb)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)


      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      logical deform,addf0,addfn
      logical p0zero(maxf),pnzero(maxf)
      logical interp(maxvlj,maxvl)
      logical fillf0(maxvl),fillfn(maxvl)
      real*8 rE
      real*8 pi,lconv,x1,y1,x2,y2,x3,y3
      real*8 dxsum1,dxsum2,dxsum3,nysum1,nysum2,nysum3,dy1,dy2,dy3
      real*8 sxx1,syy1,sxy1,sxx2,syy2,sxy2,sxx3,syy3,sxy3
      real*8 dsxxdx,dsxydx,dsxydy,dsyydy
      real*8 cosy,tx,ty,det,nx,ny,w1,w2,w3,Kch,Ksh,areagp
      real*8 alat,along,wx,wy,wz,wlong,wlat,wr,p1,p2,p3
      real*8 long(maxgp),lat(maxgp)
      real*8 sxxpot(maxgp),syypot(maxgp),sxypot(maxgp)
      real*8 Lc(maxe),Lcc(maxe),Lcs(maxe),
     1      Ls(maxe),Lsc(maxe),Lss(maxe)
      real*8 Kc(maxfs,maxf),Ks(maxfs,maxf)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 dut(0:maxfs,maxf),dun(0:maxfs,maxf)
      real*8 plat(maxrb),plong(maxrb),prate(maxrb)
      real*8 oux(maxvo),ouy(maxvo),seoux(maxvo),seouy(maxvo),
     1      rouxuy(maxvo)
      real*8 w1vo(maxvo),w2vo(maxvo),w3vo(maxvo)
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
      real*8 area0(0:3,maxe),area(maxe)
      real*8 esub(0:3,3,2,0:9,maxe)
      real*8 exx(2,0:9,maxe),eyy(2,0:9,maxe),exy(2,0:9,maxe)
      real*8 sxx(2,0:9,maxe),syy(2,0:9,maxe),sxy(2,0:9,maxe)
      real*8 fx(maxe),fy(maxe)
      real*8 Lce(maxe),Lcce(maxe),Lcse(maxe),
     1      Lse(maxe),Lsce(maxe),Lsse(maxe)
      real*8 len0(2,maxfs,maxf)
      real*8 pindu(2,0:1,maxfs,maxf),pinu(2,2,2,0:5,maxfs,maxf)
      real*8 pindu0(2,2,maxf),pindun(2,2,maxf)
      real*8 Kcfp(0:maxfs,maxf),Ksfp(0:maxfs,maxf)
      real*8 sedut(2,maxfs,maxf),sedun(2,maxfs,maxf)
      real*8 dusub(2,2,0:1,maxfs,maxf),dupot(2,2,maxfs,maxf)
      real*8 lenfs(maxfs,maxf),dufs(0:1,maxfs,maxf)
      real*8 ttfs(0:1,maxfs,maxf),tnfs(0:1,maxfs,maxf)
      real*8 Kcfs(maxfs,maxf),Ksfs(maxfs,maxf)
      real*8 Lcgp(maxgp),Lccgp(maxgp),Lcsgp(maxgp),
     1      Lsgp(maxgp),Lscgp(maxgp),Lssgp(maxgp)
      real*8 Lcf(0:maxfs,maxf),Lccf(0:maxfs,maxf),Lcsf(0:maxfs,maxf),
     1      Lsf(0:maxfs,maxf),Lscf(0:maxfs,maxf),Lssf(0:maxfs,maxf)
      real*8 Lctj(2,maxtj),Lcctj(2,maxtj),Lcstj(2,maxtj),
     1      Lstj(2,maxtj),Lsctj(2,maxtj),Lsstj(2,maxtj)
      real*8 epot(0:3,3,maxe),seesub(0:3,3,3,maxe)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb),usrb(2,maxrbs,maxrb)
      real*8 pinurb(0:5,maxrbs,maxrb)
      real*8 pinuvl(0:5,maxvls,maxvl)
      real*8 seou(2,2,maxvo),ou(2,maxvo),uvo(2,2,0:9,maxvo)
      real*8 seodu(2,2,maxfo),odu(2,maxfo),ducfo(2,2,0:1,maxfo)
      real*8 seoe(3,3,maxeo),oe(3,maxeo),eceo(3,2,0:9,maxeo)
      real*8 duc(maxfs,maxduc),dus1c(2,0:1,maxfs,maxduc),
     1      dus2c(2,0:1,maxfs,maxduc)
      real*8 ec(maxece,maxec),ee1c(2,0:9,maxece,maxec),
     1      ee2c(2,0:9,maxece,maxec)
      real*8 area1(0:3,3,maxe),area2(0:3,3,3,maxe)
      real*8 nxsum(0:3,0:9),nysum(0:3,0:9),dysum(0:3,0:9)
      real*8 len1(2,2,maxfs),len2xx(2,2,2,maxfs),
     1      len2xy(2,2,2,maxfs),len2yy(2,2,2,maxfs),
     2      fsum(2,0:1,maxfs)
      real*8 txsum(2,0:5),tysum(2,0:5)
      real*8 wgp(maxgp),wf(0:maxfs,maxf),wtj(2,maxtj)
      real*8 Lcgpe(3),Lccgpe(3),Lcsgpe(3),Lsgpe(3),Lscgpe(3),Lssgpe(3)
      real*8 L(3,3,3),Tpot(3,3),LTpot(3,3,3),Lq(3,3),Le(3,3)
      real*8 longvl(0:maxvls),latvl(0:maxvls)
      real*8 sxxvl(0:maxvls),syyvl(0:maxvls),sxyvl(0:maxvls)
      real*8 Lcvl(0:maxvls),Lccvl(0:maxvls),Lcsvl(0:maxvls),
     1      Lsvl(0:maxvls),Lscvl(0:maxvls),Lssvl(0:maxvls)
      real*8 Lcfvl(0:maxvls),Lccfvl(0:maxvls),Lcsfvl(0:maxvls),
     1      Lsfvl(0:maxvls),Lscfvl(0:maxvls),Lssfvl(0:maxvls)
      real*8 Kcfvl(0:maxvls),Ksfvl(0:maxvls)
      real*8 txfvl(0:maxvls),tyfvl(0:maxvls)
      real*8 uvoval(0:9),V(3,3)
c
c This routine builds the contribution coefficients for both the
c  constraints and the sum-of-squares functions to be minimised, plus
c  numerous associated parameters, including filling in values on
c  velocity lines and converting rigid body rotations into velocity
c  values and velocity derivatives at boundary points.
c Constraint contributions have the prefix "pin" and are of the same
c  dimension as velocity and slip rate.
c Prior to being multiplied by themselves, contributions to the apriori
c  sum-of-squares have the dimensions of square-root of area times
c  square-root of strain rate, so that the apriori sum-of-squares itself
c  has the dimensions of area times strain rate, or equivalently length
c  times slip rate.
c In contrast, observational contributions to the eventual sum of
c  squares for the aposteriori solution are normalised by their standard
c  errors and are therefore dimensionless.
c
      pi=4.0d0*datan(1.0d0)
      lconv=rE*pi/180.0d0
c
      do i=1,ne
            gp1=gp1e(i)
            gp2=gp2e(i)
            gp3=gp3e(i)
            x1=lconv*long(gp1)
            y1=lconv*lat(gp1)
            x2=lconv*long(gp2)
            y2=lconv*lat(gp2)
            x3=lconv*long(gp3)
            y3=lconv*lat(gp3)
            call evalue(rE,x1,y1,x2,y2,x3,y3,area0(0,i),area1(0,1,i),
     1            area2(0,1,1,i),nxsum,nysum,dysum,
     2            dxsum1,dxsum2,dxsum3,nysum1,nysum2,nysum3,
     3            dy1,dy2,dy3)
            area(i)=area0(0,i)+area0(1,i)+area0(2,i)+area0(3,i)
            do j=0,8
                  do iq=0,3
                        esub(iq,1,1,j,i)=nxsum(iq,j)
                        esub(iq,1,2,j,i)=nysum(iq,j)-dysum(iq,j)
                        esub(iq,2,1,j,i)=0.0d0
                        esub(iq,2,2,j,i)=dysum(iq,j)
                        esub(iq,3,1,j,i)=dysum(iq,j)-0.5d0*nysum(iq,j)
                        esub(iq,3,2,j,i)=0.5d0*nxsum(iq,j)
                  end do
                  do ic=1,2
                        exx(ic,j,i)=esub(0,1,ic,j,i)+esub(1,1,ic,j,i)
     1                        +esub(2,1,ic,j,i)+esub(3,1,ic,j,i)
                        eyy(ic,j,i)=esub(0,2,ic,j,i)+esub(1,2,ic,j,i)
     1                        +esub(2,2,ic,j,i)+esub(3,2,ic,j,i)
                        exy(ic,j,i)=esub(0,3,ic,j,i)+esub(1,3,ic,j,i)
     1                        +esub(2,3,ic,j,i)+esub(3,3,ic,j,i)
                        exx(ic,j,i)=exx(ic,j,i)/area(i)
                        eyy(ic,j,i)=eyy(ic,j,i)/area(i)
                        exy(ic,j,i)=exy(ic,j,i)/area(i)
                  end do
            end do
            sxx1=sxxpot(gp1)
            syy1=syypot(gp1)
            sxy1=sxypot(gp1)
            sxx2=sxxpot(gp2)
            syy2=syypot(gp2)
            sxy2=sxypot(gp2)
            sxx3=sxxpot(gp3)
            syy3=syypot(gp3)
            sxy3=sxypot(gp3)
            dsxxdx=(dxsum1*sxx1+dxsum2*sxx2+dxsum3*sxx3)/area(i)
            dsxydx=(dxsum1*sxy1+dxsum2*sxy2+dxsum3*sxy3)/area(i)
            dsxydy=2.0d0*(nysum1*sxy1+nysum2*sxy2
     1            +nysum3*sxy3)/area(i)
     2            -(dy1*sxy1+dy2*sxy2+dy3*sxy3)
            dsyydy=(nysum1*(syy1-sxx1)+nysum2*(syy2-sxx2)
     1            +nysum3*(syy3-sxx3))/area(i)
     2            +(dy1*sxx1+dy2*sxx2+dy3*sxx3)
            
            fx(i)=-(dsxxdx+dsxydy)
            fy(i)=-(dsxydx+dsyydy)
      end do
c
      if (nf.gt.0) then
      do i=1,nf
            x1=lconv*long(gpf(0,i))
            y1=lconv*lat(gpf(0,i))
            x2=lconv*long(gpf(1,i))
            y2=lconv*lat(gpf(1,i))
            cosy=dcos(y1/rE)
            tx=(x2-x1)*cosy
            ty=y2-y1
            det=dsqrt(tx**2+ty**2)
            tx=tx/det
            ty=ty/det
            nx=-ty
            ny=tx
            pindu0(1,1,i)=tx
            pindu0(1,2,i)=nx
            pindu0(2,1,i)=ty
            pindu0(2,2,i)=ny
            do j=1,nfs(i)
                  x1=lconv*long(gpf(j-1,i))
                  y1=lconv*lat(gpf(j-1,i))
                  x2=lconv*long(gpf(j,i))
                  y2=lconv*lat(gpf(j,i))
                  call fvalue(rE,x1,y1,x2,y2,
     1                  len0(1,j,i),len1(1,1,j),len2xx(1,1,1,j),
     2                  len2xy(1,1,1,j),len2yy(1,1,1,j),fsum(1,0,j),
     3                  txsum,tysum)
                  lenfs(j,i)=len0(1,j,i)+len0(2,j,i)
                  do ih=1,2
                        do k=0,1
                              pindu(ih,k,j,i)=fsum(ih,k,j)
     1                              /len0(ih,j,i)
                        end do
                        do k=0,5
                              pinu(ih,1,1,k,j,i)=txsum(ih,k)
     1                              /len0(ih,j,i)
                              pinu(ih,1,2,k,j,i)=tysum(ih,k)
     1                              /len0(ih,j,i)
                              pinu(ih,2,1,k,j,i)=-tysum(ih,k)
     1                              /len0(ih,j,i)
                              pinu(ih,2,2,k,j,i)=txsum(ih,k)
     1                              /len0(ih,j,i)
                        end do
                  end do
            end do
            x1=lconv*long(gpf(nfs(i)-1,i))
            y1=lconv*lat(gpf(nfs(i)-1,i))
            x2=lconv*long(gpf(nfs(i),i))
            y2=lconv*lat(gpf(nfs(i),i))
            cosy=dcos(y2/rE)
            tx=(x2-x1)*cosy
            ty=y2-y1
            det=dsqrt(tx**2+ty**2)
            tx=tx/det
            ty=ty/det
            nx=-ty
            ny=tx
            pindun(1,1,i)=tx
            pindun(1,2,i)=nx
            pindun(2,1,i)=ty
            pindun(2,2,i)=ny
c
            if (p0zero(i)) then
                  Kcfp(0,i)=0.0d0
                  Ksfp(0,i)=0.0d0
            else
                  Kcfp(0,i)=Kc(1,i)
                  Ksfp(0,i)=Ks(1,i)
            end if
            do j=2,nfs(i)
                  w1=len1(1,1,j)+len1(2,1,j)
                  w2=len1(1,2,j-1)+len1(2,2,j-1)
                  Kcfp(j-1,i)=(w1*Kc(j,i)+w2*Kc(j-1,i))/(w1+w2)
                  Ksfp(j-1,i)=(w1*Ks(j,i)+w2*Ks(j-1,i))/(w1+w2)
            end do
            if (pnzero(i)) then
                  Kcfp(nfs(i),i)=0.0d0
                  Ksfp(nfs(i),i)=0.0d0
            else
                  Kcfp(nfs(i),i)=Kc(nfs(i),i)
                  Ksfp(nfs(i),i)=Ks(nfs(i),i)
            end if
c
            do j=1,nfs(i)
                  gp1=gpf(j-1,i)
                  gp2=gpf(j,i)
                  sxx1=sxxpot(gp1)
                  syy1=syypot(gp1)
                  sxy1=sxypot(gp1)
                  sxx2=sxxpot(gp2)
                  syy2=syypot(gp2)
                  sxy2=sxypot(gp2)
                  Kcfs(j,i)=0.0d0
                  Ksfs(j,i)=0.0d0
                  do ih=1,2
                        Kch=len1(ih,1,j)*Kcfp(j-1,i)
     1                        +len1(ih,2,j)*Kcfp(j,i)
                        Ksh=len1(ih,1,j)*Ksfp(j-1,i)
     1                        +len1(ih,2,j)*Ksfp(j,i)
                        Kcfs(j,i)=Kcfs(j,i)+Kch
                        Ksfs(j,i)=Ksfs(j,i)+Ksh
                        sedut(ih,j,i)=dsqrt(Kch+Ksh)
                        sedun(ih,j,i)=dsqrt(Kch)
                        dupot(ih,1,j,i)=((len2yy(ih,1,1,j)
     1                        -len2xx(ih,1,1,j))*sxy1
     2                        +len2xy(ih,1,1,j)*(sxx1-syy1)
     3                        +(len2yy(ih,2,1,j)
     4                        -len2xx(ih,2,1,j))*sxy2
     5                        +len2xy(ih,2,1,j)*(sxx2-syy2))
     6                        *(Kcfp(j-1,i)+Ksfp(j-1,i))
     7                        +((len2yy(ih,1,2,j)
     8                        -len2xx(ih,1,2,j))*sxy1
     9                        +len2xy(ih,1,2,j)*(sxx1-syy1)
     1                        +(len2yy(ih,2,2,j)
     1                        -len2xx(ih,2,2,j))*sxy2
     2                        +len2xy(ih,2,2,j)*(sxx2-syy2))
     3                        *(Kcfp(j,i)+Ksfp(j,i))
                        dupot(ih,2,j,i)=(len2xx(ih,1,1,j)*sxx1
     1                        +2.0d0*len2xy(ih,1,1,j)*sxy1
     2                        +len2yy(ih,1,1,j)*syy1
     3                        +len2xx(ih,2,1,j)*sxx2
     4                        +2.0d0*len2xy(ih,2,1,j)*sxy2
     5                        +len2yy(ih,2,1,j)*syy2)
     6                        *Kcfp(j-1,i)
     7                        +(len2xx(ih,1,2,j)*sxx1
     8                        +2.0d0*len2xy(ih,1,2,j)*sxy1
     9                        +len2yy(ih,1,2,j)*syy1
     1                        +len2xx(ih,2,2,j)*sxx2
     1                        +2.0d0*len2xy(ih,2,2,j)*sxy2
     2                        +len2yy(ih,2,2,j)*syy2)
     3                        *Kcfp(j,i)
                        do k=0,1
                              do ic=1,2
                                    dusub(ih,ic,k,j,i)=fsum(ih,k,j)
                              end do
                              dusub(ih,1,k,j,i)=dusub(ih,1,k,j,i)
     1                              /sedut(ih,j,i)
                              dusub(ih,2,k,j,i)=dusub(ih,2,k,j,i)
     1                              /sedun(ih,j,i)
                        end do
                        dupot(ih,1,j,i)=dupot(ih,1,j,i)
     1                        /sedut(ih,j,i)
                        dupot(ih,2,j,i)=dupot(ih,2,j,i)
     1                        /sedun(ih,j,i)
                  end do
                  Kcfs(j,i)=Kcfs(j,i)/lenfs(j,i)
                  Ksfs(j,i)=Ksfs(j,i)/lenfs(j,i)
                  do k=0,1
                        dufs(k,j,i)=fsum(1,k,j)+fsum(2,k,j)
                        dufs(k,j,i)=dufs(k,j,i)/lenfs(j,i)
                        ttfs(k,j,i)=dufs(k,j,i)/(Kcfs(j,i)+Ksfs(j,i))
                        tnfs(k,j,i)=dufs(k,j,i)/Kcfs(j,i)
                  end do
            end do
      end do
      end if
c
      do i=1,ngp
            wgp(i)=0.0d0
            Lcgp(i)=0.0d0
            Lccgp(i)=0.0d0
            Lcsgp(i)=0.0d0
            Lsgp(i)=0.0d0
            Lscgp(i)=0.0d0
            Lssgp(i)=0.0d0
      end do
      if (nf.gt.0) then
      do i=1,nf
            do j=0,nfs(i)
                  wf(j,i)=0.0d0
                  Lcf(j,i)=0.0d0
                  Lccf(j,i)=0.0d0
                  Lcsf(j,i)=0.0d0
                  Lsf(j,i)=0.0d0
                  Lscf(j,i)=0.0d0
                  Lssf(j,i)=0.0d0
            end do
      end do
      end if
      if (ntj.gt.0) then
      do i=1,ntj
            do j=1,2
                  wtj(j,i)=0.0d0
                  Lctj(j,i)=0.0d0
                  Lcctj(j,i)=0.0d0
                  Lcstj(j,i)=0.0d0
                  Lstj(j,i)=0.0d0
                  Lsctj(j,i)=0.0d0
                  Lsstj(j,i)=0.0d0
            end do
      end do
      end if
      do i=1,ne
            w1=area1(0,1,i)+area1(1,1,i)+area1(2,1,i)+area1(3,1,i)
            w2=area1(0,2,i)+area1(1,2,i)+area1(2,2,i)+area1(3,2,i)
            w3=area1(0,3,i)+area1(1,3,i)+area1(2,3,i)+area1(3,3,i)
            if (iefv(i).eq.0) then
                  gp=gp1e(i)
                  wgp(gp)=wgp(gp)+w1
                  if (iprb(gp).eq.0) then
                        Lcgp(gp)=Lcgp(gp)+w1*Lc(i)
                        Lccgp(gp)=Lccgp(gp)+w1*Lcc(i)
                        Lcsgp(gp)=Lcsgp(gp)+w1*Lcs(i)
                        Lsgp(gp)=Lsgp(gp)+w1*Ls(i)
                        Lscgp(gp)=Lscgp(gp)+w1*Lsc(i)
                        Lssgp(gp)=Lssgp(gp)+w1*Lss(i)
                  end if
                  gp=gp2e(i)
                  wgp(gp)=wgp(gp)+w2
                  if (iprb(gp).eq.0) then
                        Lcgp(gp)=Lcgp(gp)+w2*Lc(i)
                        Lccgp(gp)=Lccgp(gp)+w2*Lcc(i)
                        Lcsgp(gp)=Lcsgp(gp)+w2*Lcs(i)
                        Lsgp(gp)=Lsgp(gp)+w2*Ls(i)
                        Lscgp(gp)=Lscgp(gp)+w2*Lsc(i)
                        Lssgp(gp)=Lssgp(gp)+w2*Lss(i)
                  end if
                  gp=gp3e(i)
                  wgp(gp)=wgp(gp)+w3
                  if (iprb(gp).eq.0) then
                        Lcgp(gp)=Lcgp(gp)+w3*Lc(i)
                        Lccgp(gp)=Lccgp(gp)+w3*Lcc(i)
                        Lcsgp(gp)=Lcsgp(gp)+w3*Lcs(i)
                        Lsgp(gp)=Lsgp(gp)+w3*Ls(i)
                        Lscgp(gp)=Lscgp(gp)+w3*Lsc(i)
                        Lssgp(gp)=Lssgp(gp)+w3*Lss(i)
                  end if
            end if
            if (iefv(i).ne.0) then
                  ie=iefv(i)
                  if (iprb(gp1e(i)).eq.0) then
                        deform=.true.
                  else
                        ip=iprb(gp1e(i))
                        rb=rbp(ip)
                        gp=gprbp(ip)
                        deform=(rgprb(gp,rb).ne.rgp1fv(ie))
                  end if
                  if ((rgp1fv(ie).eq.0).or.
     1                  (kgp1fv(ie).eq.2).or.
     2                  (kgp1fv(ie).eq.4).or.
     3                  ((kgp1fv(ie).eq.5).and.(rgp1fv(ie).eq.2)).or.
     4                  ((kgp1fv(ie).eq.6).and.(rgp1fv(ie).eq.1)).or.
     5                  (kgp1fv(ie).eq.7).or.
     6                  ((kgp1fv(ie).eq.8).and.(rgp1fv(ie).eq.1)).or.
     7                  (kgp1fv(ie).eq.9)) then
                        gp=gp1e(i)
                        wgp(gp)=wgp(gp)+w1
                        if (deform) then
                              Lcgp(gp)=Lcgp(gp)+w1*Lc(i)
                              Lccgp(gp)=Lccgp(gp)+w1*Lcc(i)
                              Lcsgp(gp)=Lcsgp(gp)+w1*Lcs(i)
                              Lsgp(gp)=Lsgp(gp)+w1*Ls(i)
                              Lscgp(gp)=Lscgp(gp)+w1*Lsc(i)
                              Lssgp(gp)=Lssgp(gp)+w1*Lss(i)
                        end if
                  else
                        if (kgp1fv(ie).ne.3) then
                              ip=ipf(gp1e(i))
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              wf(gp,f)=wf(gp,f)+w1
                              if (deform) then
                                    Lcf(gp,f)=Lcf(gp,f)+w1*Lc(i)
                                    Lccf(gp,f)=Lccf(gp,f)+w1*Lcc(i)
                                    Lcsf(gp,f)=Lcsf(gp,f)+w1*Lcs(i)
                                    Lsf(gp,f)=Lsf(gp,f)+w1*Ls(i)
                                    Lscf(gp,f)=Lscf(gp,f)+w1*Lsc(i)
                                    Lssf(gp,f)=Lssf(gp,f)+w1*Lss(i)
                              end if
                        else
                              tj=igp1fv(ie)
                              r=rgp1fv(ie)
                              wtj(r,tj)=wtj(r,tj)+w1
                              if (deform) then
                                    Lctj(r,tj)=Lctj(r,tj)+w1*Lc(i)
                                    Lcctj(r,tj)=Lcctj(r,tj)+w1*Lcc(i)
                                    Lcstj(r,tj)=Lcstj(r,tj)+w1*Lcs(i)
                                    Lstj(r,tj)=Lstj(r,tj)+w1*Ls(i)
                                    Lsctj(r,tj)=Lsctj(r,tj)+w1*Lsc(i)
                                    Lsstj(r,tj)=Lsstj(r,tj)+w1*Lss(i)
                              end if
                        end if
                  end if
                  if (iprb(gp2e(i)).eq.0) then
                        deform=.true.
                  else
                        ip=iprb(gp2e(i))
                        rb=rbp(ip)
                        gp=gprbp(ip)
                        deform=(rgprb(gp,rb).ne.rgp2fv(ie))
                  end if
                  if ((rgp2fv(ie).eq.0).or.
     1                  (kgp2fv(ie).eq.2).or.
     2                  (kgp2fv(ie).eq.4).or.
     3                  ((kgp2fv(ie).eq.5).and.(rgp2fv(ie).eq.2)).or.
     4                  ((kgp2fv(ie).eq.6).and.(rgp2fv(ie).eq.1)).or.
     5                  (kgp2fv(ie).eq.7).or.
     6                  ((kgp2fv(ie).eq.8).and.(rgp2fv(ie).eq.1)).or.
     7                  (kgp2fv(ie).eq.9)) then
                        gp=gp2e(i)
                        wgp(gp)=wgp(gp)+w2
                        if (deform) then
                              Lcgp(gp)=Lcgp(gp)+w2*Lc(i)
                              Lccgp(gp)=Lccgp(gp)+w2*Lcc(i)
                              Lcsgp(gp)=Lcsgp(gp)+w2*Lcs(i)
                              Lsgp(gp)=Lsgp(gp)+w2*Ls(i)
                              Lscgp(gp)=Lscgp(gp)+w2*Lsc(i)
                              Lssgp(gp)=Lssgp(gp)+w2*Lss(i)
                        end if
                  else
                        if (kgp2fv(ie).ne.3) then
                              ip=ipf(gp2e(i))
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              wf(gp,f)=wf(gp,f)+w2
                              if (deform) then
                                    Lcf(gp,f)=Lcf(gp,f)+w2*Lc(i)
                                    Lccf(gp,f)=Lccf(gp,f)+w2*Lcc(i)
                                    Lcsf(gp,f)=Lcsf(gp,f)+w2*Lcs(i)
                                    Lsf(gp,f)=Lsf(gp,f)+w2*Ls(i)
                                    Lscf(gp,f)=Lscf(gp,f)+w2*Lsc(i)
                                    Lssf(gp,f)=Lssf(gp,f)+w2*Lss(i)
                              end if
                        else
                              tj=igp2fv(ie)
                              r=rgp2fv(ie)
                              wtj(r,tj)=wtj(r,tj)+w2
                              if (deform) then
                                    Lctj(r,tj)=Lctj(r,tj)+w2*Lc(i)
                                    Lcctj(r,tj)=Lcctj(r,tj)+w2*Lcc(i)
                                    Lcstj(r,tj)=Lcstj(r,tj)+w2*Lcs(i)
                                    Lstj(r,tj)=Lstj(r,tj)+w2*Ls(i)
                                    Lsctj(r,tj)=Lsctj(r,tj)+w2*Lsc(i)
                                    Lsstj(r,tj)=Lsstj(r,tj)+w2*Lss(i)
                              end if
                        end if
                  end if
                  if (iprb(gp3e(i)).eq.0) then
                        deform=.true.
                  else
                        ip=iprb(gp3e(i))
                        rb=rbp(ip)
                        gp=gprbp(ip)
                        deform=(rgprb(gp,rb).ne.rgp3fv(ie))
                  end if
                  if ((rgp3fv(ie).eq.0).or.
     1                  (kgp3fv(ie).eq.2).or.
     2                  (kgp3fv(ie).eq.4).or.
     3                  ((kgp3fv(ie).eq.5).and.(rgp3fv(ie).eq.2)).or.
     4                  ((kgp3fv(ie).eq.6).and.(rgp3fv(ie).eq.1)).or.
     5                  (kgp3fv(ie).eq.7).or.
     6                  ((kgp3fv(ie).eq.8).and.(rgp3fv(ie).eq.1)).or.
     7                  (kgp3fv(ie).eq.9)) then
                        gp=gp3e(i)
                        wgp(gp)=wgp(gp)+w3
                        if (deform) then
                              Lcgp(gp)=Lcgp(gp)+w3*Lc(i)
                              Lccgp(gp)=Lccgp(gp)+w3*Lcc(i)
                              Lcsgp(gp)=Lcsgp(gp)+w3*Lcs(i)
                              Lsgp(gp)=Lsgp(gp)+w3*Ls(i)
                              Lscgp(gp)=Lscgp(gp)+w3*Lsc(i)
                              Lssgp(gp)=Lssgp(gp)+w3*Lss(i)
                        end if
                  else
                        if (kgp3fv(ie).ne.3) then
                              ip=ipf(gp3e(i))
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              wf(gp,f)=wf(gp,f)+w3
                              if (deform) then
                                    Lcf(gp,f)=Lcf(gp,f)+w3*Lc(i)
                                    Lccf(gp,f)=Lccf(gp,f)+w3*Lcc(i)
                                    Lcsf(gp,f)=Lcsf(gp,f)+w3*Lcs(i)
                                    Lsf(gp,f)=Lsf(gp,f)+w3*Ls(i)
                                    Lscf(gp,f)=Lscf(gp,f)+w3*Lsc(i)
                                    Lssf(gp,f)=Lssf(gp,f)+w3*Lss(i)
                              end if
                        else
                              tj=igp3fv(ie)
                              r=rgp3fv(ie)
                              wtj(r,tj)=wtj(r,tj)+w3
                              if (deform) then
                                    Lctj(r,tj)=Lctj(r,tj)+w3*Lc(i)
                                    Lcctj(r,tj)=Lcctj(r,tj)+w3*Lcc(i)
                                    Lcstj(r,tj)=Lcstj(r,tj)+w3*Lcs(i)
                                    Lstj(r,tj)=Lstj(r,tj)+w3*Ls(i)
                                    Lsctj(r,tj)=Lsctj(r,tj)+w3*Lsc(i)
                                    Lsstj(r,tj)=Lsstj(r,tj)+w3*Lss(i)
                              end if
                        end if
                  end if
            end if
      end do
      do i=1,ngp
            if (wgp(i).ne.0.0d0) then
                  Lcgp(i)=Lcgp(i)/wgp(i)
                  Lccgp(i)=Lccgp(i)/wgp(i)
                  Lcsgp(i)=Lcsgp(i)/wgp(i)
                  Lsgp(i)=Lsgp(i)/wgp(i)
                  Lscgp(i)=Lscgp(i)/wgp(i)
                  Lssgp(i)=Lssgp(i)/wgp(i)
            end if
      end do
      if (nf.gt.0) then
      do i=1,nf
            do j=0,nfs(i)
                  if (wf(j,i).ne.0.0d0) then
                        Lcf(j,i)=Lcf(j,i)/wf(j,i)
                        Lccf(j,i)=Lccf(j,i)/wf(j,i)
                        Lcsf(j,i)=Lcsf(j,i)/wf(j,i)
                        Lsf(j,i)=Lsf(j,i)/wf(j,i)
                        Lscf(j,i)=Lscf(j,i)/wf(j,i)
                        Lssf(j,i)=Lssf(j,i)/wf(j,i)
                  end if
            end do
      end do
      end if
      if (ntj.gt.0) then
      do i=1,ntj
            do j=1,2
                  if (wtj(j,i).ne.0.0d0) then
                        Lctj(j,i)=Lctj(j,i)/wtj(j,i)
                        Lcctj(j,i)=Lcctj(j,i)/wtj(j,i)
                        Lcstj(j,i)=Lcstj(j,i)/wtj(j,i)
                        Lstj(j,i)=Lstj(j,i)/wtj(j,i)
                        Lsctj(j,i)=Lsctj(j,i)/wtj(j,i)
                        Lsstj(j,i)=Lsstj(j,i)/wtj(j,i)
                  end if
            end do
      end do
      end if
c
      do i=1,ne
            if (iefv(i).eq.0) then
                  gp=gp1e(i)
                  Lcgpe(1)=Lcgp(gp)
                  Lccgpe(1)=Lccgp(gp)
                  Lcsgpe(1)=Lcsgp(gp)
                  Lsgpe(1)=Lsgp(gp)
                  Lscgpe(1)=Lscgp(gp)
                  Lssgpe(1)=Lssgp(gp)
                  gp=gp2e(i)
                  Lcgpe(2)=Lcgp(gp)
                  Lccgpe(2)=Lccgp(gp)
                  Lcsgpe(2)=Lcsgp(gp)
                  Lsgpe(2)=Lsgp(gp)
                  Lscgpe(2)=Lscgp(gp)
                  Lssgpe(2)=Lssgp(gp)
                  gp=gp3e(i)
                  Lcgpe(3)=Lcgp(gp)
                  Lccgpe(3)=Lccgp(gp)
                  Lcsgpe(3)=Lcsgp(gp)
                  Lsgpe(3)=Lsgp(gp)
                  Lscgpe(3)=Lscgp(gp)
                  Lssgpe(3)=Lssgp(gp)
            end if
            if (iefv(i).ne.0) then
                  ie=iefv(i)
                  if ((rgp1fv(ie).eq.0).or.
     1                  (kgp1fv(ie).eq.2).or.
     2                  (kgp1fv(ie).eq.4).or.
     3                  ((kgp1fv(ie).eq.5).and.(rgp1fv(ie).eq.2)).or.
     4                  ((kgp1fv(ie).eq.6).and.(rgp1fv(ie).eq.1)).or.
     5                  (kgp1fv(ie).eq.7).or.
     6                  ((kgp1fv(ie).eq.8).and.(rgp1fv(ie).eq.1)).or.
     7                  (kgp1fv(ie).eq.9)) then
                        gp=gp1e(i)
                        Lcgpe(1)=Lcgp(gp)
                        Lccgpe(1)=Lccgp(gp)
                        Lcsgpe(1)=Lcsgp(gp)
                        Lsgpe(1)=Lsgp(gp)
                        Lscgpe(1)=Lscgp(gp)
                        Lssgpe(1)=Lssgp(gp)
                  else
                        if (kgp1fv(ie).ne.3) then
                              ip=ipf(gp1e(i))
                              f=fp(jfp(ip),ip)

                              gp=gpfp(jfp(ip),ip)
                              Lcgpe(1)=Lcf(gp,f)
                              Lccgpe(1)=Lccf(gp,f)
                              Lcsgpe(1)=Lcsf(gp,f)
                              Lsgpe(1)=Lsf(gp,f)
                              Lscgpe(1)=Lscf(gp,f)
                              Lssgpe(1)=Lssf(gp,f)
                        else
                              tj=igp1fv(ie)
                              r=rgp1fv(ie)
                              Lcgpe(1)=Lctj(r,tj)
                              Lccgpe(1)=Lcctj(r,tj)
                              Lcsgpe(1)=Lcstj(r,tj)
                              Lsgpe(1)=Lstj(r,tj)
                              Lscgpe(1)=Lsctj(r,tj)
                              Lssgpe(1)=Lsstj(r,tj)
                        end if
                  end if
                  if ((rgp2fv(ie).eq.0).or.
     1                  (kgp2fv(ie).eq.2).or.
     2                  (kgp2fv(ie).eq.4).or.
     3                  ((kgp2fv(ie).eq.5).and.(rgp2fv(ie).eq.2)).or.
     4                  ((kgp2fv(ie).eq.6).and.(rgp2fv(ie).eq.1)).or.
     5                  (kgp2fv(ie).eq.7).or.
     6                  ((kgp2fv(ie).eq.8).and.(rgp2fv(ie).eq.1)).or.
     7                  (kgp2fv(ie).eq.9)) then
                        gp=gp2e(i)
                        Lcgpe(2)=Lcgp(gp)
                        Lccgpe(2)=Lccgp(gp)
                        Lcsgpe(2)=Lcsgp(gp)
                        Lsgpe(2)=Lsgp(gp)
                        Lscgpe(2)=Lscgp(gp)
                        Lssgpe(2)=Lssgp(gp)
                  else
                        if (kgp2fv(ie).ne.3) then
                              ip=ipf(gp2e(i))
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              Lcgpe(2)=Lcf(gp,f)
                              Lccgpe(2)=Lccf(gp,f)
                              Lcsgpe(2)=Lcsf(gp,f)
                              Lsgpe(2)=Lsf(gp,f)
                              Lscgpe(2)=Lscf(gp,f)
                              Lssgpe(2)=Lssf(gp,f)
                        else
                              tj=igp2fv(ie)
                              r=rgp2fv(ie)
                              Lcgpe(2)=Lctj(r,tj)
                              Lccgpe(2)=Lcctj(r,tj)
                              Lcsgpe(2)=Lcstj(r,tj)
                              Lsgpe(2)=Lstj(r,tj)
                              Lscgpe(2)=Lsctj(r,tj)
                              Lssgpe(2)=Lsstj(r,tj)
                        end if
                  end if
                  if ((rgp3fv(ie).eq.0).or.
     1                  (kgp3fv(ie).eq.2).or.
     2                  (kgp3fv(ie).eq.4).or.
     3                  ((kgp3fv(ie).eq.5).and.(rgp3fv(ie).eq.2)).or.
     4                  ((kgp3fv(ie).eq.6).and.(rgp3fv(ie).eq.1)).or.
     5                  (kgp3fv(ie).eq.7).or.
     6                  ((kgp3fv(ie).eq.8).and.(rgp3fv(ie).eq.1)).or.
     7                  (kgp3fv(ie).eq.9)) then
                        gp=gp3e(i)
                        Lcgpe(3)=Lcgp(gp)
                        Lccgpe(3)=Lccgp(gp)
                        Lcsgpe(3)=Lcsgp(gp)
                        Lsgpe(3)=Lsgp(gp)
                        Lscgpe(3)=Lscgp(gp)

                        Lssgpe(3)=Lssgp(gp)
                  else
                        if (kgp3fv(ie).ne.3) then
                              ip=ipf(gp3e(i))
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              Lcgpe(3)=Lcf(gp,f)
                              Lccgpe(3)=Lccf(gp,f)
                              Lcsgpe(3)=Lcsf(gp,f)
                              Lsgpe(3)=Lsf(gp,f)
                              Lscgpe(3)=Lscf(gp,f)
                              Lssgpe(3)=Lssf(gp,f)
                        else
                              tj=igp3fv(ie)
                              r=rgp3fv(ie)
                              Lcgpe(3)=Lctj(r,tj)
                              Lccgpe(3)=Lcctj(r,tj)
                              Lcsgpe(3)=Lcstj(r,tj)
                              Lsgpe(3)=Lstj(r,tj)
                              Lscgpe(3)=Lsctj(r,tj)
                              Lssgpe(3)=Lsstj(r,tj)
                        end if
                  end if
            end if
            if ((Lcgpe(1)+Lcgpe(2)+Lcgpe(3).eq.0).or.
     1            (Lsgpe(1)+Lsgpe(2)+Lsgpe(3).eq.0)) then
                  write(*,*) 'i=',i
                  write(*,*) 'gp1,iprb=',gp1e(i),iprb(gp1e(i))
                  write(*,*) 'gp2,iprb=',gp2e(i),iprb(gp2e(i))
                  write(*,*) 'gp3,iprb=',gp3e(i),iprb(gp3e(i))
                  stop 'This element has zero strain-rate capacity'
            end if
c
            do gp=1,3
                  L(gp,1,1)=0.5d0*(Lcgpe(gp)+Lccgpe(gp))
     1                  +0.125d0*(Lsgpe(gp)-Lscgpe(gp))
                  L(gp,2,2)=0.5d0*(Lcgpe(gp)-Lccgpe(gp))
     1                  +0.125d0*(Lsgpe(gp)-Lscgpe(gp))
                  L(gp,3,3)=0.25d0*Lcgpe(gp)
     1                  +0.125d0*(Lsgpe(gp)+Lscgpe(gp))
                  L(gp,1,2)=-0.125d0*(Lsgpe(gp)-Lscgpe(gp))
                  L(gp,1,3)=-0.25d0*Lcsgpe(gp)+0.125d0*Lssgpe(gp)
                  L(gp,2,3)=-0.25d0*Lcsgpe(gp)-0.125d0*Lssgpe(gp)
                  L(gp,2,1)=L(gp,1,2)
                  L(gp,3,1)=L(gp,1,3)
                  L(gp,3,2)=L(gp,2,3)
            end do
            Tpot(1,1)=sxxpot(gp1e(i))
            Tpot(1,2)=syypot(gp1e(i))
            Tpot(1,3)=2.0d0*sxypot(gp1e(i))
            Tpot(2,1)=sxxpot(gp2e(i))
            Tpot(2,2)=syypot(gp2e(i))
            Tpot(2,3)=2.0d0*sxypot(gp2e(i))
            Tpot(3,1)=sxxpot(gp3e(i))
            Tpot(3,2)=syypot(gp3e(i))
            Tpot(3,3)=2.0d0*sxypot(gp3e(i))
            do gp=1,3
            do j=1,3
                  do k=1,3
                        LTpot(gp,j,k)=L(gp,k,1)*Tpot(j,1)
     1                        +L(gp,k,2)*Tpot(j,2)
     2                        +L(gp,k,3)*Tpot(j,3)
                  end do
            end do
            end do
c
            do j=1,3
            do k=1,3
                  Le(k,j)=0.0d0
            end do
            end do
            do iq=0,3
                  do j=1,3
                  do k=1,3
                        Lq(k,j)=0.0d0
                        do gp=1,3
                              Lq(k,j)=Lq(k,j)
     1                              +area1(iq,gp,i)*L(gp,k,j)
                        end do
                        Le(k,j)=Le(k,j)+Lq(k,j)
                  end do
                  end do
                  do k=1,3
                        epot(iq,k,i)=0.0d0
                        do gp=1,3
                        do j=1,3
                              epot(iq,k,i)=epot(iq,k,i)
     1                              +area2(iq,gp,j,i)*LTpot(gp,j,k)
                        end do
                        end do
                  end do
                  call choles(3,3,Lq)
                  do j=1,3
                  do k=1,3
                        seesub(iq,k,j,i)=Lq(k,j)
                  end do
                  end do
                  epot(iq,1,i)=epot(iq,1,i)/Lq(1,1)
                  epot(iq,2,i)=(epot(iq,2,i)
     1                  -Lq(2,1)*epot(iq,1,i))/Lq(2,2)
                  epot(iq,3,i)=(epot(iq,3,i)
     1                  -Lq(3,1)*epot(iq,1,i)
     1                  -Lq(3,2)*epot(iq,2,i))/Lq(3,3)
                  do j=0,8
                  do ic=1,2
                        esub(iq,1,ic,j,i)=esub(iq,1,ic,j,i)/Lq(1,1)
                        esub(iq,2,ic,j,i)=(esub(iq,2,ic,j,i)
     1                        -Lq(2,1)*esub(iq,1,ic,j,i))/Lq(2,2)
                        esub(iq,3,ic,j,i)=(esub(iq,3,ic,j,i)
     1                        -Lq(3,1)*esub(iq,1,ic,j,i)
     1                        -Lq(3,2)*esub(iq,2,ic,j,i))/Lq(3,3)
                  end do
                  end do
            end do
            Lce(i)=0.0d0
            Lcce(i)=0.0d0
            Lcse(i)=0.0d0
            Lse(i)=0.0d0
            Lsce(i)=0.0d0
            Lsse(i)=0.0d0
            do gp=1,3
                  areagp=0.0d0
                  do iq=0,3
                        areagp=areagp+area1(iq,gp,i)
                  end do
                  Lce(i)=Lce(i)+areagp*Lcgpe(gp)
                  Lcce(i)=Lcce(i)+areagp*Lccgpe(gp)
                  Lcse(i)=Lcse(i)+areagp*Lcsgpe(gp)
                  Lse(i)=Lse(i)+areagp*Lsgpe(gp)
                  Lsce(i)=Lsce(i)+areagp*Lscgpe(gp)
                  Lsse(i)=Lsse(i)+areagp*Lssgpe(gp)
            end do
            Lce(i)=Lce(i)/area(i)
            Lcce(i)=Lcce(i)/area(i)
            Lcse(i)=Lcse(i)/area(i)
            Lse(i)=Lse(i)/area(i)
            Lsce(i)=Lsce(i)/area(i)
            Lsse(i)=Lsse(i)/area(i)
            do j=1,3
            do k=1,3
                  Le(k,j)=Le(k,j)/area(i)
            end do
            end do
            call choles(3,3,Le)
            do j=0,8
            do ic=1,2
                  sxx(ic,j,i)=exx(ic,j,i)/Le(1,1)
                  syy(ic,j,i)=(eyy(ic,j,i)
     1                  -Le(2,1)*sxx(ic,j,i))/Le(2,2)
                  sxy(ic,j,i)=(exy(ic,j,i)
     1                  -Le(3,1)*sxx(ic,j,i)
     1                  -Le(3,2)*syy(ic,j,i))/Le(3,3)
                  sxy(ic,j,i)=sxy(ic,j,i)/Le(3,3)
                  syy(ic,j,i)=(syy(ic,j,i)
     1                  -Le(3,2)*sxy(ic,j,i))/Le(2,2)
                  sxx(ic,j,i)=(sxx(ic,j,i)
     1                  -Le(2,1)*syy(ic,j,i)
     1                  -Le(3,1)*sxy(ic,j,i))/Le(1,1)
                  sxy(ic,j,i)=0.5d0*sxy(ic,j,i)
            end do
            end do
      end do
c
      if (nrb.gt.0) then
      do i=1,nrb
            alat=plat(i)*pi/180.0d0
            along=plong(i)*pi/180.0d0
            wx=prate(i)*dcos(alat)*dcos(along)
            wy=prate(i)*dcos(alat)*dsin(along)
            wz=prate(i)*dsin(alat)
            do j=0,nrbs(i)
                  gp=gprb(j,i)
                  along=long(gp)*pi/180.0d0
                  alat=lat(gp)*pi/180.0d0
                  wlong=-wx*dsin(along)+wy*dcos(along)
                  wlat=-(wx*dcos(along)+wy*dsin(along))*

     1                  dsin(alat)+wz*dcos(alat)
                  wr=(wx*dcos(along)+wy*dsin(along))*
     1                  dcos(alat)+wz*dsin(alat)
                  ugprb(1,0,j,i)=rE*wlat
                  ugprb(1,1,j,i)=-wlong*dsin(alat)
                  ugprb(1,2,j,i)=-wr
                  ugprb(2,0,j,i)=-rE*wlong
                  ugprb(2,1,j,i)=wr*dcos(alat)-wlat*dsin(alat)
                  ugprb(2,2,j,i)=0.0d0
                  if (ipvl(gp).ne.0) then
                        ip=ipvl(gp)
                        vl=vlp(1,ip)
                        if (ipf(gp).eq.0) then
                              k=2*gpvlp(1,ip)+1
                              ux(k,vl)=rE*wlat
                              uy(k,vl)=-rE*wlong
                        else
                              if (rgprb(j,i).le.1) then
                                    k=gpvlp(1,ip)
                                    uxp(k,vl)=rE*wlat
                                    uyp(k,vl)=-rE*wlong
                              else
                                    k=gpvlp(1,ip)
                                    uxm(k,vl)=rE*wlat
                                    uym(k,vl)=-rE*wlong
                              end if
                        end if
                  end if
            end do
c
            do j=1,nrbs(i)
                  x1=lconv*long(gprb(j-1,i))
                  y1=lconv*lat(gprb(j-1,i))
                  x2=lconv*long(gprb(j,i))
                  y2=lconv*lat(gprb(j,i))
                  pinurb(0,j,i)=0.5d0
                  pinurb(1,j,i)=0.125d0*(x2-x1)
                  pinurb(2,j,i)=0.125d0*(y2-y1)
                  pinurb(3,j,i)=0.5d0
                  pinurb(4,j,i)=0.125d0*(x1-x2)
                  pinurb(5,j,i)=0.125d0*(y1-y2)
                  usrb(1,j,i)=pinurb(0,j,i)*ugprb(1,0,j-1,i)
     1                  +pinurb(1,j,i)*ugprb(1,1,j-1,i)
     2                  +pinurb(2,j,i)*ugprb(1,2,j-1,i)
     3                  +pinurb(3,j,i)*ugprb(1,0,j,i)
     4                  +pinurb(4,j,i)*ugprb(1,1,j,i)
     5                  +pinurb(5,j,i)*ugprb(1,2,j,i)
                  usrb(2,j,i)=pinurb(0,j,i)*ugprb(2,0,j-1,i)
     1                  +pinurb(1,j,i)*ugprb(2,1,j-1,i)
     2                  +pinurb(2,j,i)*ugprb(2,2,j-1,i)
     3                  +pinurb(3,j,i)*ugprb(2,0,j,i)
     4                  +pinurb(4,j,i)*ugprb(2,1,j,i)
     5                  +pinurb(5,j,i)*ugprb(2,2,j,i)
            end do
      end do
      end if
c
      if (nvl.gt.0) then
      do i=1,nvl
            do j=1,nvls(i)
                  x1=lconv*long(gpvl(j-1,i))
                  y1=lconv*lat(gpvl(j-1,i))
                  x2=lconv*long(gpvl(j,i))
                  y2=lconv*lat(gpvl(j,i))
                  pinuvl(0,j,i)=0.5d0
                  pinuvl(1,j,i)=0.125d0*(x2-x1)
                  pinuvl(2,j,i)=0.125d0*(y2-y1)
                  pinuvl(3,j,i)=0.5d0
                  pinuvl(4,j,i)=0.125d0*(x1-x2)
                  pinuvl(5,j,i)=0.125d0*(y1-y2)
            end do
c
            do j=0,nvls(i)
                  gp=gpvl(j,i)
                  longvl(j)=lconv*long(gp)
                  latvl(j)=lconv*lat(gp)
                  sxxvl(j)=sxxpot(gp)
                  syyvl(j)=syypot(gp)
                  sxyvl(j)=sxypot(gp)
                  Lcvl(j)=Lcgp(gp)
                  Lccvl(j)=Lccgp(gp)
                  Lcsvl(j)=Lcsgp(gp)
                  Lsvl(j)=Lsgp(gp)
                  Lscvl(j)=Lscgp(gp)
                  Lssvl(j)=Lssgp(gp)
                  if (ipf(gp).ne.0) then
                        ip=ipf(gp)
                        f=fp(jfp(ip),ip)
                        k=gpfp(jfp(ip),ip)
                        Lcfvl(j)=Lcf(k,f)
                        Lccfvl(j)=Lccf(k,f)
                        Lcsfvl(j)=Lcsf(k,f)
                        Lsfvl(j)=Lsf(k,f)
                        Lscfvl(j)=Lscf(k,f)
                        Lssfvl(j)=Lssf(k,f)
                        Kcfvl(j)=Kcfp(k,f)
                        Ksfvl(j)=Ksfp(k,f)
                        if (k.eq.0) then
                              x1=lconv*long(gp)
                              y1=lconv*lat(gp)
                              x2=lconv*long(gpf(1,f))
                              y2=lconv*lat(gpf(1,f))
                              cosy=dcos(y1/rE)
                              tx=(x2-x1)*cosy
                              ty=y2-y1
                              det=dsqrt(tx**2+ty**2)
                              tx=tx/det
                              ty=ty/det
                              txfvl(j)=tx
                              tyfvl(j)=ty
                        end if
                        if ((k.gt.0).and.(k.lt.nfs(f))) then
                              x1=lconv*long(gpf(k-1,f))
                              y1=lconv*lat(gpf(k-1,f))
                              x2=lconv*long(gp)
                              y2=lconv*lat(gp)
                              cosy=dcos(y2/rE)
                              tx=(x2-x1)*cosy
                              ty=y2-y1
                              det=dsqrt(tx**2+ty**2)
                              tx=tx/det
                              ty=ty/det
                              txfvl(j)=tx
                              tyfvl(j)=ty
                              x1=lconv*long(gp)
                              y1=lconv*lat(gp)
                              x2=lconv*long(gpf(k+1,f))
                              y2=lconv*lat(gpf(k+1,f))
                              cosy=dcos(y1/rE)
                              tx=(x2-x1)*cosy
                              ty=y2-y1
                              det=dsqrt(tx**2+ty**2)
                              tx=tx/det
                              ty=ty/det
                              tx=tx+txfvl(j)
                              ty=ty+tyfvl(j)
                              det=dsqrt(tx**2+ty**2)
                              tx=tx/det
                              ty=ty/det
                              txfvl(j)=tx
                              tyfvl(j)=ty
                        end if
                        if (k.eq.nfs(f)) then
                              x1=lconv*long(gpf(k-1,f))
                              y1=lconv*lat(gpf(k-1,f))
                              x2=lconv*long(gp)
                              y2=lconv*lat(gp)
                              cosy=dcos(y2/rE)
                              tx=(x2-x1)*cosy
                              ty=y2-y1
                              det=dsqrt(tx**2+ty**2)
                              tx=tx/det
                              ty=ty/det
                              txfvl(j)=tx
                              tyfvl(j)=ty
                        end if
                  end if
            end do
c
            j=1
10          if (interp(2*j,i)) then
                  j0=j-1
20                jn=j
                  if (jn.lt.nvls(i)) then
                        if (interp(2*j+1,i)) then
                            j=j+1
                            goto 20
                        end if
                  end if
                  addf0=(j0.eq.0).and.fillf0(i)
                  addfn=(jn.eq.nvls(i)).and.fillfn(i)
                  call fillvl(j0,jn,fvl(0,i),addf0,addfn,
     1                  rE,longvl,latvl,sxxvl,syyvl,sxyvl,
     2                  Lcvl,Lccvl,Lcsvl,Lsvl,Lscvl,Lssvl,
     3                  Lcfvl,Lccfvl,Lcsfvl,Lsfvl,Lscfvl,Lssfvl,
     4                  Kcfvl,Ksfvl,txfvl,tyfvl,ux(1,i),uy(1,i),
     5                  uxm(0,i),uym(0,i),uxp(0,i),uyp(0,i))
            end if
            j=j+1
            if (j.le.nvls(i)) goto 10
c
            do j=0,nvls(i)
                  gp=gpvl(j,i)
                  if (nvlp(ipvl(gp)).eq.2) then
                        ip=ipvl(gp)
                        vl=vlp(2,ip)
                        if (vl.ne.i) then
                              k=2*gpvlp(2,ip)+1
                              ux(k,vl)=ux(2*j+1,i)
                              uy(k,vl)=uy(2*j+1,i)
                        end if
                  end if
                  if (ipf(gp).ne.0) then
                        ip=ipf(gp)
                        f=fp(jfp(ip),ip)
                        k=gpfp(jfp(ip),ip)
                        tx=txfvl(j)
                        ty=tyfvl(j)
                        nx=-ty
                        ny=tx
                        dut(k,f)=tx*(uxp(j,i)-uxm(j,i))
     1                        +ty*(uyp(j,i)-uym(j,i))
                        dun(k,f)=nx*(uxp(j,i)-uxm(j,i))
     1                        +ny*(uyp(j,i)-uym(j,i))
                  end if
            end do
      end do
      end if
c
      if (nvo.gt.0) then
      do i=1,nvo
            x1=lconv*long(gp1e(evo(i)))
            y1=lconv*lat(gp1e(evo(i)))
            x2=lconv*long(gp2e(evo(i)))
            y2=lconv*lat(gp2e(evo(i)))
            x3=lconv*long(gp3e(evo(i)))
            y3=lconv*lat(gp3e(evo(i)))
            p1=w1vo(i)
            p2=w2vo(i)
            p3=w3vo(i)
            uvoval(0)=3.0d0*p1**2-2.0d0*p1**3+2.0d0*p1*p2*p3
            uvoval(1)=(x2-x1)*(p2*p1**2+0.5d0*p1*p2*p3)
     1            +(x3-x1)*(p3*p1**2+0.5d0*p1*p2*p3)
            uvoval(2)=(y2-y1)*(p2*p1**2+0.5d0*p1*p2*p3)
     1            +(y3-y1)*(p3*p1**2+0.5d0*p1*p2*p3)
            uvoval(3)=3.0d0*p2**2-2.0d0*p2**3+2.0d0*p1*p2*p3
            uvoval(4)=(x3-x2)*(p3*p2**2+0.5d0*p1*p2*p3)
     1            +(x1-x2)*(p1*p2**2+0.5d0*p1*p2*p3)
            uvoval(5)=(y3-y2)*(p3*p2**2+0.5d0*p1*p2*p3)
     1            +(y1-y2)*(p1*p2**2+0.5d0*p1*p2*p3)
            uvoval(6)=3.0d0*p3**2-2.0d0*p3**3+2.0d0*p1*p2*p3
            uvoval(7)=(x1-x3)*(p1*p3**2+0.5d0*p1*p2*p3)
     1            +(x2-x3)*(p2*p3**2+0.5d0*p1*p2*p3)
            uvoval(8)=(y1-y3)*(p1*p3**2+0.5d0*p1*p2*p3)
     1            +(y2-y3)*(p2*p3**2+0.5d0*p1*p2*p3)
            uvoval(9)=0.0d0
            V(1,1)=seoux(i)**2

            V(1,2)=seoux(i)*seouy(i)*rouxuy(i)
            V(2,1)=V(1,2)
            V(2,2)=seouy(i)**2
            call choles(2,3,V)
            do j=1,2
            do k=1,2
                  seou(k,j,i)=V(k,j)
            end do
            end do
            ou(1,i)=oux(i)/V(1,1)
            ou(2,i)=(ouy(i)-V(2,1)*ou(1,i))/V(2,2)
            do j=0,8
                  uvo(1,1,j,i)=uvoval(j)/V(1,1)
                  uvo(1,2,j,i)=0.0d0
                  uvo(2,1,j,i)=-V(2,1)*uvo(1,1,j,i)/V(2,2)
                  uvo(2,2,j,i)=uvoval(j)/V(2,2)
            end do
      end do
      end if
c
      if (nfo.gt.0) then
      do i=1,nfo
            V(1,1)=seoduc(1,i)**2
            if (nfoc(i).eq.2) then
                  V(1,2)=seoduc(1,i)*seoduc(2,i)*rfo12(i)
                  V(2,1)=V(1,2)
                  V(2,2)=seoduc(2,i)**2
            end if
            call choles(nfoc(i),3,V)
            do j=1,nfoc(i)
            do k=1,nfoc(i)
                  seodu(k,j,i)=V(k,j)
            end do
            end do
            f=ffo(i)
            j=sfs(isf(sfo(i)))
            odu(1,i)=oduc(1,i)/V(1,1)
            do k=0,1
                  ducfo(1,1,k,i)=codut(1,i)*dufs(k,j,f)/V(1,1)
                  ducfo(1,2,k,i)=codun(1,i)*dufs(k,j,f)/V(1,1)
            end do
            if (nfoc(i).eq.2) then
                  odu(2,i)=(oduc(2,i)-V(2,1)*odu(1,i))/V(2,2)
                  do k=0,1
                        ducfo(2,1,k,i)=(codut(2,i)*dufs(k,j,f)
     1                        -V(2,1)*ducfo(1,1,k,i))/V(2,2)
                        ducfo(2,2,k,i)=(codun(2,i)*dufs(k,j,f)
     1                        -V(2,1)*ducfo(1,2,k,i))/V(2,2)
                  end do
            end if
      end do
      end if
c
      if (neo.gt.0) then
      do i=1,neo
            V(1,1)=seoec(1,i)**2
            if (neoc(i).gt.1) then
                  V(1,2)=seoec(1,i)*seoec(2,i)*reo12(i)
                  V(2,1)=V(1,2)
                  V(2,2)=seoec(2,i)**2
            end if
            if (neoc(i).eq.3) then
                  V(1,3)=seoec(1,i)*seoec(3,i)*reo13(i)
                  V(3,1)=V(1,3)
                  V(2,3)=seoec(2,i)*seoec(3,i)*reo23(i)
                  V(3,2)=V(2,3)
                  V(3,3)=seoec(3,i)**2
            end if
            call choles(neoc(i),3,V)
            do j=1,neoc(i)
            do k=1,neoc(i)
                  seoe(k,j,i)=V(k,j)
            end do
            end do
            j=eeo(i)
            oe(1,i)=oec(1,i)/V(1,1)
            do k=0,8
            do ic=1,2
                  eceo(1,ic,k,i)=(coexx(1,i)*exx(ic,k,j)
     1                  +coeyy(1,i)*eyy(ic,k,j)
     2                  +coexy(1,i)*exy(ic,k,j))/V(1,1)
            end do
            end do
            if (neoc(i).gt.1) then
                  oe(2,i)=(oec(2,i)-V(2,1)*oe(1,i))/V(2,2)
                  do k=0,8
                  do ic=1,2
                        eceo(2,ic,k,i)=(coexx(2,i)*exx(ic,k,j)
     1                        +coeyy(2,i)*eyy(ic,k,j)
     2                        +coexy(2,i)*exy(ic,k,j)
     3                        -V(2,1)*eceo(1,ic,k,i))/V(2,2)
                  end do
                  end do
            end if
            if (neoc(i).eq.3) then
                  oe(3,i)=(oec(3,i)-V(3,1)*oe(1,i)
     1                  -V(3,2)*oe(2,i))/V(3,3)
                  do k=0,8
                  do ic=1,2
                        eceo(3,ic,k,i)=(coexx(3,i)*exx(ic,k,j)
     1                        +coeyy(3,i)*eyy(ic,k,j)
     2                        +coexy(3,i)*exy(ic,k,j)
     3                        -V(3,1)*eceo(1,ic,k,i)
     4                        -V(3,2)*eceo(2,ic,k,i))/V(3,3)
                  end do
                  end do
            end if
      end do
      end if
c
      if (nduc.gt.0) then
      do i=1,nduc
            do j=1,ncduc(i)
                  s1c=s1duc(j,i)
                  s2c=s2duc(j,i)
                  duc(j,i)=refduc(s1c,i)/scduc(s1c,i)
     1                  -refduc(s2c,i)/scduc(s2c,i)
                  f=fduc(i)
                  s1=sfs(isf(sduc(s1c,i)))
                  s2=sfs(isf(sduc(s2c,i)))
                  do k=0,1
                        dus1c(1,k,j,i)=cdut(s1c,i)*dufs(k,s1,f)
     1                        /scduc(s1c,i)
                        dus1c(2,k,j,i)=cdun(s1c,i)*dufs(k,s1,f)
     1                        /scduc(s1c,i)
                        dus2c(1,k,j,i)=-cdut(s2c,i)*dufs(k,s2,f)
     1                        /scduc(s2c,i)
                        dus2c(2,k,j,i)=-cdun(s2c,i)*dufs(k,s2,f)
     1                        /scduc(s2c,i)
                  end do
            end do
      end do
      end if
c
      if (nec.gt.0) then
      do i=1,nec
            do j=1,ncec(i)
                  e1c=e1ec(j,i)
                  e2c=e2ec(j,i)
                  ec(j,i)=refec(e1c,i)/scec(e1c,i)
     1                  -refec(e2c,i)/scec(e2c,i)
                  e1=eec(e1c,i)
                  e2=eec(e2c,i)
                  do k=0,8
                  do ic=1,2
                        ee1c(ic,k,j,i)=(cexx(e1c,i)*exx(ic,k,e1)
     1                        +ceyy(e1c,i)*eyy(ic,k,e1)
     2                        +cexy(e1c,i)*exy(ic,k,e1))/scec(e1c,i)
                        ee2c(ic,k,j,i)=-(cexx(e2c,i)*exx(ic,k,e2)
     1                        +ceyy(e2c,i)*eyy(ic,k,e2)
     2                        +cexy(e2c,i)*exy(ic,k,e2))/scec(e2c,i)
                  end do
                  end do
            end do
      end do
      end if
c
      return
      end
c
c
      SUBROUTINE evalue(rE,x1,y1,x2,y2,x3,y3,area0,area1,area2,
     1      nxsum,nysum,dysum,dxsum1,dxsum2,dxsum3,
     2      nysum1,nysum2,nysum3,dy1,dy2,dy3)
c
c This routine uses quadrature that is exact for up to 5th order
c  polynomials to evaluate the perimeter-line and area integrals needed
c  from each element, split into quarters. The quarter elements are
c  indexed, with the 0th being in the middle of the element and the
c  other three having the same indices as the grid points they are
c  adjacent to.
c The integrals are:
c   area0 = area_integral[cos(lat)*dlong*dlat],
c   area1(j) = area_integral[pj*cos(lat)*dlong*dlat] for j=1,2,3,
c   area2(j,k) = area_integral[pj*pk*cos(lat)*dlong*dlat] for j,k=1,2,3,
c   nxsum(f) = line_integral[nx*cf*dl],
c   nysum(f) = line_integral[ny*cf*cos(lat)*dl],
c   dysum(f) = area_integral[cdyf*cos(lat)*dlong*dlat],
c  where p1, p2 and p3 are the internal element coordinates, the normal
c  vector (nx,ny) and the line length dl are in the underlying planar
c  (longitude,latitude) space, and cf and cdyf are coefficient values
c  and y (latitudinal) derivatives of the coefficient values for
c  function contributions f in the Hermite elements.
c The ten function contributions are:
c   0 = function value at grid point 1,
c   1 = x (longitudinal) derivative at grid point 1,
c   2 = y (latitudinal) derivative at grid point 1,
c   3 = function value at grid point 2,
c   4 = x (longitudinal) derivative at grid point 2,
c   5 = y (latitudinal) derivative at grid point 2,
c   6 = function value at grid point 3,
c   7 = x (longitudinal) derivative at grid point 3,
c   8 = y (latitudinal) derivative at grid point 3,
c   9 = function value at the middle point of the element.
c The remaining quantities returned, dxsum1, dxsum2, dxsum3, nysum1,
c  nysum2, nysum3, dy1, dy2 and dy3, are for calculating average
c  derivative values for linear functions, with the specific use being
c  getting average values of the forces given the stress potentials
c  sxxpot, syypot and sxypot.
c
      implicit none
      integer i,j,k,gp1,gp2
      real*8 rE,x1,y1,x2,y2,x3,y3
      real*8 dxsum1,dxsum2,dxsum3,nysum1,nysum2,nysum3,dy1,dy2,dy3
      real*8 det,dx1,dx2,dx3,nxl1,nyl1,nxl2,nyl2,nxl3,nyl3
      real*8 dxdyq,nxlj,nylj,p1,p2,p3,y,cosy
      real*8 area0(0:3),area1(0:3,3),area2(0:3,3,3)
      real*8 nxsum(0:3,0:9),nysum(0:3,0:9),dysum(0:3,0:9)
      real*8 ws(3),ps(3,2),we(7),pe(7,3),pq(0:3,3,3)
      real*8 nxlq(0:3,3),nylq(0:3,3)
c
cf2py intent(out) area0,area1,area2,nxsum,nysum,dysum,dxsum1,dxsum2,dxsum3,nysum1,nysum2,nysum3,dy1,dy2,dy3
c
      ws(1)=0.277777777777778d0
      ws(2)=0.277777777777778d0
      ws(3)=0.444444444444444d0
      ps(1,1)=0.887298334620742d0
      ps(1,2)=0.112701665379258d0
      ps(2,1)=0.112701665379258d0
      ps(2,2)=0.887298334620742d0
      ps(3,1)=0.500000000000000d0
      ps(3,2)=0.500000000000000d0
      we(1)=0.132394152788506d0
      we(2)=0.132394152788506d0
      we(3)=0.132394152788506d0
      we(4)=0.125939180544827d0
      we(5)=0.125939180544827d0
      we(6)=0.125939180544827d0
      we(7)=0.225000000000000d0
      pe(1,1)=0.059715871789770d0
      pe(1,2)=0.470142064105115d0
      pe(1,3)=0.470142064105115d0
      pe(2,1)=0.470142064105115d0
      pe(2,2)=0.059715871789770d0
      pe(2,3)=0.470142064105115d0
      pe(3,1)=0.470142064105115d0
      pe(3,2)=0.470142064105115d0
      pe(3,3)=0.059715871789770d0
      pe(4,1)=0.797426985353087d0
      pe(4,2)=0.101286507323456d0
      pe(4,3)=0.101286507323456d0
      pe(5,1)=0.101286507323456d0
      pe(5,2)=0.797426985353087d0
      pe(5,3)=0.101286507323456d0
      pe(6,1)=0.101286507323456d0
      pe(6,2)=0.101286507323456d0
      pe(6,3)=0.797426985353087d0
      pe(7,1)=0.333333333333333d0
      pe(7,2)=0.333333333333333d0
      pe(7,3)=0.333333333333333d0
      pq(0,1,1)=0.0d0
      pq(0,1,2)=0.5d0
      pq(0,1,3)=0.5d0
      pq(0,2,1)=0.5d0
      pq(0,2,2)=0.0d0
      pq(0,2,3)=0.5d0
      pq(0,3,1)=0.5d0
      pq(0,3,2)=0.5d0
      pq(0,3,3)=0.0d0
      pq(1,1,1)=1.0d0
      pq(1,1,2)=0.0d0
      pq(1,1,3)=0.0d0
      pq(1,2,1)=0.5d0
      pq(1,2,2)=0.0d0
      pq(1,2,3)=0.5d0
      pq(1,3,1)=0.5d0
      pq(1,3,2)=0.5d0
      pq(1,3,3)=0.0d0
      pq(2,1,1)=0.0d0
      pq(2,1,2)=0.5d0
      pq(2,1,3)=0.5d0
      pq(2,2,1)=0.0d0
      pq(2,2,2)=1.0d0
      pq(2,2,3)=0.0d0
      pq(2,3,1)=0.5d0
      pq(2,3,2)=0.5d0
      pq(2,3,3)=0.0d0
      pq(3,1,1)=0.0d0
      pq(3,1,2)=0.5d0
      pq(3,1,3)=0.5d0
      pq(3,2,1)=0.5d0
      pq(3,2,2)=0.0d0
      pq(3,2,3)=0.5d0
      pq(3,3,1)=0.0d0
      pq(3,3,2)=0.0d0
      pq(3,3,3)=1.0d0
      det=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
      dx1=(y2-y3)/det
      dy1=(x3-x2)/det
      dx2=(y3-y1)/det
      dy2=(x1-x3)/det
      dx3=(y1-y2)/det
      dy3=(x2-x1)/det
      det=dabs(det)
      nxl1=-dx1*det
      nyl1=-dy1*det
      nxl2=-dx2*det
      nyl2=-dy2*det
      nxl3=-dx3*det
      nyl3=-dy3*det
      nxlq(0,1)=-0.5d0*nxl1
      nylq(0,1)=-0.5d0*nyl1
      nxlq(0,2)=-0.5d0*nxl2
      nylq(0,2)=-0.5d0*nyl2
      nxlq(0,3)=-0.5d0*nxl3
      nylq(0,3)=-0.5d0*nyl3
      nxlq(1,1)=0.5d0*nxl1
      nylq(1,1)=0.5d0*nyl1
      nxlq(1,2)=0.5d0*nxl3
      nylq(1,2)=0.5d0*nyl3
      nxlq(1,3)=0.5d0*nxl2
      nylq(1,3)=0.5d0*nyl2
      nxlq(2,1)=0.5d0*nxl3
      nylq(2,1)=0.5d0*nyl3
      nxlq(2,2)=0.5d0*nxl2
      nylq(2,2)=0.5d0*nyl2
      nxlq(2,3)=0.5d0*nxl1
      nylq(2,3)=0.5d0*nyl1
      nxlq(3,1)=0.5d0*nxl2
      nylq(3,1)=0.5d0*nyl2
      nxlq(3,2)=0.5d0*nxl1
      nylq(3,2)=0.5d0*nyl1
      nxlq(3,3)=0.5d0*nxl3
      nylq(3,3)=0.5d0*nyl3
      dxdyq=0.125d0*det
      dxsum1=0.5d0*det*dx1
      dxsum2=0.5d0*det*dx2
      dxsum3=0.5d0*det*dx3
      nysum1=0.0d0
      nysum2=0.0d0
      nysum3=0.0d0
      do i=0,3
            nxsum(i,0)=0.0d0
            nysum(i,0)=0.0d0
            nxsum(i,1)=0.0d0
            nysum(i,1)=0.0d0
            nxsum(i,2)=0.0d0
            nysum(i,2)=0.0d0
            nxsum(i,3)=0.0d0
            nysum(i,3)=0.0d0
            nxsum(i,4)=0.0d0
            nysum(i,4)=0.0d0
            nxsum(i,5)=0.0d0
            nysum(i,5)=0.0d0
            nxsum(i,6)=0.0d0
            nysum(i,6)=0.0d0
            nxsum(i,7)=0.0d0
            nysum(i,7)=0.0d0
            nxsum(i,8)=0.0d0
            nysum(i,8)=0.0d0
            nxsum(i,9)=0.0d0
            nysum(i,9)=0.0d0
            do j=1,3
                  if (j.eq.1) then
                        gp1=2
                        gp2=3
                  end if
                  if (j.eq.2) then
                        gp1=3
                        gp2=1
                  end if
                  if (j.eq.3) then
                        gp1=1
                        gp2=2
                  end if
                  nxlj=nxlq(i,j)
                  nylj=nylq(i,j)
                  do k=1,3
                        p1=ps(k,1)*pq(i,gp1,1)+ps(k,2)*pq(i,gp2,1)
                        p2=ps(k,1)*pq(i,gp1,2)+ps(k,2)*pq(i,gp2,2)
                        p3=ps(k,1)*pq(i,gp1,3)+ps(k,2)*pq(i,gp2,3)
                        y=p1*y1+p2*y2+p3*y3
                        cosy=dcos(y/rE)
                        nxsum(i,0)=nxsum(i,0)+ws(k)*nxlj*
     1                        (3.0d0*p1**2-2.0d0*p1**3+2.0d0*p1*p2*p3)
                        nysum(i,0)=nysum(i,0)+ws(k)*nylj*cosy*
     1                        (3.0d0*p1**2-2.0d0*p1**3+2.0d0*p1*p2*p3)
                        nxsum(i,1)=nxsum(i,1)+ws(k)*nxlj*
     1                        ((x2-x1)*(p2*p1**2+0.5d0*p1*p2*p3)
     2                        +(x3-x1)*(p3*p1**2+0.5d0*p1*p2*p3))
                        nysum(i,1)=nysum(i,1)+ws(k)*nylj*cosy*
     1                        ((x2-x1)*(p2*p1**2+0.5d0*p1*p2*p3)
     2                        +(x3-x1)*(p3*p1**2+0.5d0*p1*p2*p3))
                        nxsum(i,2)=nxsum(i,2)+ws(k)*nxlj*
     1                        ((y2-y1)*(p2*p1**2+0.5d0*p1*p2*p3)
     2                        +(y3-y1)*(p3*p1**2+0.5d0*p1*p2*p3))
                        nysum(i,2)=nysum(i,2)+ws(k)*nylj*cosy*
     1                        ((y2-y1)*(p2*p1**2+0.5d0*p1*p2*p3)
     2                        +(y3-y1)*(p3*p1**2+0.5d0*p1*p2*p3))
                        nxsum(i,3)=nxsum(i,3)+ws(k)*nxlj*
     1                        (3.0d0*p2**2-2.0d0*p2**3+2.0d0*p1*p2*p3)
                        nysum(i,3)=nysum(i,3)+ws(k)*nylj*cosy*
     1                        (3.0d0*p2**2-2.0d0*p2**3+2.0d0*p1*p2*p3)
                        nxsum(i,4)=nxsum(i,4)+ws(k)*nxlj*
     1                        ((x3-x2)*(p3*p2**2+0.5d0*p1*p2*p3)
     2                        +(x1-x2)*(p1*p2**2+0.5d0*p1*p2*p3))
                        nysum(i,4)=nysum(i,4)+ws(k)*nylj*cosy*
     1                        ((x3-x2)*(p3*p2**2+0.5d0*p1*p2*p3)
     2                        +(x1-x2)*(p1*p2**2+0.5d0*p1*p2*p3))
                        nxsum(i,5)=nxsum(i,5)+ws(k)*nxlj*
     1                        ((y3-y2)*(p3*p2**2+0.5d0*p1*p2*p3)
     2                        +(y1-y2)*(p1*p2**2+0.5d0*p1*p2*p3))
                        nysum(i,5)=nysum(i,5)+ws(k)*nylj*cosy*
     1                        ((y3-y2)*(p3*p2**2+0.5d0*p1*p2*p3)
     2                        +(y1-y2)*(p1*p2**2+0.5d0*p1*p2*p3))
                        nxsum(i,6)=nxsum(i,6)+ws(k)*nxlj*
     1                        (3.0d0*p3**2-2.0d0*p3**3+2.0d0*p1*p2*p3)
                        nysum(i,6)=nysum(i,6)+ws(k)*nylj*cosy*
     1                        (3.0d0*p3**2-2.0d0*p3**3+2.0d0*p1*p2*p3)
                        nxsum(i,7)=nxsum(i,7)+ws(k)*nxlj*
     1                        ((x1-x3)*(p1*p3**2+0.5d0*p1*p2*p3)
     2                        +(x2-x3)*(p2*p3**2+0.5d0*p1*p2*p3))
                        nysum(i,7)=nysum(i,7)+ws(k)*nylj*cosy*
     1                        ((x1-x3)*(p1*p3**2+0.5d0*p1*p2*p3)
     2                        +(x2-x3)*(p2*p3**2+0.5d0*p1*p2*p3))
                        nxsum(i,8)=nxsum(i,8)+ws(k)*nxlj*
     1                        ((y1-y3)*(p1*p3**2+0.5d0*p1*p2*p3)
     2                        +(y2-y3)*(p2*p3**2+0.5d0*p1*p2*p3))
                        nysum(i,8)=nysum(i,8)+ws(k)*nylj*cosy*
     1                        ((y1-y3)*(p1*p3**2+0.5d0*p1*p2*p3)
     2                        +(y2-y3)*(p2*p3**2+0.5d0*p1*p2*p3))
                        nysum1=nysum1+ws(k)*nylj*cosy*p1
                        nysum2=nysum2+ws(k)*nylj*cosy*p2
                        nysum3=nysum3+ws(k)*nylj*cosy*p3
                  end do
            end do
            area2(i,1,1)=0.0d0
            area2(i,1,2)=0.0d0
            area2(i,1,3)=0.0d0
            area2(i,2,2)=0.0d0
            area2(i,2,3)=0.0d0
            area2(i,3,3)=0.0d0
            dysum(i,0)=0.0d0

            dysum(i,1)=0.0d0
            dysum(i,2)=0.0d0
            dysum(i,3)=0.0d0
            dysum(i,4)=0.0d0
            dysum(i,5)=0.0d0
            dysum(i,6)=0.0d0
            dysum(i,7)=0.0d0
            dysum(i,8)=0.0d0
            dysum(i,9)=0.0d0
            do k=1,7
                  p1=pe(k,1)*pq(i,1,1)+pe(k,2)*pq(i,2,1)
     1                  +pe(k,3)*pq(i,3,1)
                  p2=pe(k,1)*pq(i,1,2)+pe(k,2)*pq(i,2,2)
     1                  +pe(k,3)*pq(i,3,2)
                  p3=pe(k,1)*pq(i,1,3)+pe(k,2)*pq(i,2,3)
     1                  +pe(k,3)*pq(i,3,3)
                  y=p1*y1+p2*y2+p3*y3
                  cosy=dcos(y/rE)
                  area2(i,1,1)=area2(i,1,1)+we(k)*cosy*dxdyq*p1**2
                  area2(i,1,2)=area2(i,1,2)+we(k)*cosy*dxdyq*p1*p2
                  area2(i,1,3)=area2(i,1,3)+we(k)*cosy*dxdyq*p1*p3
                  area2(i,2,2)=area2(i,2,2)+we(k)*cosy*dxdyq*p2**2
                  area2(i,2,3)=area2(i,2,3)+we(k)*cosy*dxdyq*p2*p3
                  area2(i,3,3)=area2(i,3,3)+we(k)*cosy*dxdyq*p3**2
                  dysum(i,0)=dysum(i,0)+we(k)*cosy*dxdyq*
     1                  (dy1*(6.0d0*p1-6.0d0*p1**2+2.0d0*p2*p3)
     2                  +dy2*(2.0d0*p1*p3)+dy3*(2.0d0*p1*p2))
                  dysum(i,1)=dysum(i,1)+we(k)*cosy*dxdyq*
     1                  (dy1*((x2-x1)*(2.0d0*p1*p2+0.5d0*p2*p3)
     2                  +(x3-x1)*(2.0d0*p1*p3+0.5d0*p2*p3))
     3                  +dy2*((x2-x1)*(p1**2+0.5d0*p1*p3)
     4                  +(x3-x1)*(0.5d0*p1*p3))
     5                  +dy3*((x2-x1)*(0.5d0*p1*p2)
     6                  +(x3-x1)*(p1**2+0.5d0*p1*p2)))
                  dysum(i,2)=dysum(i,2)+we(k)*cosy*dxdyq*
     1                  (dy1*((y2-y1)*(2.0d0*p1*p2+0.5d0*p2*p3)
     2                  +(y3-y1)*(2.0d0*p1*p3+0.5d0*p2*p3))
     3                  +dy2*((y2-y1)*(p1**2+0.5d0*p1*p3)
     4                  +(y3-y1)*(0.5d0*p1*p3))
     5                  +dy3*((y2-y1)*(0.5d0*p1*p2)
     6                  +(y3-y1)*(p1**2+0.5d0*p1*p2)))
                  dysum(i,3)=dysum(i,3)+we(k)*cosy*dxdyq*
     1                  (dy2*(6.0d0*p2-6.0d0*p2**2+2.0d0*p1*p3)
     2                  +dy3*(2.0d0*p1*p2)+dy1*(2.0d0*p2*p3))
                  dysum(i,4)=dysum(i,4)+we(k)*cosy*dxdyq*
     1                  (dy2*((x3-x2)*(2.0d0*p2*p3+0.5d0*p1*p3)
     2                  +(x1-x2)*(2.0d0*p1*p2+0.5d0*p1*p3))
     3                  +dy3*((x3-x2)*(p2**2+0.5d0*p1*p2)
     4                  +(x1-x2)*(0.5d0*p1*p2))
     5                  +dy1*((x3-x2)*(0.5d0*p2*p3)
     6                  +(x1-x2)*(p2**2+0.5d0*p2*p3)))
                  dysum(i,5)=dysum(i,5)+we(k)*cosy*dxdyq*
     1                  (dy2*((y3-y2)*(2.0d0*p2*p3+0.5d0*p1*p3)
     2                  +(y1-y2)*(2.0d0*p1*p2+0.5d0*p1*p3))
     3                  +dy3*((y3-y2)*(p2**2+0.5d0*p1*p2)
     4                  +(y1-y2)*(0.5d0*p1*p2))
     5                  +dy1*((y3-y2)*(0.5d0*p2*p3)
     6                  +(y1-y2)*(p2**2+0.5d0*p2*p3)))
                  dysum(i,6)=dysum(i,6)+we(k)*cosy*dxdyq*
     1                  (dy3*(6.0d0*p3-6.0d0*p3**2+2.0d0*p1*p2)
     2                  +dy1*(2.0d0*p2*p3)+dy2*(2.0d0*p1*p3))
                  dysum(i,7)=dysum(i,7)+we(k)*cosy*dxdyq*
     1                  (dy3*((x1-x3)*(2.0d0*p1*p3+0.5d0*p1*p2)
     2                  +(x2-x3)*(2.0d0*p2*p3+0.5d0*p1*p2))
     3                  +dy1*((x1-x3)*(p3**2+0.5d0*p2*p3)
     4                  +(x2-x3)*(0.5d0*p2*p3))
     5                  +dy2*((x1-x3)*(0.5d0*p1*p3)
     6                  +(x2-x3)*(p3**2+0.5d0*p1*p3)))
                  dysum(i,8)=dysum(i,8)+we(k)*cosy*dxdyq*
     1                  (dy3*((y1-y3)*(2.0d0*p1*p3+0.5d0*p1*p2)
     2                  +(y2-y3)*(2.0d0*p2*p3+0.5d0*p1*p2))
     3                  +dy1*((y1-y3)*(p3**2+0.5d0*p2*p3)
     4                  +(y2-y3)*(0.5d0*p2*p3))
     5                  +dy2*((y1-y3)*(0.5d0*p1*p3)
     6                  +(y2-y3)*(p3**2+0.5d0*p1*p3)))
            end do
            area2(i,2,1)=area2(i,1,2)
            area2(i,3,1)=area2(i,1,3)
            area2(i,3,2)=area2(i,2,3)
            area1(i,1)=area2(i,1,1)+area2(i,1,2)+area2(i,1,3)
            area1(i,2)=area2(i,2,1)+area2(i,2,2)+area2(i,2,3)
            area1(i,3)=area2(i,3,1)+area2(i,3,2)+area2(i,3,3)
            area0(i)=area1(i,1)+area1(i,2)+area1(i,3)
      end do
c
      return
      end
c
c
      SUBROUTINE fvalue(rE,x1,y1,x2,y2,len0,len1,
     1      len2xx,len2xy,len2yy,fsum,txsum,tysum)
c
c This routine uses quadrature that is exact for up to 5th order
c  polynomials to evaluate the line integrals needed from each fault
c  segment, split into halves. The half segments are indexed according
c  to the grid points they are adjacent to.
c The integrals are:
c   len0 = line_integral[dl],
c   len1(j) = line_integral[pj*dl] for j=1,2
c   len2xx(j,k) = line_integral[pj*pk*nx*nx*dl] for j,k=1,2
c   len2xy(j,k) = line_integral[pj*pk*nx*ny*dl] for j,k=1,2
c   len2yy(j,k) = line_integral[pj*pk*ny*ny*dl] for j,k=1,2
c   fsum(f) = line_integral[cf*dl],
c  where dl is the length on the surface of the Earth, p1 and p2 are the
c  internal element coordinates, and cf is the coefficient value for
c  slip-rate function contribution f in the Hermite (i.e. cubic spline)
c  elements.
c The four function contributions are:
c   0 = function value at grid point 1,
c   1 = derivative at grid point 1,
c   2 = function value at grid point 2,
c   3 = derivative at grid point 2.
c The other arrays that are returned are as follows.
c   txsum(f) = line_integral[cf*tx*dl],
c   tysum(f) = line_integral[cf*ty*dl],
c  where cf in these line-integral cases is the coefficient value for
c  velocity functions on the element edges on the two sides of the
c  fault, in terms of the following function contributions:
c   0 = function value at grid point 1,
c   1 = x (longitudinal) derivative at grid point 1,
c   2 = y (latitudinal) derivative at grid point 1,
c   3 = function value at grid point 2,
c   4 = x (longitudinal) derivative at grid point 2,
c   5 = y (latitudinal) derivative at grid point 2.
c  The sign convention is that the tangent vector points in the positive
c  direction from the beginning of the fault towards the end of the
c  fault, and the associated normal vector points to the positive side
c  of the fault (i.e. the right side when looking from the end towards
c  the beginning).
c
      implicit none
      integer i,k
      real*8 rE,x1,y1,x2,y2
      real*8 dx,dy,dl,dxh,dyh,p1,p2,y,cosy,dlh,tx,ty,nx,ny
      real*8 len0(2),len1(2,2),len2xx(2,2,2),
     1      len2xy(2,2,2),len2yy(2,2,2),fsum(2,0:1)
      real*8 txsum(2,0:5),tysum(2,0:5)
      real*8 ws(3),ps(3,2),ph(2,2,2)
c
cf2py intent(out) len0,len1,len2xx,len2xy,len2yy,fsum,txsum,tysum
c
      ws(1)=0.277777777777778d0
      ws(2)=0.277777777777778d0
      ws(3)=0.444444444444444d0
      ps(1,1)=0.887298334620742d0
      ps(1,2)=0.112701665379258d0
      ps(2,1)=0.112701665379258d0
      ps(2,2)=0.887298334620742d0
      ps(3,1)=0.500000000000000d0
      ps(3,2)=0.500000000000000d0
      ph(1,1,1)=1.0d0
      ph(1,1,2)=0.0d0
      ph(1,2,1)=0.5d0
      ph(1,2,2)=0.5d0
      ph(2,1,1)=0.5d0
      ph(2,1,2)=0.5d0
      ph(2,2,1)=0.0d0
      ph(2,2,2)=1.0d0
      dx=x2-x1
      dy=y2-y1
      dl=dsqrt(dx**2+dy**2)
      dxh=0.5*dx
      dyh=0.5*dy
      do i=1,2
            len1(i,1)=0.0d0
            len1(i,2)=0.0d0
            len2xx(i,1,1)=0.0d0
            len2xx(i,1,2)=0.0d0
            len2xx(i,2,2)=0.0d0
            len2xy(i,1,1)=0.0d0
            len2xy(i,1,2)=0.0d0
            len2xy(i,2,2)=0.0d0
            len2yy(i,1,1)=0.0d0
            len2yy(i,1,2)=0.0d0
            len2yy(i,2,2)=0.0d0
            fsum(i,0)=0.0d0
            fsum(i,1)=0.0d0
            txsum(i,0)=0.0d0
            tysum(i,0)=0.0d0
            txsum(i,1)=0.0d0
            tysum(i,1)=0.0d0
            txsum(i,2)=0.0d0
            tysum(i,2)=0.0d0
            txsum(i,3)=0.0d0
            tysum(i,3)=0.0d0
            txsum(i,4)=0.0d0
            tysum(i,4)=0.0d0
            txsum(i,5)=0.0d0


            tysum(i,5)=0.0d0
            do k=1,3
                  p1=ps(k,1)*ph(i,1,1)+ps(k,2)*ph(i,2,1)
                  p2=ps(k,1)*ph(i,1,2)+ps(k,2)*ph(i,2,2)
                  y=p1*y1+p2*y2
                  cosy=dcos(y/rE)
                  dlh=dsqrt((dxh*cosy)**2+dyh**2)
                  tx=(dxh*cosy)/dlh
                  ty=dyh/dlh
                  nx=-ty
                  ny=tx
                  len1(i,1)=len1(i,1)+ws(k)*dlh*p1
                  len1(i,2)=len1(i,2)+ws(k)*dlh*p2
                  len2xx(i,1,1)=len2xx(i,1,1)+ws(k)*dlh*
     1                  p1*p1*nx*nx
                  len2xx(i,1,2)=len2xx(i,1,2)+ws(k)*dlh*
     1                  p1*p2*nx*nx
                  len2xx(i,2,2)=len2xx(i,2,2)+ws(k)*dlh*
     1                  p2*p2*nx*nx
                  len2xy(i,1,1)=len2xy(i,1,1)+ws(k)*dlh*
     1                  p1*p1*nx*ny
                  len2xy(i,1,2)=len2xy(i,1,2)+ws(k)*dlh*
     1                  p1*p2*nx*ny
                  len2xy(i,2,2)=len2xy(i,2,2)+ws(k)*dlh*
     1                  p2*p2*nx*ny
                  len2yy(i,1,1)=len2yy(i,1,1)+ws(k)*dlh*
     1                  p1*p1*ny*ny
                  len2yy(i,1,2)=len2yy(i,1,2)+ws(k)*dlh*
     1                  p1*p2*ny*ny
                  len2yy(i,2,2)=len2yy(i,2,2)+ws(k)*dlh*
     1                  p2*p2*ny*ny
                  fsum(i,0)=fsum(i,0)+ws(k)*dlh*p1
                  fsum(i,1)=fsum(i,1)+ws(k)*dlh*p2
                  txsum(i,0)=txsum(i,0)+ws(k)*dxh*cosy*
     1                  (3.0d0*p1**2-2.0d0*p1**3)
                  tysum(i,0)=tysum(i,0)+ws(k)*dyh*
     1                  (3.0d0*p1**2-2.0d0*p1**3)
                  txsum(i,1)=txsum(i,1)+ws(k)*dxh*cosy*
     1                  (dx*p2*p1**2)
                  tysum(i,1)=tysum(i,1)+ws(k)*dyh*
     1                  (dx*p2*p1**2)
                  txsum(i,2)=txsum(i,2)+ws(k)*dxh*cosy*
     1                  (dy*p2*p1**2)
                  tysum(i,2)=tysum(i,2)+ws(k)*dyh*
     1                  (dy*p2*p1**2)
                  txsum(i,3)=txsum(i,3)+ws(k)*dxh*cosy*
     1                  (3.0d0*p2**2-2.0d0*p2**3)
                  tysum(i,3)=tysum(i,3)+ws(k)*dyh*
     1                  (3.0d0*p2**2-2.0d0*p2**3)
                  txsum(i,4)=txsum(i,4)+ws(k)*dxh*cosy*
     1                  (-dx*p1*p2**2)
                  tysum(i,4)=tysum(i,4)+ws(k)*dyh*
     1                  (-dx*p1*p2**2)
                  txsum(i,5)=txsum(i,5)+ws(k)*dxh*cosy*
     1                  (-dy*p1*p2**2)
                  tysum(i,5)=tysum(i,5)+ws(k)*dyh*
     1                  (-dy*p1*p2**2)
            end do
            len2xx(i,2,1)=len2xx(i,1,2)
            len2xy(i,2,1)=len2xy(i,1,2)
            len2yy(i,2,1)=len2yy(i,1,2)
            len0(i)=len1(i,1)+len1(i,2)
      end do
c
      return
      end
c
c
      SUBROUTINE choles(n,nmax,var)
c
c This replaces the positive definite variance-like matrix var with its
c  lower triangular Cholesky decomposition, which has the form of a
c  standard error matrix. 
c
      implicit none
      integer n,nmax
      integer i,im,j,jm,k
      real*8 var(nmax,nmax)
      real*8 si,sj
c
      do i=1,n
            si=var(i,i)
            if (i.gt.1) then
                  im=i-1
                  do j=1,im
                        sj=var(i,j)
                        if (j.gt.1) then
                              jm=j-1
                              do k=1,jm
                                    sj=sj-var(i,k)*var(j,k)
                              end do
                        end if
                        var(i,j)=sj/var(j,j)
                        var(j,i)=0.0d0
                        si=si-var(i,j)**2
                  end do
            end if
            if (si.le.0.0d0) stop 'var IS NOT POSITIVE DEFINITE'
            var(i,i)=dsqrt(si)
      end do
      return
      end
c
c
      SUBROUTINE fillvl(j0,jn,fvl,addf0,addfn,rE,x,y,sxx,syy,sxy,
     1      Lc,Lcc,Lcs,Ls,Lsc,Lss,Lcf,Lccf,Lcsf,Lsf,Lscf,Lssf,
     2      Kcf,Ksf,txf,tyf,ux,uy,uxm,uym,uxp,uyp)
c
c This fills in a portion of a velocity line between grid points with
c  specified values, by solving the 1D version of the full 2D problem
c  under the presumption that there are no variations in great-circle
c  component values perpendicular to the velocity line. Propagators are
c  used that take account of variations in (longitude,latitude)
c  coordinate axes directions along the line when projected onto the
c  Earth.
c The solution involves two levels of integration. The first integration
c  is of forces specified at increments of one eighth of a line segment,
c  to give traction terms specified at increments of one quarter of a
c  line segment. The traction terms are converted into strain-rate
c  contributions, which are then integrated to give velocity terms
c  specified at end points and midpoints of segments.
c There is a single pair of unknowns in the system. These are the two
c  components of traction at the first end of the portion of velocity
c  line, which is the starting point for the integrations. They are
c  solved for by matching the the two components of velocity at the
c  second end of the portion of velocity line.
c
      integer maxvls,maxvlj
      parameter(maxvls=200,maxvlj=2*maxvls+1)
      integer j0,jn
      integer j1,i,im,k,km,k0,k1,k2,ic,j
      integer fvl(0:maxvls)
      integer fsign(0:maxvls)
      logical addf0,addfn,rb0,rb1
      real*8 rE
      real*8 w0,w1,w2,am,ap,dx,dy,a,dl,dxdl,dydl
      real*8 dsxxdl,dsyydl,dsxydl,dlk,dxk,dyk,yk
      real*8 sxxk,syyk,sxyk,dpsidl,fxk,fyk
      real*8 Lc0,Lcc0,Lcs0,Ls0,Lsc0,Lss0
      real*8 Lc1,Lcc1,Lcs1,Ls1,Lsc1,Lss1
      real*8 dLcdl,dLccdl,dLcsdl,dLsdl,dLscdl,dLssdl
      real*8 Lck,Lcck,Lcsk,Lsk,Lsck,Lssk
      real*8 x(0:maxvls),y(0:maxvls)
      real*8 sxx(0:maxvls),syy(0:maxvls),sxy(0:maxvls)
      real*8 Lc(0:maxvls),Lcc(0:maxvls),Lcs(0:maxvls),
     1      Ls(0:maxvls),Lsc(0:maxvls),Lss(0:maxvls)
      real*8 Lcf(0:maxvls),Lccf(0:maxvls),Lcsf(0:maxvls),
     1      Lsf(0:maxvls),Lscf(0:maxvls),Lssf(0:maxvls)
      real*8 Kcf(0:maxvls),Ksf(0:maxvls)
      real*8 txf(0:maxvls),tyf(0:maxvls)
      real*8 ux(maxvlj),uy(maxvlj)
      real*8 uxm(0:maxvls),uym(0:maxvls),
     1      uxp(0:maxvls),uyp(0:maxvls)
      real*8 Kf(2,2,0:maxvls)
      real*8 psi(0:8),fint(2,0:4),Lfint(2,0:2,0:maxvls),
     1      Lint(2,2,0:2,0:maxvls)
      real*8 dlkdl(0:8),tx(0:8),ty(0:8),P(2,2,0:8,0:maxvls),
     1      f(2,0:8)
      real*8 E(3,2),L(3,3),Lk(2,2,0:4),Lf(2,0:4),Lfn(2)
      real*8 u0(2),un(2),t0(2)
c
      j1=j0+1
      w0=1.0d0/6.0d0
      w1=2.0d0/3.0d0
      w2=1.0d0/6.0d0
c
      do i=j0,jn
             if (fvl(i).ne.0) then
                   am=datan2(-txf(i),-tyf(i))
                   ap=datan2(txf(i),tyf(i))
                   if (i.eq.j0) then
                         dx=x(i+1)-x(i)
                         dy=y(i+1)-y(i)
                   else
                         dx=x(i)-x(i-1)
                         dy=y(i)-y(i-1)
                   end if
                   a=datan2(dx*dcos(y(i)/rE),dy)
                   if (am.lt.ap) then
                         if ((am.lt.a).and.(a.lt.ap)) then
                               fsign(i)=1
                         else
                               fsign(i)=-1
                         end if
                   else
                         if ((ap.lt.a).and.(a.lt.am)) then
                               fsign(i)=-1
                         else
                               fsign(i)=1
                         end if
                   end if
                   Kf(1,1,i)=Kcf(i)+Ksf(i)*txf(i)**2
                   Kf(1,2,i)=Ksf(i)*txf(i)*tyf(i)
                   Kf(2,1,i)=Kf(1,2,i)
                   Kf(2,2,i)=Kcf(i)+Ksf(i)*tyf(i)**2
             else
                   fsign(i)=0
             end if
      end do
c
      psi(8)=0.0d0
      fint(1,4)=0.0d0
      fint(2,4)=0.0d0
      Lfint(1,2,j0)=0.0d0
      Lfint(2,2,j0)=0.0d0
      Lint(1,1,2,j0)=0.0d0
      Lint(1,2,2,j0)=0.0d0
      Lint(2,1,2,j0)=0.0d0
      Lint(2,2,2,j0)=0.0d0
      do i=j1,jn
            im=i-1
            dx=x(i)-x(im)
            dy=y(i)-y(im)
            dl=dsqrt(dx**2+dy**2)
            dxdl=dx/dl
            dydl=dy/dl
            dsxxdl=(sxx(i)-sxx(im))/dl
            dsyydl=(syy(i)-syy(im))/dl
            dsxydl=(sxy(i)-sxy(im))/dl
            do k=0,8
                  dlk=0.125d0*dfloat(k)*dl
                  dxk=dxdl*dlk
                  dyk=dydl*dlk
                  yk=y(im)+dyk
                  tx(k)=dxdl*dcos(yk/rE)
                  ty(k)=dydl
                  dlkdl(k)=dsqrt(tx(k)**2+ty(k)**2)
                  tx(k)=tx(k)/dlkdl(k)
                  ty(k)=ty(k)/dlkdl(k)
                  if (dyk.eq.0.0d0) then
                        psi(k)=psi(8)+(dxk/rE)*dsin(yk/rE)
                  else
                        psi(k)=psi(8)
     1                        +(dxk/dyk)*2.0d0*dsin(0.5d0*dyk/rE)
     2                        *dsin(0.5d0*(y(im)+yk)/rE)
                  end if
                  P(1,1,k,i)=dcos(psi(k))
                  P(1,2,k,i)=dsin(psi(k))
                  P(2,1,k,i)=-P(1,2,k,i)
                  P(2,2,k,i)=P(1,1,k,i)
                  sxxk=sxx(im)+dsxxdl*dlk
                  syyk=syy(im)+dsyydl*dlk
                  sxyk=sxy(im)+dsxydl*dlk
                  dpsidl=(dxdl/rE)*dsin(yk/rE)
                  fxk=-tx(k)*(dsxxdl-2.0d0*sxyk*dpsidl)
     1                  -ty(k)*(dsxydl+(sxxk-syyk)*dpsidl)
                  fyk=-tx(k)*(dsxydl+(sxxk-syyk)*dpsidl)
     1                  -ty(k)*(dsyydl+2.0d0*sxyk*dpsidl)
                  f(1,k)=P(1,1,k,i)*fxk+P(2,1,k,i)*fyk
                  f(2,k)=P(1,2,k,i)*fxk+P(2,2,k,i)*fyk
            end do
c
            fint(1,0)=fint(1,4)
            fint(2,0)=fint(2,4)
            do k=1,4
                  km=k-1
                  k0=2*km
                  k1=km+k
                  k2=2*k
                  fint(1,k)=fint(1,km)+0.25d0*dl
     1                  *(w0*f(1,k0)+w1*f(1,k1)+w2*f(1,k2))
                  fint(2,k)=fint(2,km)+0.25d0*dl
     1                  *(w0*f(2,k0)+w1*f(2,k1)+w2*f(2,k2))
            end do
c
            if ((fvl(im).ne.0).and.(fsign(im).eq.-1)) then
                  Lc0=Lcf(im)
                  Lcc0=Lccf(im)
                  Lcs0=Lcsf(im)
                  Ls0=Lsf(im)
                  Lsc0=Lscf(im)
                  Lss0=Lssf(im)
            else
                  Lc0=Lc(im)
                  Lcc0=Lcc(im)
                  Lcs0=Lcs(im)
                  Ls0=Ls(im)
                  Lsc0=Lsc(im)
                  Lss0=Lss(im)
            end if
            if ((fvl(i).ne.0).and.(fsign(i).eq.1)) then
                  Lc1=Lcf(i)
                  Lcc1=Lccf(i)
                  Lcs1=Lcsf(i)
                  Ls1=Lsf(i)
                  Lsc1=Lscf(i)
                  Lss1=Lssf(i)
            else
                  Lc1=Lc(i)
                  Lcc1=Lcc(i)
                  Lcs1=Lcs(i)
                  Ls1=Ls(i)
                  Lsc1=Lsc(i)
                  Lss1=Lss(i)
            end if
            dLcdl=(Lc1-Lc0)/dl
            dLccdl=(Lcc1-Lcc0)/dl
            dLcsdl=(Lcs1-Lcs0)/dl
            dLsdl=(Ls1-Ls0)/dl
            dLscdl=(Lsc1-Lsc0)/dl
            dLssdl=(Lss1-Lss0)/dl
            rb0=(Lc0.eq.0.0d0).and.(Ls0.eq.0.0d0)
            rb1=(Lc1.eq.0.0d0).and.(Ls1.eq.0.0d0)
            do k=0,4
                  if (((k.eq.0).and.rb0).or.
     1                  ((k.eq.4).and.rb1)) then
                        Lk(1,1,k)=0.0d0
                        Lk(1,2,k)=0.0d0
                        Lk(2,1,k)=0.0d0
                        Lk(2,2,k)=0.0d0
                        Lf(1,k)=0.0d0
                        Lf(2,k)=0.0d0
                        goto 10
                  end if
                  k2=2*k
                  dlk=0.25d0*dfloat(k)*dl
                  E(1,1)=tx(k2)/dlkdl(k2)
                  E(1,2)=0.0d0
                  E(2,1)=0.0d0
                  E(2,2)=ty(k2)/dlkdl(k2)
                  E(3,1)=0.5d0*ty(k2)/dlkdl(k2)
                  E(3,2)=0.5d0*tx(k2)/dlkdl(k2)
                  Lck=Lc0+dLcdl*dlk
                  Lcck=Lcc0+dLccdl*dlk
                  Lcsk=Lcs0+dLcsdl*dlk
                  Lsk=Ls0+dLsdl*dlk
                  Lsck=Lsc0+dLscdl*dlk
                  Lssk=Lss0+dLssdl*dlk
                  L(1,1)=0.5d0*(Lck+Lcck)
     1                  +0.125d0*(Lsk-Lsck)
                  L(2,2)=0.5d0*(Lck-Lcck)
     1                  +0.125d0*(Lsk-Lsck)
                  L(3,3)=0.25d0*Lck
     1                  +0.125d0*(Lsk+Lsck)
                  L(1,2)=-0.125d0*(Lsk-Lsck)
                  L(1,3)=-0.25d0*Lcsk+0.125d0*Lssk
                  L(2,3)=-0.25d0*Lcsk-0.125d0*Lssk
                  L(2,1)=L(1,2)
                  L(3,1)=L(1,3)
                  L(3,2)=L(2,3)
                  call choles(3,3,L)
                  do ic=1,2
                        E(1,ic)=E(1,ic)/L(1,1)
                        E(2,ic)=(E(2,ic)-L(2,1)*E(1,ic))/L(2,2)
                        E(3,ic)=(E(3,ic)-L(3,1)*E(1,ic)
     1                        -L(3,2)*E(2,ic))/L(3,3)
                  end do
                  L(1,1)=E(1,1)*E(1,1)+E(2,1)*E(2,1)+E(3,1)*E(3,1)
                  L(1,2)=E(1,1)*E(1,2)+E(2,1)*E(2,2)+E(3,1)*E(3,2)
                  L(2,1)=L(1,2)
                  L(2,2)=E(1,2)*E(1,2)+E(2,2)*E(2,2)+E(3,2)*E(3,2)
                  call choles(2,3,L)
                  do ic=1,2
                        E(1,ic)=P(1,ic,k2,i)/L(1,1)
                        E(2,ic)=(P(2,ic,k2,i)-L(2,1)*E(1,ic))/L(2,2)
                  end do
                  L(1,1)=E(1,1)*E(1,1)+E(2,1)*E(2,1)
                  L(1,2)=E(1,1)*E(1,2)+E(2,1)*E(2,2)
                  L(2,2)=E(1,2)*E(1,2)+E(2,2)*E(2,2)
                  Lk(1,1,k)=L(1,1)/dlkdl(k2)
                  Lk(1,2,k)=L(1,2)/dlkdl(k2)
                  Lk(2,1,k)=Lk(1,2,k)
                  Lk(2,2,k)=L(2,2)/dlkdl(k2)
                  Lf(1,k)=Lk(1,1,k)*fint(1,k)+Lk(1,2,k)*fint(2,k)
                  Lf(2,k)=Lk(2,1,k)*fint(1,k)+Lk(2,2,k)*fint(2,k)
10                continue
            end do
c
            if ((fvl(im).ne.0).and.((i.gt.j1).or.addf0)) then
                  E(1,1)=Kf(1,1,im)*P(1,1,0,i)+Kf(1,2,im)*P(2,1,0,i)
                  E(1,2)=Kf(1,1,im)*P(1,2,0,i)+Kf(1,2,im)*P(2,2,0,i)
                  E(2,1)=Kf(2,1,im)*P(1,1,0,i)+Kf(2,2,im)*P(2,1,0,i)
                  E(2,2)=Kf(2,1,im)*P(1,2,0,i)+Kf(2,2,im)*P(2,2,0,i)
                  L(1,1)=P(1,1,0,i)*E(1,1)+P(2,1,0,i)*E(2,1)
                  L(1,2)=P(1,1,0,i)*E(1,2)+P(2,1,0,i)*E(2,2)
                  L(2,1)=L(1,2)
                  L(2,2)=P(1,2,0,i)*E(1,2)+P(2,2,0,i)*E(2,2)
                  Lfint(1,0,i)=Lfint(1,2,im)+L(1,1)*fint(1,0)
     1                  +L(1,2)*fint(2,0)
                  Lfint(2,0,i)=Lfint(2,2,im)+L(2,1)*fint(1,0)
     1                  +L(2,2)*fint(2,0)
                  Lint(1,1,0,i)=Lint(1,1,2,im)+L(1,1)
                  Lint(1,2,0,i)=Lint(1,2,2,im)+L(1,2)
                  Lint(2,1,0,i)=Lint(2,1,2,im)+L(2,1)
                  Lint(2,2,0,i)=Lint(2,2,2,im)+L(2,2)
            else
                  Lfint(1,0,i)=Lfint(1,2,im)
                  Lfint(2,0,i)=Lfint(2,2,im)
                  Lint(1,1,0,i)=Lint(1,1,2,im)
                  Lint(1,2,0,i)=Lint(1,2,2,im)
                  Lint(2,1,0,i)=Lint(2,1,2,im)
                  Lint(2,2,0,i)=Lint(2,2,2,im)
            end if
            do k=1,2
                  km=k-1
                  k0=2*km
                  k1=km+k
                  k2=2*k
                  Lfint(1,k,i)=Lfint(1,km,i)+0.5d0*dl
     1                  *(w0*Lf(1,k0)+w1*Lf(1,k1)+w2*Lf(1,k2))
                  Lfint(2,k,i)=Lfint(2,km,i)+0.5d0*dl
     1                  *(w0*Lf(2,k0)+w1*Lf(2,k1)+w2*Lf(2,k2))
                  Lint(1,1,k,i)=Lint(1,1,km,i)+0.5d0*dl
     1                  *(w0*Lk(1,1,k0)+w1*Lk(1,1,k1)+w2*Lk(1,1,k2))
                  Lint(1,2,k,i)=Lint(1,2,km,i)+0.5d0*dl
     1                  *(w0*Lk(1,2,k0)+w1*Lk(1,2,k1)+w2*Lk(1,2,k2))
                  Lint(2,1,k,i)=Lint(1,2,k,i)
                  Lint(2,2,k,i)=Lint(2,2,km,i)+0.5d0*dl
     1                  *(w0*Lk(2,2,k0)+w1*Lk(2,2,k1)+w2*Lk(2,2,k2))
            end do
      end do
      if ((fvl(jn).ne.0).and.addfn) then
            E(1,1)=Kf(1,1,jn)*P(1,1,8,jn)+Kf(1,2,jn)*P(2,1,8,jn)
            E(1,2)=Kf(1,1,jn)*P(1,2,8,jn)+Kf(1,2,jn)*P(2,2,8,jn)
            E(2,1)=Kf(2,1,jn)*P(1,1,8,jn)+Kf(2,2,jn)*P(2,1,8,jn)
            E(2,2)=Kf(2,1,jn)*P(1,2,8,jn)+Kf(2,2,jn)*P(2,2,8,jn)
            L(1,1)=P(1,1,8,jn)*E(1,1)+P(2,1,8,jn)*E(2,1)
            L(1,2)=P(1,1,8,jn)*E(1,2)+P(2,1,8,jn)*E(2,2)
            L(2,1)=L(1,2)
            L(2,2)=P(1,2,8,jn)*E(1,2)+P(2,2,8,jn)*E(2,2)
            Lfn(1)=Lfint(1,2,jn)+L(1,1)*fint(1,4)
     1            +L(1,2)*fint(2,4)
            Lfn(2)=Lfint(2,2,jn)+L(2,1)*fint(1,4)
     1            +L(2,2)*fint(2,4)
            L(1,1)=Lint(1,1,2,jn)+L(1,1)
            L(1,2)=Lint(1,2,2,jn)+L(1,2)
            L(2,1)=Lint(2,1,2,jn)+L(2,1)
            L(2,2)=Lint(2,2,2,jn)+L(2,2)
      else
            Lfn(1)=Lfint(1,2,jn)
            Lfn(2)=Lfint(2,2,jn)
            L(1,1)=Lint(1,1,2,jn)
            L(1,2)=Lint(1,2,2,jn)
            L(2,1)=Lint(2,1,2,jn)
            L(2,2)=Lint(2,2,2,jn)
      end if
c
      if (fvl(j0).ne.0) then
            if (addf0) then
                  if (fsign(j0).eq.1) then
                        u0(1)=uxm(j0)
                        u0(2)=uym(j0)
                  else
                        u0(1)=uxp(j0)
                        u0(2)=uyp(j0)
                  end if
            else
                  if (fsign(j0).eq.-1) then
                        u0(1)=uxm(j0)
                        u0(2)=uym(j0)
                  else
                        u0(1)=uxp(j0)
                        u0(2)=uyp(j0)
                  end if
            end if
      else
            j=2*j0+1
            u0(1)=ux(j)
            u0(2)=uy(j)
      end if
      if (fvl(jn).ne.0) then
            if (addfn) then
                  if (fsign(jn).eq.-1) then
                        un(1)=P(1,1,8,jn)*uxm(jn)+P(2,1,8,jn)*uym(jn)
                        un(2)=P(1,2,8,jn)*uxm(jn)+P(2,2,8,jn)*uym(jn)
                  else
                        un(1)=P(1,1,8,jn)*uxp(jn)+P(2,1,8,jn)*uyp(jn)
                        un(2)=P(1,2,8,jn)*uxp(jn)+P(2,2,8,jn)*uyp(jn)
                  end if
            else
                  if (fsign(jn).eq.1) then
                        un(1)=P(1,1,8,jn)*uxm(jn)+P(2,1,8,jn)*uym(jn)
                        un(2)=P(1,2,8,jn)*uxm(jn)+P(2,2,8,jn)*uym(jn)
                  else
                        un(1)=P(1,1,8,jn)*uxp(jn)+P(2,1,8,jn)*uyp(jn)
                        un(2)=P(1,2,8,jn)*uxp(jn)+P(2,2,8,jn)*uyp(jn)
                  end if
            end if
      else
            j=2*jn+1
            un(1)=P(1,1,8,jn)*ux(j)+P(2,1,8,jn)*uy(j)
            un(2)=P(1,2,8,jn)*ux(j)+P(2,2,8,jn)*uy(j)
      end if
      call choles(2,3,L)
      t0(1)=(un(1)-u0(1)+Lfn(1))/L(1,1)
      t0(2)=(un(2)-u0(2)+Lfn(2)-L(2,1)*t0(1))/L(2,2)
      t0(2)=t0(2)/L(2,2)
      t0(1)=(t0(1)-L(2,1)*t0(2))/L(1,1)
c
      if ((fvl(j0).ne.0).and.addf0) then
            if (fsign(j0).eq.1) then
                  uxp(j0)=uxm(j0)+Kf(1,1,j0)*t0(1)
     1                  +Kf(1,2,j0)*t0(2)
                  uyp(j0)=uym(j0)+Kf(2,1,j0)*t0(1)
     1                  +Kf(2,2,j0)*t0(2)
            else
                  uxm(j0)=uxp(j0)+Kf(1,1,j0)*t0(1)
     1                  +Kf(1,2,j0)*t0(2)
                  uym(j0)=uyp(j0)+Kf(2,1,j0)*t0(1)
     1                  +Kf(2,2,j0)*t0(2)
            end if

      end if
      do i=j1,jn
            j=2*i
            un(1)=u0(1)-Lfint(1,1,i)+Lint(1,1,1,i)*t0(1)
     1            +Lint(1,2,1,i)*t0(2)
            un(2)=u0(2)-Lfint(2,1,i)+Lint(2,1,1,i)*t0(1)
     1            +Lint(2,2,1,i)*t0(2)
            ux(j)=P(1,1,4,i)*un(1)+P(1,2,4,i)*un(2)
            uy(j)=P(2,1,4,i)*un(1)+P(2,2,4,i)*un(2)
            if (fvl(i).ne.0) then
                  if ((i.lt.jn).or.addfn) then
                        un(1)=u0(1)-Lfint(1,2,i)
     1                        +Lint(1,1,2,i)*t0(1)
     2                        +Lint(1,2,2,i)*t0(2)
                        un(2)=u0(2)-Lfint(2,2,i)
     1                        +Lint(2,1,2,i)*t0(1)
     2                        +Lint(2,2,2,i)*t0(2)
                        if (fsign(i).eq.1) then
                              uxm(i)=P(1,1,8,i)*un(1)
     1                              +P(1,2,8,i)*un(2)
                              uym(i)=P(2,1,8,i)*un(1)
     1                              +P(2,2,8,i)*un(2)
                        else
                              uxp(i)=P(1,1,8,i)*un(1)
     1                              +P(1,2,8,i)*un(2)
                              uyp(i)=P(2,1,8,i)*un(1)
     1                              +P(2,2,8,i)*un(2)
                        end if
                  end if
                  if (i.lt.jn) then
                        j=i+1
                        un(1)=u0(1)-Lfint(1,0,j)
     1                        +Lint(1,1,0,j)*t0(1)
     2                        +Lint(1,2,0,j)*t0(2)
                        un(2)=u0(2)-Lfint(2,0,j)
     1                        +Lint(2,1,0,j)*t0(1)
     2                        +Lint(2,2,0,j)*t0(2)
                        if (fsign(i).eq.-1) then
                              uxm(i)=P(1,1,8,i)*un(1)
     1                              +P(1,2,8,i)*un(2)
                              uym(i)=P(2,1,8,i)*un(1)
     1                              +P(2,2,8,i)*un(2)
                        else
                              uxp(i)=P(1,1,8,i)*un(1)
     1                              +P(1,2,8,i)*un(2)
                              uyp(i)=P(2,1,8,i)*un(1)
     1                              +P(2,2,8,i)*un(2)
                        end if
                  end if
            else
                  if (i.lt.jn) then
                        j=2*i+1
                        un(1)=u0(1)-Lfint(1,2,i)
     1                        +Lint(1,1,2,i)*t0(1)
     2                        +Lint(1,2,2,i)*t0(2)
                        un(2)=u0(2)-Lfint(2,2,i)
     1                        +Lint(2,1,2,i)*t0(1)
     2                        +Lint(2,2,2,i)*t0(2)
                        ux(j)=P(1,1,8,i)*un(1)+P(1,2,8,i)*un(2)
                        uy(j)=P(2,1,8,i)*un(1)+P(2,2,8,i)*un(2)
                  end if
            end if
      end do
c
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE order(ngp,ns,ne,nf,nvl,ntj,gp1s,gp2s,e1s,e2s,
     1      s1e,s2e,s3e,gp1e,gp2e,gp3e,nfs,gpf,nvls,gpvl,gprb,
     2      ipf,ipvl,iprb,isf,isvl,isrb,fs,sfs,fp,gpfp,jfp,tjfp,
     3      vls,svls,rbs,srbs,nJvl,nfT,nTf,nTvl,nXf,nXvl,
     4      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,iefv,
     5      rs1fv,rs2fv,rs3fv,rgp1fv,rgp2fv,rgp3fv,rgprb,
     6      p0zero,pnzero,long,lat,nz,nx1,nx,nx2,nx0,
     7      gprank,gplist,smingp,smaxgp,srank,slist,smins,smaxs,
     8      erank,elist,smine,smaxe,z1,zn,sz,kz,jz,zmin,zmax,
     9      csqlen,xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,
     1      xTvlu,xXfu,xXvlu,xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     1      ux,uy,uxm,uym,uxp,uyp,dut,dun,ugprb,x)
c
c This puts the variables in the system into appropriate order for the
c  sparse matrix and matrix-vector calculations performed in the solver
c  program. It involves sequencing the constraints, and separating the
c  velocity and slip-rate variables into three classes.
c There are four classes of constraint:
c    0 = continuity of normal derivative at an internal side,
c    1 = fault-related,
c    2 = velocity at midpoints of segments on velocity lines,
c    3 = velocity at midpoints of segments on rigid boundaries.
c  The fault-related class has the five sub-classes
c    0 = matching slip rates at triple junctions at the start of faults,
c    1 = matching slip rates at the first quarter point of each segment,
c    2 = setting the third derivative of slip rate to zero,
c    3 = matching slip rates at the other quarter point of each segment,
c    4 = matching slip rates at triple junctions at the ends of faults,
c  and the sub-classes of the midpoint-of-segment velocity-line
c  constraints are
c    0 = positive side of the line,
c    1 = negative side of the line.
c
c The first class of variables (x1) has a one-to-one relationship with
c  the constraints (z), which control their values. The x1 variables are
c  chosen to align as closely as possible with the geometrical locations
c  of the constraints, while satisfying necessary rules.
c The subset of constraints that is trickiest to handle is the set
c  enforcing continuity of normal derivatives at internal sides within
c  the model. There are one and a half times as many sides as elements
c  and three times as many sides as grid points. Details of how all x1
c  variables are chosen are explained in the subroutine loadx1. As a
c  reasonably accurate guide, though slightly dependent on the model,
c  60% of the variables will be in the x1 class.
c
c The second class (x2) consists of the remaining 40% of variables,
c  whose values are determined in the least squares inversions for the
c  apriori and aposteriori solutions. The number of degrees of freedom
c  in the system is equal to the number (nx2) of variables in the x2
c  class.
c The x2 variables are also sequenced in line with the constraints, as
c  the only full matrix-vector calculations performed in the solver
c  program use sparse matrices constructed from the constraints to
c  relate the x1 and x2 variable values. All other matrix-vector
c  calculations are done using small sub-matrices that naturally align
c  with the sparse structure of the constraints once the x1 and x2
c  variables are in apppropriate order.
c
c The remaining class (x0) consists of all variables whose values are
c  pre-determined. These are velocity values at grid points on velocity
c  lines and rigid boundaries, and zero-valued slip-rates and slip-rate
c  derivatives at zero-slip ends of faults, plus velocity derivatives
c  at grid points on rigid boundaries that are attached to faults but
c  not otherwise connected to the model domain.
c
c In situations where more than one region is involved the region 0
c  variables (i.e. those on the positive side in the case of faults and
c  velocity lines) are assigned to the grid point.
c Also, the convention with the extra slip-rate variables that are
c  needed when faults cross velocity lines is that the extra variables
c  (xfTdu and xXfdu) apply to the side of the velocity line closer to
c  the start of the fault.
c
c The starting point in constructing the ordering of constraints and
c  variables is the first element side in the order the sides are input
c  that is not internal to the model - i.e. the first side without an
c  element on both sides. Adjoining elements, sides and grid points are
c  then put into the order they connect to each other.
c Users of the program have some control over the sequencing, in the
c  sense that they can change the order that sides are input. The key
c  thing to watch is the overall sparsity of the system given by the
c  length of the big Csq array. The most compact sparsity structure is
c  likely to occur when the first external side is at or near a corner
c  of the model.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx,maxz
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer ngp,ns,ne,nf,nvl,ntj
      integer nJvl,nfT,nTf,nTvl,nXf,nXvl,nefv
      integer nz,nx1,nx,nx2,nx0
      integer i,e,gpn,sn,en,e0,gp1,gp2,gp3,s1,s2,s3
      integer s,ic,f,j,zmins,zmaxs,jc,r
      integer gp1s(maxs),gp2s(maxs)
      integer e1s(maxs),e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer rs1fv(maxefv),rs2fv(maxefv),rs3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer gprank(maxgp),gplist(maxgp)
      integer smingp(maxgp),smaxgp(maxgp)
      integer srank(maxs),slist(maxs)
      integer smins(maxs),smaxs(maxs)
      integer erank(maxe),elist(maxe)
      integer smine(maxe),smaxe(maxe)
      integer z1(maxs),zn(maxs)
      integer sz(maxz),kz(maxz),jz(maxz)
      integer zmin(maxz),zmax(maxz)
      integer csqlen(maxz)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer smaxx(maxx),kx(maxx),ix(maxx),jx(maxx),
     1      jcx(maxx),icx(maxx)
      integer gpe1s(maxs),gpe2s(maxs)
      integer e1e(maxe),e2e(maxe),e3e(maxe)
      logical p0zero(maxf),pnzero(maxf)
      logical efree(maxe),todos(maxs)
      real*8 long(maxgp),lat(maxgp)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 dut(0:maxfs,maxf),dun(0:maxfs,maxf)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb)
      real*8 x(maxx)
c
      do i=1,ngp
            gprank(i)=0
            gplist(i)=0
            smingp(i)=0
            smaxgp(i)=0

      end do
      do i=1,ns
            srank(i)=0
            slist(i)=0
            if (isf(i)+isvl(i)+isrb(i).eq.0) then
                  e=e1s(i)
                  if ((gp1e(e).ne.gp1s(i)).and.
     1                  (gp1e(e).ne.gp2s(i))) gpe1s(i)=gp1e(e)
                  if ((gp2e(e).ne.gp1s(i)).and.
     1                  (gp2e(e).ne.gp2s(i))) gpe1s(i)=gp2e(e)
                  if ((gp3e(e).ne.gp1s(i)).and.
     1                  (gp3e(e).ne.gp2s(i))) gpe1s(i)=gp3e(e)
                  e=e2s(i)
                  if ((gp1e(e).ne.gp1s(i)).and.
     1                  (gp1e(e).ne.gp2s(i))) gpe2s(i)=gp1e(e)
                  if ((gp2e(e).ne.gp1s(i)).and.
     1                  (gp2e(e).ne.gp2s(i))) gpe2s(i)=gp2e(e)
                  if ((gp3e(e).ne.gp1s(i)).and.
     1                  (gp3e(e).ne.gp2s(i))) gpe2s(i)=gp3e(e)
            else
                  gpe1s(i)=0
                  gpe2s(i)=0
            end if
      end do
      do i=1,ne
            erank(i)=0
            elist(i)=0
            if (e1s(s1e(i)).eq.i) then
                  e1e(i)=e2s(s1e(i))
            else
                  e1e(i)=e1s(s1e(i))
            end if
            if (e1s(s2e(i)).eq.i) then
                  e2e(i)=e2s(s2e(i))
            else
                  e2e(i)=e1s(s2e(i))
            end if
            if (e1s(s3e(i)).eq.i) then
                  e3e(i)=e2s(s3e(i))
            else
                  e3e(i)=e1s(s3e(i))
            end if
      end do
      i=0
10    i=i+1
      if (e2s(i).ne.0) goto 10
      gpn=0
      sn=0
      en=1
      erank(e1s(i))=1
      elist(1)=e1s(i)
      e0=1
20    e=elist(e0)
      gp1=gp1e(e)
      gp2=gp2e(e)
      gp3=gp3e(e)
      s1=s1e(e)
      s2=s2e(e)
      s3=s3e(e)
      if (srank(s1).eq.0) then
            sn=sn+1
            srank(s1)=sn
            slist(sn)=s1
            smaxgp(gp2)=sn
            smaxgp(gp3)=sn
            if (gprank(gp2).eq.0) then
                  gpn=gpn+1
                  gprank(gp2)=gpn
                  gplist(gpn)=gp2
                  smingp(gp2)=sn
            end if
            if (gprank(gp3).eq.0) then
                  gpn=gpn+1
                  gprank(gp3)=gpn
                  gplist(gpn)=gp3
                  smingp(gp3)=sn
            end if
            if (isf(s1)+isvl(s1)+isrb(s1).eq.0) then
                  smaxgp(gpe1s(s1))=sn
                  smaxgp(gpe2s(s1))=sn
                  if (gprank(gpe1s(s1)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe1s(s1))=gpn
                        gplist(gpn)=gpe1s(s1)
                        smingp(gpe1s(s1))=sn
                  end if
                  if (gprank(gpe2s(s1)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe2s(s1))=gpn
                        gplist(gpn)=gpe2s(s1)
                        smingp(gpe2s(s1))=sn
                  end if
            end if
            if (e1e(e).ne.0) then
                  if (erank(e1e(e)).eq.0) then
                        en=en+1
                        erank(e1e(e))=en
                        elist(en)=e1e(e)
                  end if
            end if
      end if
      if (srank(s2).eq.0) then
            sn=sn+1
            srank(s2)=sn
            slist(sn)=s2
            smaxgp(gp3)=sn
            smaxgp(gp1)=sn
            if (gprank(gp1).eq.0) then
                  gpn=gpn+1
                  gprank(gp1)=gpn
                  gplist(gpn)=gp1
                  smingp(gp1)=sn
            end if
            if (isf(s2)+isvl(s2)+isrb(s2).eq.0) then
                  smaxgp(gpe1s(s2))=sn
                  smaxgp(gpe2s(s2))=sn
                  if (gprank(gpe1s(s2)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe1s(s2))=gpn
                        gplist(gpn)=gpe1s(s2)
                        smingp(gpe1s(s2))=sn
                  end if
                  if (gprank(gpe2s(s2)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe2s(s2))=gpn
                        gplist(gpn)=gpe2s(s2)
                        smingp(gpe2s(s2))=sn
                  end if
            end if
            if (e2e(e).ne.0) then
                  if (erank(e2e(e)).eq.0) then
                        en=en+1
                        erank(e2e(e))=en
                        elist(en)=e2e(e)
                  end if
            end if
      end if
      if (srank(s3).eq.0) then
            sn=sn+1
            srank(s3)=sn
            slist(sn)=s3
            smaxgp(gp1)=sn
            smaxgp(gp2)=sn
            if (isf(s3)+isvl(s3)+isrb(s3).eq.0) then
                  smaxgp(gpe1s(s3))=sn
                  smaxgp(gpe2s(s3))=sn
                  if (gprank(gpe1s(s3)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe1s(s3))=gpn
                        gplist(gpn)=gpe1s(s3)
                        smingp(gpe1s(s3))=sn
                  end if
                  if (gprank(gpe2s(s3)).eq.0) then
                        gpn=gpn+1
                        gprank(gpe2s(s3))=gpn
                        gplist(gpn)=gpe2s(s3)
                        smingp(gpe2s(s3))=sn
                  end if
            end if
            if (e3e(e).ne.0) then
                  if (erank(e3e(e)).eq.0) then
                        en=en+1
                        erank(e3e(e))=en
                        elist(en)=e3e(e)
                  end if
            end if
      end if
      e0=e0+1
      if (e0.le.en) goto 20
c
      if ((gpn.ne.ngp).or.(sn.ne.ns).or.(en.ne.ne)) then
            write(*,*) 'gpn,ngp=',gpn,ngp
            write(*,*) 'sn,ns=',sn,ns
            write(*,*) 'en,ne=',en,ne
      stop 'Subdivide the model so that elements are all connected'
      end if
      do i=1,ns
            smins(i)=ns
            smaxs(i)=1
            gp1=gp1s(i)
            gp2=gp2s(i)
            if (smingp(gp1).lt.smins(i)) smins(i)=smingp(gp1)
            if (smaxgp(gp1).gt.smaxs(i)) smaxs(i)=smaxgp(gp1)
            if (smingp(gp2).lt.smins(i)) smins(i)=smingp(gp2)
            if (smaxgp(gp2).gt.smaxs(i)) smaxs(i)=smaxgp(gp2)
            if (isf(i)+isvl(i)+isrb(i).eq.0) then
                  gp1=gpe1s(i)
                  gp2=gpe2s(i)
                  if (smingp(gp1).lt.smins(i)) smins(i)=smingp(gp1)
                  if (smaxgp(gp1).gt.smaxs(i)) smaxs(i)=smaxgp(gp1)
                  if (smingp(gp2).lt.smins(i)) smins(i)=smingp(gp2)
                  if (smaxgp(gp2).gt.smaxs(i)) smaxs(i)=smaxgp(gp2)
            end if
      end do
      do i=1,ne
            smine(i)=ns
            smaxe(i)=1
            s1=s1e(i)
            s2=s2e(i)
            s3=s3e(i)
            if (isf(s1)+isvl(s1)+isrb(s1).eq.0) then
                  if (smins(s1).lt.smine(i)) smine(i)=smins(s1)
                  if (smaxs(s1).gt.smaxe(i)) smaxe(i)=smaxs(s1)
            end if
            if (isf(s2)+isvl(s2)+isrb(s2).eq.0) then
                  if (smins(s2).lt.smine(i)) smine(i)=smins(s2)
                  if (smaxs(s2).gt.smaxe(i)) smaxe(i)=smaxs(s2)
            end if
            if (isf(s3)+isvl(s3)+isrb(s3).eq.0) then
                  if (smins(s3).lt.smine(i)) smine(i)=smins(s3)
                  if (smaxs(s3).gt.smaxe(i)) smaxe(i)=smaxs(s3)
            end if
            if (smine(i).gt.smaxe(i)) then
                  write(*,*) 'i,s1,s2,s3=',i,s1,s2,s3
      stop 'Reconstruct the model so that all elements have open sides'
            end if
      end do
c
      nz=0
      do i=1,ns
            s=slist(i)
            z1(i)=nz+1
c
            if (isf(s).ne.0) then
                  f=fs(isf(s))
                  j=sfs(isf(s))
                  gp1=gpf(j-1,f)
                  gp2=gpf(j,f)
                  do ic=1,2
                        sz(nz+ic)=s
                        kz(nz+ic)=1
                        jz(nz+ic)=1
                  end do
                  nz=nz+2
                  do ic=1,2
                        sz(nz+ic)=s
                        kz(nz+ic)=1
                        jz(nz+ic)=3
                  end do
                  nz=nz+2
            end if
c
            if (isvl(s).ne.0) then
                  if (e2s(s).eq.0) then
                        e=e1s(s)
                        if (s.eq.s1e(e)) r=rs1fv(iefv(e))
                        if (s.eq.s2e(e)) r=rs2fv(iefv(e))
                        if (s.eq.s3e(e)) r=rs3fv(iefv(e))
                        do ic=1,2
                              sz(nz+ic)=s
                              kz(nz+ic)=2
                              jz(nz+ic)=r
                        end do
                        nz=nz+2
                  else
                        do ic=1,2
                              sz(nz+ic)=s
                              kz(nz+ic)=2
                              jz(nz+ic)=0
                        end do
                        nz=nz+2
                        do ic=1,2
                              kz(nz+ic)=2
                              jz(nz+ic)=1
                        end do
                        nz=nz+2
                  end if
            end if
c
            if ((isrb(s).ne.0).and.(isf(s).eq.0)) then
                  do ic=1,2
                        sz(nz+ic)=s
                        kz(nz+ic)=3
                        jz(nz+ic)=0
                  end do
                  nz=nz+2
            end if
            zn(i)=nz
      end do
c
      do i=1,ns
            s=slist(i)
            zmins=z1(smins(s))
            zmaxs=zn(smaxs(s))
            do j=z1(i),zn(i)
                  zmin(j)=zmins
                  zmax(j)=zmaxs
            end do
      end do
      csqlen(1)=1
      do i=2,nz
            csqlen(i)=csqlen(i-1)+i-zmin(i)+1
      end do
      write(*,*) 'The length of the big Csq array is',csqlen(nz)
c
      do i=1,ngp
            do jc=0,2
            do ic=1,2
                  xgpu(ic,jc,i)=0
            end do
            end do
      end do
      do i=1,ne
            do ic=1,2
                  xeu(ic,i)=0
            end do
      end do
      if (nf.gt.0) then
      do i=1,nf
            do j=0,nfs(i)
                  do jc=0,2
                  do ic=1,2
                        xfu(ic,jc,j,i)=0
                  end do
                  end do
                  do jc=0,1
                  do ic=1,2
                        xfdu(ic,jc,j,i)=0
                  end do
                  end do
            end do
      end do
      end if
      if (nvl.gt.0) then
      do i=1,nvl
            do j=0,nvls(i)
                  do jc=0,2
                  do ic=1,2
                        xvlu(ic,jc,j,i)=0
                  end do
                  end do
            end do
      end do
      end if
      if (ntj.gt.0) then
      do i=1,ntj
            do r=1,2
                  do jc=0,2
                  do ic=1,2
                        xtju(ic,jc,r,i)=0
                  end do
                  end do
            end do
      end do
      end if
      if (nJvl.gt.0) then
      do i=1,nJvl
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xJvlu(ic,jc,r,i)=0
                  end do

                  end do
            end do
      end do
      end if
      if (nfT.gt.0) then
      do i=1,nfT
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xfTu(ic,jc,r,i)=0
                  end do
                  end do
            end do
            do jc=0,1
            do ic=1,2
                  xfTdu(ic,jc,i)=0
            end do
            end do
      end do
      end if
      if (nTf.gt.0) then
      do i=1,nTf
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xTfu(ic,jc,r,i)=0
                  end do

                  end do
            end do
      end do
      end if
      if (nTvl.gt.0) then
      do i=1,nTvl
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xTvlu(ic,jc,r,i)=0
                  end do
                  end do
            end do
      end do
      end if
      if (nXf.gt.0) then
      do i=1,nXf
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xXfu(ic,jc,r,i)=0
                  end do
                  end do
            end do
            do jc=0,1
            do ic=1,2
                  xXfdu(ic,jc,i)=0
            end do
            end do
      end do
      end if
      if (nXvl.gt.0) then
      do i=1,nXvl
            do r=1,3
                  do jc=0,2
                  do ic=1,2
                        xXvlu(ic,jc,r,i)=0
                  end do
                  end do
            end do
      end do
      end if
c
      do i=1,ne
            gp1=gp1e(i)
            gp2=gp2e(i)
            gp3=gp3e(i)
            s1=s1e(i)
            s2=s2e(i)
            s3=s3e(i)
            efree(i)=(ipvl(gp1)+iprb(gp1).eq.0).or.
     1            (ipvl(gp2)+iprb(gp2).eq.0).or.
     1            (ipvl(gp3)+iprb(gp3).eq.0)
            if (isf(s1)+isvl(s1)+isrb(s1).ne.0) e1e(i)=0
            if (isf(s2)+isvl(s2)+isrb(s2).ne.0) e2e(i)=0
            if (isf(s3)+isvl(s3)+isrb(s3).ne.0) e3e(i)=0
      end do
      do i=1,ns
            todos(i)=.true.
      end do
      nx1=0
      do i=1,ne
            e=elist(i)
            gp1=gp1e(e)
            gp2=gp2e(e)
            gp3=gp3e(e)
            s1=s1e(e)
            s2=s2e(e)
            s3=s3e(e)
c
            if (todos(s1)) then
                  if (nx1+1.ne.z1(srank(s1))) then
                        write(*,*) 'nx1,z1,s1=',nx1,
     1                        z1(srank(s1)),s1
                  stop 'There is a sequencing bug in the code'
                  end if
c
                  call loadx1(nx1,s1,e,gp1,gp2,gp3,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,gprank,ipf,ipvl,iprb,
     5                  efree,erank,e1e,e2e,e3e,gp1e,gp2e,gp3e,
     6                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     7                  iefv,rgp1fv,rgp2fv,rgp3fv,
     8                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     9                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     1                  nfs,p0zero,pnzero,nvls)
c
                  if (nx1.ne.zn(srank(s1))) then
                        write(*,*) 'nx1,zn,s1=',nx1,
     1                        zn(srank(s1)),s1
                  stop 'There is a sequencing bug in the code'
                  end if
                  todos(s1)=.false.
            end if
c
            if (todos(s2)) then
                  if (nx1+1.ne.z1(srank(s2))) then
                        write(*,*) 'nx1,z1,s2=',nx1,
     1                        z1(srank(s2)),s2
                  stop 'There is a sequencing bug in the code'
                  end if
c
                  call loadx1(nx1,s2,e,gp2,gp3,gp1,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,gprank,ipf,ipvl,iprb,
     5                  efree,erank,e1e,e2e,e3e,gp1e,gp2e,gp3e,
     6                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     7                  iefv,rgp1fv,rgp2fv,rgp3fv,
     8                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     9                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     1                  nfs,p0zero,pnzero,nvls)
c
                  if (nx1.ne.zn(srank(s2))) then
                        write(*,*) 'nx1,zn,s2=',nx1,
     1                        zn(srank(s2)),s2
                  stop 'There is a sequencing bug in the code'
                  end if
                  todos(s2)=.false.
            end if
c
            if (todos(s3)) then
                  if (nx1+1.ne.z1(srank(s3))) then
                        write(*,*) 'nx1,z1,s3=',nx1,
     1                        z1(srank(s3)),s3
                  stop 'There is a sequencing bug in the code'
                  end if
c
                  call loadx1(nx1,s3,e,gp3,gp1,gp2,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,gprank,ipf,ipvl,iprb,
     5                  efree,erank,e1e,e2e,e3e,gp1e,gp2e,gp3e,
     6                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     7                  iefv,rgp1fv,rgp2fv,rgp3fv,
     8                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     9                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     1                  nfs,p0zero,pnzero,nvls)
c
                  if (nx1.ne.zn(srank(s3))) then
                        write(*,*) 'nx1,zn,s3=',nx1,
     1                        zn(srank(s3)),s3
                  stop 'There is a sequencing bug in the code'
                  end if
                  todos(s3)=.false.
            end if
      end do
c
      do i=1,ns
            todos(i)=.true.
      end do
      nx=nx1
      do i=1,ne
            e=elist(i)
            gp1=gp1e(e)
            gp2=gp2e(e)
            gp3=gp3e(e)
            s1=s1e(e)
            s2=s2e(e)
            s3=s3e(e)
c
            if (todos(s1)) then
                  call loadx2(nx,s1,e,gp1,gp2,gp3,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls)
                  todos(s1)=.false.
            end if
c
            if (todos(s2)) then
                  call loadx2(nx,s2,e,gp2,gp3,gp1,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls)
                  todos(s2)=.false.
            end if

c
            if (todos(s3)) then
                  call loadx2(nx,s3,e,gp3,gp1,gp2,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xeu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls)
                  todos(s3)=.false.
            end if
      end do
      nx2=nx-nx1
c
      do i=1,ns
            todos(i)=.true.
      end do
      do i=1,ne
            e=elist(i)
            gp1=gp1e(e)
            gp2=gp2e(e)
            gp3=gp3e(e)
            s1=s1e(e)
            s2=s2e(e)
            s3=s3e(e)
c
            if (todos(s1)) then
                  call loadx0(nx,s1,e,gp1,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls,ux,uy,
     1                  uxm,uym,uxp,uyp,dut,dun,ugprb,x)
                  todos(s1)=.false.
            end if
c
            if (todos(s2)) then
                  call loadx0(nx,s2,e,gp2,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls,ux,uy,
     1                  uxm,uym,uxp,uyp,dut,dun,ugprb,x)
                  todos(s2)=.false.
            end if
c
            if (todos(s3)) then
                  call loadx0(nx,s3,e,gp3,e1s,e2s,
     1                  isf,isvl,isrb,smaxgp,xgpu,xfu,xvlu,
     2                  xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3                  xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     4                  long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     5                  Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     6                  iefv,rgp1fv,rgp2fv,rgp3fv,
     7                  fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     8                  vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     9                  nfs,p0zero,pnzero,nvls,ux,uy,
     1                  uxm,uym,uxp,uyp,dut,dun,ugprb,x)
                  todos(s3)=.false.
            end if
      end do
      nx0=nx-nx1-nx2
c
      return
      end
c
c
      SUBROUTINE loadx1(nx1,s,e,gp1,gp2,gp3,e1s,e2s,isf,isvl,isrb,
     1      smaxgp,xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,xTvlu,
     2      xXfu,xXvlu,xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     3      long,lat,gprank,ipf,ipvl,iprb,efree,erank,e1e,e2e,e3e,
     4      gp1e,gp2e,gp3e,Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     5      iefv,rgp1fv,rgp2fv,rgp3fv,fp,gpfp,jfp,tjfp,fs,sfs,gpf,
     6      vls,svls,gpvl,rbs,srbs,gprb,rgprb,nfs,p0zero,pnzero,
     7      nvls)
c
c This routine chooses and orders the x1 variables, in a one-to-one
c  relationship with the constraints. 
c
c For most models by far the biggest number of x1 variables are those
c  associated with constraint class 0. Of these two thirds will be
c  midpoint-of-element variables (xeu), which are the first options
c  explored. For some models, with small sub-regions between
c  combinations of faults, velocity lines and rigid boundaries, not all
c  xeu variables have sides where normal-derivative constraints apply
c  that they can be assigned to, so they are included in the x2
c  variables.
c The next options explored for constraint class 0 are derivative
c  variables at the grid points at the two ends of each element side
c  that are not on faults, velocity lines or rigid boundaries. The
c  derivative component (x=1 or y=2) chosen is that which contributes
c  more at the midpoint of the side, and the lower ranked grid point is
c  investigated first.
c The final option is to hunt back for the lowest ranked grid point, not
c  on a velocity line or a rigid boundary, with a non-derivative (jc=0)
c  variable available that is found on an open pathway connected to the
c  side with the constraint.
c The idea with these latter two options is to get a reasonably even
c  spread throughout the model of the extra class 0 variables. Overall,
c  of order a third of the variables at grid points, that are not
c  earmarked for the other classes of constraint, will become x1
c  variables of class 0, with the remaining two thirds becoming x2
c  variables.
c
c Of the class 1 fault constraints the sub-class 0 and 4 ones at triple
c  junctions of faults are the most straightforward. The x1 variables
c  assigned are the obvious ones of non-derivative (jc=0) slip rates at
c  the triple junction for each fault, which are equated by the
c  constraints to the difference in velocity values across the fault
c  (i.e. positive side minus negative side).
c
c The other class 1 fault constraints require a convention to ensure
c  mutual consistency (i.e. no assigning of a variable to more than one
c  fault constraint), and also consistency in the same sense with class
c  2 velocity lines constraints and class 3 rigid boundary constraints,
c  which need to use the same convention. As well, a little care is
c  required with sub-class 2 constraints at triple junctions to ensure
c  consistency with sub-class 0 and 4 ones.
c
c The convention with sub-class 1 and 3 constraints at quarter points on
c  faults, and midpoint constraints (classes 2 and 3) on velocity lines
c  and rigid boundaries, is that on the positive side of the line the
c  more appropriate velocity derivative (x=1 or y=2) at the first grid
c  point (j-1) is assigned as the x1 variable, while on the negative
c  side of the line the derivative assigned as the x1 variable is at the
c  second grid point (j).
c For faults sub-class 1 constraints matching slip rate to difference in
c  velocity at the first quarter point are associated with the x1
c  derivative variable on the positive side of the line at the first
c  grid point, and the corresponding sub-class 3 constraint at the other
c  quarter point is associated with the x1 derivative variable on the
c  negative side of the line at the second grid point.
c
c For velocity lines the x1 derivative variable on each side of the line
c  is associated with the midpoint constraint on that side of the line.
c  In contrast, rigid boundaries are all external, and therefore have
c  only one side. (For external velocity lines the side that is used is
c  the one, positive or negative, that fits in with the sign convention
c  for assigning x1 variables - i.e. external velocity lines are not
c  treated as special cases, other than having variables on only one
c  side.)
c For both velocity lines and rigid boundaries the midpoint constraints
c  match the velocity field to the prescribed velocity value at the
c  midpoint, which is done on both sides of the line in the case of

c  internal velocity lines.
c
c With faults there is the possibility of a rigid boundary line on one
c  side, in which case there is are no derivative variables on that side
c  to assign as x1 variables. In this case the slip-rate derivative
c  variables for the fault itself are used instead. The first point
c  (j-1) slip-rate derivative variables are used if the rigid boundary
c  is on the positive side of the fault, and the second point (j)
c  variables are used when the fault is on the negative side.
c At zero-slip ends of faults there are no sub-class 1 or 3 constraints
c  at the adjacent quarter points, as the need for constraints is
c  replaced by the hardwired (x0) specification of zero slip rate and
c  zero slip-rate derivative at such end points.

c
c For the remaining fault sub-class 2 a reverse convention is used to
c  that used for sub-classes 1 and 3. This ensures that slip-rate
c  derivative variables are available at triple junctions where
c  non-derivative slip rate variables are already taken up by sub-class
c  0 and 4 constraints, even in cases where the triple junction is at a
c  rigid boundary. At all other points, the x1 variables assigned to
c  sub-class 2 constraints are non-derivative slip rate variables, and
c  the following convention is used.
c If there is a rigid boundary on the positive side of the fault, the
c  non-derivative (jc=0) variables assigned to the sub-class 2
c  constraints are at the second (j) point, and where there is a triple
c  junction the slip-rate derivative (jc=1) variable at the second (j)
c  point is used instead.
c Otherwise, the x1 variables assigned to sub-class 2 constraints are at
c  the first (j-1) point, with again the slip-rate derivative variable
c  being used at triple junctions instead of the non-derivative slip
c  rate variable.
c
c Note that in no case is a non-derivative velocity variable assigned to
c  a class 1, 2 or 3 constraint. Non-derivative velocity variables at
c  grid points on faults are therefore available to be used for class 0
c  constraints.
c At grid points on velocity lines and rigid boundaries the
c  non-derivative velocity variables are hardwired as x0 variables to be
c  equal to the specified velocities at those points.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)))
      integer nx1,s,e,gp1,gp2,gp3
      integer e2,ic,jc,gp,enow,nx1gp,r,ip,f,j,tj,gp1f,gp2f,ep,em,jcrb
      integer fT,Tf,Xf,vl,gp1vl,gp2vl,Jvl,Tvl,Xvl,rb,gp1rb,gp2rb
      integer e1s(maxs),e2s(maxs)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer smaxgp(maxgp)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer smaxx(maxx),kx(maxx),ix(maxx),jx(maxx),
     1      jcx(maxx),icx(maxx)
      integer gprank(maxgp),erank(maxe)
      integer e1e(maxe),e2e(maxe),e3e(maxe)
      logical p0zero(maxf),pnzero(maxf)
      logical efree(maxe)

      real*8 dx,dy,x1,y1,dxe,dye
      real*8 long(maxgp),lat(maxgp)
c
      if (e1s(s).eq.e) then
            e2=e2s(s)
      else
            e2=e1s(s)
      end if
c
c CONSTRAINT CLASS = 1
c
      if (isf(s).ne.0) then
            f=fs(isf(s))
            j=sfs(isf(s))
            gp1f=gpf(j-1,f)
            gp2f=gpf(j,f)
            x1=long(gp1f)
            y1=lat(gp1f)
            dx=long(gp2f)-x1
            dy=lat(gp2f)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=e2
            else
                  em=e
                  ep=e2
            end if
            if (dabs(dx).gt.dabs(dy)) then
                  jc=1
                  jcrb=2
            else
                  jc=2
                  jcrb=1
            end if
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1
c
            if ((j.eq.1).and.p0zero(f).and.(ep.ne.0)) then
                  do ic=1,2
                        xfdu(ic,0,j,f)=nx1+ic
                        smaxx(nx1+ic)=smaxgp(gp1f)
                        kx(nx1+ic)=21
                        ix(nx1+ic)=f
                        jx(nx1+ic)=j
                        jcx(nx1+ic)=0
                        icx(nx1+ic)=ic
                  end do
                  nx1=nx1+2
            end if
            if ((j.ne.1).or.(.not.p0zero(f))) then
                  if (ep.ne.0) then
                        if (gp1f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                        if (gp1f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                        if (gp1f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                        ip=ipf(gp1f)
                        if (r.eq.0) then
                              if ((em.ne.0).and.(ipvl(gp1f).eq.0)
     1                              .and.(tjfp(ip).eq.0)) then
                                    do ic=1,2
                                          xgpu(ic,0,gp1f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp1f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                                    goto 110
                              end if
                              if ((em.ne.0).and.(tjfp(ip).ne.0)) then
                                    do ic=1,2
                                          xfdu(ic,0,j-1,f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=21
                                          ix(nx1+ic)=f
                                          jx(nx1+ic)=j-1
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                                    goto 110
                              end if
                              if (tjfp(ip).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp1f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp1f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=jc
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              if (em.eq.0) then
                                    if ((ipvl(gp1f).eq.0)
     1                                    .and.(tjfp(ip).eq.0)) then
                                          nx1=nx1+2
                                          do ic=1,2
                                                xgpu(ic,0,gp1f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp1f)
                                                kx(nx1+ic)=0
                                                ix(nx1+ic)=gp1f
                                                jx(nx1+ic)=0
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          goto 110
                                    end if
                                    if (tjfp(ip).ne.0) then
                                          do ic=1,2
                                                xfdu(ic,0,j-1,f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp1f)
                                                kx(nx1+ic)=21
                                                ix(nx1+ic)=f
                                                jx(nx1+ic)=j-1
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          nx1=nx1+2
                                          do ic=1,2
                                                xgpu(ic,0,gp1f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp1f)
                                                kx(nx1+ic)=0
                                                ix(nx1+ic)=gp1f
                                                jx(nx1+ic)=0
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          goto 110
                                    end if
                                    nx1=nx1+2
                                    do ic=1,2
                                          xgpu(ic,jcrb,gp1f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp1f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 110
                        end if
                        if (tjfp(ip).ne.0) then
                              tj=tjfp(ip)
                              do ic=1,2
                                    xfdu(ic,0,j-1,f)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp1f)
                                    kx(nx1+ic)=21
                                    ix(nx1+ic)=f
                                    jx(nx1+ic)=j-1
                                    jcx(nx1+ic)=0
                                    icx(nx1+ic)=ic
                              end do
                              if (em.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xtju(ic,0,r,tj)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=3
                                          ix(nx1+ic)=tj
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 110
                        end if
                        ip=ipvl(gp1f)
                        if (fTvlp(ip).ne.0) then
                              fT=fTvlp(ip)
                              do ic=1,2
                                    xfTu(ic,jc,r,fT)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp1f)
                                    kx(nx1+ic)=5
                                    ix(nx1+ic)=fT
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (em.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xfTu(ic,jcrb,r,fT)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=5
                                          ix(nx1+ic)=fT
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 110
                        end if
                        if (Tfvlp(ip).ne.0) then
                              Tf=Tfvlp(ip)
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp1f)
                                    kx(nx1+ic)=6
                                    ix(nx1+ic)=Tf
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (em.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xTfu(ic,jcrb,r,Tf)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=6
                                          ix(nx1+ic)=Tf
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 110
                        end if
                        if (Xfvlp(ip).ne.0) then
                              Xf=Xfvlp(ip)
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp1f)
                                    kx(nx1+ic)=8
                                    ix(nx1+ic)=Xf
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (em.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xXfu(ic,jcrb,r,Xf)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp1f)
                                          kx(nx1+ic)=8
                                          ix(nx1+ic)=Xf
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                        end if
110                     nx1=nx1+2
                  end if
            end if
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 3
c
            if ((j.ne.nfs(f)).or.(.not.pnzero(f))) then
                  if (em.ne.0) then
                        ip=ipf(gp2f)
                        if ((tjfp(ip).eq.0).and.
     1                        (ipvl(gp2f).eq.0)) then
                              do ic=1,2
                                    xfu(ic,jc,j,f)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2f)
                                    kx(nx1+ic)=1
                                    ix(nx1+ic)=f
                                    jx(nx1+ic)=j
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (ep.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xfu(ic,0,j,f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=1
                                          ix(nx1+ic)=f
                                          jx(nx1+ic)=j
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 130
                        end if
                        if (gp2f.eq.gp1e(em)) r=rgp1fv(iefv(em))
                        if (gp2f.eq.gp2e(em)) r=rgp2fv(iefv(em))
                        if (gp2f.eq.gp3e(em)) r=rgp3fv(iefv(em))
                        if (r.eq.0) then
                              if ((ep.ne.0).and.(ipvl(gp2f).eq.0)
     1                              .and.(tjfp(ip).eq.0)) then
                                    do ic=1,2
                                          xgpu(ic,0,gp2f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp2f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                                    goto 130
                              end if
                              if ((ep.ne.0).and.(tjfp(ip).ne.0)) then
                                    do ic=1,2
                                          xfdu(ic,0,j,f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=21
                                          ix(nx1+ic)=f
                                          jx(nx1+ic)=j
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                                    goto 130
                              end if
                              if (tjfp(ip).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp2f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp2f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=jc
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              if (ep.eq.0) then
                                    if ((ipvl(gp2f).eq.0)
     1                                    .and.(tjfp(ip).eq.0)) then
                                          nx1=nx1+2
                                          do ic=1,2
                                                xgpu(ic,0,gp2f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp2f)
                                                kx(nx1+ic)=0
                                                ix(nx1+ic)=gp2f
                                                jx(nx1+ic)=0
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          goto 130
                                    end if
                                    if (tjfp(ip).ne.0) then
                                          do ic=1,2
                                                xfdu(ic,0,j,f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp2f)
                                                kx(nx1+ic)=21
                                                ix(nx1+ic)=f
                                                jx(nx1+ic)=j
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          nx1=nx1+2
                                          do ic=1,2
                                                xgpu(ic,0,gp2f)=nx1+ic
                                                smaxx(nx1+ic)=
     1                                                smaxgp(gp2f)
                                                kx(nx1+ic)=0
                                                ix(nx1+ic)=gp2f
                                                jx(nx1+ic)=0
                                                jcx(nx1+ic)=0
                                                icx(nx1+ic)=ic
                                          end do
                                          goto 130
                                    end if
                                    nx1=nx1+2
                                    do ic=1,2
                                          xgpu(ic,jcrb,gp2f)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=0
                                          ix(nx1+ic)=gp2f
                                          jx(nx1+ic)=0
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 130
                        end if
                        if (tjfp(ip).ne.0) then
                              tj=tjfp(ip)
                              do ic=1,2
                                    xfdu(ic,0,j,f)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2f)
                                    kx(nx1+ic)=21
                                    ix(nx1+ic)=f
                                    jx(nx1+ic)=j
                                    jcx(nx1+ic)=0
                                    icx(nx1+ic)=ic
                              end do
                              if (ep.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xtju(ic,0,r,tj)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=1
                                          ix(nx1+ic)=tj
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=0
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 130
                        end if
                        ip=ipvl(gp2f)
                        if (fTvlp(ip).ne.0) then
                              fT=fTvlp(ip)
                              do ic=1,2
                                    xfTu(ic,jc,r,fT)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2f)
                                    kx(nx1+ic)=5
                                    ix(nx1+ic)=fT
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (ep.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xfTu(ic,jcrb,r,fT)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=5
                                          ix(nx1+ic)=fT
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 130
                        end if
                        if (Tfvlp(ip).ne.0) then
                              Tf=Tfvlp(ip)
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2f)
                                    kx(nx1+ic)=6
                                    ix(nx1+ic)=Tf
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (ep.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xTfu(ic,jcrb,r,Tf)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=6
                                          ix(nx1+ic)=Tf
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                              goto 130
                        end if
                        if (Xfvlp(ip).ne.0) then
                              Xf=Xfvlp(ip)
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2f)
                                    kx(nx1+ic)=8
                                    ix(nx1+ic)=Xf
                                    jx(nx1+ic)=r
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              if (ep.eq.0) then
                                    nx1=nx1+2
                                    do ic=1,2
                                          xXfu(ic,jcrb,r,Xf)=nx1+ic
                                          smaxx(nx1+ic)=smaxgp(gp2f)
                                          kx(nx1+ic)=8
                                          ix(nx1+ic)=Xf
                                          jx(nx1+ic)=r
                                          jcx(nx1+ic)=jcrb
                                          icx(nx1+ic)=ic
                                    end do
                              end if
                        end if
130                     nx1=nx1+2
                  end if
            end if
            if ((j.eq.nfs(f)).and.pnzero(f).and.(em.ne.0)) then
                  do ic=1,2
                        xfdu(ic,0,j-1,f)=nx1+ic
                        smaxx(nx1+ic)=smaxgp(gp2f)
                        kx(nx1+ic)=21
                        ix(nx1+ic)=f
                        jx(nx1+ic)=j-1
                        jcx(nx1+ic)=0
                        icx(nx1+ic)=ic
                  end do
                  nx1=nx1+2
            end if
c
      end if
c
c CONSTRAINT CLASS = 2
c
      if (isvl(s).ne.0) then
            vl=vls(isvl(s))
            j=svls(isvl(s))
            gp1vl=gpvl(j-1,vl)
            gp2vl=gpvl(j,vl)
            x1=long(gp1vl)
            y1=lat(gp1vl)
            dx=long(gp2vl)-x1
            dy=lat(gp2vl)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=e2
            else
                  em=e
                  ep=e2
            end if
            if (dabs(dx).gt.dabs(dy)) then
                  jc=1
            else
                  jc=2
            end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 0
c
            if (ep.ne.0) then
                  if (gp1vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                  if (gp1vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                  if (gp1vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                  if (r.eq.0) then
                        do ic=1,2
                              xgpu(ic,jc,gp1vl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=0
                              ix(nx1+ic)=gp1vl
                              jx(nx1+ic)=0
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  ip=ipvl(gp1vl)
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do ic=1,2
                              xJvlu(ic,jc,r,Jvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=4
                              ix(nx1+ic)=Jvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do ic=1,2
                              xfTu(ic,jc,r,fT)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=5
                              ix(nx1+ic)=fT
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do ic=1,2
                              xTfu(ic,jc,r,Tf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=6
                              ix(nx1+ic)=Tf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do ic=1,2
                              xTvlu(ic,jc,r,Tvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=7
                              ix(nx1+ic)=Tvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do ic=1,2
                              xXfu(ic,jc,r,Xf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=8
                              ix(nx1+ic)=Xf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 140
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do ic=1,2
                              xXvlu(ic,jc,r,Xvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1vl)
                              kx(nx1+ic)=9
                              ix(nx1+ic)=Xvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                  end if
140               nx1=nx1+2
            end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 1
c
            if (em.ne.0) then
                  ip=ipvl(gp2vl)
                  if ((j.ne.nvls(vl)).and.
     1                  (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0))
     3                  then
                        do ic=1,2
                              xvlu(ic,jc,j,vl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=2
                              ix(nx1+ic)=vl
                              jx(nx1+ic)=j
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if ((j.eq.nvls(vl)).and.
     1                  (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     2                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0))
     3                  then
                        do ic=1,2
                              xgpu(ic,jc,gp2vl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=0
                              ix(nx1+ic)=gp2vl
                              jx(nx1+ic)=0
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (gp2vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
                  if (gp2vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
                  if (gp2vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do ic=1,2
                              xJvlu(ic,jc,r,Jvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=4
                              ix(nx1+ic)=Jvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do ic=1,2
                              xfTu(ic,jc,r,fT)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=5
                              ix(nx1+ic)=fT
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do ic=1,2
                              xTfu(ic,jc,r,Tf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=6
                              ix(nx1+ic)=Tf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do ic=1,2
                              xTvlu(ic,jc,r,Tvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=7
                              ix(nx1+ic)=Tvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do ic=1,2
                              xXfu(ic,jc,r,Xf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=8
                              ix(nx1+ic)=Xf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 150
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do ic=1,2
                              xXvlu(ic,jc,r,Xvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2vl)
                              kx(nx1+ic)=9
                              ix(nx1+ic)=Xvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                  end if
150               nx1=nx1+2
            end if
c
      end if
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0
c
      if ((isrb(s).ne.0).and.(isf(s).eq.0)) then
            rb=rbs(isrb(s))
            j=srbs(isrb(s))
            gp1rb=gprb(j-1,rb)
            gp2rb=gprb(j,rb)
            x1=long(gp1rb)
            y1=lat(gp1rb)
            dx=long(gp2rb)-x1
            dy=lat(gp2rb)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=0
            else
                  em=e
                  ep=0
            end if
            if (dabs(dx).gt.dabs(dy)) then
                  jc=1
            else
                  jc=2
            end if
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0 (positive side)
c
            if (ep.ne.0) then
                  r=rgprb(j-1,rb)
                  if (r.eq.0) then
                        do ic=1,2
                              xgpu(ic,jc,gp1rb)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1rb)
                              kx(nx1+ic)=0
                              ix(nx1+ic)=gp1rb
                              jx(nx1+ic)=0
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 160
                  end if
                  if (ipvl(gp1rb).eq.0) then
                        ip=ipf(gp1rb)
                        if (tjfp(ip).eq.0) then
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              do ic=1,2
                                    xfu(ic,jc,gp,f)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp1rb)
                                    kx(nx1+ic)=1
                                    ix(nx1+ic)=f
                                    jx(nx1+ic)=gp
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              goto 160
                        end if
                        tj=tjfp(ip)
                        do ic=1,2
                              xtju(ic,jc,r,tj)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1rb)
                              kx(nx1+ic)=3
                              ix(nx1+ic)=tj
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 160
                  end if
                  ip=ipvl(gp1rb)
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do ic=1,2
                              xJvlu(ic,jc,r,Jvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1rb)
                              kx(nx1+ic)=4
                              ix(nx1+ic)=Jvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 160
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do ic=1,2
                              xTfu(ic,jc,r,Tf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1rb)
                              kx(nx1+ic)=6
                              ix(nx1+ic)=Tf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 160
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do ic=1,2
                              xXfu(ic,jc,r,Xf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp1rb)
                              kx(nx1+ic)=8
                              ix(nx1+ic)=Xf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                  end if
160               nx1=nx1+2
            end if
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0 (negative side)
c
            if (em.ne.0) then
                  r=rgprb(j,rb)
                  if (r.eq.0) then
                        do ic=1,2
                              xgpu(ic,jc,gp2rb)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2rb)
                              kx(nx1+ic)=0
                              ix(nx1+ic)=gp2rb
                              jx(nx1+ic)=0
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 170
                  end if
                  if (ipvl(gp2rb).eq.0) then
                        ip=ipf(gp2rb)
                        if (tjfp(ip).eq.0) then
                              f=fp(jfp(ip),ip)
                              gp=gpfp(jfp(ip),ip)
                              do ic=1,2
                                    xfu(ic,jc,gp,f)=nx1+ic
                                    smaxx(nx1+ic)=smaxgp(gp2rb)
                                    kx(nx1+ic)=1
                                    ix(nx1+ic)=f
                                    jx(nx1+ic)=gp
                                    jcx(nx1+ic)=jc
                                    icx(nx1+ic)=ic
                              end do
                              goto 170
                        end if
                        tj=tjfp(ip)
                        do ic=1,2
                              xtju(ic,jc,r,tj)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2rb)
                              kx(nx1+ic)=3
                              ix(nx1+ic)=tj
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 170
                  end if
                  ip=ipvl(gp2rb)
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do ic=1,2
                              xJvlu(ic,jc,r,Jvl)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2rb)
                              kx(nx1+ic)=4
                              ix(nx1+ic)=Jvl
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 170
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do ic=1,2
                              xTfu(ic,jc,r,Tf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2rb)
                              kx(nx1+ic)=6
                              ix(nx1+ic)=Tf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                        goto 170
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do ic=1,2
                              xXfu(ic,jc,r,Xf)=nx1+ic
                              smaxx(nx1+ic)=smaxgp(gp2rb)
                              kx(nx1+ic)=8
                              ix(nx1+ic)=Xf
                              jx(nx1+ic)=r
                              jcx(nx1+ic)=jc
                              icx(nx1+ic)=ic
                        end do
                  end if
170               nx1=nx1+2
            end if
c
      end if
c
      return
      end
c
c
      SUBROUTINE loadx2(nx,s,e,gp1,gp2,gp3,e1s,e2s,isf,isvl,isrb,
     1      smaxgp,xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,xTvlu,
     2      xXfu,xXvlu,xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     3      long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     4      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     5      iefv,rgp1fv,rgp2fv,rgp3fv,fp,gpfp,jfp,tjfp,
     6      fs,sfs,gpf,vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     7      nfs,p0zero,pnzero,nvls)
c
c This routine loads variables into the x2 part of the x vector. The
c  sequencing follows that for the x1 part.
c All variables that are not either x1 variables or (x0) variables with
c  prescibed values are added.
c The x1, x2 and x0 variables all come in (x,y) or (t,n) component
c  pairs, so only the first of each pair needs to checked to make sure
c  the pair is not already included.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)))
      integer nx,s,e,gp1,gp2,gp3
      integer e2,ic,jc,gp,r,ip,f,j,tj,gp1f,gp2f,ep,em
      integer fT,Tf,Xf,vl,gp1vl,gp2vl,Jvl,Tvl,Xvl,rb,gp1rb,gp2rb
      integer e1s(maxs),e2s(maxs)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer smaxgp(maxgp)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer smaxx(maxx),kx(maxx),ix(maxx),jx(maxx),
     1      jcx(maxx),icx(maxx)
      logical p0zero(maxf),pnzero(maxf)
      real*8 dx,dy,x1,y1,dxe,dye
      real*8 long(maxgp),lat(maxgp)
c
      if (e1s(s).eq.e) then
            e2=e2s(s)
      else
            e2=e1s(s)
      end if
c
c SIDE CLASS = 0, SUB-CLASS = 0
c
      if (isf(s)+isvl(s)+isrb(s).eq.0) then
            if (smaxgp(gp2).lt.smaxgp(gp3)) then
                  gp=gp3
            else
                  gp=gp2
            end if
      end if
c
      if (ipf(gp2)+ipvl(gp2)+iprb(gp2).eq.0) then
            do jc=0,2
                  if (xgpu(1,jc,gp2).eq.0) then
                        do ic=1,2
                              xgpu(ic,jc,gp2)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp2
                              jx(nx+ic)=0
                              jcx(nx+ic)=jc
                              icx(nx+ic)=ic
                        end do
                        nx=nx+2
                  end if
            end do
      end if
c
      if (ipf(gp3)+ipvl(gp3)+iprb(gp3).eq.0) then
            do jc=0,2
                  if (xgpu(1,jc,gp3).eq.0) then
                        do ic=1,2
                              xgpu(ic,jc,gp3)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp3)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp3
                              jx(nx+ic)=0
                              jcx(nx+ic)=jc
                              icx(nx+ic)=ic
                        end do
                        nx=nx+2
                  end if
            end do
      end if

c
c SIDE CLASS = 1
c
      if (isf(s).ne.0) then
            f=fs(isf(s))
            j=sfs(isf(s))
            gp1f=gpf(j-1,f)
            gp2f=gpf(j,f)
            x1=long(gp1f)
            y1=lat(gp1f)
            dx=long(gp2f)-x1
            dy=lat(gp2f)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=e2
            else
                  em=e
                  ep=e2
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 0 (zero-slip end that is not in the middle
c   of another fault or on a velocity line)
c
            ip=ipf(gp1f)
            if ((j.eq.1).and.p0zero(f).and.(jfp(ip).eq.0)
     1            .and.(ipvl(gp1f).eq.0)) then
                  do jc=0,2
                        if (xgpu(1,jc,gp1f).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp1f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1f)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp1f
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 1 (positive side)
c
            if ((j.ne.1).or.(.not.p0zero(f))) then
                  if (ep.eq.0) goto 110
                  if (gp1f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                  if (gp1f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                  if (gp1f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                  if ((r.eq.0).and.(ipvl(gp1f).eq.0)) then
                        do jc=0,2
                              if (xgpu(1,jc,gp1f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp1f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 110
                  end if
                  if ((r.eq.0).and.(ipvl(gp1f).ne.0)) then
                        do jc=1,2
                              if (xgpu(1,jc,gp1f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp1f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 110
                  end if
                  ip=ipf(gp1f)
                  if (tjfp(ip).ne.0) then
                        tj=tjfp(ip)
                        do jc=0,2
                              if (xtju(1,jc,r,tj).eq.0) then
                                    do ic=1,2
                                          xtju(ic,jc,r,tj)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=3
                                          ix(nx+ic)=tj
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 110
                  end if
                  ip=ipvl(gp1f)
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 110
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 110
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
110               continue
c
c SIDE CLASS = 1, SUB-CLASS = 1 (negative side)
c
                  if (em.eq.0) goto 120
                  ip=ipf(gp1f)
                  if ((tjfp(ip).eq.0).and.
     1                        (ipvl(gp1f).eq.0)) then
                        do jc=0,2
                              if (xfu(1,jc,j-1,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,j-1,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=j-1
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 120
                  end if
                  if (gp1f.eq.gp1e(em)) r=rgp1fv(iefv(em))
                  if (gp1f.eq.gp2e(em)) r=rgp2fv(iefv(em))
                  if (gp1f.eq.gp3e(em)) r=rgp3fv(iefv(em))
                  if (r.eq.0) then
                        do jc=0,2
                              if (xgpu(1,jc,gp1f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp1f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 120
                  end if
                  if (tjfp(ip).ne.0) then
                        tj=tjfp(ip)
                        do jc=0,2
                              if (xtju(1,jc,r,tj).eq.0) then
                                    do ic=1,2
                                          xtju(ic,jc,r,tj)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=3
                                          ix(nx+ic)=tj
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 120
                  end if
                  ip=ipvl(gp1f)
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 120
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 120
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1f)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
120               continue
c
c SIDE CLASS = 1, SUB-CLASS = 1 (fault)
c
                  if (ipvl(gp1f).eq.0) then
                        if (xfdu(1,0,j-1,f).eq.0) then
                              do ic=1,2
                                    xfdu(ic,0,j-1,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1f)
                                    kx(nx+ic)=21
                                    ix(nx+ic)=f
                                    jx(nx+ic)=j-1
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end if
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 3 (positive side)
c
            if ((j.ne.nfs(f)).or.(.not.pnzero(f))) then
                  if (ep.eq.0) goto 130
                  if (gp2f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                  if (gp2f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                  if (gp2f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                  if ((r.eq.0).and.(ipvl(gp2f).eq.0)) then
                        do jc=0,2
                              if (xgpu(1,jc,gp2f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp2f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 130
                  end if
                  if ((r.eq.0).and.(ipvl(gp2f).ne.0)) then
                        do jc=1,2
                              if (xgpu(1,jc,gp2f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp2f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 130
                  end if
                  ip=ipf(gp2f)
                  if (tjfp(ip).ne.0) then
                        tj=tjfp(ip)
                        do jc=0,2
                              if (xtju(1,jc,r,tj).eq.0) then
                                    do ic=1,2
                                          xtju(ic,jc,r,tj)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=3
                                          ix(nx+ic)=tj
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 130
                  end if
                  ip=ipvl(gp2f)
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 130
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 130
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
130               continue
c
c SIDE CLASS = 1, SUB-CLASS = 3 (negative side)
c
                  if (em.eq.0) goto 140
                  ip=ipf(gp2f)
                  if ((tjfp(ip).eq.0).and.
     1                        (ipvl(gp2f).eq.0)) then
                        do jc=0,2
                              if (xfu(1,jc,j,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,j,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=j
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 140
                  end if
                  if (gp2f.eq.gp1e(em)) r=rgp1fv(iefv(em))
                  if (gp2f.eq.gp2e(em)) r=rgp2fv(iefv(em))
                  if (gp2f.eq.gp3e(em)) r=rgp3fv(iefv(em))
                  if (r.eq.0) then
                        do jc=0,2
                              if (xgpu(1,jc,gp2f).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp2f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2f
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 140
                  end if
                  if (tjfp(ip).ne.0) then
                        tj=tjfp(ip)
                        do jc=0,2
                              if (xtju(1,jc,r,tj).eq.0) then
                                    do ic=1,2
                                          xtju(ic,jc,r,tj)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=3
                                          ix(nx+ic)=tj
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 140
                  end if
                  ip=ipvl(gp2f)
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 140
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 140
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2f)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
140               continue
c
c SIDE CLASS = 1, SUB-CLASS = 3 (fault)
c
                  if (ipvl(gp2f).eq.0) then
                        if (xfdu(1,0,j,f).eq.0) then
                              do ic=1,2
                                    xfdu(ic,0,j,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2f)
                                    kx(nx+ic)=21
                                    ix(nx+ic)=f
                                    jx(nx+ic)=j
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                        goto 150
                  end if
150               continue
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 4 (zero-slip end that is not in the middle
c   of another fault or on a velocity line)
c
            ip=ipf(gp2f)
            if ((j.eq.nfs(f)).and.pnzero(f).and.(jfp(ip).eq.0)
     1            .and.(ipvl(gp2f).eq.0)) then
                  do jc=0,2
                        if (xgpu(1,jc,gp2f).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp2f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2f)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp2f
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
c
      end if
c
c SIDE CLASS = 2
c
      if (isvl(s).ne.0) then
            vl=vls(isvl(s))
            j=svls(isvl(s))
            gp1vl=gpvl(j-1,vl)
            gp2vl=gpvl(j,vl)
            x1=long(gp1vl)
            y1=lat(gp1vl)
            dx=long(gp2vl)-x1
            dy=lat(gp2vl)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=e2
            else
                  em=e
                  ep=e2
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 0 (simple end)
c
            ip=ipvl(gp1vl)
            if ((j.eq.1).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
                  do jc=1,2
                        if (xgpu(1,jc,gp1vl).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp1vl)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1vl)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp1vl
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2

                        end if
                  end do
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 0 (positive side)
c
            if ((j.ne.1).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
                  if (ep.eq.0) goto 160
                  if (gp1vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                  if (gp1vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                  if (gp1vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                  if (r.eq.0) then
                        do jc=1,2
                              if (xgpu(1,jc,gp1vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp1vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do jc=1,2

                              if (xJvlu(1,jc,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,jc,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do jc=1,2
                              if (xTvlu(1,jc,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,jc,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 160
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do jc=1,2
                              if (xXvlu(1,jc,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,jc,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
160               continue
c
c SIDE CLASS = 2, SUB-CLASS = 0 (negative side)
c
                  if (em.eq.0) goto 170
                  if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0) then
                        do jc=1,2
                              if (xvlu(1,jc,j-1,vl).eq.0) then
                                    do ic=1,2
                                          xvlu(ic,jc,j-1,vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=2
                                          ix(nx+ic)=vl
                                          jx(nx+ic)=j-1
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (gp1vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
                  if (gp1vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
                  if (gp1vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do jc=1,2
                              if (xJvlu(1,jc,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,jc,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do jc=1,2
                              if (xTvlu(1,jc,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,jc,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do jc=1,2
                              if (xXvlu(1,jc,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,jc,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
170               continue
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 1 (positive side)
c
            ip=ipvl(gp2vl)
            if ((j.ne.nvls(vl)).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
                  if (ep.eq.0) goto 180
                  if (gp2vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                  if (gp2vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                  if (gp2vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                  if (r.eq.0) then
                        do jc=1,2
                              if (xgpu(1,jc,gp2vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,jc,gp2vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 180
                  end if
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do jc=1,2
                              if (xJvlu(1,jc,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,jc,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 180
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2

                              end if
                        end do
                        goto 180
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2

                              end if
                        end do
                        goto 180
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do jc=1,2
                              if (xTvlu(1,jc,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,jc,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 180
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 180
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do jc=1,2
                              if (xXvlu(1,jc,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,jc,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
180               continue
c
c SIDE CLASS = 2, SUB-CLASS = 1 (negative side)
c
                  if (em.eq.0) goto 190
                  if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                  +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0) then
                        do jc=1,2
                              if (xvlu(1,jc,j,vl).eq.0) then
                                    do ic=1,2
                                          xvlu(ic,jc,j,vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=2
                                          ix(nx+ic)=vl
                                          jx(nx+ic)=j
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 190
                  end if
                  if (gp2vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
                  if (gp2vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
                  if (gp2vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
                  if (Jvlvlp(ip).ne.0) then
                        Jvl=Jvlvlp(ip)
                        do jc=1,2
                              if (xJvlu(1,jc,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,jc,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 190
                  end if
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        do jc=1,2
                              if (xfTu(1,jc,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,jc,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 190
                  end if
                  if (Tfvlp(ip).ne.0) then
                        Tf=Tfvlp(ip)
                        do jc=1,2
                              if (xTfu(1,jc,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,jc,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 190
                  end if
                  if (Tvlvlp(ip).ne.0) then
                        Tvl=Tvlvlp(ip)
                        do jc=1,2
                              if (xTvlu(1,jc,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,jc,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if

                        end do
                        goto 190
                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        do jc=1,2
                              if (xXfu(1,jc,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,jc,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 190
                  end if
                  if (Xvlvlp(ip).ne.0) then
                        Xvl=Xvlvlp(ip)
                        do jc=1,2
                              if (xXvlu(1,jc,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,jc,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                  end if
190               continue
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 1 (simple end)
c
            if ((j.eq.nvls(vl)).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
                  do jc=1,2
                        if (xgpu(1,jc,gp2vl).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp2vl)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2vl)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp2vl
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
c
      end if
c
c SIDE CLASS = 3, SUB-CLASS = 0
c
      if ((isrb(s).ne.0).and.(isf(s).eq.0)) then
            rb=rbs(isrb(s))
            j=srbs(isrb(s))
            gp1rb=gprb(j-1,rb)
            gp2rb=gprb(j,rb)
c
c SIDE CLASS = 3, SUB-CLASS = 0 (first point)
c
            r=rgprb(j-1,rb)
            if (r.eq.0) then
                  do jc=1,2
                        if (xgpu(1,jc,gp1rb).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp1rb)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp1rb
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 200
            end if
            if (ipvl(gp1rb).eq.0) then
                  ip=ipf(gp1rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        do jc=1,2
                              if (xfu(1,jc,gp,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,gp,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1rb)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=gp
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 200
                  end if
                  tj=tjfp(ip)
                  do jc=1,2
                        if (xtju(1,jc,r,tj).eq.0) then
                              do ic=1,2
                                    xtju(ic,jc,r,tj)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=3
                                    ix(nx+ic)=tj
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 200
            end if
            ip=ipvl(gp1rb)
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=1,2
                        if (xJvlu(1,jc,r,Jvl).eq.0) then
                              do ic=1,2
                                    xJvlu(ic,jc,r,Jvl)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=4
                                    ix(nx+ic)=Jvl

                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 200
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=1,2
                        if (xTfu(1,jc,r,Tf).eq.0) then
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=6
                                    ix(nx+ic)=Tf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 200
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=1,2
                        if (xXfu(1,jc,r,Xf).eq.0) then
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=8
                                    ix(nx+ic)=Xf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
200         continue
c
c SIDE CLASS = 3, SUB-CLASS = 0 (second point)
c
            r=rgprb(j,rb)
            if (r.eq.0) then
                  do jc=1,2
                        if (xgpu(1,jc,gp2rb).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp2rb)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp2rb
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 210
            end if
            if (ipvl(gp2rb).eq.0) then
                  ip=ipf(gp2rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        do jc=1,2
                              if (xfu(1,jc,gp,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,gp,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2rb)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=gp
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 210
                  end if
                  tj=tjfp(ip)
                  do jc=1,2
                        if (xtju(1,jc,r,tj).eq.0) then
                              do ic=1,2
                                    xtju(ic,jc,r,tj)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=3
                                    ix(nx+ic)=tj
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 210
            end if
            ip=ipvl(gp2rb)
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=1,2
                        if (xJvlu(1,jc,r,Jvl).eq.0) then
                              do ic=1,2
                                    xJvlu(ic,jc,r,Jvl)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=4
                                    ix(nx+ic)=Jvl
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 210
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=1,2
                        if (xTfu(1,jc,r,Tf).eq.0) then
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=6
                                    ix(nx+ic)=Tf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 210
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=1,2
                        if (xXfu(1,jc,r,Xf).eq.0) then
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=8
                                    ix(nx+ic)=Xf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
210         continue
c
      end if
c
      return
      end
c
c
      SUBROUTINE loadx0(nx,s,e,gp1,e1s,e2s,isf,isvl,isrb,
     1      smaxgp,xgpu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,xTvlu,
     2      xXfu,xXvlu,xfdu,xfTdu,xXfdu,smaxx,kx,ix,jx,jcx,icx,
     3      long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     4      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     5      iefv,rgp1fv,rgp2fv,rgp3fv,fp,gpfp,jfp,tjfp,

     6      fs,sfs,gpf,vls,svls,gpvl,rbs,srbs,gprb,rgprb,
     7      nfs,p0zero,pnzero,nvls,ux,uy,uxm,uym,uxp,uyp,
     8      dut,dun,ugprb,x)
c
c This routine loads variables into the x0 part at the end of the x
c  vector. The sequencing again follows that for the x1 part.
c The variables included in the x0 part are those with pre-determined
c  values. These are velocity values at grid points on velocity
c  lines and rigid boundaries, and zero-valued slip-rates and slip-rate
c  derivatives at zero-slip ends of faults, plus velocity derivatives
c  at grid points on rigid boundaries that are attached to faults but
c  not otherwise connected to the model domain.
c As mentioned in the "loadx2" routine, the x1, x2 and x0 variables all
c  come in (x,y) or (t,n) component pairs, so only the first of each
c  pair needs to checked to make sure the pair is not already included.
c Here the checking would not be necessary if it were not for the fact
c  that each grid point is at the end of more than one side, and the
c  assignment of variables is done in the order of the sides, not in the
c  order of the grid points.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)))
      integer nx,s,e,gp1
      integer e2,ic,jc,gp,r,ip,f,j,tj,gp1f,gp2f,ep,em,fT,Tf,Xf
      integer vl,gp1vl,gp2vl,jgp,Jvl,Tvl,Xvl,rb,gp1rb,gp2rb
      integer e1s(maxs),e2s(maxs)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer smaxgp(maxgp)
      integer xgpu(2,0:2,maxgp)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer smaxx(maxx),kx(maxx),ix(maxx),jx(maxx),
     1      jcx(maxx),icx(maxx)
      logical p0zero(maxf),pnzero(maxf)
      real*8 xval,dx,dy,x1,y1,dxe,dye
      real*8 long(maxgp),lat(maxgp)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl),
     1      uxm(0:maxvls,maxvl),uym(0:maxvls,maxvl),
     2      uxp(0:maxvls,maxvl),uyp(0:maxvls,maxvl)
      real*8 dut(0:maxfs,maxf),dun(0:maxfs,maxf)
      real*8 ugprb(2,0:2,0:maxrbs,maxrb)
      real*8 x(maxx)
c
      if (e1s(s).eq.e) then
            e2=e2s(s)
      else
            e2=e1s(s)
      end if
c
c SIDE CLASS = 1
c
      if (isf(s).ne.0) then
            f=fs(isf(s))
            j=sfs(isf(s))
            gp1f=gpf(j-1,f)
            gp2f=gpf(j,f)
c
c SIDE CLASS = 1, SUB-CLASS = 0 (zero slip)
c
            ip=ipf(gp1f)
            if ((j.eq.1).and.p0zero(f)) then
                  do ic=1,2
                        xfdu(ic,0,j-1,f)=nx+ic
                        smaxx(nx+ic)=smaxgp(gp1f)
                        kx(nx+ic)=21
                        ix(nx+ic)=f
                        jx(nx+ic)=j-1
                        jcx(nx+ic)=0
                        icx(nx+ic)=ic
                        x(nx+ic)=0.0d0
                  end do
                  nx=nx+2
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 1 (fault)
c
            if ((j.ne.1).or.(.not.p0zero(f))) then
                  if (ipvl(gp1f).ne.0) then
                        if (xfdu(1,0,j-1,f).eq.0) then
                              do ic=1,2
                                    xfdu(ic,0,j-1,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1f)
                                    kx(nx+ic)=21
                                    ix(nx+ic)=f
                                    jx(nx+ic)=j-1
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    if (ic.eq.1) xval=dut(j-1,f)
                                    if (ic.eq.2) xval=dun(j-1,f)
                                    x(nx+ic)=xval
                              end do
                              nx=nx+2
                        end if
                  end if
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 3 (fault)
c
            if ((j.ne.nfs(f)).or.(.not.pnzero(f))) then
                  if (ipvl(gp2f).eq.0) goto 100
                  ip=ipvl(gp2f)
                  if (fTvlp(ip).ne.0) then
                        fT=fTvlp(ip)
                        if (xfTdu(1,0,fT).eq.0) then
                              do ic=1,2
                                    xfTdu(ic,0,fT)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2f)
                                    kx(nx+ic)=25
                                    ix(nx+ic)=fT
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    if (ic.eq.1) xval=dut(j,f)
                                    if (ic.eq.2) xval=dun(j,f)
                                    x(nx+ic)=xval
                              end do
                              nx=nx+2
                        end if
                        goto 100
                  end if
                  if (Tfvlp(ip).ne.0) then
                        if (xfdu(1,0,j,f).eq.0) then
                              do ic=1,2
                                    xfdu(ic,0,j,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2f)
                                    kx(nx+ic)=21
                                    ix(nx+ic)=f
                                    jx(nx+ic)=j
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    if (ic.eq.1) xval=dut(j,f)
                                    if (ic.eq.2) xval=dun(j,f)
                                    x(nx+ic)=xval
                              end do
                              nx=nx+2
                        end if
                        goto 100

                  end if
                  if (Xfvlp(ip).ne.0) then
                        Xf=Xfvlp(ip)
                        if (xXfdu(1,0,Xf).eq.0) then
                              do ic=1,2
                                    xXfdu(ic,0,Xf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2f)
                                    kx(nx+ic)=28
                                    ix(nx+ic)=Xf
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    if (ic.eq.1) xval=dut(j,f)
                                    if (ic.eq.2) xval=dun(j,f)
                                    x(nx+ic)=xval
                              end do
                              nx=nx+2
                        end if
                  end if
100               continue
            end if
c
c SIDE CLASS = 1, SUB-CLASS = 4 (zero slip)
c
            ip=ipf(gp2f)
            if ((j.eq.nfs(f)).and.pnzero(f)) then
                  do ic=1,2
                        xfdu(ic,0,j,f)=nx+ic
                        smaxx(nx+ic)=smaxgp(gp2f)
                        kx(nx+ic)=21
                        ix(nx+ic)=f
                        jx(nx+ic)=j
                        jcx(nx+ic)=0
                        icx(nx+ic)=ic
                        x(nx+ic)=0.0d0
                  end do
                  nx=nx+2
            end if
c
      end if
c
c SIDE CLASS = 2
c
      if (isvl(s).ne.0) then
            vl=vls(isvl(s))
            j=svls(isvl(s))
            gp1vl=gpvl(j-1,vl)
            gp2vl=gpvl(j,vl)
            x1=long(gp1vl)
            y1=lat(gp1vl)
            dx=long(gp2vl)-x1
            dy=lat(gp2vl)-y1
            dxe=long(gp1)-x1
            dye=lat(gp1)-y1
            if (dx*dye-dy*dxe.gt.0.0d0) then
                  ep=e
                  em=e2
            else
                  em=e
                  ep=e2
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 0 (simple end)
c
            ip=ipvl(gp1vl)
            jgp=2*j-1
            if ((j.eq.1).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
                  if (xgpu(1,0,gp1vl).eq.0) then
                        do ic=1,2
                              xgpu(ic,0,gp1vl)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1vl)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp1vl
                              jx(nx+ic)=0
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              if (ic.eq.1) xval=ux(jgp,vl)
                              if (ic.eq.2) xval=uy(jgp,vl)
                              x(nx+ic)=xval
                        end do
                        nx=nx+2
                  end if
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 0 (positive side)
c
            if ((j.ne.1).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
                  if (ep.ne.0) then
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        then
                              if (xgpu(1,0,gp1vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp1vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (gp1vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                        if (gp1vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                        if (gp1vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                        if ((r.eq.0).and.
     1                        (fTvlp(ip)+Tfvlp(ip)+Xfvlp(ip).eq.0))
     2                        then
                              if (xgpu(1,0,gp1vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp1vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if ((r.eq.0).and.
     1                        (fTvlp(ip)+Tfvlp(ip)+Xfvlp(ip).ne.0))
     2                        then
                              if (xgpu(1,0,gp1vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp1vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp1vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j-1,vl)
                                          if (ic.eq.2) xval=uyp(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (Jvlvlp(ip).ne.0) then
                              Jvl=Jvlvlp(ip)
                              if (xJvlu(1,0,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,0,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (fTvlp(ip).ne.0) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (Tfvlp(ip).ne.0) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (Tvlvlp(ip).ne.0) then
                              Tvl=Tvlvlp(ip)
                              if (xTvlu(1,0,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,0,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (Xfvlp(ip).ne.0) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 110
                        end if
                        if (Xvlvlp(ip).ne.0) then
                              Xvl=Xvlvlp(ip)
                              if (xXvlu(1,0,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,0,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                        end if
                  end if
110               continue
c
c SIDE CLASS = 2, SUB-CLASS = 0 (negative side)
c
                  if (em.ne.0) then
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        then
                              if (xvlu(1,0,j-1,vl).eq.0) then
                                    do ic=1,2
                                          xvlu(ic,0,j-1,vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=2
                                          ix(nx+ic)=vl
                                          jx(nx+ic)=j-1
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if (gp1vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
                        if (gp1vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
                        if (gp1vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
                        if (Jvlvlp(ip).ne.0) then
                              Jvl=Jvlvlp(ip)
                              if (xJvlu(1,0,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,0,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if ((fTvlp(ip).ne.0).and.(r.eq.2)) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j-1,vl)
                                          if (ic.eq.2) xval=uyp(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if ((fTvlp(ip).ne.0).and.(r.eq.3)) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if ((Tfvlp(ip).ne.0).and.(r.eq.1)) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j-1,vl)
                                          if (ic.eq.2) xval=uyp(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if ((Tfvlp(ip).ne.0).and.(r.eq.3)) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if (Tvlvlp(ip).ne.0) then
                              Tvl=Tvlvlp(ip)
                              if (xTvlu(1,0,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,0,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if ((Xfvlp(ip).ne.0).and.(r.eq.1)) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j-1,vl)
                                          if (ic.eq.2) xval=uyp(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120

                        end if
                        if ((Xfvlp(ip).ne.0).and.(r.eq.3)) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j-1,vl)
                                          if (ic.eq.2) xval=uym(j-1,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 120
                        end if
                        if (Xvlvlp(ip).ne.0) then
                              Xvl=Xvlvlp(ip)
                              if (xXvlu(1,0,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,0,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                        end if
                  end if
120               continue
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 1 (positive side)
c
            ip=ipvl(gp2vl)
            jgp=2*j+1
            if ((j.ne.nvls(vl)).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
                  if (ep.ne.0) then
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        then
                              if (xgpu(1,0,gp2vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp2vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (gp2vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
                        if (gp2vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
                        if (gp2vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
                        if ((r.eq.0).and.
     1                        (fTvlp(ip)+Tfvlp(ip)+Xfvlp(ip).eq.0))
     2                        then
                              if (xgpu(1,0,gp2vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp2vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if ((r.eq.0).and.
     1                        (fTvlp(ip)+Tfvlp(ip)+Xfvlp(ip).ne.0))
     2                        then
                              if (xgpu(1,0,gp2vl).eq.0) then
                                    do ic=1,2
                                          xgpu(ic,0,gp2vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=0
                                          ix(nx+ic)=gp2vl
                                          jx(nx+ic)=0
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j,vl)
                                          if (ic.eq.2) xval=uyp(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (Jvlvlp(ip).ne.0) then
                              Jvl=Jvlvlp(ip)
                              if (xJvlu(1,0,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,0,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (fTvlp(ip).ne.0) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT

                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (Tfvlp(ip).ne.0) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (Tvlvlp(ip).ne.0) then
                              Tvl=Tvlvlp(ip)
                              if (xTvlu(1,0,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,0,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (Xfvlp(ip).ne.0) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval

                                    end do
                                    nx=nx+2
                              end if
                              goto 130
                        end if
                        if (Xvlvlp(ip).ne.0) then
                              Xvl=Xvlvlp(ip)
                              if (xXvlu(1,0,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,0,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                        end if
                  end if
130               continue
c
c SIDE CLASS = 2, SUB-CLASS = 1 (negative side)
c
                  if (em.ne.0) then
                        if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1                        +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2                        then
                              if (xvlu(1,0,j,vl).eq.0) then
                                    do ic=1,2
                                          xvlu(ic,0,j,vl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=2
                                          ix(nx+ic)=vl
                                          jx(nx+ic)=j
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if (gp2vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
                        if (gp2vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
                        if (gp2vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
                        if (Jvlvlp(ip).ne.0) then
                              Jvl=Jvlvlp(ip)
                              if (xJvlu(1,0,r,Jvl).eq.0) then
                                    do ic=1,2
                                          xJvlu(ic,0,r,Jvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=4
                                          ix(nx+ic)=Jvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((fTvlp(ip).ne.0).and.(r.eq.2)) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j,vl)
                                          if (ic.eq.2) xval=uyp(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((fTvlp(ip).ne.0).and.(r.eq.3)) then
                              fT=fTvlp(ip)
                              if (xfTu(1,0,r,fT).eq.0) then
                                    do ic=1,2
                                          xfTu(ic,0,r,fT)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=5
                                          ix(nx+ic)=fT
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((Tfvlp(ip).ne.0).and.(r.eq.1)) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j,vl)
                                          if (ic.eq.2) xval=uyp(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((Tfvlp(ip).ne.0).and.(r.eq.3)) then
                              Tf=Tfvlp(ip)
                              if (xTfu(1,0,r,Tf).eq.0) then
                                    do ic=1,2
                                          xTfu(ic,0,r,Tf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=6
                                          ix(nx+ic)=Tf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if (Tvlvlp(ip).ne.0) then
                              Tvl=Tvlvlp(ip)
                              if (xTvlu(1,0,r,Tvl).eq.0) then
                                    do ic=1,2
                                          xTvlu(ic,0,r,Tvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=7
                                          ix(nx+ic)=Tvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((Xfvlp(ip).ne.0).and.(r.eq.1)) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxp(j,vl)
                                          if (ic.eq.2) xval=uyp(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if ((Xfvlp(ip).ne.0).and.(r.eq.3)) then
                              Xf=Xfvlp(ip)
                              if (xXfu(1,0,r,Xf).eq.0) then
                                    do ic=1,2
                                          xXfu(ic,0,r,Xf)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=8
                                          ix(nx+ic)=Xf
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=uxm(j,vl)
                                          if (ic.eq.2) xval=uym(j,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                              goto 140
                        end if
                        if (Xvlvlp(ip).ne.0) then
                              Xvl=Xvlvlp(ip)
                              if (xXvlu(1,0,r,Xvl).eq.0) then
                                    do ic=1,2
                                          xXvlu(ic,0,r,Xvl)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2vl)
                                          kx(nx+ic)=9
                                          ix(nx+ic)=Xvl
                                          jx(nx+ic)=r
                                          jcx(nx+ic)=0
                                          icx(nx+ic)=ic
                                          if (ic.eq.1) xval=ux(jgp,vl)
                                          if (ic.eq.2) xval=uy(jgp,vl)
                                          x(nx+ic)=xval
                                    end do
                                    nx=nx+2
                              end if
                        end if
                  end if
140               continue
            end if
c
c SIDE CLASS = 2, SUB-CLASS = 1 (simple end)
c
            if ((j.eq.nvls(vl)).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
                  if (xgpu(1,0,gp2vl).eq.0) then
                        do ic=1,2
                              xgpu(ic,0,gp2vl)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2vl)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp2vl
                              jx(nx+ic)=0
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              if (ic.eq.1) xval=ux(jgp,vl)
                              if (ic.eq.2) xval=uy(jgp,vl)
                              x(nx+ic)=xval
                        end do
                        nx=nx+2
                  end if
            end if
c
      end if
c
c SIDE CLASS = 3, SUB-CLASS = 0
c
      if ((isrb(s).ne.0).and.(isf(s).eq.0)) then
            rb=rbs(isrb(s))
            j=srbs(isrb(s))
            gp1rb=gprb(j-1,rb)
            gp2rb=gprb(j,rb)
c
c SIDE CLASS = 3, SUB-CLASS = 0 (first point)
c
            r=rgprb(j-1,rb)
            if (r.eq.0) then
                  if (xgpu(1,0,gp1rb).eq.0) then
                        do ic=1,2
                              xgpu(ic,0,gp1rb)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1rb)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp1rb
                              jx(nx+ic)=0
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j-1,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 150
            end if
            if (ipvl(gp1rb).eq.0) then
                  ip=ipf(gp1rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        if (xfu(1,0,gp,f).eq.0) then
                              do ic=1,2
                                    xfu(ic,0,gp,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=1
                                    ix(nx+ic)=f
                                    jx(nx+ic)=gp
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,0,j-1,rb)
                              end do
                              nx=nx+2
                        end if
                        goto 150
                  end if
                  tj=tjfp(ip)
                  if (xtju(1,0,r,tj).eq.0) then
                        do ic=1,2
                              xtju(ic,0,r,tj)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1rb)
                              kx(nx+ic)=3
                              ix(nx+ic)=tj
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j-1,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 150
            end if
            ip=ipvl(gp1rb)
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  if (xJvlu(1,0,r,Jvl).eq.0) then
                        do ic=1,2
                              xJvlu(ic,0,r,Jvl)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1rb)
                              kx(nx+ic)=4
                              ix(nx+ic)=Jvl
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j-1,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 150
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  if (xTfu(1,0,r,Tf).eq.0) then
                        do ic=1,2
                              xTfu(ic,0,r,Tf)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1rb)
                              kx(nx+ic)=6
                              ix(nx+ic)=Tf
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j-1,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 150
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  if (xXfu(1,0,r,Xf).eq.0) then
                        do ic=1,2
                              xXfu(ic,0,r,Xf)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp1rb)
                              kx(nx+ic)=8
                              ix(nx+ic)=Xf
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j-1,rb)
                        end do
                        nx=nx+2
                  end if
            end if
150         continue
c
c SIDE CLASS = 3, SUB-CLASS = 0 (second point)
c
            r=rgprb(j,rb)
            if (r.eq.0) then
                  if (xgpu(1,0,gp2rb).eq.0) then
                        do ic=1,2
                              xgpu(ic,0,gp2rb)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2rb)
                              kx(nx+ic)=0
                              ix(nx+ic)=gp2rb
                              jx(nx+ic)=0
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 160
            end if
            if (ipvl(gp2rb).eq.0) then
                  ip=ipf(gp2rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        if (xfu(1,0,gp,f).eq.0) then
                              do ic=1,2
                                    xfu(ic,0,gp,f)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=1
                                    ix(nx+ic)=f
                                    jx(nx+ic)=gp
                                    jcx(nx+ic)=0
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,0,j,rb)
                              end do
                              nx=nx+2
                        end if
                        goto 160
                  end if
                  tj=tjfp(ip)
                  if (xtju(1,0,r,tj).eq.0) then
                        do ic=1,2
                              xtju(ic,0,r,tj)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2rb)
                              kx(nx+ic)=3
                              ix(nx+ic)=tj
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 160
            end if
            ip=ipvl(gp2rb)
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  if (xJvlu(1,0,r,Jvl).eq.0) then
                        do ic=1,2
                              xJvlu(ic,0,r,Jvl)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2rb)
                              kx(nx+ic)=4
                              ix(nx+ic)=Jvl
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 160
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  if (xTfu(1,0,r,Tf).eq.0) then
                        do ic=1,2
                              xTfu(ic,0,r,Tf)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2rb)
                              kx(nx+ic)=6
                              ix(nx+ic)=Tf
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j,rb)
                        end do
                        nx=nx+2
                  end if
                  goto 160
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  if (xXfu(1,0,r,Xf).eq.0) then
                        do ic=1,2
                              xXfu(ic,0,r,Xf)=nx+ic
                              smaxx(nx+ic)=smaxgp(gp2rb)
                              kx(nx+ic)=8
                              ix(nx+ic)=Xf
                              jx(nx+ic)=r
                              jcx(nx+ic)=0
                              icx(nx+ic)=ic
                              x(nx+ic)=ugprb(ic,0,j,rb)
                        end do
                        nx=nx+2
                  end if
            end if
160         continue
c
      end if
c
c SIDE CLASS = 3 (with fault), SUB-CLASS = 0
c
      if ((isrb(s).ne.0).and.(isf(s).ne.0)) then
            rb=rbs(isrb(s))
            j=srbs(isrb(s))
            gp1rb=gprb(j-1,rb)
            gp2rb=gprb(j,rb)
c
c SIDE CLASS = 3 (with fault), SUB-CLASS = 0 (first point)
c
            r=rgprb(j-1,rb)
            if (r.eq.0) then
                  do jc=0,2
                        if (xgpu(1,jc,gp1rb).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp1rb)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp1rb
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j-1,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 170
            end if
            if (ipvl(gp1rb).eq.0) then
                  ip=ipf(gp1rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        do jc=0,2
                              if (xfu(1,jc,gp,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,gp,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp1rb)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=gp
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                          x(nx+ic)=ugprb(ic,jc,j-1,rb)
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 170
                  end if
                  tj=tjfp(ip)
                  do jc=0,2
                        if (xtju(1,jc,r,tj).eq.0) then
                              do ic=1,2
                                    xtju(ic,jc,r,tj)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=3
                                    ix(nx+ic)=tj
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j-1,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 170
            end if
            ip=ipvl(gp1rb)
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                        if (xTfu(1,jc,r,Tf).eq.0) then
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=6
                                    ix(nx+ic)=Tf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j-1,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 170
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                        if (xXfu(1,jc,r,Xf).eq.0) then
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp1rb)
                                    kx(nx+ic)=8
                                    ix(nx+ic)=Xf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j-1,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
            end if
170         continue
c
c SIDE CLASS = 3 (with fault), SUB-CLASS = 0 (second point)
c
            r=rgprb(j,rb)
            if (r.eq.0) then
                  do jc=0,2
                        if (xgpu(1,jc,gp2rb).eq.0) then
                              do ic=1,2
                                    xgpu(ic,jc,gp2rb)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=0
                                    ix(nx+ic)=gp2rb
                                    jx(nx+ic)=0
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 180
            end if
            if (ipvl(gp2rb).eq.0) then
                  ip=ipf(gp2rb)
                  if (tjfp(ip).eq.0) then
                        f=fp(jfp(ip),ip)
                        gp=gpfp(jfp(ip),ip)
                        do jc=0,2
                              if (xfu(1,jc,gp,f).eq.0) then
                                    do ic=1,2
                                          xfu(ic,jc,gp,f)=nx+ic
                                          smaxx(nx+ic)=smaxgp(gp2rb)
                                          kx(nx+ic)=1
                                          ix(nx+ic)=f
                                          jx(nx+ic)=gp
                                          jcx(nx+ic)=jc
                                          icx(nx+ic)=ic
                                          x(nx+ic)=ugprb(ic,jc,j,rb)
                                    end do
                                    nx=nx+2
                              end if
                        end do
                        goto 180
                  end if
                  tj=tjfp(ip)
                  do jc=0,2
                        if (xtju(1,jc,r,tj).eq.0) then
                              do ic=1,2
                                    xtju(ic,jc,r,tj)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=3
                                    ix(nx+ic)=tj
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 180
            end if
            ip=ipvl(gp2rb)
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                        if (xTfu(1,jc,r,Tf).eq.0) then
                              do ic=1,2
                                    xTfu(ic,jc,r,Tf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=6
                                    ix(nx+ic)=Tf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
                  goto 180
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                        if (xXfu(1,jc,r,Xf).eq.0) then
                              do ic=1,2
                                    xXfu(ic,jc,r,Xf)=nx+ic
                                    smaxx(nx+ic)=smaxgp(gp2rb)
                                    kx(nx+ic)=8
                                    ix(nx+ic)=Xf
                                    jx(nx+ic)=r
                                    jcx(nx+ic)=jc
                                    icx(nx+ic)=ic
                                    x(nx+ic)=ugprb(ic,jc,j,rb)
                              end do
                              nx=nx+2
                        end if
                  end do
            end if

180         continue
c
      end if
c
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE makem(ns,ne,nx1,nx2,nz,ny,nf,ny0,nvo,nfo,neo,nduc,nec,
     1      ny1,elist,gp1e,gp2e,gp3e,s1e,s2e,s3e,isf,isvl,isrb,e1s,e2s,
     2      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     3      iefv,kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,jgp1fv,
     4      jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,xz,cz,z,x,
     5      x1maxz,x2maxz,x0maxz,xfdu,xfTdu,xXfdu,long,lat,ipf,ipvl,
     6      iprb,Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,tjfp,fs,sfs,
     7      gpf,rbp,gprbp,rgprb,nfs,p0zero,pnzero,pindu0,pindun,pindu,
     8      pinu,vls,svls,gpvl,nvls,pinuvl,ux,uy,rbs,srbs,gprb,
     9      fp,gpfp,jfp,pinurb,usrb,epot,esub,xy,ay,y,x1maxy,x2maxy,
     1      x0maxy,dusub,dupot,evo,ou,uvo,ffo,sfo,nfoc,odu,ducfo,eeo,
     1      neoc,oe,eceo,ncduc,s1duc,s2duc,fduc,sduc,duc,dus1c,dus2c,
     2      ncec,e1ec,e2ec,eec,ec,ee1c,ee2c)
c
c This routine constructs the remaining, bulk of the input, in terms of
c  vectors and matrices, needed by the solver program.
c In the first, constraints part it follows the ordering established in
c  the 'order' routine, and uses the "pin*' and other arrays created in
c  the 'build' routine, together with such ux and uy values as were
c  input at midpoints of sides on velocity lines (with any remaining
c  values having been determined by interpolation in the 'fillvl'
c  subroutine of 'build').
c In the second, observables part it gets all its data input in
c  normalised form from 'build'. The two classes of observable are dealt
c  with in order.
c The first class consists of the ingredients for the apriori solutions,
c  which include observable-like strain-rate quantities in elements
c  (divided into quarters) and slip-rate quantities on fault segments
c  (divided into halves). Both sets of these normalised quantities
c  derive from spatial averages on their respective subdomains. The
c  splitting into averages on quarter elements and half fault segments
c  ensures the inversion for unconstrained velocity and slip-rate
c  components is substantially over-determined. From a statistical
c  perspective it doesn't matter how many observables there are in
c  this class, as the total sum of squares in the apriori solution
c  relates to the number of unknowns, and from the perspective of
c  finite-elements modelling of dynamics it is usual to over-determine
c  by a factor of two in each spatial dimension.
c The remaining class derives from the actual observables input for
c  inclusion in the aposteriori solution, which are dealt with the
c  order they were entered at the start of the program.
c
c Each of the nz constraints has associated with it a constraint value
c  z, a coefficient vector cz, an index vector xz, indicating the
c  elements of the x vector (consisting of velocities and slip rates)
c  that are involved, and pointers x1maxz, x2maxz and x0maxz to where in
c  the xz vector the x1, x2 and x0 parts of the x vector are to be
c  found. The constraint-controlled x1 part comes first, followed by the
c  inverted-for x2 part, and then the pre-determined x0 part. Both the
c  cz and xz vectors start at index 0 and end at index x0maxz.
c  
c Correspondingly, each of the ny observables has associated with it a
c  normalised value y, a coefficient vector ay, an index vector xy,
c  indicating the elements of the x vector that are involved, and
c  pointers x1maxy, x2maxy and x0maxy to where in the xy vector the x1,
c  x2 and x0 parts of the x vector are to be found. Again, the x1 part
c  comes first, followed by the x2 part, and then the x0 part, with the
c  ay and xy vectors starting at index 0 and ending at index x0maxy.
c The ny observables are split into ny0 observable-like quantities
c  associated with the apriori solution and ny1 actual observables, for
c  inclusion in the aposteriori analysis.
c
c All subroutines here put the elements of the vectors cz, xz, ay and xy
c  into the sequential order of the x vector, which itself has been
c  constructed (in the 'order' routine) to follow the order of the z
c  vector. This is critical for the sparse matrix calculations in the
c  solver program, where the biggest computational tasks are related to
c  the constraints, including Cholesky decomposition of the (nz x nz)
c  matrix resulting from multiplying the full constraint matrix by its
c  transpose. In comparison the computational tasks related to the
c  observables are minor: they are restricted to multiplying ay vectors
c  by x(xy) vectors, multiplying individual elements of y vectors by
c  their corresponding ay vectors, and performing simple scalar sums to
c  construct new x vectors.
c In addition, the subroutines cmat0, ducmat and ecmat combine the
c  multiple entries in the cz and ay vectors that arise in these cases
c  as a result of components of the x vector being common to the two
c  elements or fault segments considered. In doing so, the lengths of
c  the cz, xz, ay and xy vectors are reduced. For cmat0 and ecmat the
c  common components are velocity values and velocity derivatives at
c  the two grid points where the two elements meet, and for ducmat the
c  common components are the slip-rate values and derivatives at the
c  grid point where the two fault segments are joined.
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
      integer ns,ne,nf,nvo,nfo,neo,nduc,nec
      integer nz,nx1,nx2,ny,ny0,ny1
      integer nzc,i,e,gp1,gp2,gp3,s1,s2,s3
      integer j,s1c,s2c,f,e1c,e2c,e1,e2
      integer e1s(maxs),e2s(maxs)
      integer s1e(maxe),s2e(maxe),s3e(maxe)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer gprb(0:maxrbs,maxrb)
      integer evo(maxvo)
      integer ffo(maxfo),sfo(maxfo),nfoc(maxfo)
      integer eeo(maxeo),neoc(maxeo)
      integer fduc(maxduc),sduc(maxfs,maxduc)
      integer ncduc(maxduc),s1duc(maxfs,maxduc),s2duc(maxfs,maxduc)
      integer eec(maxece,maxec)
      integer ncec(maxec),e1ec(maxece,maxec),e2ec(maxece,maxec)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs),isvl(maxs),isrb(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf),gptj(maxtj)
      integer vls(maxsvl),svls(maxsvl)
      integer rbs(maxsrb),srbs(maxsrb)
      integer rbp(maxprb),gprbp(maxprb)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer ks1fv(maxefv),ks2fv(maxefv),ks3fv(maxefv)
      integer is1fv(maxefv),is2fv(maxefv),is3fv(maxefv)
      integer js1fv(maxefv),js2fv(maxefv),js3fv(maxefv)
      integer rs1fv(maxefv),rs2fv(maxefv),rs3fv(maxefv)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer elist(maxe)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      logical p0zero(maxf),pnzero(maxf)
      logical todos(maxs)
      real*8 long(maxgp),lat(maxgp)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl)
      real*8 esub(0:3,3,2,0:9,maxe)
      real*8 pindu(2,0:1,maxfs,maxf),pinu(2,2,2,0:5,maxfs,maxf)
      real*8 pindu0(2,2,maxf),pindun(2,2,maxf)
      real*8 dusub(2,2,0:1,maxfs,maxf),dupot(2,2,maxfs,maxf)
      real*8 epot(0:3,3,maxe)
      real*8 usrb(2,maxrbs,maxrb)
      real*8 pinurb(0:5,maxrbs,maxrb)
      real*8 pinuvl(0:5,maxvls,maxvl)
      real*8 ou(2,maxvo),uvo(2,2,0:9,maxvo)
      real*8 odu(2,maxfo),ducfo(2,2,0:1,maxfo)
      real*8 oe(3,maxeo),eceo(3,2,0:9,maxeo)
      real*8 duc(maxfs,maxduc),dus1c(2,0:1,maxfs,maxduc),
     1      dus2c(2,0:1,maxfs,maxduc)
      real*8 ec(maxece,maxec),ee1c(2,0:9,maxece,maxec),
     1      ee2c(2,0:9,maxece,maxec)
      real*8 cz(0:25,maxz),z(maxz)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      nzc=0
      do i=1,ns
            todos(i)=.true.
      end do
      do i=1,ne
            e=elist(i)
            gp1=gp1e(e)
            gp2=gp2e(e)
            gp3=gp3e(e)
            s1=s1e(e)
            s2=s2e(e)
            s3=s3e(e)
c
            if (todos(s1)) then
                  if (isf(s1).ne.0) then
                        call cmat1(nzc,s1,e,gp1,nx1,nx2,e1s,e2s,
     1                        isf,xgpu,xfu,xtju,xfTu,xTfu,xXfu,
     2                        xfdu,xfTdu,xXfdu,long,lat,
     3                        ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     3                        fTvlp,Tfvlp,Xfvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,tjfp,
     4                        fs,sfs,gpf,rbp,gprbp,rgprb,
     7                        nfs,p0zero,pnzero,pindu0,pindun,
     8                        pindu,pinu,xz,cz,z,x,
     9                        x1maxz,x2maxz,x0maxz)
                  end if
                  if (isvl(s1).ne.0) then
                        call cmat2(nzc,s1,e,gp1,nx1,nx2,e1s,e2s,
     1                        isvl,xgpu,xvlu,xJvlu,xfTu,xTfu,
     2                        xTvlu,xXfu,xXvlu,long,lat,ipvl,
     3                        gp1e,gp2e,gp3e,Jvlvlp,fTvlp,Tfvlp,
     4                        Tvlvlp,Xfvlp,Xvlvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,vls,svls,gpvl,
     6                        nvls,pinuvl,ux,uy,xz,cz,z,x,
     7                        x1maxz,x2maxz,x0maxz)
                  end if
                  if ((isrb(s1).ne.0).and.(isf(s1).eq.0)) then
                        call cmat3(nzc,s1,nx1,nx2,isrb,rbs,srbs,
     1                        gprb,rgprb,ipf,ipvl,fp,gpfp,jfp,
     2                        tjfp,Jvlvlp,Tfvlp,Xfvlp,xgpu,xfu,
     3                        xtju,xJvlu,xTfu,xXfu,pinurb,usrb,
     4                        xz,cz,z,x,x1maxz,x2maxz,x0maxz)
                  end if
                  todos(s1)=.false.
            end if
c
            if (todos(s2)) then
                  if (isf(s2).ne.0) then
                        call cmat1(nzc,s2,e,gp2,nx1,nx2,e1s,e2s,
     1                        isf,xgpu,xfu,xtju,xfTu,xTfu,xXfu,
     2                        xfdu,xfTdu,xXfdu,long,lat,
     3                        ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     3                        fTvlp,Tfvlp,Xfvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,tjfp,
     4                        fs,sfs,gpf,rbp,gprbp,rgprb,
     7                        nfs,p0zero,pnzero,pindu0,pindun,
     8                        pindu,pinu,xz,cz,z,x,
     9                        x1maxz,x2maxz,x0maxz)
                  end if
                  if (isvl(s2).ne.0) then
                        call cmat2(nzc,s2,e,gp2,nx1,nx2,e1s,e2s,
     1                        isvl,xgpu,xvlu,xJvlu,xfTu,xTfu,
     2                        xTvlu,xXfu,xXvlu,long,lat,ipvl,
     3                        gp1e,gp2e,gp3e,Jvlvlp,fTvlp,Tfvlp,
     4                        Tvlvlp,Xfvlp,Xvlvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,vls,svls,gpvl,
     6                        nvls,pinuvl,ux,uy,xz,cz,z,x,
     7                        x1maxz,x2maxz,x0maxz)
                  end if
                  if ((isrb(s2).ne.0).and.(isf(s2).eq.0)) then
                        call cmat3(nzc,s2,nx1,nx2,isrb,rbs,srbs,
     1                        gprb,rgprb,ipf,ipvl,fp,gpfp,jfp,
     2                        tjfp,Jvlvlp,Tfvlp,Xfvlp,xgpu,xfu,
     3                        xtju,xJvlu,xTfu,xXfu,pinurb,usrb,
     4                        xz,cz,z,x,x1maxz,x2maxz,x0maxz)
                  end if
                  todos(s2)=.false.
            end if
c
            if (todos(s3)) then
                  if (isf(s3).ne.0) then
                        call cmat1(nzc,s3,e,gp3,nx1,nx2,e1s,e2s,
     1                        isf,xgpu,xfu,xtju,xfTu,xTfu,xXfu,
     2                        xfdu,xfTdu,xXfdu,long,lat,
     3                        ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     3                        fTvlp,Tfvlp,Xfvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,tjfp,
     4                        fs,sfs,gpf,rbp,gprbp,rgprb,
     7                        nfs,p0zero,pnzero,pindu0,pindun,
     8                        pindu,pinu,xz,cz,z,x,
     9                        x1maxz,x2maxz,x0maxz)
                  end if
                  if (isvl(s3).ne.0) then
                        call cmat2(nzc,s3,e,gp3,nx1,nx2,e1s,e2s,
     1                        isvl,xgpu,xvlu,xJvlu,xfTu,xTfu,
     2                        xTvlu,xXfu,xXvlu,long,lat,ipvl,
     3                        gp1e,gp2e,gp3e,Jvlvlp,fTvlp,Tfvlp,
     4                        Tvlvlp,Xfvlp,Xvlvlp,iefv,
     5                        rgp1fv,rgp2fv,rgp3fv,vls,svls,gpvl,
     6                        nvls,pinuvl,ux,uy,xz,cz,z,x,
     7                        x1maxz,x2maxz,x0maxz)
                  end if
                  if ((isrb(s3).ne.0).and.(isf(s3).eq.0)) then
                        call cmat3(nzc,s3,nx1,nx2,isrb,rbs,srbs,
     1                        gprb,rgprb,ipf,ipvl,fp,gpfp,jfp,
     2                        tjfp,Jvlvlp,Tfvlp,Xfvlp,xgpu,xfu,
     3                        xtju,xJvlu,xTfu,xXfu,pinurb,usrb,
     4                        xz,cz,z,x,x1maxz,x2maxz,x0maxz)
                  end if
                  todos(s3)=.false.
            end if
      end do
      if (nzc.ne.nz) then
            write(*,*) 'nz,nzc=',nz,nzc
            stop 'There is a sequencing bug in assembling constraints'
      end if
c
      ny=0
c
      do i=1,ne
            call emat(ny,i,nx1,nx2,gp1e,gp2e,gp3e,
     1            xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2            xTfu,xTvlu,xXfu,xXvlu,iefv,
     3            kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     4            jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     5            epot(0,1,i),esub(0,1,1,0,i),xy,ay,y,x,
     6            x1maxy,x2maxy,x0maxy)
      end do
c
      if (nf.gt.0) then
      do i=1,nf
            do j=1,nfs(i)
                  call dumat(ny,i,j,nx1,nx2,gpf,ipvl,
     1                  fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2                  dusub(1,1,0,j,i),dupot(1,1,j,i),
     3                  xy,ay,y,x,x1maxy,x2maxy,x0maxy)
            end do
      end do
      end if
c
      ny0=ny
c
      if (nvo.gt.0) then
      do i=1,nvo
            call vomat(ny,evo(i),nx1,nx2,gp1e,gp2e,gp3e,
     1            xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2            xTfu,xTvlu,xXfu,xXvlu,iefv,
     3            kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     4            jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     5            ou(1,i),uvo(1,1,0,i),xy,ay,y,x,
     6            x1maxy,x2maxy,x0maxy)
      end do
      end if
c
      if (nfo.gt.0) then
      do i=1,nfo
            s1=sfs(isf(sfo(i)))
            call fomat(ny,ffo(i),s1,nx1,nx2,gpf,ipvl,
     1            fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2            nfoc(i),odu(1,i),ducfo(1,1,0,i),
     3            xy,ay,y,x,x1maxy,x2maxy,x0maxy)
      end do
      end if
c
      if (neo.gt.0) then
      do i=1,neo
            call eomat(ny,eeo(i),nx1,nx2,gp1e,gp2e,gp3e,
     1            xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2            xTfu,xTvlu,xXfu,xXvlu,iefv,
     3            kgp1fv,kgp2fv,kgp3fv,igp1fv,igp2fv,igp3fv,
     4            jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     5            neoc(i),oe(1,i),eceo(1,1,0,i),xy,ay,y,x,
     6            x1maxy,x2maxy,x0maxy)
      end do
      end if
c
      if (nduc.gt.0) then
      do i=1,nduc
            do j=1,ncduc(i)
                  s1c=s1duc(j,i)
                  s2c=s2duc(j,i)
                  f=fduc(i)
                  s1=sfs(isf(sduc(s1c,i)))
                  s2=sfs(isf(sduc(s2c,i)))
                  call ducmat(ny,f,s1,s2,nx1,nx2,gpf,ipvl,
     1                  fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2                  duc(j,i),dus1c(1,0,j,i),dus2c(1,0,j,i),
     3                  xy,ay,y,x,x1maxy,x2maxy,x0maxy)
            end do
      end do
      end if
c
      if (nec.gt.0) then
      do i=1,nec
            do j=1,ncec(i)
                  e1c=e1ec(j,i)
                  e2c=e2ec(j,i)
                  e1=eec(e1c,i)
                  e2=eec(e2c,i)
                  call ecmat(ny,e1,e2,nx1,nx2,gp1e,gp2e,gp3e,
     1                  xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2                  xTfu,xTvlu,xXfu,xXvlu,iefv,
     3                  kgp1fv,kgp2fv,kgp3fv,
     4                  igp1fv,igp2fv,igp3fv,
     5                  jgp1fv,jgp2fv,jgp3fv,
     6                  rgp1fv,rgp2fv,rgp3fv,
     7                  ec(j,i),ee1c(1,0,j,i),ee2c(1,0,j,i),
     8                  xy,ay,y,x,x1maxy,x2maxy,x0maxy)
            end do
      end do
      end if
c
      ny1=ny-ny0
c

      return
      end
c
c
      SUBROUTINE cmat1(nz,s,e,gp1,nx1,nx2,e1s,e2s,isf,
     1      xgpu,xfu,xtju,xfTu,xTfu,xXfu,xfdu,xfTdu,xXfdu,
     2      long,lat,ipf,ipvl,iprb,gp1e,gp2e,gp3e,
     3      fTvlp,Tfvlp,Xfvlp,iefv,rgp1fv,rgp2fv,rgp3fv,tjfp,
     4      fs,sfs,gpf,rbp,gprbp,rgprb,nfs,p0zero,pnzero,
     5      pindu0,pindun,pindu,pinu,xz,cz,z,x,
     6      x1maxz,x2maxz,x0maxz)

c
c This routine assembles the constraint matrices for constraint class 1,
c  consisting of the five sub-classes of constraints on faults:
c    0 = matching slip rates at triple junctions at the start of faults,
c    1 = matching slip rates at the first quarter point of each segment,
c    2 = setting the third derivative of slip rate to zero,
c    3 = matching slip rates at the other quarter point of each segment,
c    4 = matching slip rates at triple junctions at the ends of faults.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,
     1      maxvl,maxvls,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxprb,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx,maxz
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,maxprb=maxsrb+maxrb,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer nz,s,e,gp1,nx1,nx2
      integer e2,f,j,gp1f,gp2f,ep,em,ip,r,rb,gp,ic,jc,tj,fT,Tf,Xf
      integer iz,i,xzj,im,m,jp
      integer e1s(maxs),e2s(maxs)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nfs(maxf)
      integer gpf(0:maxfs,maxf)
      integer ipf(maxgp),ipvl(maxgp),iprb(maxgp)
      integer isf(maxs)
      integer fs(maxsf),sfs(maxsf)
      integer tjfp(maxpf)
      integer rbp(maxprb),gprbp(maxprb)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer iefv(maxe)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer rgprb(0:maxrbs,maxrb)
      integer xgpu(2,0:2,maxgp)
      integer xfu(2,0:2,0:maxfs,maxf)
      integer xtju(2,0:2,2,maxtj)
      integer xfTu(2,0:2,3,maxfT),xTfu(2,0:2,3,maxTf),
     1      xXfu(2,0:2,3,maxXf)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xc(2,0:19)
      logical p0zero(maxf),pnzero(maxf)
      real*8 dx,dy,x1,y1,dxe,dye,czj
      real*8 long(maxgp),lat(maxgp)
      real*8 pindu0(2,2,maxf),pindun(2,2,maxf)
      real*8 pindu(2,0:1,maxfs,maxf),pinu(2,2,2,0:5,maxfs,maxf)
      real*8 cz(0:25,maxz),z(maxz)
      real*8 x(maxx)
c
      if (e1s(s).eq.e) then
            e2=e2s(s)
      else
            e2=e1s(s)
      end if
c
c CONSTRAINT CLASS = 1
c
c      if (isf(s).ne.0) then
c
      f=fs(isf(s))
      j=sfs(isf(s))
      gp1f=gpf(j-1,f)
      gp2f=gpf(j,f)
      x1=long(gp1f)
      y1=lat(gp1f)
      dx=long(gp2f)-x1
      dy=lat(gp2f)-y1
      dxe=long(gp1)-x1
      dye=lat(gp1)-y1
      if (dx*dye-dy*dxe.gt.0.0d0) then
            ep=e
            em=e2
      else
            em=e
            ep=e2
      end if
c
      if ((j.eq.1).and.p0zero(f)) then
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (positive side at second point)
c SPECIAL CASE = p0zero
c
      if (ep.ne.0) then
            if (gp2f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp2f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp2f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
      else
            rb=rbp(iprb(gp2f))
            gp=gprbp(iprb(gp2f))
            r=rgprb(gp,rb)
      end if
      if ((r.eq.0).and.(ipvl(gp2f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp2f)
            end do
            end do
            goto 330
      end if
      ip=ipf(gp2f)
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 330
      end if
      stop 'A fault has a zero end one point from a velocity line'
330   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (negative side at second point)
c SPECIAL CASE = p0zero
c
      ip=ipf(gp2f)
      if ((j.ne.nfs(f)).and.(ipvl(gp2f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xfu(ic,jc,j,f)
            end do
            end do
            goto 340
      end if
      if (em.ne.0) then
            if (gp2f.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp2f.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp2f.eq.gp3e(em)) r=rgp3fv(iefv(em))
      else
            rb=rbp(iprb(gp2f))
            gp=gprbp(iprb(gp2f))
            r=rgprb(gp,rb)
      end if
      if ((r.eq.0).and.(ipvl(gp2f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp2f)
            end do
            end do
            goto 340
      end if
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 340
      end if
      stop 'A fault has a zero end one point from a velocity line'
340   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (fault at second point)
c SPECIAL CASE = p0zero
c
      if (ipvl(gp2f).eq.0) then
            do jc=0,0
            do ic=1,2
                  xc(ic,jc+6)=xfdu(ic,jc,j,f)
            end do
            end do
            goto 350
      end if
      stop 'A fault has a zero end one point from a velocity line'
350   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1
c SPECIAL CASE = p0zero
c
      do ic=1,2
            iz=nz+ic
            do i=0,2
                 xz(i,iz)=xc(1,i)
                 xz(i+3,iz)=xc(2,i)
                 xz(i+6,iz)=xc(1,i+3)
                 xz(i+9,iz)=xc(2,i+3)
                 cz(i,iz)=-pinu(1,ic,1,i+3,j,f)
                 cz(i+3,iz)=-pinu(1,ic,2,i+3,j,f)
                 cz(i+6,iz)=pinu(1,ic,1,i+3,j,f)
                 cz(i+9,iz)=pinu(1,ic,2,i+3,j,f)
            end do
            do i=0,0
                  xz(i+12,iz)=xc(ic,i+6)
                  cz(i+12,iz)=pindu(1,i+1,j,f)
            end do
            do jc=1,12
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 370
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
370               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=12
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 3
c SPECIAL CASE = p0zero
c
      do ic=1,2
            iz=nz+ic
            do i=0,2
                 xz(i,iz)=xc(1,i)
                 xz(i+3,iz)=xc(2,i)
                 xz(i+6,iz)=xc(1,i+3)
                 xz(i+9,iz)=xc(2,i+3)
                 cz(i,iz)=-pinu(2,ic,1,i+3,j,f)
                 cz(i+3,iz)=-pinu(2,ic,2,i+3,j,f)
                 cz(i+6,iz)=pinu(2,ic,1,i+3,j,f)
                 cz(i+9,iz)=pinu(2,ic,2,i+3,j,f)
            end do
            do i=0,0
                  xz(i+12,iz)=xc(ic,i+6)
                  cz(i+12,iz)=pindu(2,i+1,j,f)
            end do
            do jc=1,12
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 390
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
390               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=12
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
            return
      end if
c
      if ((j.eq.nfs(f)).and.pnzero(f)) then
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (positive side at first point)
c SPECIAL CASE = pnzero
c
      if (ep.ne.0) then
            if (gp1f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp1f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp1f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
      else
            rb=rbp(iprb(gp1f))
            gp=gprbp(iprb(gp1f))
            r=rgprb(gp,rb)
      end if
      if ((r.eq.0).and.(ipvl(gp1f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp1f)
            end do
            end do
            goto 410
      end if
      ip=ipf(gp1f)
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 410
      end if
      stop 'A fault has a zero end one point from a velocity line'
410   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (negative side at first point)
c SPECIAL CASE = pnzero
c
      ip=ipf(gp1f)
      if ((j.ne.1).and.(ipvl(gp1f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xfu(ic,jc,j-1,f)
            end do
            end do
            goto 420
      end if
      if (em.ne.0) then
            if (gp1f.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp1f.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp1f.eq.gp3e(em)) r=rgp3fv(iefv(em))
      else
            rb=rbp(iprb(gp1f))
            gp=gprbp(iprb(gp1f))
            r=rgprb(gp,rb)
      end if
      if ((r.eq.0).and.(ipvl(gp1f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp1f)
            end do
            end do
            goto 420
      end if
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 420
      end if
      stop 'A fault has a zero end one point from a velocity line'
420   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (fault at first point)
c SPECIAL CASE = pnzero
c
      do jc=0,0
      do ic=1,2
           xc(ic,jc+6)=xfdu(ic,jc,j-1,f)
      end do
      end do
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1
c SPECIAL CASE = pnzero
c
      do ic=1,2
            iz=nz+ic
            do i=0,2
                 xz(i,iz)=xc(1,i)
                 xz(i+3,iz)=xc(2,i)
                 xz(i+6,iz)=xc(1,i+3)
                 xz(i+9,iz)=xc(2,i+3)
                 cz(i,iz)=-pinu(1,ic,1,i,j,f)
                 cz(i+3,iz)=-pinu(1,ic,2,i,j,f)
                 cz(i+6,iz)=pinu(1,ic,1,i,j,f)
                 cz(i+9,iz)=pinu(1,ic,2,i,j,f)
            end do
            do i=0,0
                  xz(i+12,iz)=xc(ic,i+6)
                  cz(i+12,iz)=pindu(1,i,j,f)
            end do
            do jc=1,12
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 470
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
470               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=12
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 3
c SPECIAL CASE = pnzero
c
      do ic=1,2
            iz=nz+ic
            do i=0,2
                 xz(i,iz)=xc(1,i)
                 xz(i+3,iz)=xc(2,i)
                 xz(i+6,iz)=xc(1,i+3)
                 xz(i+9,iz)=xc(2,i+3)
                 cz(i,iz)=-pinu(2,ic,1,i,j,f)
                 cz(i+3,iz)=-pinu(2,ic,2,i,j,f)
                 cz(i+6,iz)=pinu(2,ic,1,i,j,f)
                 cz(i+9,iz)=pinu(2,ic,2,i,j,f)
            end do
            do i=0,0
                  xz(i+12,iz)=xc(ic,i+6)
                  cz(i+12,iz)=pindu(2,i,j,f)
            end do
            do jc=1,12
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 490
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
490               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=12
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
            return
      end if
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (positive side at first point)
c
      if (ep.ne.0) then
            if (gp1f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp1f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp1f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
      else
            rb=rbp(iprb(gp1f))
            gp=gprbp(iprb(gp1f))
            r=rgprb(gp,rb)
      end if
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp1f)
            end do
            end do
            goto 110
      end if
      ip=ipf(gp1f)
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 110
      end if
      ip=ipvl(gp1f)
      if (fTvlp(ip).ne.0) then
            fT=fTvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xfTu(ic,jc,r,fT)
            end do
            end do
            goto 110
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 110
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
110   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (negative side at first point)
c
      ip=ipf(gp1f)
      if ((j.ne.1).and.(ipvl(gp1f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xfu(ic,jc,j-1,f)
            end do
            end do
            goto 120
      end if
      if (em.ne.0) then
            if (gp1f.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp1f.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp1f.eq.gp3e(em)) r=rgp3fv(iefv(em))
      else
            rb=rbp(iprb(gp1f))
            gp=gprbp(iprb(gp1f))
            r=rgprb(gp,rb)
      end if
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xgpu(ic,jc,gp1f)
            end do
            end do
            goto 120
      end if
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 120
      end if
      ip=ipvl(gp1f)
      if (fTvlp(ip).ne.0) then
            fT=fTvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xfTu(ic,jc,r,fT)
            end do
            end do
            goto 120
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 120
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+6)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
120   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (fault at first point)
c
      do jc=0,0
      do ic=1,2
           xc(ic,jc+12)=xfdu(ic,jc,j-1,f)
      end do
      end do
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (positive side at second point)
c
      if (ep.ne.0) then
            if (gp2f.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp2f.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp2f.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
      else
            rb=rbp(iprb(gp2f))
            gp=gprbp(iprb(gp2f))
            r=rgprb(gp,rb)
      end if
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp2f)
            end do
            end do
            goto 130
      end if
      ip=ipf(gp2f)
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 130
      end if
      ip=ipvl(gp2f)
      if (fTvlp(ip).ne.0) then
            fT=fTvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xfTu(ic,jc,r,fT)
            end do
            end do
            goto 130
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 130
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
130   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (negative side at second point)
c
      ip=ipf(gp2f)
      if ((j.ne.nfs(f)).and.(ipvl(gp2f).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xfu(ic,jc,j,f)
            end do
            end do
            goto 140
      end if
      if (em.ne.0) then
            if (gp2f.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp2f.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp2f.eq.gp3e(em)) r=rgp3fv(iefv(em))
      else
            rb=rbp(iprb(gp2f))
            gp=gprbp(iprb(gp2f))
            r=rgprb(gp,rb)
      end if
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xgpu(ic,jc,gp2f)
            end do
            end do
            goto 140
      end if
      if (tjfp(ip).ne.0) then
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 140
      end if
      ip=ipvl(gp2f)
      if (fTvlp(ip).ne.0) then
            fT=fTvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xfTu(ic,jc,r,fT)
            end do
            end do
            goto 140
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 140
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+9)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
140   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1-3 (fault at second point)
c
      if (ipvl(gp2f).eq.0) then
            do jc=0,0
            do ic=1,2
                  xc(ic,jc+13)=xfdu(ic,jc,j,f)
            end do
            end do
            goto 150
      end if
      ip=ipvl(gp2f)
      if (fTvlp(ip).ne.0) then
            fT=fTvlp(ip)
            do jc=0,0
            do ic=1,2
                  xc(ic,jc+13)=xfTdu(ic,jc,fT)
            end do
            end do
            goto 150
      end if
      if (Tfvlp(ip).ne.0) then
            do jc=0,0
            do ic=1,2
                  xc(ic,jc+13)=xfdu(ic,jc,j,f)
            end do
            end do
            goto 150
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,0
            do ic=1,2
                  xc(ic,jc+13)=xXfdu(ic,jc,Xf)
            end do
            end do
      end if
150   continue
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 1
c
      do ic=1,2
            iz=nz+ic
            do i=0,5
                 xz(i,iz)=xc(1,i)
                 xz(i+6,iz)=xc(2,i)
                 xz(i+12,iz)=xc(1,i+6)
                 xz(i+18,iz)=xc(2,i+6)
                 cz(i,iz)=-pinu(1,ic,1,i,j,f)
                 cz(i+6,iz)=-pinu(1,ic,2,i,j,f)
                 cz(i+12,iz)=pinu(1,ic,1,i,j,f)
                 cz(i+18,iz)=pinu(1,ic,2,i,j,f)
            end do
            do i=0,1
                  xz(i+24,iz)=xc(ic,i+12)
                  cz(i+24,iz)=pindu(1,i,j,f)
            end do
            do jc=1,25
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 170
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
170               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=25
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
c
c CONSTRAINT CLASS = 1, SUB-CLASS = 3
c
      do ic=1,2
            iz=nz+ic
            do i=0,5
                 xz(i,iz)=xc(1,i)
                 xz(i+6,iz)=xc(2,i)
                 xz(i+12,iz)=xc(1,i+6)
                 xz(i+18,iz)=xc(2,i+6)
                 cz(i,iz)=-pinu(2,ic,1,i,j,f)
                 cz(i+6,iz)=-pinu(2,ic,2,i,j,f)
                 cz(i+12,iz)=pinu(2,ic,1,i,j,f)
                 cz(i+18,iz)=pinu(2,ic,2,i,j,f)
            end do
            do i=0,1
                  xz(i+24,iz)=xc(ic,i+12)
                  cz(i+24,iz)=pindu(2,i,j,f)
            end do
            do jc=1,25
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 190
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
190               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=25
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=0.0d0
      end do
      nz=nz+2      
c
      return
      end
c
c
      SUBROUTINE cmat2(nz,s,e,gp1,nx1,nx2,e1s,e2s,isvl,
     1      xgpu,xvlu,xJvlu,xfTu,xTfu,xTvlu,xXfu,xXvlu,
     2      long,lat,ipvl,gp1e,gp2e,gp3e,
     3      Jvlvlp,fTvlp,Tfvlp,Tvlvlp,Xfvlp,Xvlvlp,
     4      iefv,rgp1fv,rgp2fv,rgp3fv,vls,svls,gpvl,
     5      nvls,pinuvl,ux,uy,xz,cz,z,x,
     6      x1maxz,x2maxz,x0maxz)
c
c This routine assembles the constraint matrices for constraint class 2,
c  consisting of the two sub-classes for velocity lines of matching
c  velocities at the midpoints of segments:
c    0 = positive side of the line,
c    1 = negative side of the line.
c
      implicit none
      integer maxgp,maxs,maxe,maxf,maxfs,

     1      maxvl,maxvls,maxvlj,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx,maxz
      parameter(maxgp=40000,maxs=3*maxgp,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,maxvlj=2*maxvls+1,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer nz,s,e,gp1,nx1,nx2
      integer e2,vl,j,gp1vl,gp2vl,ep,em,js,ip,jc,ic,r
      integer Jvl,fT,Tf,Tvl,Xf,Xvl,iz,i,xzj,im,m
      integer e1s(maxs),e2s(maxs)
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer nvls(maxvl)
      integer gpvl(0:maxvls,maxvl)
      integer ipvl(maxgp)
      integer isvl(maxs)
      integer vls(maxsvl),svls(maxsvl)
      integer Jvlvlp(maxpvl),fTvlp(maxpvl),Tfvlp(maxpvl),
     1      Tvlvlp(maxpvl),Xfvlp(maxpvl),Xvlvlp(maxpvl)
      integer iefv(maxe)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer xgpu(2,0:2,maxgp)
      integer xvlu(2,0:2,0:maxvls,maxvl)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xc(2,0:5)
      real*8 dx,dy,x1,y1,dxe,dye,czj
      real*8 long(maxgp),lat(maxgp)
      real*8 pinuvl(0:5,maxvls,maxvl)
      real*8 ux(maxvlj,maxvl),uy(maxvlj,maxvl)
      real*8 cz(0:25,maxz),z(maxz)
      real*8 x(maxx)
c
      if (e1s(s).eq.e) then
            e2=e2s(s)
      else
            e2=e1s(s)
      end if
c
c CONSTRAINT CLASS = 2
c
c      if (isvl(s).ne.0) then
c
      vl=vls(isvl(s))
      j=svls(isvl(s))
      gp1vl=gpvl(j-1,vl)
      gp2vl=gpvl(j,vl)
      x1=long(gp1vl)
      y1=lat(gp1vl)
      dx=long(gp2vl)-x1
      dy=lat(gp2vl)-y1
      dxe=long(gp1)-x1
      dye=lat(gp1)-y1
      if (dx*dye-dy*dxe.gt.0.0d0) then
            ep=e
            em=e2
      else
            em=e
            ep=e2
      end if
      js=2*j
c
      if (ep.eq.0) goto 10
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 0 (simple end at first point)
c
      ip=ipvl(gp1vl)
      if ((j.eq.1).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp1vl)
            end do
            end do
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 0 (first point)
c
      if ((j.ne.1).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
            if (gp1vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp1vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp1vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
            if (r.eq.0) then
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xgpu(ic,jc,gp1vl)
                  end do
                  end do
                  goto 160
            end if
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xJvlu(ic,jc,r,Jvl)
                  end do
                  end do
                  goto 160
            end if
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xfTu(ic,jc,r,fT)
                  end do
                  end do
                  goto 160
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xTfu(ic,jc,r,Tf)
                  end do
                  end do
                  goto 160
            end if
            if (Tvlvlp(ip).ne.0) then
                  Tvl=Tvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xTvlu(ic,jc,r,Tvl)
                  end do
                  end do
                  goto 160
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xXfu(ic,jc,r,Xf)
                  end do
                  end do
                  goto 160
            end if
            if (Xvlvlp(ip).ne.0) then
                  Xvl=Xvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xXvlu(ic,jc,r,Xvl)
                  end do
                  end do
            end if
160         continue
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 0 (second point)
c
      ip=ipvl(gp2vl)
      if ((j.ne.nvls(vl)).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
            if (gp2vl.eq.gp1e(ep)) r=rgp1fv(iefv(ep))
            if (gp2vl.eq.gp2e(ep)) r=rgp2fv(iefv(ep))
            if (gp2vl.eq.gp3e(ep)) r=rgp3fv(iefv(ep))
            if (r.eq.0) then
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xgpu(ic,jc,gp2vl)
                  end do
                  end do
                  goto 180
            end if
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=0,2
                  do ic=1,2

                        xc(ic,jc+3)=xJvlu(ic,jc,r,Jvl)
                  end do
                  end do
                  goto 180
            end if
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xfTu(ic,jc,r,fT)
                  end do
                  end do
                  goto 180
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xTfu(ic,jc,r,Tf)
                  end do
                  end do
                  goto 180
            end if
            if (Tvlvlp(ip).ne.0) then
                  Tvl=Tvlvlp(ip)

                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xTvlu(ic,jc,r,Tvl)
                  end do
                  end do
                  goto 180
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xXfu(ic,jc,r,Xf)
                  end do
                  end do
                  goto 180
            end if
            if (Xvlvlp(ip).ne.0) then
                  Xvl=Xvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xXvlu(ic,jc,r,Xvl)
                  end do
                  end do
            end if
180         continue
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 0 (simple end at second point)
c
      if ((j.eq.nvls(vl)).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)

     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp2vl)
            end do
            end do
      end if
c
      do ic=1,2
            iz=nz+ic
            do i=0,5
                  xz(i,iz)=xc(ic,i)
                  cz(i,iz)=pinuvl(i,j,vl)
            end do
            do jc=1,5
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 270
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
270               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=5
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            if (ic.eq.1) z(iz)=ux(js,vl)
            if (ic.eq.2) z(iz)=uy(js,vl)
      end do
      nz=nz+2      
c
10    if (em.eq.0) return
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 1 (simple end at first point)
c
      ip=ipvl(gp1vl)
      if ((j.eq.1).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp1vl)
            end do
            end do
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 1 (first point)
c
      if ((j.ne.1).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
            if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2            then
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xvlu(ic,jc,j-1,vl)
                  end do
                  end do
                  goto 170
            end if
            if (gp1vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp1vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp1vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xJvlu(ic,jc,r,Jvl)
                  end do
                  end do
                  goto 170
            end if
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xfTu(ic,jc,r,fT)
                  end do
                  end do
                  goto 170
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xTfu(ic,jc,r,Tf)
                  end do
                  end do
                  goto 170
            end if
            if (Tvlvlp(ip).ne.0) then
                  Tvl=Tvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xTvlu(ic,jc,r,Tvl)
                  end do
                  end do
                  goto 170
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xXfu(ic,jc,r,Xf)
                  end do
                  end do
                  goto 170
            end if
            if (Xvlvlp(ip).ne.0) then
                  Xvl=Xvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xXvlu(ic,jc,r,Xvl)
                  end do
                  end do
            end if
170         continue
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 1 (second point)
c
      ip=ipvl(gp2vl)
      if ((j.ne.nvls(vl)).or.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).ne.0)) then
            if (Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1            +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)
     2            then
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xvlu(ic,jc,j,vl)
                  end do
                  end do
                  goto 190
            end if
            if (gp2vl.eq.gp1e(em)) r=rgp1fv(iefv(em))
            if (gp2vl.eq.gp2e(em)) r=rgp2fv(iefv(em))
            if (gp2vl.eq.gp3e(em)) r=rgp3fv(iefv(em))
            if (Jvlvlp(ip).ne.0) then
                  Jvl=Jvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xJvlu(ic,jc,r,Jvl)
                  end do
                  end do
                  goto 190
            end if
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xfTu(ic,jc,r,fT)
                  end do
                  end do
                  goto 190
            end if
            if (Tfvlp(ip).ne.0) then
                  Tf=Tfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xTfu(ic,jc,r,Tf)
                  end do
                  end do
                  goto 190
            end if
            if (Tvlvlp(ip).ne.0) then
                  Tvl=Tvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xTvlu(ic,jc,r,Tvl)
                  end do
                  end do
                  goto 190
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xXfu(ic,jc,r,Xf)
                  end do
                  end do
                  goto 190
            end if
            if (Xvlvlp(ip).ne.0) then
                  Xvl=Xvlvlp(ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xXvlu(ic,jc,r,Xvl)
                  end do
                  end do
            end if
190         continue
      end if
c
c CONSTRAINT CLASS = 2, SUB-CLASS = 1 (simple end at second point)
c
      if ((j.eq.nvls(vl)).and.(Jvlvlp(ip)+fTvlp(ip)+Tfvlp(ip)
     1      +Tvlvlp(ip)+Xfvlp(ip)+Xvlvlp(ip).eq.0)) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp2vl)
            end do
            end do
      end if
c
      do ic=1,2
            iz=nz+ic
            do i=0,5
                  xz(i,iz)=xc(ic,i)
                  cz(i,iz)=pinuvl(i,j,vl)
            end do
            do jc=1,5
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 280

                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
280               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=5
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            if (ic.eq.1) z(iz)=ux(js,vl)
            if (ic.eq.2) z(iz)=uy(js,vl)
      end do
      nz=nz+2      
c
      return
      end
c
c
      SUBROUTINE cmat3(nz,s,nx1,nx2,isrb,rbs,srbs,gprb,rgprb,
     1      ipf,ipvl,fp,gpfp,jfp,tjfp,Jvlvlp,Tfvlp,Xfvlp,
     2      xgpu,xfu,xtju,xJvlu,xTfu,xXfu,pinurb,usrb,
     3      xz,cz,z,x,x1maxz,x2maxz,x0maxz)
c
c This routine assembles the constraint matrices for constraint class 3,
c  matching velocities at the midpoints of rigid boundary segments.
c
      implicit none
      integer maxgp,maxs,maxf,maxfs,
     1      maxvl,maxvls,maxrb,maxrbs,
     2      maxsf,maxsvl,maxsrb,maxpf,maxpvl,maxfp,
     3      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     4      maxefv,maxx,maxz
      parameter(maxgp=40000,maxs=3*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxrb=10,maxrbs=200,
     4      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     5      maxsrb=maxrb*maxrbs,maxpf=maxsf+maxf,
     6      maxpvl=maxsvl+maxvl,
     7      maxfp=6,maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxz=2*(2*maxsf+2*maxsvl+maxsrb+3*maxtj))
      integer nz,s,nx1,nx2
      integer rb,j,gp1rb,gp2rb,r,jc,ic,ip,f,gp,tj,Jvl,Tf,Xf
      integer iz,i,xzj,im,m
      integer gprb(0:maxrbs,maxrb)
      integer ipf(maxgp),ipvl(maxgp)
      integer isrb(maxs)
      integer fp(maxfp,maxpf),gpfp(maxfp,maxpf)
      integer jfp(maxpf),tjfp(maxpf)
      integer rbs(maxsrb),srbs(maxsrb)
      integer Jvlvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer rgprb(0:maxrbs,maxrb)
      integer xgpu(2,0:2,maxgp)
      integer xfu(2,0:2,0:maxfs,maxf)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xTfu(2,0:2,3,maxTf),
     1      xXfu(2,0:2,3,maxXf)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xc(2,0:5)
      real*8 czj
      real*8 pinurb(0:5,maxrbs,maxrb),usrb(2,maxrbs,maxrb)
      real*8 cz(0:25,maxz),z(maxz)
      real*8 x(maxx)
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0

c
c      if ((isrb(s).ne.0).and.(isf(s).eq.0)) then
c
      rb=rbs(isrb(s))
      j=srbs(isrb(s))
      gp1rb=gprb(j-1,rb)
      gp2rb=gprb(j,rb)
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0 (first point)
c
      r=rgprb(j-1,rb)
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xgpu(ic,jc,gp1rb)
            end do
            end do
            goto 200
      end if
      if (ipvl(gp1rb).eq.0) then
            ip=ipf(gp1rb)
            if (tjfp(ip).eq.0) then
                  f=fp(jfp(ip),ip)
                  gp=gpfp(jfp(ip),ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc)=xfu(ic,jc,gp,f)
                  end do
                  end do
                  goto 200
            end if
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 200
      end if
      ip=ipvl(gp1rb)
      if (Jvlvlp(ip).ne.0) then
            Jvl=Jvlvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xJvlu(ic,jc,r,Jvl)
            end do
            end do
            goto 200
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 200
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
200         continue
c
c CONSTRAINT CLASS = 3, SUB-CLASS = 0 (second point)
c
      r=rgprb(j,rb)
      if (r.eq.0) then
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xgpu(ic,jc,gp2rb)
            end do
            end do
            goto 210
      end if
      if (ipvl(gp2rb).eq.0) then
            ip=ipf(gp2rb)
            if (tjfp(ip).eq.0) then
                  f=fp(jfp(ip),ip)
                  gp=gpfp(jfp(ip),ip)
                  do jc=0,2
                  do ic=1,2
                        xc(ic,jc+3)=xfu(ic,jc,j,f)
                  end do
                  end do
                  goto 210
            end if
            tj=tjfp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xtju(ic,jc,r,tj)
            end do
            end do
            goto 210
      end if
      ip=ipvl(gp2rb)
      if (Jvlvlp(ip).ne.0) then
            Jvl=Jvlvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xJvlu(ic,jc,r,Jvl)
            end do
            end do
            goto 210
      end if
      if (Tfvlp(ip).ne.0) then
            Tf=Tfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xTfu(ic,jc,r,Tf)
            end do
            end do
            goto 210
      end if
      if (Xfvlp(ip).ne.0) then
            Xf=Xfvlp(ip)
            do jc=0,2
            do ic=1,2
                  xc(ic,jc+3)=xXfu(ic,jc,r,Xf)
            end do
            end do
      end if
210         continue
c
      do ic=1,2
            iz=nz+ic
            do i=0,5
                  xz(i,iz)=xc(ic,i)
                  cz(i,iz)=pinurb(i,j,rb)
            end do
            do jc=1,5
                  xzj=xz(jc,iz)
                  czj=cz(jc,iz)
                  do i=jc,1,-1
                        im=i-1
                        if (xz(im,iz).le.xzj) goto 370
                        xz(i,iz)=xz(im,iz)
                        cz(i,iz)=cz(im,iz)
                  end do
                  i=0
370               xz(i,iz)=xzj
                  cz(i,iz)=czj
            end do
            m=5
            x1maxz(iz)=0
            x2maxz(iz)=0
            do i=1,m
                  if (xz(i,iz).le.nx1) x1maxz(iz)=i
                  if (xz(i,iz).le.nx1+nx2) x2maxz(iz)=i
            end do
            x0maxz(iz)=m
            z(iz)=usrb(ic,j,rb)
      end do
      nz=nz+2      
c
      return
      end
c
c
      SUBROUTINE emat(ny,e,nx1,nx2,gp1e,gp2e,gp3e,
     1      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2      xTfu,xTvlu,xXfu,xXvlu,iefv,kgp1fv,kgp2fv,kgp3fv,
     3      igp1fv,igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,
     4      rgp1fv,rgp2fv,rgp3fv,epot,esub,xy,ay,y,x,
     5      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the observation-like matrices for strain-rate
c  contributions to the apriori solution.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxefv,maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     4      +maxfs*maxduc+maxece*maxec)
      integer ny,e,nx1,nx2
      integer gp1e1,gp2e1,gp3e1
      integer ic,r,kgp,igp,jgp,iq,iy,i,j,xyj,im,m
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:9)
      real*8 ayj
      real*8 epot(0:3,3),esub(0:3,3,2,0:9)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1e1=gp1e(e)
      gp2e1=gp2e(e)
      gp3e1=gp3e(e)
      if (iefv(e).eq.0) then
            do ic=1,2
                 xa(ic,0)=xgpu(ic,0,gp1e1)
                 xa(ic,1)=xgpu(ic,1,gp1e1)
                 xa(ic,2)=xgpu(ic,2,gp1e1)
                 xa(ic,3)=xgpu(ic,0,gp2e1)
                 xa(ic,4)=xgpu(ic,1,gp2e1)
                 xa(ic,5)=xgpu(ic,2,gp2e1)
                 xa(ic,6)=xgpu(ic,0,gp3e1)
                 xa(ic,7)=xgpu(ic,1,gp3e1)
                 xa(ic,8)=xgpu(ic,2,gp3e1)
            end do
      else
            r=rgp1fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,0)=xgpu(ic,0,gp1e1)
                       xa(ic,1)=xgpu(ic,1,gp1e1)
                       xa(ic,2)=xgpu(ic,2,gp1e1)
                  end do
                  goto 10
            end if
            kgp=kgp1fv(iefv(e))
            igp=igp1fv(iefv(e))
            jgp=jgp1fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,0)=xfu(ic,0,jgp,igp)
                       xa(ic,1)=xfu(ic,1,jgp,igp)
                       xa(ic,2)=xfu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,0)=xvlu(ic,0,jgp,igp)
                       xa(ic,1)=xvlu(ic,1,jgp,igp)
                       xa(ic,2)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,0)=xtju(ic,0,r,igp)
                       xa(ic,1)=xtju(ic,1,r,igp)
                       xa(ic,2)=xtju(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,0)=xJvlu(ic,0,r,igp)
                       xa(ic,1)=xJvlu(ic,1,r,igp)
                       xa(ic,2)=xJvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,0)=xfTu(ic,0,r,igp)
                       xa(ic,1)=xfTu(ic,1,r,igp)
                       xa(ic,2)=xfTu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,0)=xTfu(ic,0,r,igp)
                       xa(ic,1)=xTfu(ic,1,r,igp)
                       xa(ic,2)=xTfu(ic,2,r,igp)

                  end do
                  goto 10
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,0)=xTvlu(ic,0,r,igp)
                       xa(ic,1)=xTvlu(ic,1,r,igp)
                       xa(ic,2)=xTvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,0)=xXfu(ic,0,r,igp)
                       xa(ic,1)=xXfu(ic,1,r,igp)
                       xa(ic,2)=xXfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,0)=xXvlu(ic,0,r,igp)
                       xa(ic,1)=xXvlu(ic,1,r,igp)
                       xa(ic,2)=xXvlu(ic,2,r,igp)
                  end do
            end if
10          continue
c
            r=rgp2fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,3)=xgpu(ic,0,gp2e1)
                       xa(ic,4)=xgpu(ic,1,gp2e1)
                       xa(ic,5)=xgpu(ic,2,gp2e1)
                  end do
                  goto 20
            end if
            kgp=kgp2fv(iefv(e))
            igp=igp2fv(iefv(e))
            jgp=jgp2fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,3)=xfu(ic,0,jgp,igp)
                       xa(ic,4)=xfu(ic,1,jgp,igp)
                       xa(ic,5)=xfu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,3)=xvlu(ic,0,jgp,igp)
                       xa(ic,4)=xvlu(ic,1,jgp,igp)
                       xa(ic,5)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,3)=xtju(ic,0,r,igp)
                       xa(ic,4)=xtju(ic,1,r,igp)
                       xa(ic,5)=xtju(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,3)=xJvlu(ic,0,r,igp)
                       xa(ic,4)=xJvlu(ic,1,r,igp)
                       xa(ic,5)=xJvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,3)=xfTu(ic,0,r,igp)
                       xa(ic,4)=xfTu(ic,1,r,igp)
                       xa(ic,5)=xfTu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,3)=xTfu(ic,0,r,igp)
                       xa(ic,4)=xTfu(ic,1,r,igp)
                       xa(ic,5)=xTfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,3)=xTvlu(ic,0,r,igp)
                       xa(ic,4)=xTvlu(ic,1,r,igp)
                       xa(ic,5)=xTvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,3)=xXfu(ic,0,r,igp)
                       xa(ic,4)=xXfu(ic,1,r,igp)
                       xa(ic,5)=xXfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,3)=xXvlu(ic,0,r,igp)
                       xa(ic,4)=xXvlu(ic,1,r,igp)
                       xa(ic,5)=xXvlu(ic,2,r,igp)
                  end do
            end if
20          continue
c
            r=rgp3fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,6)=xgpu(ic,0,gp3e1)
                       xa(ic,7)=xgpu(ic,1,gp3e1)
                       xa(ic,8)=xgpu(ic,2,gp3e1)
                  end do
                  goto 30
            end if
            kgp=kgp3fv(iefv(e))
            igp=igp3fv(iefv(e))
            jgp=jgp3fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,6)=xfu(ic,0,jgp,igp)
                       xa(ic,7)=xfu(ic,1,jgp,igp)
                       xa(ic,8)=xfu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,6)=xvlu(ic,0,jgp,igp)
                       xa(ic,7)=xvlu(ic,1,jgp,igp)
                       xa(ic,8)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,6)=xtju(ic,0,r,igp)
                       xa(ic,7)=xtju(ic,1,r,igp)
                       xa(ic,8)=xtju(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,6)=xJvlu(ic,0,r,igp)
                       xa(ic,7)=xJvlu(ic,1,r,igp)
                       xa(ic,8)=xJvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,6)=xfTu(ic,0,r,igp)
                       xa(ic,7)=xfTu(ic,1,r,igp)
                       xa(ic,8)=xfTu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,6)=xTfu(ic,0,r,igp)
                       xa(ic,7)=xTfu(ic,1,r,igp)
                       xa(ic,8)=xTfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,6)=xTvlu(ic,0,r,igp)
                       xa(ic,7)=xTvlu(ic,1,r,igp)
                       xa(ic,8)=xTvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,6)=xXfu(ic,0,r,igp)
                       xa(ic,7)=xXfu(ic,1,r,igp)
                       xa(ic,8)=xXfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,6)=xXvlu(ic,0,r,igp)
                       xa(ic,7)=xXvlu(ic,1,r,igp)
                       xa(ic,8)=xXvlu(ic,2,r,igp)
                  end do
            end if
30          continue
      end if
c
      do iq=0,3
      do ic=1,3
            iy=ny+ic+3*iq
            do i=0,8
                  xy(i,iy)=xa(1,i)
                  ay(i,iy)=esub(iq,ic,1,i)
            end do
            do i=9,17
                  xy(i,iy)=xa(2,i-9)
                  ay(i,iy)=esub(iq,ic,2,i-9)
            end do
            do j=1,17
                  xyj=xy(j,iy)
                  ayj=ay(j,iy)
                  do i=j,1,-1
                        im=i-1
                        if (xy(im,iy).le.xyj) goto 70
                        xy(i,iy)=xy(im,iy)
                        ay(i,iy)=ay(im,iy)
                  end do
                  i=0
70                xy(i,iy)=xyj
                  ay(i,iy)=ayj
            end do
            m=17
            x1maxy(iy)=0
            x2maxy(iy)=0
            do i=1,m
                  if (xy(i,iy).le.nx1) x1maxy(iy)=i
                  if (xy(i,iy).le.nx1+nx2) x2maxy(iy)=i
            end do
            x0maxy(iy)=m
            y(iy)=epot(iq,ic)
      end do
      end do
      ny=ny+12
c
      return
      end
c
c
      SUBROUTINE dumat(ny,f,s,nx1,nx2,gpf,ipvl,
     1      fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2      dusub,dupot,xy,ay,y,x,x1maxy,x2maxy,x0maxy)
c
c This routine assembles the observation-like matrices for slip-rate
c  contributions to the apriori solution.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     1      +2*(maxpf+maxfT+maxXf)),
     2      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     3      +maxfs*maxduc+maxece*maxec)
      integer ny,f,s,nx1,nx2
      integer gp1s1,gp2s1
      integer ic,ip,fT,Xf,ih,iy,i,j,xyj,im,m
      integer gpf(0:maxfs,maxf)
      integer ipvl(maxgp)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:1)
      real*8 ayj
      real*8 dusub(2,2,0:1),dupot(2,2)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1s1=gpf(s-1,f)
      gp2s1=gpf(s,f)
      do ic=1,2
            xa(ic,0)=xfdu(ic,0,s-1,f)
      end do
      if (ipvl(gp2s1).eq.0) then
            do ic=1,2
                 xa(ic,1)=xfdu(ic,0,s,f)
            end do
      else
            ip=ipvl(gp2s1)
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xfTdu(ic,0,fT)
                  end do
                  goto 10
            end if
            if (Tfvlp(ip).ne.0) then
                  do ic=1,2
                       xa(ic,1)=xfdu(ic,0,s,f)
                  end do
                  goto 10
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xXfdu(ic,0,Xf)
                  end do
            end if
10          continue
      end if
c
      do ih=1,2
      do ic=1,2
            iy=ny+ic+2*(ih-1)
            do i=0,1
                  xy(i,iy)=xa(ic,i)
                  ay(i,iy)=dusub(ih,ic,i)
            end do
            do j=1,1
                  xyj=xy(j,iy)
                  ayj=ay(j,iy)
                  do i=j,1,-1
                        im=i-1
                        if (xy(im,iy).le.xyj) goto 70
                        xy(i,iy)=xy(im,iy)
                        ay(i,iy)=ay(im,iy)
                  end do
                  i=0
70                xy(i,iy)=xyj
                  ay(i,iy)=ayj
            end do
            m=1
            x1maxy(iy)=0
            x2maxy(iy)=0
            do i=1,m
                  if (xy(i,iy).le.nx1) x1maxy(iy)=i
                  if (xy(i,iy).le.nx1+nx2) x2maxy(iy)=i
            end do
            x0maxy(iy)=m
            y(iy)=dupot(ih,ic)
      end do
      end do
      ny=ny+4
c
      return
      end
c
c
      SUBROUTINE vomat(ny,e,nx1,nx2,gp1e,gp2e,gp3e,
     1      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2      xTfu,xTvlu,xXfu,xXvlu,iefv,kgp1fv,kgp2fv,kgp3fv,
     3      igp1fv,igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,
     4      rgp1fv,rgp2fv,rgp3fv,ou,uvo,xy,ay,y,x,
     5      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the matrices for velocity-observation
c  contributions to the aposteriori analysis.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxefv,maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     4      +maxfs*maxduc+maxece*maxec)
      integer ny,e,nx1,nx2
      integer gp1e1,gp2e1,gp3e1
      integer ic,r,kgp,igp,jgp,iy,i,j,xyj,im,m
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:9)
      real*8 ayj
      real*8 ou(2),uvo(2,2,0:9)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1e1=gp1e(e)
      gp2e1=gp2e(e)
      gp3e1=gp3e(e)
      if (iefv(e).eq.0) then
            do ic=1,2
                 xa(ic,0)=xgpu(ic,0,gp1e1)
                 xa(ic,1)=xgpu(ic,1,gp1e1)
                 xa(ic,2)=xgpu(ic,2,gp1e1)
                 xa(ic,3)=xgpu(ic,0,gp2e1)
                 xa(ic,4)=xgpu(ic,1,gp2e1)
                 xa(ic,5)=xgpu(ic,2,gp2e1)
                 xa(ic,6)=xgpu(ic,0,gp3e1)
                 xa(ic,7)=xgpu(ic,1,gp3e1)
                 xa(ic,8)=xgpu(ic,2,gp3e1)
            end do
      else
            r=rgp1fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,0)=xgpu(ic,0,gp1e1)
                       xa(ic,1)=xgpu(ic,1,gp1e1)
                       xa(ic,2)=xgpu(ic,2,gp1e1)
                  end do
                  goto 10
            end if
            kgp=kgp1fv(iefv(e))
            igp=igp1fv(iefv(e))
            jgp=jgp1fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,0)=xfu(ic,0,jgp,igp)
                       xa(ic,1)=xfu(ic,1,jgp,igp)
                       xa(ic,2)=xfu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,0)=xvlu(ic,0,jgp,igp)
                       xa(ic,1)=xvlu(ic,1,jgp,igp)
                       xa(ic,2)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,0)=xtju(ic,0,r,igp)
                       xa(ic,1)=xtju(ic,1,r,igp)
                       xa(ic,2)=xtju(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,0)=xJvlu(ic,0,r,igp)
                       xa(ic,1)=xJvlu(ic,1,r,igp)
                       xa(ic,2)=xJvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,0)=xfTu(ic,0,r,igp)
                       xa(ic,1)=xfTu(ic,1,r,igp)
                       xa(ic,2)=xfTu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,0)=xTfu(ic,0,r,igp)
                       xa(ic,1)=xTfu(ic,1,r,igp)
                       xa(ic,2)=xTfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,0)=xTvlu(ic,0,r,igp)
                       xa(ic,1)=xTvlu(ic,1,r,igp)
                       xa(ic,2)=xTvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,0)=xXfu(ic,0,r,igp)
                       xa(ic,1)=xXfu(ic,1,r,igp)
                       xa(ic,2)=xXfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,0)=xXvlu(ic,0,r,igp)
                       xa(ic,1)=xXvlu(ic,1,r,igp)
                       xa(ic,2)=xXvlu(ic,2,r,igp)
                  end do
            end if
10          continue
c
            r=rgp2fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,3)=xgpu(ic,0,gp2e1)
                       xa(ic,4)=xgpu(ic,1,gp2e1)
                       xa(ic,5)=xgpu(ic,2,gp2e1)
                  end do
                  goto 20
            end if
            kgp=kgp2fv(iefv(e))
            igp=igp2fv(iefv(e))
            jgp=jgp2fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,3)=xfu(ic,0,jgp,igp)
                       xa(ic,4)=xfu(ic,1,jgp,igp)
                       xa(ic,5)=xfu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,3)=xvlu(ic,0,jgp,igp)
                       xa(ic,4)=xvlu(ic,1,jgp,igp)
                       xa(ic,5)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,3)=xtju(ic,0,r,igp)
                       xa(ic,4)=xtju(ic,1,r,igp)
                       xa(ic,5)=xtju(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,3)=xJvlu(ic,0,r,igp)
                       xa(ic,4)=xJvlu(ic,1,r,igp)
                       xa(ic,5)=xJvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,3)=xfTu(ic,0,r,igp)
                       xa(ic,4)=xfTu(ic,1,r,igp)
                       xa(ic,5)=xfTu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,3)=xTfu(ic,0,r,igp)
                       xa(ic,4)=xTfu(ic,1,r,igp)
                       xa(ic,5)=xTfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,3)=xTvlu(ic,0,r,igp)
                       xa(ic,4)=xTvlu(ic,1,r,igp)
                       xa(ic,5)=xTvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,3)=xXfu(ic,0,r,igp)
                       xa(ic,4)=xXfu(ic,1,r,igp)
                       xa(ic,5)=xXfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,3)=xXvlu(ic,0,r,igp)
                       xa(ic,4)=xXvlu(ic,1,r,igp)
                       xa(ic,5)=xXvlu(ic,2,r,igp)
                  end do
            end if
20          continue
c
            r=rgp3fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,6)=xgpu(ic,0,gp3e1)
                       xa(ic,7)=xgpu(ic,1,gp3e1)
                       xa(ic,8)=xgpu(ic,2,gp3e1)
                  end do
                  goto 30
            end if
            kgp=kgp3fv(iefv(e))
            igp=igp3fv(iefv(e))
            jgp=jgp3fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,6)=xfu(ic,0,jgp,igp)
                       xa(ic,7)=xfu(ic,1,jgp,igp)
                       xa(ic,8)=xfu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,6)=xvlu(ic,0,jgp,igp)
                       xa(ic,7)=xvlu(ic,1,jgp,igp)
                       xa(ic,8)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,6)=xtju(ic,0,r,igp)
                       xa(ic,7)=xtju(ic,1,r,igp)
                       xa(ic,8)=xtju(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,6)=xJvlu(ic,0,r,igp)
                       xa(ic,7)=xJvlu(ic,1,r,igp)
                       xa(ic,8)=xJvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,6)=xfTu(ic,0,r,igp)
                       xa(ic,7)=xfTu(ic,1,r,igp)
                       xa(ic,8)=xfTu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,6)=xTfu(ic,0,r,igp)
                       xa(ic,7)=xTfu(ic,1,r,igp)
                       xa(ic,8)=xTfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,6)=xTvlu(ic,0,r,igp)
                       xa(ic,7)=xTvlu(ic,1,r,igp)
                       xa(ic,8)=xTvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,6)=xXfu(ic,0,r,igp)
                       xa(ic,7)=xXfu(ic,1,r,igp)
                       xa(ic,8)=xXfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,6)=xXvlu(ic,0,r,igp)
                       xa(ic,7)=xXvlu(ic,1,r,igp)
                       xa(ic,8)=xXvlu(ic,2,r,igp)
                  end do
            end if
30          continue
      end if
c
      do ic=1,2
            iy=ny+ic
            do i=0,8
                  xy(i,iy)=xa(1,i)
                  ay(i,iy)=uvo(ic,1,i)
            end do
            do i=9,17
                  xy(i,iy)=xa(2,i-9)
                  ay(i,iy)=uvo(ic,2,i-9)
            end do
            do j=1,17
                  xyj=xy(j,iy)
                  ayj=ay(j,iy)
                  do i=j,1,-1
                        im=i-1
                        if (xy(im,iy).le.xyj) goto 70
                        xy(i,iy)=xy(im,iy)
                        ay(i,iy)=ay(im,iy)
                  end do
                  i=0
70                xy(i,iy)=xyj
                  ay(i,iy)=ayj
            end do
            m=17
            x1maxy(iy)=0
            x2maxy(iy)=0
            do i=1,m
                  if (xy(i,iy).le.nx1) x1maxy(iy)=i
                  if (xy(i,iy).le.nx1+nx2) x2maxy(iy)=i
            end do
            x0maxy(iy)=m
            y(iy)=ou(ic)
      end do
      ny=ny+2
c
      return
      end
c
c
      SUBROUTINE fomat(ny,f,s,nx1,nx2,gpf,ipvl,
     1      fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2      nfoc,odu,ducfo,xy,ay,y,x,
     3      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the matrices for slip-rate-observation
c  contributions to the aposteriori analysis.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     1      +2*(maxpf+maxfT+maxXf)),
     2      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     3      +maxfs*maxduc+maxece*maxec)
      integer ny,f,s,nx1,nx2,nfoc
      integer gp1s1,gp2s1
      integer ic,ip,fT,Xf,iy,i,j,xyj,im,m
      integer gpf(0:maxfs,maxf)
      integer ipvl(maxgp)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:3)
      real*8 ayj
      real*8 odu(2),ducfo(2,2,0:1)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1s1=gpf(s-1,f)
      gp2s1=gpf(s,f)
      do ic=1,2
            xa(ic,0)=xfdu(ic,0,s-1,f)
      end do
      if (ipvl(gp2s1).eq.0) then
            do ic=1,2
                 xa(ic,1)=xfdu(ic,0,s,f)
            end do
      else
            ip=ipvl(gp2s1)
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xfTdu(ic,0,fT)
                  end do
                  goto 10
            end if
            if (Tfvlp(ip).ne.0) then
                  do ic=1,2
                       xa(ic,1)=xfdu(ic,0,s,f)
                  end do
                  goto 10
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xXfdu(ic,0,Xf)
                  end do
            end if
10          continue
      end if
c
      do ic=1,nfoc
            iy=ny+ic
            do i=0,1
                  xy(i,iy)=xa(1,i)
                  ay(i,iy)=ducfo(ic,1,i)
            end do
            do i=2,3
                  xy(i,iy)=xa(2,i-2)
                  ay(i,iy)=ducfo(ic,2,i-2)
            end do
            do j=1,3
                  xyj=xy(j,iy)
                  ayj=ay(j,iy)
                  do i=j,1,-1
                        im=i-1
                        if (xy(im,iy).le.xyj) goto 70
                        xy(i,iy)=xy(im,iy)
                        ay(i,iy)=ay(im,iy)
                  end do
                  i=0
70                xy(i,iy)=xyj
                  ay(i,iy)=ayj
            end do
            m=3
            x1maxy(iy)=0
            x2maxy(iy)=0
            do i=1,m
                  if (xy(i,iy).le.nx1) x1maxy(iy)=i
                  if (xy(i,iy).le.nx1+nx2) x2maxy(iy)=i
            end do
            x0maxy(iy)=m
            y(iy)=odu(ic)
      end do
      ny=ny+nfoc
c
      return
      end
c
c
      SUBROUTINE eomat(ny,e,nx1,nx2,gp1e,gp2e,gp3e,
     1      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2      xTfu,xTvlu,xXfu,xXvlu,iefv,kgp1fv,kgp2fv,kgp3fv,
     3      igp1fv,igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,
     4      rgp1fv,rgp2fv,rgp3fv,neoc,oe,eceo,xy,ay,y,x,
     5      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the matrices for strain-rate-observation
c  contributions to the aposteriori analysis.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxefv,maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     4      +maxfs*maxduc+maxece*maxec)
      integer ny,e,nx1,nx2,neoc
      integer gp1e1,gp2e1,gp3e1
      integer ic,r,kgp,igp,jgp,iy,i,j,xyj,im,m
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:9)
      real*8 ayj
      real*8 oe(3),eceo(3,2,0:9)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1e1=gp1e(e)
      gp2e1=gp2e(e)
      gp3e1=gp3e(e)
      if (iefv(e).eq.0) then
            do ic=1,2
                 xa(ic,0)=xgpu(ic,0,gp1e1)
                 xa(ic,1)=xgpu(ic,1,gp1e1)
                 xa(ic,2)=xgpu(ic,2,gp1e1)
                 xa(ic,3)=xgpu(ic,0,gp2e1)
                 xa(ic,4)=xgpu(ic,1,gp2e1)
                 xa(ic,5)=xgpu(ic,2,gp2e1)
                 xa(ic,6)=xgpu(ic,0,gp3e1)
                 xa(ic,7)=xgpu(ic,1,gp3e1)
                 xa(ic,8)=xgpu(ic,2,gp3e1)
            end do
      else
            r=rgp1fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,0)=xgpu(ic,0,gp1e1)
                       xa(ic,1)=xgpu(ic,1,gp1e1)
                       xa(ic,2)=xgpu(ic,2,gp1e1)
                  end do
                  goto 10
            end if
            kgp=kgp1fv(iefv(e))
            igp=igp1fv(iefv(e))
            jgp=jgp1fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,0)=xfu(ic,0,jgp,igp)
                       xa(ic,1)=xfu(ic,1,jgp,igp)
                       xa(ic,2)=xfu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,0)=xvlu(ic,0,jgp,igp)
                       xa(ic,1)=xvlu(ic,1,jgp,igp)
                       xa(ic,2)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,0)=xtju(ic,0,r,igp)
                       xa(ic,1)=xtju(ic,1,r,igp)
                       xa(ic,2)=xtju(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,0)=xJvlu(ic,0,r,igp)
                       xa(ic,1)=xJvlu(ic,1,r,igp)
                       xa(ic,2)=xJvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,0)=xfTu(ic,0,r,igp)
                       xa(ic,1)=xfTu(ic,1,r,igp)
                       xa(ic,2)=xfTu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,0)=xTfu(ic,0,r,igp)
                       xa(ic,1)=xTfu(ic,1,r,igp)
                       xa(ic,2)=xTfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,0)=xTvlu(ic,0,r,igp)
                       xa(ic,1)=xTvlu(ic,1,r,igp)
                       xa(ic,2)=xTvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,0)=xXfu(ic,0,r,igp)
                       xa(ic,1)=xXfu(ic,1,r,igp)
                       xa(ic,2)=xXfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,0)=xXvlu(ic,0,r,igp)
                       xa(ic,1)=xXvlu(ic,1,r,igp)
                       xa(ic,2)=xXvlu(ic,2,r,igp)
                  end do
            end if
10          continue
c
            r=rgp2fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,3)=xgpu(ic,0,gp2e1)
                       xa(ic,4)=xgpu(ic,1,gp2e1)
                       xa(ic,5)=xgpu(ic,2,gp2e1)
                  end do
                  goto 20
            end if
            kgp=kgp2fv(iefv(e))
            igp=igp2fv(iefv(e))
            jgp=jgp2fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,3)=xfu(ic,0,jgp,igp)
                       xa(ic,4)=xfu(ic,1,jgp,igp)
                       xa(ic,5)=xfu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,3)=xvlu(ic,0,jgp,igp)
                       xa(ic,4)=xvlu(ic,1,jgp,igp)
                       xa(ic,5)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,3)=xtju(ic,0,r,igp)
                       xa(ic,4)=xtju(ic,1,r,igp)
                       xa(ic,5)=xtju(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,3)=xJvlu(ic,0,r,igp)
                       xa(ic,4)=xJvlu(ic,1,r,igp)
                       xa(ic,5)=xJvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,3)=xfTu(ic,0,r,igp)
                       xa(ic,4)=xfTu(ic,1,r,igp)
                       xa(ic,5)=xfTu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,3)=xTfu(ic,0,r,igp)
                       xa(ic,4)=xTfu(ic,1,r,igp)
                       xa(ic,5)=xTfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,3)=xTvlu(ic,0,r,igp)
                       xa(ic,4)=xTvlu(ic,1,r,igp)
                       xa(ic,5)=xTvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,3)=xXfu(ic,0,r,igp)
                       xa(ic,4)=xXfu(ic,1,r,igp)
                       xa(ic,5)=xXfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,3)=xXvlu(ic,0,r,igp)
                       xa(ic,4)=xXvlu(ic,1,r,igp)
                       xa(ic,5)=xXvlu(ic,2,r,igp)
                  end do
            end if
20          continue
c
            r=rgp3fv(iefv(e))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,6)=xgpu(ic,0,gp3e1)
                       xa(ic,7)=xgpu(ic,1,gp3e1)
                       xa(ic,8)=xgpu(ic,2,gp3e1)
                  end do

                  goto 30
            end if
            kgp=kgp3fv(iefv(e))
            igp=igp3fv(iefv(e))
            jgp=jgp3fv(iefv(e))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,6)=xfu(ic,0,jgp,igp)
                       xa(ic,7)=xfu(ic,1,jgp,igp)
                       xa(ic,8)=xfu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,6)=xvlu(ic,0,jgp,igp)
                       xa(ic,7)=xvlu(ic,1,jgp,igp)
                       xa(ic,8)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,6)=xtju(ic,0,r,igp)
                       xa(ic,7)=xtju(ic,1,r,igp)
                       xa(ic,8)=xtju(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,6)=xJvlu(ic,0,r,igp)
                       xa(ic,7)=xJvlu(ic,1,r,igp)
                       xa(ic,8)=xJvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,6)=xfTu(ic,0,r,igp)
                       xa(ic,7)=xfTu(ic,1,r,igp)
                       xa(ic,8)=xfTu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,6)=xTfu(ic,0,r,igp)
                       xa(ic,7)=xTfu(ic,1,r,igp)
                       xa(ic,8)=xTfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,6)=xTvlu(ic,0,r,igp)
                       xa(ic,7)=xTvlu(ic,1,r,igp)
                       xa(ic,8)=xTvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,6)=xXfu(ic,0,r,igp)
                       xa(ic,7)=xXfu(ic,1,r,igp)
                       xa(ic,8)=xXfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,6)=xXvlu(ic,0,r,igp)
                       xa(ic,7)=xXvlu(ic,1,r,igp)
                       xa(ic,8)=xXvlu(ic,2,r,igp)
                  end do
            end if
30          continue
      end if
c
      do ic=1,neoc
            iy=ny+ic
            do i=0,8
                  xy(i,iy)=xa(1,i)
                  ay(i,iy)=eceo(ic,1,i)
            end do
            do i=9,17
                  xy(i,iy)=xa(2,i-9)
                  ay(i,iy)=eceo(ic,2,i-9)
            end do
            do j=1,17
                  xyj=xy(j,iy)
                  ayj=ay(j,iy)
                  do i=j,1,-1
                        im=i-1
                        if (xy(im,iy).le.xyj) goto 70
                        xy(i,iy)=xy(im,iy)
                        ay(i,iy)=ay(im,iy)
                  end do
                  i=0
70                xy(i,iy)=xyj
                  ay(i,iy)=ayj
            end do
            m=17
            x1maxy(iy)=0
            x2maxy(iy)=0
            do i=1,m
                  if (xy(i,iy).le.nx1) x1maxy(iy)=i
                  if (xy(i,iy).le.nx1+nx2) x2maxy(iy)=i
            end do
            x0maxy(iy)=m
            y(iy)=oe(ic)
      end do
      ny=ny+neoc
c
      return
      end
c
c
      SUBROUTINE ducmat(ny,f,s1,s2,nx1,nx2,gpf,ipvl,
     1      fTvlp,Tfvlp,Xfvlp,xfdu,xfTdu,xXfdu,
     2      duc,dus1c,dus2c,xy,ay,y,x,
     3      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the matrices for slip-rate-correlation
c  contributions to the aposteriori analysis.
c
      implicit none
      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     1      +2*(maxpf+maxfT+maxXf)),
     2      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     3      +maxfs*maxduc+maxece*maxec)
      integer ny,f,s1,s2,nx1,nx2
      integer gp1s1,gp2s1,gp1s2,gp2s2
      integer ic,ip,fT,Xf,i,j,xyj,im,m,jp
      integer gpf(0:maxfs,maxf)
      integer ipvl(maxgp)
      integer fTvlp(maxpvl),Tfvlp(maxpvl),Xfvlp(maxpvl)
      integer xfdu(2,0:1,0:maxfs,maxf)
      integer xfTdu(2,0:1,maxfT),xXfdu(2,0:1,maxXf)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:7)
      real*8 duc
      real*8 ayj
      real*8 dus1c(2,0:1),dus2c(2,0:1)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1s1=gpf(s1-1,f)
      gp2s1=gpf(s1,f)
      gp1s2=gpf(s2-1,f)
      gp2s2=gpf(s2,f)
      do ic=1,2
            xa(ic,0)=xfdu(ic,0,s1-1,f)
      end do
      if (ipvl(gp2s1).eq.0) then
            do ic=1,2
                 xa(ic,1)=xfdu(ic,0,s1,f)
            end do
      else
            ip=ipvl(gp2s1)
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xfTdu(ic,0,fT)
                  end do
                  goto 10
            end if
            if (Tfvlp(ip).ne.0) then
                  do ic=1,2
                       xa(ic,1)=xfdu(ic,0,s1,f)
                  end do
                  goto 10
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do ic=1,2
                       xa(ic,1)=xXfdu(ic,0,Xf)
                  end do

            end if
10          continue
      end if
c
      do ic=1,2
            xa(ic,2)=xfdu(ic,0,s2-1,f)
      end do
      if (ipvl(gp2s2).eq.0) then
            do ic=1,2
                 xa(ic,3)=xfdu(ic,0,s2,f)
            end do
      else
            ip=ipvl(gp2s2)
            if (fTvlp(ip).ne.0) then
                  fT=fTvlp(ip)
                  do ic=1,2
                       xa(ic,3)=xfTdu(ic,0,fT)
                  end do
                  goto 20
            end if
            if (Tfvlp(ip).ne.0) then
                  do ic=1,2
                       xa(ic,3)=xfdu(ic,0,s2,f)
                  end do
                  goto 20
            end if
            if (Xfvlp(ip).ne.0) then
                  Xf=Xfvlp(ip)
                  do ic=1,2
                       xa(ic,3)=xXfdu(ic,0,Xf)
                  end do
            end if
20          continue
      end if
c
      ny=ny+1
      do i=0,1
            xy(i,ny)=xa(1,i)
            ay(i,ny)=dus1c(1,i)
            xy(i+4,ny)=xa(2,i)
            ay(i+4,ny)=dus1c(2,i)
      end do
      do i=2,3
            xy(i,ny)=xa(1,i)
            ay(i,ny)=dus2c(1,i-4)
            xy(i+4,ny)=xa(2,i)
            ay(i+4,ny)=dus2c(2,i-4)
      end do
      do j=1,7
            xyj=xy(j,ny)
            ayj=ay(j,ny)
            do i=j,1,-1
                  im=i-1
                  if (xy(im,ny).le.xyj) goto 70
                  xy(i,ny)=xy(im,ny)
                  ay(i,ny)=ay(im,ny)
            end do
            i=0
70          xy(i,ny)=xyj
            ay(i,ny)=ayj
      end do
      m=7
      do i=7,1,-1
            im=i-1
            if (xy(i,ny).eq.xy(im,ny)) then
                  ay(im,ny)=ay(im,ny)+ay(i,ny)
                  m=m-1
                  if (i.le.m) then
                        do j=i,m
                              jp=j+1
                              xy(j,ny)=xy(jp,ny)
                              ay(j,ny)=ay(jp,ny)
                        end do
                  end if
            end if
      end do
      x1maxy(ny)=0
      x2maxy(ny)=0
      do i=1,m
            if (xy(i,ny).le.nx1) x1maxy(ny)=i
            if (xy(i,ny).le.nx1+nx2) x2maxy(ny)=i
      end do
      x0maxy(ny)=m
      y(ny)=duc
c
      return
      end
c
c
      SUBROUTINE ecmat(ny,e1,e2,nx1,nx2,gp1e,gp2e,gp3e,
     1      xgpu,xeu,xfu,xvlu,xtju,xJvlu,xfTu,
     2      xTfu,xTvlu,xXfu,xXvlu,iefv,kgp1fv,kgp2fv,kgp3fv,
     3      igp1fv,igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,
     4      rgp1fv,rgp2fv,rgp3fv,ec,ee1c,ee2c,xy,ay,y,x,
     5      x1maxy,x2maxy,x0maxy)
c
c This routine assembles the matrices for strain-rate-correlation
c  contributions to the aposteriori analysis.
c
      implicit none

      integer maxgp,maxe,maxf,maxfs,
     1      maxvl,maxvls,
     2      maxvo,maxfo,maxeo,maxduc,maxec,maxece,
     3      maxsf,maxsvl,maxpf,maxpvl,
     4      maxtj,maxJvl,maxfT,maxTf,maxTvl,maxXf,maxXvl,
     5      maxefv,maxx,maxy
      parameter(maxgp=40000,maxe=2*maxgp,
     1      maxf=600,maxfs=100,
     2      maxvl=10,maxvls=200,
     3      maxvo=5000,maxfo=maxf*maxfs,maxeo=maxe,
     4      maxduc=2*maxf,maxec=100,maxece=500,
     5      maxsf=maxf*maxfs,maxsvl=maxvl*maxvls,
     6      maxpf=maxsf+maxf,maxpvl=maxsvl+maxvl,
     7      maxtj=80,maxJvl=10,maxfT=10,
     8      maxTf=40,maxTvl=10,maxXf=200,maxXvl=10,
     9      maxefv=12*(maxsf+maxsvl),
     1      maxx=2*(3*(maxgp+maxpf+maxpvl+2*maxtj
     1      +3*(maxJvl+maxfT+maxTf+maxTvl+maxXf+maxXvl))
     2      +2*(maxpf+maxfT+maxXf)),
     3      maxy=12*maxe+4*maxsf+2*maxvo+2*maxfo+3*maxeo
     4      +maxfs*maxduc+maxece*maxec)
      integer ny,e1,e2,nx1,nx2
      integer gp1e1,gp2e1,gp3e1,gp1e2,gp2e2,gp3e2
      integer ic,r,kgp,igp,jgp,i,j,xyj,im,m,jp
      integer gp1e(maxe),gp2e(maxe),gp3e(maxe)
      integer iefv(maxe)
      integer kgp1fv(maxefv),kgp2fv(maxefv),kgp3fv(maxefv)
      integer igp1fv(maxefv),igp2fv(maxefv),igp3fv(maxefv)
      integer jgp1fv(maxefv),jgp2fv(maxefv),jgp3fv(maxefv)
      integer rgp1fv(maxefv),rgp2fv(maxefv),rgp3fv(maxefv)
      integer xgpu(2,0:2,maxgp),xeu(2,maxe)
      integer xfu(2,0:2,0:maxfs,maxf),xvlu(2,0:2,0:maxvls,maxvl)
      integer xtju(2,0:2,2,maxtj)
      integer xJvlu(2,0:2,3,maxJvl),xfTu(2,0:2,3,maxfT),
     1      xTfu(2,0:2,3,maxTf),xTvlu(2,0:2,3,maxTvl),
     2      xXfu(2,0:2,3,maxXf),xXvlu(2,0:2,3,maxXvl)
      integer xy(0:35,maxy)
      integer x1maxy(maxy),x2maxy(maxy),x0maxy(maxy)
      integer xa(2,0:19)
      real*8 ec
      real*8 ayj
      real*8 ee1c(2,0:9),ee2c(2,0:9)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      gp1e1=gp1e(e1)
      gp2e1=gp2e(e1)
      gp3e1=gp3e(e1)
      gp1e2=gp1e(e2)
      gp2e2=gp2e(e2)
      gp3e2=gp3e(e2)
      if (iefv(e1).eq.0) then
            do ic=1,2
                 xa(ic,0)=xgpu(ic,0,gp1e1)
                 xa(ic,1)=xgpu(ic,1,gp1e1)
                 xa(ic,2)=xgpu(ic,2,gp1e1)
                 xa(ic,3)=xgpu(ic,0,gp2e1)
                 xa(ic,4)=xgpu(ic,1,gp2e1)
                 xa(ic,5)=xgpu(ic,2,gp2e1)
                 xa(ic,6)=xgpu(ic,0,gp3e1)
                 xa(ic,7)=xgpu(ic,1,gp3e1)
                 xa(ic,8)=xgpu(ic,2,gp3e1)
            end do
      else
            r=rgp1fv(iefv(e1))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,0)=xgpu(ic,0,gp1e1)
                       xa(ic,1)=xgpu(ic,1,gp1e1)
                       xa(ic,2)=xgpu(ic,2,gp1e1)
                  end do
                  goto 10
            end if
            kgp=kgp1fv(iefv(e1))
            igp=igp1fv(iefv(e1))
            jgp=jgp1fv(iefv(e1))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,0)=xfu(ic,0,jgp,igp)
                       xa(ic,1)=xfu(ic,1,jgp,igp)
                       xa(ic,2)=xfu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,0)=xvlu(ic,0,jgp,igp)
                       xa(ic,1)=xvlu(ic,1,jgp,igp)
                       xa(ic,2)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,0)=xtju(ic,0,r,igp)
                       xa(ic,1)=xtju(ic,1,r,igp)
                       xa(ic,2)=xtju(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,0)=xJvlu(ic,0,r,igp)
                       xa(ic,1)=xJvlu(ic,1,r,igp)
                       xa(ic,2)=xJvlu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,0)=xfTu(ic,0,r,igp)
                       xa(ic,1)=xfTu(ic,1,r,igp)
                       xa(ic,2)=xfTu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,0)=xTfu(ic,0,r,igp)
                       xa(ic,1)=xTfu(ic,1,r,igp)
                       xa(ic,2)=xTfu(ic,2,r,igp)

                  end do
                  goto 10
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,0)=xTvlu(ic,0,r,igp)
                       xa(ic,1)=xTvlu(ic,1,r,igp)
                       xa(ic,2)=xTvlu(ic,2,r,igp)
                  end do
                  goto 10

            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,0)=xXfu(ic,0,r,igp)
                       xa(ic,1)=xXfu(ic,1,r,igp)
                       xa(ic,2)=xXfu(ic,2,r,igp)
                  end do
                  goto 10
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,0)=xXvlu(ic,0,r,igp)
                       xa(ic,1)=xXvlu(ic,1,r,igp)
                       xa(ic,2)=xXvlu(ic,2,r,igp)
                  end do
            end if
10          continue
c
            r=rgp2fv(iefv(e1))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,3)=xgpu(ic,0,gp2e1)
                       xa(ic,4)=xgpu(ic,1,gp2e1)
                       xa(ic,5)=xgpu(ic,2,gp2e1)
                  end do
                  goto 20
            end if
            kgp=kgp2fv(iefv(e1))
            igp=igp2fv(iefv(e1))
            jgp=jgp2fv(iefv(e1))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,3)=xfu(ic,0,jgp,igp)
                       xa(ic,4)=xfu(ic,1,jgp,igp)
                       xa(ic,5)=xfu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,3)=xvlu(ic,0,jgp,igp)
                       xa(ic,4)=xvlu(ic,1,jgp,igp)
                       xa(ic,5)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,3)=xtju(ic,0,r,igp)
                       xa(ic,4)=xtju(ic,1,r,igp)
                       xa(ic,5)=xtju(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,3)=xJvlu(ic,0,r,igp)
                       xa(ic,4)=xJvlu(ic,1,r,igp)
                       xa(ic,5)=xJvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.5) then
                  do ic=1,2

                       xa(ic,3)=xfTu(ic,0,r,igp)
                       xa(ic,4)=xfTu(ic,1,r,igp)
                       xa(ic,5)=xfTu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,3)=xTfu(ic,0,r,igp)
                       xa(ic,4)=xTfu(ic,1,r,igp)
                       xa(ic,5)=xTfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,3)=xTvlu(ic,0,r,igp)
                       xa(ic,4)=xTvlu(ic,1,r,igp)
                       xa(ic,5)=xTvlu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,3)=xXfu(ic,0,r,igp)
                       xa(ic,4)=xXfu(ic,1,r,igp)
                       xa(ic,5)=xXfu(ic,2,r,igp)
                  end do
                  goto 20
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,3)=xXvlu(ic,0,r,igp)
                       xa(ic,4)=xXvlu(ic,1,r,igp)
                       xa(ic,5)=xXvlu(ic,2,r,igp)
                  end do
            end if
20          continue
c
            r=rgp3fv(iefv(e1))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,6)=xgpu(ic,0,gp3e1)
                       xa(ic,7)=xgpu(ic,1,gp3e1)
                       xa(ic,8)=xgpu(ic,2,gp3e1)
                  end do
                  goto 30
            end if
            kgp=kgp3fv(iefv(e1))
            igp=igp3fv(iefv(e1))
            jgp=jgp3fv(iefv(e1))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,6)=xfu(ic,0,jgp,igp)
                       xa(ic,7)=xfu(ic,1,jgp,igp)
                       xa(ic,8)=xfu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,6)=xvlu(ic,0,jgp,igp)
                       xa(ic,7)=xvlu(ic,1,jgp,igp)
                       xa(ic,8)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,6)=xtju(ic,0,r,igp)
                       xa(ic,7)=xtju(ic,1,r,igp)
                       xa(ic,8)=xtju(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,6)=xJvlu(ic,0,r,igp)
                       xa(ic,7)=xJvlu(ic,1,r,igp)

                       xa(ic,8)=xJvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,6)=xfTu(ic,0,r,igp)
                       xa(ic,7)=xfTu(ic,1,r,igp)
                       xa(ic,8)=xfTu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,6)=xTfu(ic,0,r,igp)
                       xa(ic,7)=xTfu(ic,1,r,igp)
                       xa(ic,8)=xTfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,6)=xTvlu(ic,0,r,igp)
                       xa(ic,7)=xTvlu(ic,1,r,igp)
                       xa(ic,8)=xTvlu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,6)=xXfu(ic,0,r,igp)
                       xa(ic,7)=xXfu(ic,1,r,igp)
                       xa(ic,8)=xXfu(ic,2,r,igp)
                  end do
                  goto 30
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,6)=xXvlu(ic,0,r,igp)
                       xa(ic,7)=xXvlu(ic,1,r,igp)
                       xa(ic,8)=xXvlu(ic,2,r,igp)
                  end do
            end if
30          continue
      end if
c
      if (iefv(e2).eq.0) then
            do ic=1,2
                 xa(ic,9)=xgpu(ic,0,gp1e2)
                 xa(ic,10)=xgpu(ic,1,gp1e2)
                 xa(ic,11)=xgpu(ic,2,gp1e2)
                 xa(ic,12)=xgpu(ic,0,gp2e2)
                 xa(ic,13)=xgpu(ic,1,gp2e2)
                 xa(ic,14)=xgpu(ic,2,gp2e2)
                 xa(ic,15)=xgpu(ic,0,gp3e2)
                 xa(ic,16)=xgpu(ic,1,gp3e2)
                 xa(ic,17)=xgpu(ic,2,gp3e2)
            end do
      else
            r=rgp1fv(iefv(e2))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,9)=xgpu(ic,0,gp1e2)
                       xa(ic,10)=xgpu(ic,1,gp1e2)
                       xa(ic,11)=xgpu(ic,2,gp1e2)
                  end do
                  goto 40
            end if
            kgp=kgp1fv(iefv(e2))
            igp=igp1fv(iefv(e2))
            jgp=jgp1fv(iefv(e2))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,9)=xfu(ic,0,jgp,igp)
                       xa(ic,10)=xfu(ic,1,jgp,igp)
                       xa(ic,11)=xfu(ic,2,jgp,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.2) then
                  do ic=1,2
                       xa(ic,9)=xvlu(ic,0,jgp,igp)
                       xa(ic,10)=xvlu(ic,1,jgp,igp)
                       xa(ic,11)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,9)=xtju(ic,0,r,igp)
                       xa(ic,10)=xtju(ic,1,r,igp)
                       xa(ic,11)=xtju(ic,2,r,igp)

                  end do
                  goto 40
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,9)=xJvlu(ic,0,r,igp)
                       xa(ic,10)=xJvlu(ic,1,r,igp)
                       xa(ic,11)=xJvlu(ic,2,r,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,9)=xfTu(ic,0,r,igp)
                       xa(ic,10)=xfTu(ic,1,r,igp)
                       xa(ic,11)=xfTu(ic,2,r,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,9)=xTfu(ic,0,r,igp)
                       xa(ic,10)=xTfu(ic,1,r,igp)
                       xa(ic,11)=xTfu(ic,2,r,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,9)=xTvlu(ic,0,r,igp)
                       xa(ic,10)=xTvlu(ic,1,r,igp)
                       xa(ic,11)=xTvlu(ic,2,r,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,9)=xXfu(ic,0,r,igp)
                       xa(ic,10)=xXfu(ic,1,r,igp)
                       xa(ic,11)=xXfu(ic,2,r,igp)
                  end do
                  goto 40
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,9)=xXvlu(ic,0,r,igp)
                       xa(ic,10)=xXvlu(ic,1,r,igp)
                       xa(ic,11)=xXvlu(ic,2,r,igp)
                  end do
            end if
40          continue
c
            r=rgp2fv(iefv(e2))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,12)=xgpu(ic,0,gp2e2)
                       xa(ic,13)=xgpu(ic,1,gp2e2)
                       xa(ic,14)=xgpu(ic,2,gp2e2)
                  end do
                  goto 50
            end if
            kgp=kgp2fv(iefv(e2))
            igp=igp2fv(iefv(e2))
            jgp=jgp2fv(iefv(e2))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,12)=xfu(ic,0,jgp,igp)
                       xa(ic,13)=xfu(ic,1,jgp,igp)
                       xa(ic,14)=xfu(ic,2,jgp,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.2) then

                  do ic=1,2
                       xa(ic,12)=xvlu(ic,0,jgp,igp)
                       xa(ic,13)=xvlu(ic,1,jgp,igp)
                       xa(ic,14)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,12)=xtju(ic,0,r,igp)
                       xa(ic,13)=xtju(ic,1,r,igp)

                       xa(ic,14)=xtju(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,12)=xJvlu(ic,0,r,igp)
                       xa(ic,13)=xJvlu(ic,1,r,igp)
                       xa(ic,14)=xJvlu(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,12)=xfTu(ic,0,r,igp)
                       xa(ic,13)=xfTu(ic,1,r,igp)
                       xa(ic,14)=xfTu(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,12)=xTfu(ic,0,r,igp)
                       xa(ic,13)=xTfu(ic,1,r,igp)
                       xa(ic,14)=xTfu(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,12)=xTvlu(ic,0,r,igp)
                       xa(ic,13)=xTvlu(ic,1,r,igp)
                       xa(ic,14)=xTvlu(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,12)=xXfu(ic,0,r,igp)
                       xa(ic,13)=xXfu(ic,1,r,igp)
                       xa(ic,14)=xXfu(ic,2,r,igp)
                  end do
                  goto 50
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,12)=xXvlu(ic,0,r,igp)
                       xa(ic,13)=xXvlu(ic,1,r,igp)
                       xa(ic,14)=xXvlu(ic,2,r,igp)
                  end do
            end if
50          continue
c
            r=rgp3fv(iefv(e2))
            if (r.eq.0) then
                  do ic=1,2
                       xa(ic,15)=xgpu(ic,0,gp3e2)
                       xa(ic,16)=xgpu(ic,1,gp3e2)
                       xa(ic,17)=xgpu(ic,2,gp3e2)
                  end do
                  goto 60
            end if
            kgp=kgp3fv(iefv(e2))
            igp=igp3fv(iefv(e2))
            jgp=jgp3fv(iefv(e2))
            if (kgp.eq.1) then
                  do ic=1,2
                       xa(ic,15)=xfu(ic,0,jgp,igp)
                       xa(ic,16)=xfu(ic,1,jgp,igp)
                       xa(ic,17)=xfu(ic,2,jgp,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.2) then

                  do ic=1,2
                       xa(ic,15)=xvlu(ic,0,jgp,igp)
                       xa(ic,16)=xvlu(ic,1,jgp,igp)
                       xa(ic,17)=xvlu(ic,2,jgp,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.3) then
                  do ic=1,2
                       xa(ic,15)=xtju(ic,0,r,igp)
                       xa(ic,16)=xtju(ic,1,r,igp)
                       xa(ic,17)=xtju(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.4) then
                  do ic=1,2
                       xa(ic,15)=xJvlu(ic,0,r,igp)

                       xa(ic,16)=xJvlu(ic,1,r,igp)
                       xa(ic,17)=xJvlu(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.5) then
                  do ic=1,2
                       xa(ic,15)=xfTu(ic,0,r,igp)
                       xa(ic,16)=xfTu(ic,1,r,igp)
                       xa(ic,17)=xfTu(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.6) then
                  do ic=1,2
                       xa(ic,15)=xTfu(ic,0,r,igp)
                       xa(ic,16)=xTfu(ic,1,r,igp)
                       xa(ic,17)=xTfu(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.7) then
                  do ic=1,2
                       xa(ic,15)=xTvlu(ic,0,r,igp)
                       xa(ic,16)=xTvlu(ic,1,r,igp)
                       xa(ic,17)=xTvlu(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.8) then
                  do ic=1,2
                       xa(ic,15)=xXfu(ic,0,r,igp)
                       xa(ic,16)=xXfu(ic,1,r,igp)
                       xa(ic,17)=xXfu(ic,2,r,igp)
                  end do
                  goto 60
            end if
            if (kgp.eq.9) then
                  do ic=1,2
                       xa(ic,15)=xXvlu(ic,0,r,igp)
                       xa(ic,16)=xXvlu(ic,1,r,igp)
                       xa(ic,17)=xXvlu(ic,2,r,igp)
                  end do
            end if
60          continue
      end if
c
      ny=ny+1
      do i=0,8
            xy(i,ny)=xa(1,i)
            ay(i,ny)=ee1c(1,i)
            xy(i+18,ny)=xa(2,i)
            ay(i+18,ny)=ee1c(2,i)
      end do
      do i=9,17
            xy(i,ny)=xa(1,i)
            ay(i,ny)=ee2c(1,i-9)
            xy(i+18,ny)=xa(2,i)
            ay(i+18,ny)=ee2c(2,i-9)
      end do
      do j=1,35
            xyj=xy(j,ny)
            ayj=ay(j,ny)
            do i=j,1,-1
                  im=i-1
                  if (xy(im,ny).le.xyj) goto 70
                  xy(i,ny)=xy(im,ny)
                  ay(i,ny)=ay(im,ny)
            end do
            i=0
70          xy(i,ny)=xyj
            ay(i,ny)=ayj
      end do
      m=35
      do i=35,1,-1
            im=i-1
            if (xy(i,ny).eq.xy(im,ny)) then
                  ay(im,ny)=ay(im,ny)+ay(i,ny)
                  m=m-1
                  if (i.le.m) then
                        do j=i,m
                              jp=j+1
                              xy(j,ny)=xy(jp,ny)
                              ay(j,ny)=ay(jp,ny)
                        end do
                  end if
            end if
      end do
      x1maxy(ny)=0
      x2maxy(ny)=0
      do i=1,m
            if (xy(i,ny).le.nx1) x1maxy(ny)=i
            if (xy(i,ny).le.nx1+nx2) x2maxy(ny)=i
      end do
      x0maxy(ny)=m
      y(ny)=ec
c
      return
      end
c
c23456789112345678921234567893123456789412345678951234567896123456789712
c
      SUBROUTINE output(ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec,
     1      npf,npvl,nprb,nsf,nsvl,nsrb,ntj,nJvl,nfT,nTf,nTvl,
     2      nXf,nXvl,nefv,nz,nx1,nx,nx2,nx0,ny,ny0,ny1,e1s,e2s,
     3      s1e,s2e,s3e,gp1e,gp2e,gp3e,nfs,sf,gpf,nvls,gpvl,svl,fvl,
     4      nrbs,srb,gprb,evo,ffo,sfo,nfoc,eeo,neoc,fduc,nducs,sduc,
     5      ncduc,s1duc,s2duc,nece,eec,ncec,e1ec,e2ec,ipf,ipvl,iprb,
     6      isf,isvl,isrb,fs,sfs,nfp,fp,gpfp,jfp,tjfp,vls,svls,
     7      nvlp,vlp,gpvlp,rbs,srbs,rbp,gprbp,Jvlvlp,fTvlp,Tfvlp,
     8      Tvlvlp,Xfvlp,Xvlvlp,iefv,kgp1fv,kgp2fv,kgp3fv,igp1fv,
     9      igp2fv,igp3fv,jgp1fv,jgp2fv,jgp3fv,rgp1fv,rgp2fv,rgp3fv,
     1      rgprb,sz,kz,jz,kx,ix,jx,jcx,icx,zmin,csqlen,xz,x1maxz,
     1      x2maxz,x0maxz,xy,x2maxy,x0maxy,extern,rE,long,lat,
     2      sxxpot,syypot,sxypot,ux,uy,uxm,uym,uxp,uyp,volong,volat,
     3      oux,ouy,oduc,oec,area,exx,eyy,exy,sxx,syy,sxy,fx,fy,
     4      Lce,Lcce,Lcse,Lse,Lsce,Lsse,Kcfp,Ksfp,lenfs,dufs,
     5      ttfs,tnfs,Kcfs,Ksfs,Lcgp,Lccgp,Lcsgp,Lsgp,Lscgp,Lssgp,
     6      Lcf,Lccf,Lcsf,Lsf,Lscf,Lssf,Lctj,Lcctj,Lcstj,
     7      Lstj,Lsctj,Lsstj,ugprb,usrb,seou,seodu,seoe,cz,z,ay,y,x)
c
c This routine outputs in binary files the control and input data
c  required by the inversion and output programs.
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
      integer i,j,k,l,ix1
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
      integer zmin(maxz)
      integer csqlen(maxz)
      integer xz(0:25,maxz)
      integer x1maxz(maxz),x2maxz(maxz),x0maxz(maxz)
      integer xy(0:35,maxy)
      integer x2maxy(maxy),x0maxy(maxy)
      logical extern(maxgp)
      real*8 rE
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
      real*8 cz(0:25,maxz),z(maxz)
      real*8 ay(0:35,maxy),y(maxy)
      real*8 x(maxx)
c
      open(2,file='output_control.bin',form='unformatted')
      write(2) ngp,ns,ne,nf,nvl,nrb,nvo,nfo,neo,nduc,nec
      write(2) npf,npvl,nprb,nsf,nsvl,nsrb,ntj
      write(2) nJvl,nfT,nTf,nTvl,nXf,nXvl,nefv
      write(2) nz,nx
      write(2) (e1s(i),e2s(i),i=1,ns)
      write(2) (s1e(i),s2e(i),s3e(i),i=1,ne)
      write(2) (gp1e(i),gp2e(i),gp3e(i),i=1,ne)
      write(2) (nfs(i),i=1,nf)
      write(2) ((sf(j,i),j=1,nfs(i)),i=1,nf)
      write(2) ((gpf(j,i),j=0,nfs(i)),i=1,nf)
      write(2) (nvls(i),i=1,nvl)
      write(2) ((gpvl(j,i),j=0,nvls(i)),i=1,nvl)
      write(2) ((svl(j,i),j=1,nvls(i)),i=1,nvl)
      write(2) ((fvl(j,i),j=0,nvls(i)),i=1,nvl)
      write(2) (nrbs(i),i=1,nrb)
      write(2) ((srb(j,i),j=1,nrbs(i)),i=1,nrb)
      write(2) ((gprb(j,i),j=0,nrbs(i)),i=1,nrb)
      write(2) (evo(i),i=1,nvo)
      write(2) (ffo(i),sfo(i),nfoc(i),i=1,nfo)
      write(2) (eeo(i),neoc(i),i=1,neo)
      write(2) (fduc(i),nducs(i),i=1,nduc)
      write(2) ((sduc(j,i),j=1,nducs(i)),i=1,nduc)
      write(2) (ncduc(i),i=1,nduc)
      write(2) ((s1duc(j,i),s2duc(j,i),j=1,ncduc(i)),i=1,nduc)
      write(2) (nece(i),i=1,nec)
      write(2) ((eec(j,i),j=1,nece(i)),i=1,nec)
      write(2) (ncec(i),i=1,nec)
      write(2) ((e1ec(j,i),e2ec(j,i),j=1,ncec(i)),i=1,nec)
      write(2) (ipf(i),ipvl(i),iprb(i),i=1,ngp)
      write(2) (isf(i),isvl(i),isrb(i),i=1,ns)
      write(2) (fs(i),sfs(i),i=1,nsf)
      write(2) (nfp(i),i=1,npf)
      write(2) ((fp(j,i),gpfp(j,i),j=1,nfp(i)),i=1,npf)
      write(2) (jfp(i),tjfp(i),i=1,npf)
      write(2) (vls(i),svls(i),i=1,nsvl)
      write(2) (nvlp(i),i=1,npvl)
      write(2) ((vlp(j,i),gpvlp(j,i),j=1,nvlp(i)),i=1,npvl)
      write(2) (rbs(i),srbs(i),i=1,nsrb)
      write(2) (rbp(i),gprbp(i),i=1,nprb)
      write(2) (Jvlvlp(i),fTvlp(i),Tfvlp(i),i=1,npvl)
      write(2) (Tvlvlp(i),Xfvlp(i),Xvlvlp(i),i=1,npvl)
      write(2) (iefv(i),i=1,ne)
      write(2) (kgp1fv(i),kgp2fv(i),kgp3fv(i),i=1,nefv)
      write(2) (igp1fv(i),igp2fv(i),igp3fv(i),i=1,nefv)
      write(2) (jgp1fv(i),jgp2fv(i),jgp3fv(i),i=1,nefv)
      write(2) (rgp1fv(i),rgp2fv(i),rgp3fv(i),i=1,nefv)
      write(2) ((rgprb(j,i),j=0,nrbs(i)),i=1,nrb)
      write(2) (sz(i),kz(i),jz(i),i=1,nz)
      write(2) (kx(i),ix(i),jx(i),jcx(i),icx(i),i=1,nx)
      write(2) (extern(i),i=1,ngp)
      write(2) rE
      write(2) (long(i),lat(i),i=1,ngp)
      write(2) (sxxpot(i),syypot(i),sxypot(i),i=1,ngp)
      do i=1,nvl
            nvlj(i)=2*nvls(i)+1
      end do
      write(2) ((ux(j,i),uy(j,i),j=1,nvlj(i)),i=1,nvl)
      write(2) ((uxm(j,i),uym(j,i),j=0,nvls(i)),i=1,nvl)

      write(2) ((uxp(j,i),uyp(j,i),j=0,nvls(i)),i=1,nvl)
      write(2) (volong(i),volat(i),i=1,nvo)
      write(2) (oux(i),ouy(i),i=1,nvo)
      write(2) ((oduc(j,i),j=1,nfoc(i)),i=1,nfo)
      write(2) ((oec(j,i),j=1,neoc(i)),i=1,neo)
      write(2) (area(i),i=1,ne)
      write(2) (((exx(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (((eyy(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (((exy(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (((sxx(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (((syy(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (((sxy(k,j,i),k=1,2),j=0,8),i=1,ne)
      write(2) (fx(i),fy(i),i=1,ne)
      write(2) (Lce(i),Lcce(i),Lcse(i),i=1,ne)
      write(2) (Lse(i),Lsce(i),Lsse(i),i=1,ne)
      write(2) ((Kcfp(j,i),Ksfp(j,i),j=0,nfs(i)),i=1,nf)
      write(2) ((lenfs(j,i),j=1,nfs(i)),i=1,nf)
      write(2) (((dufs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      write(2) (((ttfs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      write(2) (((tnfs(k,j,i),k=0,1),j=1,nfs(i)),i=1,nf)
      write(2) ((Kcfs(j,i),Ksfs(j,i),j=1,nfs(i)),i=1,nf)
      write(2) (Lcgp(i),Lccgp(i),Lcsgp(i),i=1,ngp)
      write(2) (Lsgp(i),Lscgp(i),Lssgp(i),i=1,ngp)
      write(2) ((Lcf(j,i),Lccf(j,i),Lcsf(j,i),j=0,nfs(i)),i=1,nf)
      write(2) ((Lsf(j,i),Lscf(j,i),Lssf(j,i),j=0,nfs(i)),i=1,nf)
      write(2) ((Lctj(j,i),Lcctj(j,i),Lcstj(j,i),j=1,2),i=1,ntj)
      write(2) ((Lstj(j,i),Lsctj(j,i),Lsstj(j,i),j=1,2),i=1,ntj)
      write(2) ((((ugprb(l,k,j,i),l=1,2),k=0,2),j=0,nrbs(i)),i=1,nrb)
      write(2) (((usrb(k,j,i),k=1,2),j=1,nrbs(i)),i=1,nrb)
      write(2) (((seou(k,j,i),k=1,2),j=1,2),i=1,nvo)
      write(2) (((seodu(k,j,i),k=1,nfoc(i)),j=1,nfoc(i)),i=1,nfo)
      write(2) (((seoe(k,j,i),k=1,neoc(i)),j=1,neoc(i)),i=1,neo)
      close(2)
c
      open(3,file='invert_input.bin',form='unformatted')
      write(3) nz,ny,ny0,ny1,nx,nx1,nx2,nx0
      write(3) (zmin(i),i=1,nz)
      write(3) (csqlen(i),i=1,nz)
      write(3) (x1maxz(i),i=1,nz)
      write(3) (x2maxz(i),i=1,nz)
      write(3) (x0maxz(i),i=1,nz)
      do i=1,nz
            write(3) (xz(j,i),j=0,x0maxz(i))
            write(3) (cz(j,i),j=0,x0maxz(i))
      end do
      write(3) (x2maxy(i),i=1,ny)
      write(3) (x0maxy(i),i=1,ny)
      do i=1,ny
            write(3) (xy(j,i),j=0,x0maxy(i))
            write(3) (ay(j,i),j=0,x0maxy(i))
      end do
      write(3) (z(i),i=1,nz)
      write(3) (y(i),i=1,ny)
      ix1=nx-nx0+1
      write(3) (x(i),i=ix1,nx)
      close(3)
c
      return
      end
c

