      SUBROUTINE invert()
c
c This is the prototype of a flexible class of inversion programs. At
c  present a single set of inputs is accepted. The input set consists of
c  constraint values z, normalised observations y, and a subset x0 of
c  variables with prescribed values.
c In an extended version there will be further sets of inputs consisting
c  of perturbations to the z, y and x0 values associated with global
c  variables, which will typically have wide-spread influence. There
c  will be a separate set of z, y and x0 values for each global
c  variable, and these values will be the partial derivatives with
c  respect to the global variable value.
c Strategies will be available for determining optimum values for global
c  variables, in conjunction with standard variables x, based on the
c  split in this program into apriori, purely dynamics-related solution
c  and hybrid aposteriori solution incorporating matching to
c  observational data.
c For purists the strategy of choice will be finding the global variable
c  values that minimise the mismatch of the observations to apriori
c  solutions, while adventurous users will try minimising the mismatch
c  to aposteriori solutions as well. Those achieve better match to the
c  observations at the expense of adding mismatch to the dynamics-like
c  apriori part of the solutions.
c
c The real virtue of aposteriori solutions is going beyond simply noting
c  there is mismatch between observations and input to apriori
c  solutions. In all solutions, apriori or aposteriori, everything is
c  interlinked. Rarely, if ever, will features needing correction to
c  remove mismatch to observations be restricted to the immediate
c  vicinity of the observations.
c In compromising the fit to the dynamics-like part of the problem to
c  improve the fit to observations, aposteriori solutions provide
c  indications of the magnitude and spatial extent of corrections
c  required to input to the apriori model. This applies irrespective of
c  whether there are unknown global variables in the apriori model,
c  though differences between aposteriori and apriori solutions will
c  often simply reflect where specification of global variables could be
c  improved.
c
c What is the difference between global variables and standard variables
c  x? Standard variables are the basic set of velocities, velocity
c  derivatives, slip rates and slip-rate derivatives at nodes in the
c  medium required to specify velocities in elements and slip rates
c  along faults. The nodes are either grid points or midpoints of
c  elements. Standard variable have influence only in the elements and
c  fault segments that the nodes belong to.
c Global variables can be any other quantities affecting some or all of
c  the z, y and x0 values, which in turn control the x-value solutions.
c Two primary examples are the following, both of which have the
c  advantage that the dependence of z, y and x0 on the global variable
c  values is linear and the global variable values can be determined by
c  straightforward linear inversion.
c
c The first example is rotation rates of rigid plates, which give z and
c  x0 values around the boundaries of the plates, and also influence z
c  and x0 values along velocity lines where velocity values are obtained
c  by interpolation of rigid boundary values, via solution to
c  corresponding 1D problems.
c A second class of global variables consists of parameters for force
c  distributions. Forces are specified through the spatial pattern of
c  stress-like potentials sxxpot, syypot and sxypot at grid points,
c  which combined with strain-rate capacities determine the observation
c  values y for the apriori part of the problem. They also influence z
c  and x0 values along velocity lines where velocity values are obtained
c  by interpolation.
c Global variables with nonlinear influence, such as distributions of
c  strain-rate and slip-rate capacities, are less well-suited to
c  straightforward inversion, but values can be inferred by trial and
c  error guided by comparisons of apriori and aposteriori solutions.
c In this inversion program global variables are required to have preset
c  values.
c
c The constraint values z and the observation values y are related to
c  the unknown x variables by the cz and ay matrices. After solving a
c  preliminary problem to redefine the apriori, dynamics-related y
c  values the contributions from the subset x0 of x variables with
c  prescribed values are subtracted from both z and y. The x0 variables
c  affect the apriori and aposteriori inversions in no other way, but
c  are required by output programs to reconstruct the solutions.
c The difference between constraints and observations is that
c  constraints are matched exactly, whereas observations are matched in
c  the least-squares sense.
c The remaining, unknown x variables are split into two classes x1 and
c  x2. The x1 variables have a one-to-one relationship with the
c  constraints (i.e. nx1 = nz), and are passive in the least-squares
c  part of the inversion, whereas the x2 variables are actual degrees of
c  freedom. The ordering of variables within the x vectors is x1 first,
c  x2 second, and x0 third. Aside from input of x0 values at the start
c  of the program and their output at the end, only x1 and x2 parts are
c  used in the body program.
c
c The observations y are also split into separate classes. The first, y0
c  part of the y vector consists of the observable-like inputs to the
c  apriori problem, with dynamics-style normalisation corresponding to
c  scaling stresses to be dimensionless and of order unity. The second,
c  y1 part consists of actual observables, normalised to have unit
c  standard errors and to be statistically independent.
c Only the y0 part is used in obtaining the apriori solution, at the
c  conclusion of which the y0 values are renormalised into being
c  statistical-like quantities with total sum of squares for the apriori
c  solution predictions equal to the nx2 degrees of freedom in the
c  model, where nx2 is the number of x2 variables.
c The x2 variables equate to effectively two independent components of
c  average strain rate per element, with the third component being given
c  by strain-rate compatibility, and the two components of average slip
c  rate per fault segment. In other words, the renormalisation of y0
c  values corresponds to taking the apriori solution to be derived from
c  a comprehensive set of potential slip-rate and strain-rate
c  observations, each being a fault-segment or element average.
c With the y0 part scaled from a statistical perspective, the complete y
c  vector is then used in obtaining the aposteriori solution.
c
c The naming convention on output fits in with the subdivision of the
c  y vector, but even so could be a little confusing. There are three
c  stages to the program, and the z and y vectors at each stage are
c  residuals.
c In the first stage a solution vector xc is constructed that accounts
c  for the constraints, effectively reducing z to zero. With the
c  variables scaled so that the y0 part of the ay matrix has uniform
c  magnitude, this vector is the smallest possible that eliminates z.
c  Both xc and what is left of z are output, with the latter
c  indicating any precision issues.
c The remaining parts of the apriori and aposteriori solutions belong
c  to a subspace of x vectors perpendicular to the cz matrix, and the
c  associated projection operator is constructed as part of the
c  constraint-handling process. A convenient set of vectors spanning the
c  subspace is obtained by applying the projection operator to x vectors
c  with non-zero x2 parts and zero x1 parts. There is one independent
c  vector corresponding to each x2 variable.
c The coefficients of these vectors in the apriori and aposteriori
c  solutions are computed by the conjugate gradient method in the second
c  and third stages of the program, using what remains of the y vector
c  after the contribution from xc has been removed.
c
c The apriori solution, based on the y0 part of y, is denoted on output
c  as x0. This is a complete x vector, with x1, x2 and x0 parts, and has
c  xc added in. The associated residual vector is denoted as y0, and is
c  also a complete y vector with y0 and y1 parts. The y0 part of this
c  vector retains the original dynamics-style scaling.
c There is an equivalent statistically scaled version of the same
c  complete y vector, denoted as y. This is the starting point for the
c  aposteriori solution, which the program computes as a perturbation
c  that it adds to the apriori solution with xc included.
c The resulting full aposteriori solution, which uses the y1 part of y
c  as well as the y0 part, is denoted on output as x1, and the
c  associated residual vector is denoted as y1. Again both are complete
c  vectors, with x1 having x1, x2 and x0 parts, and y1 having y0 and y1
c  parts.
c The sums of squares for the apriori and aposteriori solutions are
c  are also output.
c
      implicit none
      integer maxx,maxy,maxz,maxzsq,maxxy,maxxz
      parameter(maxx=200000,maxy=750000,maxz=20000,
     1      maxzsq=5000000,maxxy=35,maxxz=27)
      integer nx,ny,nz,nzsq,nx1,nx2,nx0,ny0,ny1
      integer ix1,iy1,iter,i,j1,jmax,j
      integer zmin(maxz),csqlen(maxz),x2maxy(maxy),x0maxy(maxy),
     1      x1maxz(maxz),x2maxz(maxz),x0maxz(maxz),
     2      xy(0:maxxy,maxy),xz(0:maxxz,maxz)
      real*8 s0,s,s2,alpha,s1,beta,sy0,si
      real*8 csq(maxzsq),ay(0:maxxy,maxy),cz(0:maxxz,maxz),
     1      y(maxy),z(maxz),w1(maxy),w2(maxx),xc(maxx),p(maxx),
     2      q(maxx),asqinv(maxx),x2(maxx),r(maxx),ra(maxx),
     3      x0(maxx),y0(maxy),x1(maxx),y1(maxy),xscale(maxx)
c
      open(1,file='invert_input.bin',form='unformatted')
      read(1) nz,ny,ny0,ny1,nx,nx1,nx2,nx0
      if ((nx.gt.maxx).or.(ny.gt.maxy).or.(nz.gt.maxz)) then
            write(*,*) 'nx,ny,nz,maxx,maxy,maxz=',
     1            nx,ny,nz,maxx,maxy,maxz
            stop 'Recompile with increased max[x/y/z] value or values'
      end if
      read(1) (zmin(i),i=1,nz)
      read(1) (csqlen(i),i=1,nz)
      if (csqlen(nz).gt.maxzsq) then
            write(*,*) 'csqlen(nz),maxzsq=',csqlen(nz),maxzsq
            stop 'Recompile with an increased maxzsq value'
      end if
      read(1) (x1maxz(i),i=1,nz)
      read(1) (x2maxz(i),i=1,nz)
      read(1) (x0maxz(i),i=1,nz)
      do i=1,nz
            read(1) (xz(j,i),j=0,x0maxz(i))
            read(1) (cz(j,i),j=0,x0maxz(i))
      end do
      read(1) (x2maxy(i),i=1,ny)
      read(1) (x0maxy(i),i=1,ny)
      do i=1,ny
            read(1) (xy(j,i),j=0,x0maxy(i))
            read(1) (ay(j,i),j=0,x0maxy(i))
      end do
      read(1) (z(i),i=1,nz)
      read(1) (y(i),i=1,ny)
      ix1=nx-nx0+1
      read(1) (x0(i),i=ix1,nx)
      do i=ix1,nx
            xc(i)=x0(i)
            x1(i)=x0(i)
      end do
c
      open(2,file='invert_output.bin',form='unformatted')
      write(2) nz,ny,ny0,ny1,nx,nx1,nx2,nx0
c
      nx=nx-nx0
      ix1=nx1+1
      iy1=ny0+1
c
c Scale x using the apriori solution part of ay
c
      call scalex(nx,ny,nz,ny0,maxxy,maxxz,x2maxy,x2maxz,
     1      xy,xz,ay,cz,xscale)
c
c Construct the Cholesky decomposition of the product of cz and its
c  matrix transpose, which is used in the projection of x vectors
c  perpendicular to cz and in obtaining the vector xc from z
c
      call getcsq(nz,nzsq,zmin,csqlen,csq,maxxz,x1maxz,x2maxz,
     1      xz,cz)
c
c Remove the contribution the x0 part of x makes to y and z
c
      do i=1,ny
            si=0.0d0
            j1=x2maxy(i)+1
            jmax=x0maxy(i)
            do j=j1,jmax
                  si=si+ay(j,i)*x0(xy(j,i))
            end do
            y(i)=y(i)-si
      end do
      do i=1,nz
            si=0.0d0
            j1=x2maxz(i)+1
            jmax=x0maxz(i)
            do j=j1,jmax
                  si=si+cz(j,i)*x0(xz(j,i))
            end do
            z(i)=z(i)-si
      end do
c
c Handle the constraints, obtaining the contribution xc that z makes to
c  x and removing the contribution xc makes to y and z
c
      call getxc(nx,ny,nz,nzsq,zmin,csqlen,csq,maxxy,maxxz,
     1      x2maxy,x2maxz,xy,xz,ay,cz,y,z,w1,xc)
c
c Apriori solution
c
      call scaler(nx,ny0,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,p,w1,w2,q,asqinv)
      call getx2a(nx,ny0,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,y,w1,w2,q)
      iter=0
      s0=0.0d0
      do i=ix1,nx
            x2(i)=0.0d0
            r(i)=q(i)
            ra(i)=asqinv(i)*r(i)
            s0=s0+r(i)*ra(i)
            p(i)=ra(i)
      end do
      s=s0
c
10    iter=iter+1
      call nextx2(nx,ny0,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,p,w1,w2,q)
      s2=0.0d0
      do i=ix1,nx
            s2=s2+p(i)*q(i)
      end do
      alpha=s/s2
      s1=0.0d0
      do i=ix1,nx
            x2(i)=x2(i)+alpha*p(i)
            r(i)=r(i)-alpha*q(i)
            ra(i)=asqinv(i)*r(i)
            s1=s1+r(i)*ra(i)
      end do
      write(*,*) 'iter,s1=',iter,s1
c      if (iter.eq.nx2) goto 20
      if ((iter.eq.nx2).or.(s1.lt.s0*1.0d-48)) goto 20
      beta=s1/s
      do i=ix1,nx
            p(i)=ra(i)+beta*p(i)
      end do
      s=s1
      goto 10
c
20    call x2tox(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,
     1      x1maxz,x2maxz,xz,cz,x2,w2,x0)
      call xtoy(nx,ny,maxxy,x2maxy,xy,ay,x0,y0)
      do i=1,nx
            x0(i)=xc(i)+x0(i)
      end do
      do i=1,ny
            y0(i)=y(i)-y0(i)
            y(i)=y0(i)
      end do
      sy0=0.0d0
      do i=1,ny0
            si=0.0d0
            jmax=x0maxy(i)
            do j=0,jmax
                  si=si+ay(j,i)*x0(xy(j,i))
            end do
            sy0=sy0+si**2
      end do
      s0=0.0d0
      do i=1,ny0
            s0=s0+y0(i)**2
      end do
      s1=0.0d0
      if (ny1.gt.0) then
            do i=iy1,ny
                  s1=s1+y0(i)**2
            end do
      end if
      write(*,*) 'In the apriori solution the total sum of squares'
      write(*,*) ' for the dynamics part is',sy0
      write(*,*) 'In preparation for the statistical aposteriori'
      write(*,*) ' solution, using observables as well as the'
      write(*,*) ' dynamics part, the dynamics is scaled so that'
      write(*,*) ' this sum of squares becomes the number of'
      write(*,*) ' degrees of freedom in the model',nx2
      write(*,*) 'The residual sum of squares in the dynamics part'
      write(*,*) ' of the apriori solution is',s0
      write(*,*) 'After the scaling this becomes',
     1      s0*dfloat(nx2)/sy0
      write(*,*) 'The number of actual observables is',ny1
      write(*,*) 'With the apriori solution the residual sum of'
      write(*,*) ' squares for (unused) actual observables is',s1
      write(*,*) 'Therefore, the total residual sum of squares'
      write(*,*) ' before aposteriori minimisation is',
     1      s1+s0*dfloat(nx2)/sy0
c
      write(2) sy0,s0,s1
c
c Aposteriori solution
c
      alpha=dsqrt(dfloat(nx2)/sy0)
      do i=1,ny0
            y(i)=alpha*y(i)
            jmax=x0maxy(i)
            do j=0,jmax
                  ay(j,i)=alpha*ay(j,i)
            end do
      end do
      if (ny1.eq.0) then
            do i=1,nx
                  x1(i)=x0(i)
            end do
            do i=1,ny
                  y1(i)=y(i)
            end do
            goto 50
      end if
      call scaler(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,p,w1,w2,q,asqinv)
      call getx2a(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,y,w1,w2,q)
      iter=0
      s0=0.0d0
      do i=ix1,nx
            x2(i)=0.0d0
            r(i)=q(i)
            ra(i)=asqinv(i)*r(i)
            s0=s0+r(i)*ra(i)
            p(i)=ra(i)
      end do
      s=s0
c
30    iter=iter+1
      call nextx2(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,maxxz,
     1      x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,p,w1,w2,q)
      s2=0.0d0
      do i=ix1,nx
            s2=s2+p(i)*q(i)
      end do
      alpha=s/s2
      s1=0.0d0
      do i=ix1,nx
            x2(i)=x2(i)+alpha*p(i)
            r(i)=r(i)-alpha*q(i)
            ra(i)=asqinv(i)*r(i)
            s1=s1+r(i)*ra(i)
      end do
      write(*,*) 'iter,s1=',iter,s1
c      if (iter.eq.nx2) goto 40
      if ((iter.eq.nx2).or.(s1.lt.s0*1.0d-48)) goto 40
      beta=s1/s
      do i=ix1,nx
            p(i)=ra(i)+beta*p(i)
      end do
      s=s1
      goto 30
c
40    call x2tox(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,
     1      x1maxz,x2maxz,xz,cz,x2,w2,x1)
      call xtoy(nx,ny,maxxy,x2maxy,xy,ay,x1,y1)
      do i=1,nx
            x1(i)=x0(i)+x1(i)
      end do
      do i=1,ny
            y1(i)=y(i)-y1(i)
      end do
50    sy0=0.0d0
      do i=1,ny0
            si=0.0d0
            jmax=x0maxy(i)
            do j=0,jmax
                  si=si+ay(j,i)*x1(xy(j,i))
            end do
            sy0=sy0+si**2
      end do
      s0=0.0d0
      do i=1,ny0
            s0=s0+y1(i)**2
      end do
      s1=0.0d0
      if (ny1.gt.0) then
            do i=iy1,ny
                  s1=s1+y1(i)**2
            end do
      end if
      write(*,*) 'In the aposteriori solution the total sum of'
      write(*,*) ' squares for the scaled dynamics part is',sy0
      write(*,*) 'In the aposteriori solution the residual sum of'
      write(*,*) ' squares for the scaled dynamics part increases'
      write(*,*) ' through inclusion of observables to',s0
      write(*,*) 'The number of actual observables is',ny1
      write(*,*) 'In the aposteriori solution the residual sum of'
      write(*,*) ' squares for the actual observables is',s1
      write(*,*) 'So the total residual sum of squares reduces'
      write(*,*) ' through inclusion of observables to',s0+s1
c
      write(2) sy0,s0,s1
c
      do i=1,nx
            xc(i)=xscale(i)*xc(i)
            x0(i)=xscale(i)*x0(i)
            x1(i)=xscale(i)*x1(i)
      end do
      nx=nx+nx0
c
      write(2) (xc(i),i=1,nx)
      write(2) (z(i),i=1,nz)
      write(2) (x0(i),i=1,nx)
      write(2) (y0(i),i=1,ny0)
      write(2) (y(i),i=1,ny)
      write(2) (x1(i),i=1,nx)
      write(2) (y1(i),i=1,ny)
c
      close(1)
      close(2)
      return
      end
c
c
      SUBROUTINE scalex(nx,ny,nz,ny0,maxxy,maxxz,x2maxy,x2maxz,
     1      xy,xz,ay,cz,xscale)
c
      implicit none
      integer nx,ny,nz,ny0,maxxy,maxxz
      integer i,jmax,j
      integer x2maxy(ny),x2maxz(nz),xy(0:maxxy,ny),xz(0:maxxz,nz)
      real*8 ay(0:maxxy,ny),cz(0:maxxz,nz),xscale(nx)
c
      do i=1,nx
            xscale(i)=0.0d0
      end do
      do i=1,ny0
            jmax=x2maxy(i)
            do j=0,jmax
                  xscale(xy(j,i))=xscale(xy(j,i))+ay(j,i)**2
            end do
      end do
      do i=1,nx
            xscale(i)=1.0d0/dsqrt(xscale(i))
      end do
      do i=1,ny
            jmax=x2maxy(i)
            do j=0,jmax
                  ay(j,i)=ay(j,i)*xscale(xy(j,i))
            end do
      end do
      do i=1,nz
            jmax=x2maxz(i)
            do j=0,jmax
                  cz(j,i)=cz(j,i)*xscale(xz(j,i))
            end do
      end do
      return
      end
c
c
      SUBROUTINE decomp(nz,nzsq,zmin,csqlen,csq)
c
      implicit none
      integer nz,nzsq
      integer i,ii,im,jmin,i0,j,ij,jm,kmin,j0,k,ik,jk,jj
      integer zmin(nz),csqlen(nz)
      real*8 si,sj
      real*8 csq(nzsq)
c
      do i=1,nz
            ii=csqlen(i)
            si=csq(ii)
            im=i-1
            jmin=zmin(i)
            if (jmin.le.im) then
                  i0=csqlen(i)-i
                  do j=jmin,im
                        ij=i0+j
                        sj=csq(ij)
                        jm=j-1
                        kmin=max0(zmin(j),jmin)
                        if (kmin.le.jm) then
                              j0=csqlen(j)-j
                              do k=kmin,jm
                                    ik=i0+k
                                    jk=j0+k
                                    sj=sj-csq(ik)*csq(jk)
                              end do
                        end if
                        jj=csqlen(j)
                        csq(ij)=sj/csq(jj)
                        si=si-csq(ij)**2
                  end do
            end if
            if (si.le.0.0d0) stop 'csq IS NOT POSITIVE DEFINITE'
            csq(ii)=dsqrt(si)
      end do
      return
      end
c
c
      SUBROUTINE getcsq(nz,nzsq,zmin,csqlen,csq,maxxz,
     1      x1maxz,x2maxz,xz,cz)
c
      implicit none
      integer nz,nzsq,maxxz
      integer i,jmin,i0,j,kmax,ki,kj,k1,ij
      integer zmin(nz),csqlen(nz),x1maxz(nz),x2maxz(nz),
     1      xz(0:maxxz,nz)
      real*8 sij
      real*8 csq(nzsq),cz(0:maxxz,nz)
c
      do i=1,nz
            jmin=zmin(i)
            i0=csqlen(i)-i
            do j=jmin,i
                  sij=0.0d0
                  if (xz(x1maxz(i),i).ge.xz(x1maxz(j),j)) then
                        kmax=x1maxz(j)
                        ki=0
                        do kj=0,kmax
10                            if (xz(ki,i).ge.xz(kj,j)) goto 20
                              ki=ki+1
                              goto 10
20                            if (xz(ki,i).eq.xz(kj,j))
     1                               sij=sij+cz(ki,i)*cz(kj,j)
                        end do
                  else
                        kmax=x1maxz(i)
                        kj=0
                        do ki=0,kmax
30                            if (xz(ki,i).le.xz(kj,j)) goto 40
                              kj=kj+1
                              goto 30
40                            if (xz(ki,i).eq.xz(kj,j))
     1                               sij=sij+cz(ki,i)*cz(kj,j)
                        end do
                  end if
                  if (xz(x2maxz(i),i).ge.xz(x2maxz(j),j)) then
                        k1=x1maxz(j)+1
                        kmax=x2maxz(j)
                        ki=x1maxz(i)+1
                        do kj=k1,kmax
50                            if (xz(ki,i).ge.xz(kj,j)) goto 60
                              ki=ki+1
                              goto 50
60                            if (xz(ki,i).eq.xz(kj,j))
     1                               sij=sij+cz(ki,i)*cz(kj,j)
                        end do
                  else
                        k1=x1maxz(i)+1
                        kmax=x2maxz(i)
                        kj=x1maxz(j)+1
                        do ki=k1,kmax
70                            if (xz(ki,i).le.xz(kj,j)) goto 80
                              kj=kj+1
                              goto 70
80                            if (xz(ki,i).eq.xz(kj,j))
     1                               sij=sij+cz(ki,i)*cz(kj,j)
                        end do
                  end if
                  ij=i0+j
                  csq(ij)=sij
            end do
      end do
      call decomp(nz,nzsq,zmin,csqlen,csq)
      return
      end
c
c
      SUBROUTINE csqinv(nz,nzsq,zmin,csqlen,csq,zin,z)
c
      implicit none
      integer nz,nzsq
      integer i,im,jmin,i0,j,ij,ii
      integer zmin(nz),csqlen(nz)
      real*8 si
      real*8 csq(nzsq),zin(nz),z(nz)
c
      do i=1,nz
            si=zin(i)
            im=i-1
            jmin=zmin(i)
            if (jmin.le.im) then
                  i0=csqlen(i)-i
                  do j=jmin,im
                        ij=i0+j
                        si=si-csq(ij)*z(j)
                  end do
            end if
            ii=csqlen(i)
            z(i)=si/csq(ii)
      end do
      do i=nz,1,-1
            ii=csqlen(i)
            z(i)=z(i)/csq(ii)
            im=i-1
            jmin=zmin(i)
            if (jmin.le.im) then
                  i0=csqlen(i)-i
                  do j=jmin,im
                        ij=i0+j
                        z(j)=z(j)-csq(ij)*z(i)
                  end do
            end if
      end do
      return
      end
c
c

      SUBROUTINE xtoy(nx,ny,maxxy,x2maxy,xy,ay,xin,y)

c
      implicit none
      integer nx,ny,maxxy
      integer i,jmax,j
      integer x2maxy(ny),xy(0:maxxy,ny)
      real*8 si
      real*8 ay(0:maxxy,ny),xin(nx),y(ny)
c
      do i=1,ny
            si=0.0d0
            jmax=x2maxy(i)
            do j=0,jmax
                  si=si+ay(j,i)*xin(xy(j,i))
            end do
            y(i)=si
      end do
      return
      end
c
c
      SUBROUTINE ytox(nx,ny,maxxy,x2maxy,xy,ay,yin,x)
c
      implicit none
      integer nx,ny,maxxy
      integer i,jmax,j
      integer x2maxy(ny),xy(0:maxxy,ny)
      real*8 ay(0:maxxy,ny),yin(ny),x(nx)
c
      do i=1,nx
            x(i)=0.0d0
      end do
      do i=1,ny
            jmax=x2maxy(i)
            do j=0,jmax
                  x(xy(j,i))=x(xy(j,i))+ay(j,i)*yin(i)
            end do
      end do
      return
      end
c
c

      SUBROUTINE xtoz(nx,nz,maxxz,x2maxz,xz,cz,xin,z)
c
      implicit none
      integer nx,nz,maxxz
      integer i,jmax,j
      integer x2maxz(nz),xz(0:maxxz,nz)
      real*8 si
      real*8 cz(0:maxxz,nz),xin(nx),z(nz)
c
      do i=1,nz
            si=0.0d0
            jmax=x2maxz(i)
            do j=0,jmax
                  si=si+cz(j,i)*xin(xz(j,i))
            end do
            z(i)=si
      end do
      return
      end
c
c
      SUBROUTINE ztox(nx,nz,maxxz,x2maxz,xz,cz,zin,x)
c
      implicit none
      integer nx,nz,maxxz
      integer i,jmax,j
      integer x2maxz(nz),xz(0:maxxz,nz)
      real*8 cz(0:maxxz,nz),zin(nz),x(nx)
c
      do i=1,nx
            x(i)=0.0d0
      end do
      do i=1,nz
            jmax=x2maxz(i)
            do j=0,jmax
                  x(xz(j,i))=x(xz(j,i))+cz(j,i)*zin(i)
            end do
      end do
      return
      end
c
c
      SUBROUTINE x2toz(nx,nz,maxxz,x1maxz,x2maxz,xz,cz,x2in,z)
c
      implicit none
      integer nx,nz,maxxz
      integer i,j1,jmax,j
      integer x1maxz(nz),x2maxz(nz),xz(0:maxxz,nz)
      real*8 si
      real*8 cz(0:maxxz,nz),x2in(nx),z(nz)
c
      do i=1,nz
            si=0.0d0
            j1=x1maxz(i)+1
            jmax=x2maxz(i)
            do j=j1,jmax
                  si=si+cz(j,i)*x2in(xz(j,i))
            end do
            z(i)=si
      end do
      return
      end
c
c
      SUBROUTINE ztox2(nx,nz,maxxz,x1maxz,x2maxz,xz,cz,zin,x2)
c
      implicit none
      integer nx,nz,maxxz
      integer i,j1,jmax,j
      integer x1maxz(nz),x2maxz(nz),xz(0:maxxz,nz)
      real*8 cz(0:maxxz,nz),zin(nz),x2(nx)
c
      do i=1,nx
            x2(i)=0.0d0
      end do
      do i=1,nz
            j1=x1maxz(i)+1
            jmax=x2maxz(i)
            do j=j1,jmax
                  x2(xz(j,i))=x2(xz(j,i))+cz(j,i)*zin(i)
            end do
      end do
      return
      end
c
c
      SUBROUTINE x2tox(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,
     1      x1maxz,x2maxz,xz,cz,x2in,w,x)
c
      implicit none
      integer nx,nz,nzsq,nx1,maxxz
      integer i,ix1
      integer zmin(nz),csqlen(nz),x1maxz(nz),x2maxz(nz),
     1      xz(0:maxxz,nz)
      real*8 csq(nzsq),cz(0:maxxz,nz),x2in(nx),w(nz),x(nx)
c
      call x2toz(nx,nz,maxxz,x1maxz,x2maxz,xz,cz,x2in,x)
      call csqinv(nz,nzsq,zmin,csqlen,csq,x,w)
      call ztox(nx,nz,maxxz,x2maxz,xz,cz,w,x)
      do i=1,nx1
            x(i)=-x(i)
      end do
      ix1=nx1+1
      do i=ix1,nx
            x(i)=x2in(i)-x(i)
      end do
      return
      end
c
c
      SUBROUTINE xtox2(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,
     1      x1maxz,x2maxz,xz,cz,xin,w,x2)
c
      implicit none
      integer nx,nz,nzsq,nx1,maxxz
      integer ix1,i
      integer zmin(nz),csqlen(nz),x1maxz(nz),x2maxz(nz),
     1      xz(0:maxxz,nz)
      real*8 csq(nzsq),cz(0:maxxz,nz),xin(nx),w(nz),x2(nx)
c
      call xtoz(nx,nz,maxxz,x2maxz,xz,cz,xin,x2)
      call csqinv(nz,nzsq,zmin,csqlen,csq,x2,w)
      call ztox2(nx,nz,maxxz,x1maxz,x2maxz,xz,cz,w,x2)
      ix1=nx1+1
      do i=ix1,nx
            x2(i)=xin(i)-x2(i)
      end do
      return
      end
c
c
      SUBROUTINE getxc(nx,ny,nz,nzsq,zmin,csqlen,csq,maxxy,maxxz,
     1      x2maxy,x2maxz,xy,xz,ay,cz,y,z,w,xc)
c
      implicit none
      integer nx,ny,nz,nzsq,maxxy,maxxz
      integer i
      integer zmin(nz),csqlen(nz),x2maxy(ny),x2maxz(nz),
     1      xy(0:maxxy,ny),xz(0:maxxz,nz)
      real*8 csq(nzsq),ay(0:maxxy,ny),cz(0:maxxz,nz),y(ny),z(nz),
     1      w(ny),xc(nx)
c
      call csqinv(nz,nzsq,zmin,csqlen,csq,z,w)
      call ztox(nx,nz,maxxz,x2maxz,xz,cz,w,xc)
      call xtoy(nx,ny,maxxy,x2maxy,xy,ay,xc,w)
      do i=1,ny
            y(i)=y(i)-w(i)
      end do
      call xtoz(nx,nz,maxxz,x2maxz,xz,cz,xc,w)
      do i=1,nz
            z(i)=z(i)-w(i)
      end do
      return
      end
c
c
      SUBROUTINE nextx2(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,
     1      maxxz,x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,x2in,w1,w2,x2)
c
      implicit none
      integer nx,ny,nz,nzsq,nx1,maxxy,maxxz
      integer zmin(nz),csqlen(nz),x2maxy(ny),x1maxz(nz),x2maxz(nz),
     1      xy(0:maxxy,ny),xz(0:maxxz,nz)
      real*8 csq(nzsq),ay(0:maxxy,ny),cz(0:maxxz,nz),x2in(nx),
     1      w1(ny),w2(nx),x2(nx)
c
      call x2tox(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,x1maxz,x2maxz,
     1      xz,cz,x2in,w1,w2)
      call xtoy(nx,ny,maxxy,x2maxy,xy,ay,w2,w1)
      call ytox(nx,ny,maxxy,x2maxy,xy,ay,w1,w2)
      call xtox2(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,x1maxz,x2maxz,
     1      xz,cz,w2,w1,x2)
      return
      end
c
c
      SUBROUTINE scaler(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,
     1      maxxz,x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,x2in,w1,w2,x2a,
     2      asqinv)
c
      implicit none
      integer nx,ny,nz,nzsq,nx1,maxxy,maxxz
      integer ix1,i
      integer zmin(nz),csqlen(nz),x2maxy(ny),x1maxz(nz),x2maxz(nz),
     1      xy(0:maxxy,ny),xz(0:maxxz,nz)
      real*8 csq(nzsq),ay(0:maxxy,ny),cz(0:maxxz,nz),x2in(nx),
     1      w1(ny),w2(nx),x2a(nx),asqinv(nx)
c
      ix1=nx1+1
      do i=ix1,nx
            x2in(i)=0.0d0
      end do
      do i=ix1,nx
            x2in(i)=1.0d0
            call nextx2(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,
     1            maxxz,x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,x2in,w1,
     2            w2,x2a)
            asqinv(i)=1.0d0/x2a(i)
            x2in(i)=0.0d0
      end do
      return
      end
c
c
      SUBROUTINE getx2a(nx,ny,nz,nzsq,zmin,csqlen,csq,nx1,maxxy,
     1      maxxz,x2maxy,x1maxz,x2maxz,xy,xz,ay,cz,y,w1,w2,x2a)
c
      implicit none
      integer nx,ny,nz,nzsq,nx1,maxxy,maxxz
      integer zmin(nz),csqlen(nz),x2maxy(ny),x1maxz(nz),x2maxz(nz),
     1      xy(0:maxxy,ny),xz(0:maxxz,nz)
      real*8 csq(nzsq),ay(0:maxxy,ny),cz(0:maxxz,nz),y(ny),
     1      w1(nx),w2(nz),x2a(nx)
c
      call ytox(nx,ny,maxxy,x2maxy,xy,ay,y,w1)
      call xtox2(nx,nz,nzsq,zmin,csqlen,csq,nx1,maxxz,x1maxz,x2maxz,
     1      xz,cz,w1,w2,x2a)
      return
      end
c

