!!$************************* WaveTransSpinor *******************************
!!$
!!$   input the WAVECAR file in binary format from VASP, and write
!!$   selected real space wavefunction in a3 direction to standard output
!!$   Plane wave coefficients are written to GCOEFF.txt
!!$
!!$   Compile with gfortran or ifort. Flag "-assume byterecl" is required
!!$   for ifort.
!!$
!!$   version 2.0 - Jul 11, 2012 - R. M. Feenstra and M. Widom, 
!!$                                using updated 'c' value
!!$   version 2.1 - Sep 17, 2014 - changed estimator for max. no. of
!!$                                plane waves
!!$   version 2.2 - Nov 12, 2014 - output (n1,n2,n3) indices in both first half
!!$                                and second half of GCOEFF.txt output file
!!$
!!$   options are -f filename -k k-point -b band -x coord -y coord
!!$   defaults are -f WAVECAR -k 1 -b 1 -x 0 -y 0
!!$   coordinates are direct coordinates

subroutine WAVECAR_INFO( filename, fnameLength, nrecl, nspin, nprec, nwk, nband, ecut, npmax, nb1max, nb2max, nb3max, a1mag, a2mag, a3mag, a,b, ierr)
implicit real*8 (a-h, o-z)
complex*8, allocatable :: coeff(:)
complex*16, allocatable :: cener(:)
integer, allocatable :: igall(:,:)
dimension a1(3),a2(3),a3(3),b1(3),b2(3),b3(3),a2xa3(3),sumkg(3),vtmp(3)
real*8 a(3,3), b(3,3)
dimension wk(3),xyz(3),wkpg(3),ig(3)
complex*16 csum1,csum2
integer kpoint,band
character*16394 filename
integer fnameLength
real*8  a1mag,a2mag,a3mag

!!$*   constant 'c' below is 2m/hbar**2 in units of 1/eV Ang^2 (value is
!!$*   adjusted in final decimal places to agree with VASP value; program
!!$*   checks for discrepancy of any results between this and VASP values)

data c/0.262465831d0/
pi=4.*atan(1.)

ierr =0
!!$*   input


nrecl=24
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
write(0,*) 'open error - iostat =',iost
ierr=-1
return
endif

read(unit=10,rec=1) xnrecl,xnspin,xnprec
close(unit=10)
nrecl=nint(xnrecl)
nspin=nint(xnspin)
nprec=nint(xnprec)
if(nprec.eq.45210) then
   write(0,*) '*** error - WAVECAR_double requires complex*16'
   ierr = -2
   return
endif
if(nspin.eq.2) then
   write(0,*) '*** error - Not a spinor WAVECAR. ISPIN =',nspin
   ierr = -3
   return
endif
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
   write(0,*) 'open error - iostat =',iost
   ierr = -1
   return
endif
read(unit=10,rec=2) xnwk,xnband,ecut,(a1(j),j=1,3),(a2(j),j=1,3), &
     (a3(j),j=1,3)
nwk=nint(xnwk)
nband=nint(xnband)
close(unit=10)
call vcross(a2xa3,a2,a3)
Vcell=dot_product(a1,a2xa3)
a1mag=dsqrt(dot_product(a1,a1))
a2mag=dsqrt(dot_product(a2,a2))
a3mag=dsqrt(dot_product(a3,a3))
call vcross(b1,a2,a3)
call vcross(b2,a3,a1)
call vcross(b3,a1,a2)
   b1=2.*pi*b1/Vcell
   b2=2.*pi*b2/Vcell
   b3=2.*pi*b3/Vcell
b1mag=dsqrt(b1(1)**2+b1(2)**2+b1(3)**2)
b2mag=dsqrt(b2(1)**2+b2(2)**2+b2(3)**2)
b3mag=dsqrt(b3(1)**2+b3(2)**2+b3(3)**2)

phi12=acos((b1(1)*b2(1)+b1(2)*b2(2)+b1(3)*b2(3))/(b1mag*b2mag))
call vcross(vtmp,b1,b2)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b3(1)*vtmp(1)+b3(2)*vtmp(2)+b3(3)*vtmp(3))/(vmag*b3mag)
nb1maxA=(dsqrt(ecut*c)/(b1mag*abs(sin(phi12))))+1
nb2maxA=(dsqrt(ecut*c)/(b2mag*abs(sin(phi12))))+1
nb3maxA=(dsqrt(ecut*c)/(b3mag*abs(sinphi123)))+1
npmaxA=nint(4.*pi*nb1maxA*nb2maxA*nb3maxA/3.)

phi13=acos((b1(1)*b3(1)+b1(2)*b3(2)+b1(3)*b3(3))/(b1mag*b3mag))
call vcross(vtmp,b1,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b2(1)*vtmp(1)+b2(2)*vtmp(2)+b2(3)*vtmp(3))/(vmag*b2mag)
phi123=abs(asin(sinphi123))
nb1maxB=(dsqrt(ecut*c)/(b1mag*abs(sin(phi13))))+1
nb2maxB=(dsqrt(ecut*c)/(b2mag*abs(sinphi123)))+1
nb3maxB=(dsqrt(ecut*c)/(b3mag*abs(sin(phi13))))+1
npmaxB=nint(4.*pi*nb1maxB*nb2maxB*nb3maxB/3.)

phi23=acos((b2(1)*b3(1)+b2(2)*b3(2)+b2(3)*b3(3))/(b2mag*b3mag))
call vcross(vtmp,b2,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b1(1)*vtmp(1)+b1(2)*vtmp(2)+b1(3)*vtmp(3))/(vmag*b1mag)
phi123=abs(asin(sinphi123))
nb1maxC=(dsqrt(ecut*c)/(b1mag*abs(sinphi123)))+1
nb2maxC=(dsqrt(ecut*c)/(b2mag*abs(sin(phi23))))+1
nb3maxC=(dsqrt(ecut*c)/(b3mag*abs(sin(phi23))))+1
npmaxC=nint(4.*pi*nb1maxC*nb2maxC*nb3maxC/3.)

nb1max=max0(nb1maxA,nb1maxB,nb1maxC)
nb2max=max0(nb2maxA,nb2maxB,nb2maxC)
nb3max=max0(nb3maxA,nb3maxB,nb3maxC)
!! 2* to handle two component spinors
npmax=2*min0(npmaxA,npmaxB,npmaxC)

do i=1,3
 a(i,1)=a1(i)
 a(i,2)=a2(i)
 a(i,3)=a3(i)
 b(i,1)=b1(i)
 b(i,2)=b2(i)
 b(i,3)=b3(i)
enddo

return
end




subroutine READ_WAVECAR_LDOS( filename, fnameLength, xyz, energy,occ_full,nFi, nHi,ierr)

!implicit real*8 (a-h, o-z)
implicit none
complex*8, allocatable :: coeff(:)
complex*16, allocatable :: cener(:)
real*8, allocatable ::  occ(:)
integer, allocatable :: igall(:,:)
real*8  a1(3),a2(3),a3(3),b1(3),b2(3),b3(3),a2xa3(3),sumkg(3),vtmp(3)
real*8 wk(3),xyz(3),wkpg(3)
integer ig(3)
complex*16 csum1,csum2
integer kpoint,band
character*16394 filename
real*8 nFi(*), nHi(*), z, energy(*),occ_full(*)
integer fnameLength
complex*8, allocatable :: expArray(:)
complex*8 c_dot_product
integer nplaneMax
real *8 c,pi, xnrecl, xnspin, xnprec,xnwk, xnband, ecut,vcell,a3mag
real *8 b1mag, b2mag, b3mag, phi12, vmag, sinphi123, phi13, phi123
real*8 phi23,xnplane, etot, gtot
integer nband, ierr,j,nrecl, iost, nspin, nprec, nb1max, nb2max, nb3max 
integer nwk, nb1maxa, nb2maxa, nb3maxa, nb1maxb, nb2maxb, nb3maxb
integer nb1maxc, nb2maxc, nb3maxc, npmaxa, npmaxb, npmaxc, npmax, irec, irec0
integer nplane, i, iband, jj, ncnt, ig1, ig1p, ig2, ig2p, ig3, ig3p, iplane
complex*16 S1,S2
!!$*   constant 'c' below is 2m/hbar**2 in units of 1/eV Ang^2 (value is
!!$*   adjusted in final decimal places to agree with VASP value; program
!!$*   checks for discrepancy of any results between this and VA:SP values)

data c/0.262465831d0/ 
pi=4.*atan(1.)
ierr=0

kpoint=1

!!$ parse arguments
!xyz(1)=x
!xyz(2)=y
!xyz(3)=z
!!$*   input

nrecl=24
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
  ierr = -1
  write(6,*) 'open error - iostat =',iost
  return
endif            
read(unit=10,rec=1) xnrecl,xnspin,xnprec
close(unit=10)
nrecl=nint(xnrecl)
nspin=nint(xnspin)
nprec=nint(xnprec)
if(nprec.eq.45210) then
   write(6,*) '*** error - WAVECAR_double requires complex*16'
   ierr = -2
   return
endif
if(nspin.eq.2) then
   write(6,*) '*** error - Not a spinor WAVECAR. ISPIN =',nspin
   ierr = -3
   return
endif
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
 write(6,*) 'open error - iostat =',iost
 ierr = -1
 return
endif
read(unit=10,rec=2) xnwk,xnband,ecut,(a1(j),j=1,3),(a2(j),j=1,3), &
     (a3(j),j=1,3)
nwk=nint(xnwk)
nband=nint(xnband)

allocate(cener(nband))
allocate(occ(nband))

!!$*   compute reciprocal properties

call vcross(a2xa3,a2,a3)
Vcell=dot_product(a1,a2xa3)
a3mag=dsqrt(dot_product(a3,a3))
call vcross(b1,a2,a3)
call vcross(b2,a3,a1)
call vcross(b3,a1,a2)
   b1=2.*pi*b1/Vcell
   b2=2.*pi*b2/Vcell
   b3=2.*pi*b3/Vcell
b1mag=dsqrt(b1(1)**2+b1(2)**2+b1(3)**2)
b2mag=dsqrt(b2(1)**2+b2(2)**2+b2(3)**2)
b3mag=dsqrt(b3(1)**2+b3(2)**2+b3(3)**2)

phi12=acos((b1(1)*b2(1)+b1(2)*b2(2)+b1(3)*b2(3))/(b1mag*b2mag))
call vcross(vtmp,b1,b2)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b3(1)*vtmp(1)+b3(2)*vtmp(2)+b3(3)*vtmp(3))/(vmag*b3mag)
nb1maxA=(dsqrt(ecut*c)/(b1mag*abs(sin(phi12))))+1
nb2maxA=(dsqrt(ecut*c)/(b2mag*abs(sin(phi12))))+1
nb3maxA=(dsqrt(ecut*c)/(b3mag*abs(sinphi123)))+1
npmaxA=nint(4.*pi*nb1maxA*nb2maxA*nb3maxA/3.)
      
phi13=acos((b1(1)*b3(1)+b1(2)*b3(2)+b1(3)*b3(3))/(b1mag*b3mag))
call vcross(vtmp,b1,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b2(1)*vtmp(1)+b2(2)*vtmp(2)+b2(3)*vtmp(3))/(vmag*b2mag)
phi123=abs(asin(sinphi123))
nb1maxB=(dsqrt(ecut*c)/(b1mag*abs(sin(phi13))))+1
nb2maxB=(dsqrt(ecut*c)/(b2mag*abs(sinphi123)))+1
nb3maxB=(dsqrt(ecut*c)/(b3mag*abs(sin(phi13))))+1
npmaxB=nint(4.*pi*nb1maxB*nb2maxB*nb3maxB/3.)
      
phi23=acos((b2(1)*b3(1)+b2(2)*b3(2)+b2(3)*b3(3))/(b2mag*b3mag))
call vcross(vtmp,b2,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b1(1)*vtmp(1)+b1(2)*vtmp(2)+b1(3)*vtmp(3))/(vmag*b1mag)
phi123=abs(asin(sinphi123))
nb1maxC=(dsqrt(ecut*c)/(b1mag*abs(sinphi123)))+1
nb2maxC=(dsqrt(ecut*c)/(b2mag*abs(sin(phi23))))+1
nb3maxC=(dsqrt(ecut*c)/(b3mag*abs(sin(phi23))))+1 
npmaxC=nint(4.*pi*nb1maxC*nb2maxC*nb3maxC/3.)

nb1max=max0(nb1maxA,nb1maxB,nb1maxC)
nb2max=max0(nb2maxA,nb2maxB,nb2maxC)
nb3max=max0(nb3maxA,nb3maxB,nb3maxC)
!! 2* to handle two component spinors
npmax=2*min0(npmaxA,npmaxB,npmaxC)

print*, 'npmax=', npmax

allocate (igall(3,npmax))
allocate (coeff(npmax))
allocate(expArray(npmax/2+1))

print*,'nwk=', nwk

do 2000 kpoint=1,nwk
print*,'kpoint=', kpoint
!!$*   Find desired wavefunction
irec=3+(kpoint-1)*(nband+1)
read(unit=10,rec=irec) xnplane,(wk(i),i=1,3), &
     (cener(iband),occ(iband),iband=1,nband)
nplane=nint(xnplane)


do jj=1,nband
 energy(jj+(kpoint-1)*nband)=real(cener(jj))
 if(dabs(occ(jj)).gt. 1.e-3) then
  occ_full(jj+(kpoint-1)*nband)=occ(jj)
 else
  occ_full(jj+(kpoint-1)*nband)=0.d0
 endif
enddo
      
!!$*   Calculate plane waves
ncnt=0
do ig3=0,2*nb3max
   ig3p=ig3
   if (ig3.gt.nb3max) ig3p=ig3-2*nb3max-1
   do ig2=0,2*nb2max
      ig2p=ig2
      if (ig2.gt.nb2max) ig2p=ig2-2*nb2max-1
      do ig1=0,2*nb1max
         ig1p=ig1
         if (ig1.gt.nb1max) ig1p=ig1-2*nb1max-1
         do j=1,3
            sumkg(j)=(wk(1)+ig1p)*b1(j)+ &
                 (wk(2)+ig2p)*b2(j)+(wk(3)+ig3p)*b3(j)
         enddo
         gtot=sqrt(dot_product(sumkg,sumkg))
         etot=gtot**2/c
         if (etot.lt.ecut) then
            ncnt=ncnt+1
            igall(1,ncnt)=ig1p
            igall(2,ncnt)=ig2p
            igall(3,ncnt)=ig3p
         end if
      enddo
   enddo
enddo
if (2*ncnt.ne.nplane) then
   write(0,*) '*** error - computed 2*ncnt=',2*ncnt, &
        ' != input nplane=',nplane
   ierr = -10
   close(unit=10)
   deallocate(igall)
   deallocate(coeff)
   deallocate(cener)
   deallocate(occ)
   deallocate(expArray)
   return
endif
print *,'ncnt=',ncnt

do iplane=1,ncnt
 ig=igall(:,iplane)
 wkpg=wk+ig
 expArray(iplane)=2.*pi*cmplx(0.,-1.)*dot_product(wkpg,xyz)
enddo
call vcexp( ncnt, expArray, expArray )
! print*,'expArray: ', c_dot_product(ncnt,expArray,expArray)
irec0=irec
do 1000 band=1,nband
! print*,'band: ', band
irec=irec0+band
read(unit=10,rec=irec) (coeff(iplane), iplane=1,nplane)

csum1 = c_dot_product(ncnt,expArray,coeff)
csum2=c_dot_product(ncnt,expArray,coeff(ncnt+1))
!   print*, 'csum2=', csum2
nFi(band+(kpoint-1)*nband)=csum1*conjg(csum1)/Vcell
nHi(band+(kpoint-1)*nband)=csum2*conjg(csum2)/Vcell

!!$ output z*a3mag for units of Angstroms

1000 continue

2000 continue


close(unit=10)

deallocate(igall)
deallocate(coeff)
deallocate(cener)
deallocate(occ)
deallocate(expArray)
return
end


subroutine READ_WAVECAR_BANDS_LDOS( filename, fnameLength, coords, nCoords, bands, nBands, &
 kPoints, nKpoints, energy, lDOS, lSigmaX, lSigmaY, lSigmaZ, ierr)

implicit none
real*8 coords(*), kPoints(*), lDOS(*), lSigmaX(*), lSigmaY(*), lSigmaZ(*)
integer nCoords, bands(*), nBands, nKpoints
complex*8, allocatable :: coeff(:)
complex*16, allocatable :: cener(:)
real*8, allocatable ::  occ(:)
integer, allocatable :: igall(:,:)
real*8  a1(3),a2(3),a3(3),b1(3),b2(3),b3(3),a2xa3(3),sumkg(3),vtmp(3)
real*8 wk(3),xyz(3),wkpg(3)
integer ig(3)
complex*16 csum1,csum2
integer kpoint,band
character*16394 filename
real*8  z, energy(*)
integer fnameLength
complex*8, allocatable :: expArray(:)
complex*8 c_dot_product
real*8 d_dot_product
integer nplaneMax
real *8 c,pi, xnrecl, xnspin, xnprec,xnwk, xnband, ecut,vcell,a3mag
real *8 b1mag, b2mag, b3mag, phi12, vmag, sinphi123, phi13, phi123
real*8 phi23,xnplane, etot, gtot
integer nband, ierr,j,nrecl, iost, nspin, nprec, nb1max, nb2max, nb3max 
integer nwk, nb1maxa, nb2maxa, nb3maxa, nb1maxb, nb2maxb, nb3maxb
integer nb1maxc, nb2maxc, nb3maxc, npmaxa, npmaxb, npmaxc, npmax, irec, irec0
integer nplane, i, iband, jj, ncnt, ig1, ig1p, ig2, ig2p, ig3, ig3p, iplane, icoord
complex*16 S1,S2
integer*8 i8Tmp
!!$*   constant 'c' below is 2m/hbar**2 in units of 1/eV Ang^2 (value is
!!$*   adjusted in final decimal places to agree with VASP value; program
!!$*   checks for discrepancy of any results between this and VASP values)

data c/0.262465831d0/ 
pi=4.*atan(1.)
ierr=0

kpoint=1

!!$ parse arguments
!xyz(1)=x
!xyz(2)=y
!xyz(3)=z
!!$*   input

nrecl=24
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
  ierr = -1
  write(6,*) 'open error - iostat =',iost
  return
endif            
read(unit=10,rec=1) xnrecl,xnspin,xnprec
close(unit=10)
nrecl=nint(xnrecl)
nspin=nint(xnspin)
nprec=nint(xnprec)
if(nprec.eq.45210) then
   write(6,*) '*** error - WAVECAR_double requires complex*16'
   ierr = -2
   return
endif
if(nspin.eq.2) then
   write(6,*) '*** error - Not a spinor WAVECAR. ISPIN =',nspin
   ierr = -3
   return
endif
open(unit=10,file=filename(1:fnameLength),access='direct',recl=nrecl, &
     iostat=iost,status='old')
if (iost.ne.0) then
 write(6,*) 'open error - iostat =',iost
 ierr = -1
 return
endif
read(unit=10,rec=2) xnwk,xnband,ecut,(a1(j),j=1,3),(a2(j),j=1,3), &
     (a3(j),j=1,3)
nwk=nint(xnwk)
if (nKpoints.lt.nwk) then
 write(6,*) 'nKpoints (',nKpoints,') < nwk (',nwk,')'
 ierr = -1
 return
endif

nband=nint(xnband)
jj=0
do i=1, nBands
 if ((bands(i).le.0).or.(bands(i).gt.nband)) then
  write(6,*) 'Wrong Band number ',i,': ', bands(i)
  jj=jj+1
 endif
enddo
if(jj.gt.0) then
 ierr = -1
 return
endif

allocate(cener(nband))
allocate(occ(nband))

!!$*   compute reciprocal properties

call vcross(a2xa3,a2,a3)
Vcell=dot_product(a1,a2xa3)
a3mag=dsqrt(dot_product(a3,a3))
call vcross(b1,a2,a3)
call vcross(b2,a3,a1)
call vcross(b3,a1,a2)
   b1=2.*pi*b1/Vcell
   b2=2.*pi*b2/Vcell
   b3=2.*pi*b3/Vcell
b1mag=dsqrt(b1(1)**2+b1(2)**2+b1(3)**2)
b2mag=dsqrt(b2(1)**2+b2(2)**2+b2(3)**2)
b3mag=dsqrt(b3(1)**2+b3(2)**2+b3(3)**2)

phi12=acos((b1(1)*b2(1)+b1(2)*b2(2)+b1(3)*b2(3))/(b1mag*b2mag))
call vcross(vtmp,b1,b2)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b3(1)*vtmp(1)+b3(2)*vtmp(2)+b3(3)*vtmp(3))/(vmag*b3mag)
nb1maxA=(dsqrt(ecut*c)/(b1mag*abs(sin(phi12))))+1
nb2maxA=(dsqrt(ecut*c)/(b2mag*abs(sin(phi12))))+1
nb3maxA=(dsqrt(ecut*c)/(b3mag*abs(sinphi123)))+1
npmaxA=nint(4.*pi*nb1maxA*nb2maxA*nb3maxA/3.)
      
phi13=acos((b1(1)*b3(1)+b1(2)*b3(2)+b1(3)*b3(3))/(b1mag*b3mag))
call vcross(vtmp,b1,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b2(1)*vtmp(1)+b2(2)*vtmp(2)+b2(3)*vtmp(3))/(vmag*b2mag)
phi123=abs(asin(sinphi123))
nb1maxB=(dsqrt(ecut*c)/(b1mag*abs(sin(phi13))))+1
nb2maxB=(dsqrt(ecut*c)/(b2mag*abs(sinphi123)))+1
nb3maxB=(dsqrt(ecut*c)/(b3mag*abs(sin(phi13))))+1
npmaxB=nint(4.*pi*nb1maxB*nb2maxB*nb3maxB/3.)
      
phi23=acos((b2(1)*b3(1)+b2(2)*b3(2)+b2(3)*b3(3))/(b2mag*b3mag))
call vcross(vtmp,b2,b3)
vmag=dsqrt(vtmp(1)**2+vtmp(2)**2+vtmp(3)**2)
sinphi123=(b1(1)*vtmp(1)+b1(2)*vtmp(2)+b1(3)*vtmp(3))/(vmag*b1mag)
phi123=abs(asin(sinphi123))
nb1maxC=(dsqrt(ecut*c)/(b1mag*abs(sinphi123)))+1
nb2maxC=(dsqrt(ecut*c)/(b2mag*abs(sin(phi23))))+1
nb3maxC=(dsqrt(ecut*c)/(b3mag*abs(sin(phi23))))+1 
npmaxC=nint(4.*pi*nb1maxC*nb2maxC*nb3maxC/3.)

nb1max=max0(nb1maxA,nb1maxB,nb1maxC)
nb2max=max0(nb2maxA,nb2maxB,nb2maxC)
nb3max=max0(nb3maxA,nb3maxB,nb3maxC)
!! 2* to handle two component spinors
npmax=2*min0(npmaxA,npmaxB,npmaxC)

print*, 'npmax=', npmax

allocate (igall(3,npmax))
allocate (coeff(npmax))
allocate(expArray((npmax/2+1)*int8(nCoords)))

print*,'nwk=', nwk

do 2000 kpoint=1,nwk
print*,'kpoint=', kpoint
!!$*   Find desired wavefunction
irec=3+(kpoint-1)*(nband+1)
read(unit=10,rec=irec) xnplane,(wk(i),i=1,3), &
     (cener(iband),occ(iband),iband=1,nband)
nplane=nint(xnplane)
do i=1,3
 kPoints(3*(kpoint-1)+i)=wk(i)
enddo

do jj=1,nBands
 energy((jj-1)*nwk+kpoint)=real(cener(Bands(jj)))
enddo
      
!!$*   Calculate plane waves
ncnt=0
do ig3=0,2*nb3max
   ig3p=ig3
   if (ig3.gt.nb3max) ig3p=ig3-2*nb3max-1
   do ig2=0,2*nb2max
      ig2p=ig2
      if (ig2.gt.nb2max) ig2p=ig2-2*nb2max-1
      do ig1=0,2*nb1max
         ig1p=ig1
         if (ig1.gt.nb1max) ig1p=ig1-2*nb1max-1
         do j=1,3
            sumkg(j)=(wk(1)+ig1p)*b1(j)+ &
                 (wk(2)+ig2p)*b2(j)+(wk(3)+ig3p)*b3(j)
         enddo
         gtot=sqrt(dot_product(sumkg,sumkg))
         etot=gtot**2/c
         if (etot.lt.ecut) then
            ncnt=ncnt+1
            igall(1,ncnt)=ig1p
            igall(2,ncnt)=ig2p
            igall(3,ncnt)=ig3p
         end if
      enddo
   enddo
enddo
if (2*ncnt.ne.nplane) then
   write(0,*) '*** error - computed 2*ncnt=',2*ncnt, &
        ' != input nplane=',nplane
   ierr = -10
   close(unit=10)
   deallocate(igall)
   deallocate(coeff)
   deallocate(cener)
   deallocate(occ)
   deallocate(expArray)
   return
endif
print *,'ncnt=',ncnt

do iplane=1,ncnt
 ig=igall(:,iplane)
 wkpg=wk+ig
 do iCoord=1,nCoords
  expArray(iplane+(iCoord-1)*int8(ncnt))=2.*pi*cmplx(0.,-1.)*d_dot_product(3,wkpg,coords(3*(iCoord-1)+1))
 enddo
enddo
call vcexp( ncnt*int8(nCoords), expArray, expArray )
! print*,'expArray: ', c_dot_product(ncnt,expArray,expArray)
irec0=irec
do 1000 i=1,nBands
band=bands(i)
! print*,'band: ', band
irec=irec0+band
read(unit=10,rec=irec) (coeff(iplane), iplane=1,nplane)
do iCoord=1,nCoords
!   print*, 'start csums'
   i8Tmp=(iCoord-1)*int8(ncnt)+1
   csum1 = c_dot_product(ncnt,expArray(i8Tmp),coeff)
   csum2=c_dot_product(ncnt,expArray(i8Tmp),coeff(ncnt+1))
   S1=csum1*conjg(csum1)/Vcell
   S2=csum2*conjg(csum2)/Vcell
   i8Tmp = ((i-1)*nwk+kpoint-1)*int8(nCoords)+iCoord
   lDOS(i8Tmp)= S1+S2
   lSigmaZ(i8Tmp)= (S1-S2)*0.5
   S1=csum2*conjg(csum1)/Vcell
   lSigmaX(i8Tmp)= real(S1)
   lSigmaY(i8Tmp)= aimag(S1)
enddo

!!$ output z*a3mag for units of Angstroms

1000 continue

2000 continue


close(unit=10)

deallocate(igall)
deallocate(coeff)
deallocate(cener)
deallocate(occ)
deallocate(expArray)
return
end



!!$*   routine for computing vector cross-product
subroutine vcross(a,b,c)
  implicit real*8(a-h,o-z)
  dimension a(3),b(3),c(3)
  
  a(1)=b(2)*c(3)-b(3)*c(2)
  a(2)=b(3)*c(1)-b(1)*c(3)
  a(3)=b(1)*c(2)-b(2)*c(1)
  return
end subroutine vcross      

complex*8 function c_dot_product(n, a, b)
  implicit none
  integer n
  complex*8  a(n),b(n)
  c_dot_product=dot_product(a,b)
  return
end function c_dot_product

real*8 function d_dot_product(n, a, b)
  implicit none
  integer n
  real*8  a(n),b(n)
  d_dot_product=dot_product(a,b)
  return
end function d_dot_product    

!***************************************************************
!* Given an N x N matrix A, this routine replaces it by the LU *
!* decomposition of a rowwise permutation of itself. A and N   *
!* are input. INDX is an output vector which records the row   *
!* permutation effected by the partial pivoting; D is output   *
!* as -1 or 1, depending on whether the number of row inter-   *
!* changes was even or odd, respectively. This routine is used *
!* in combination with LUBKSB to solve linear equations or to  *
!* invert a matrix. Return code is 1, if matrix is singular.   *
!***************************************************************
 Subroutine LUDCMP(A,N,NP,INDX,D,CODE)
 implicit none
 real *8 TINY
 PARAMETER(TINY=1D-12)
 real*8  AMAX,DUM, SUM, A(NP,NP),VV(N)
 INTEGER CODE, D, N, NP,  INDX(N), I,J,K, IMAX

 D=1; CODE=0

 DO I=1,N
   AMAX=0.
   DO J=1,N
     IF (ABS(A(I,J)).GT.AMAX) AMAX=ABS(A(I,J))
   END DO ! j loop
   IF(AMAX.LT.TINY) THEN
     CODE = 1
     RETURN
   END IF
   VV(I) = 1. / AMAX
 END DO ! i loop

 DO J=1,N
   DO I=1,J-1
     SUM = A(I,J)
     DO K=1,I-1
       SUM = SUM - A(I,K)*A(K,J) 
     END DO ! k loop
     A(I,J) = SUM
   END DO ! i loop
   AMAX = 0.
   DO I=J,N
     SUM = A(I,J)
     DO K=1,J-1
       SUM = SUM - A(I,K)*A(K,J) 
     END DO ! k loop
     A(I,J) = SUM
     DUM = VV(I)*ABS(SUM)
     IF(DUM.GE.AMAX) THEN
       IMAX = I
       AMAX = DUM
     END IF
   END DO ! i loop  
   
   IF(J.NE.IMAX) THEN
     DO K=1,N
       DUM = A(IMAX,K)
       A(IMAX,K) = A(J,K)
       A(J,K) = DUM
     END DO ! k loop
     D = -D
     VV(IMAX) = VV(J)
   END IF

   INDX(J) = IMAX
   IF(ABS(A(J,J)) < TINY) A(J,J) = TINY

   IF(J.NE.N) THEN
     DUM = 1. / A(J,J)
     DO I=J+1,N
       A(I,J) = A(I,J)*DUM
     END DO ! i loop
   END IF 
 END DO ! j loop

 RETURN
 END


!******************************************************************
!* Solves the set of N linear equations A . X = B.  Here A is     *
!* input, not as the matrix A but rather as its LU decomposition, *
!* determined by the routine LUDCMP. INDX is input as the permuta-*
!* tion vector returned by LUDCMP. B is input as the right-hand   *
!* side vector B, and returns with the solution vector X. A, N and*
!* INDX are not modified by this routine and can be used for suc- *
!* cessive calls with different right-hand sides. This routine is *
!* also efficient for plain matrix inversion.                     *
!******************************************************************
 Subroutine LUBKSB(A,N,NP,INDX,B)
 implicit none
 real*8  SUM, A(NP,NP),B(N)
 INTEGER N,NP, INDX(N), II,I, LL, J

 II = 0

 DO I=1,N
   LL = INDX(I)
   SUM = B(LL)
   B(LL) = B(I)
   IF(II.NE.0) THEN
     DO J=II,I-1
       SUM = SUM - A(I,J)*B(J)
     END DO ! j loop
   ELSE IF(SUM.NE.0.) THEN
     II = I
   END IF
   B(I) = SUM
 END DO ! i loop

 DO I=N,1,-1
   SUM = B(I)
   IF(I < N) THEN
     DO J=I+1,N
       SUM = SUM - A(I,J)*B(J)
     END DO ! j loop
   END IF
   B(I) = SUM / A(I,I)
 END DO ! i loop

 RETURN
 END
  
