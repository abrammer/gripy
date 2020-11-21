! C FILE: libgbytes.F90
  subroutine gbyte(in,iout,iskip,nbyte)
    integer in(*)
    integer iout(*)
    call gbytes(in,iout,iskip,nbyte,0,1)
    return
  end


  subroutine gbytes(in,iout,iskip,nbyte,nskip,n)
    !          Get bytes - unpack bits:  Extract arbitrary size values from a
    !          packed bit string, right justifying each value in the unpacked
    !          array.
    !            IN    = character*1 array input
    !            IOUT  = unpacked array output
    !            ISKIP = initial number of bits to skip
    !            NBYTE = number of bits to take
    !            NSKIP = additional number of bits to skip on each iteration
    !            N     = number of iterations
    ! v1.1
    !
    integer in(*)
    integer iout(N)
    integer inint
    integer tbit, bitcnt
    integer, parameter :: ones(8) = (/ 1,3,7,15,31,63,127,255 /)
    !f2py intent(in) IN
    !f2py intent(out) IOUT
    !f2py depend(N) IOUT
    !f2py intent(in) ISKIP
    !f2py intent(in) NBYTE
    !f2py intent(in) NSKIP
    !f2py intent(in) N

    !     nbit is the start position of the field in bits
    nbit = iskip
    ! print*, 'pos', nbit
    ! print*, 'nbits', nbyte
    do i = 1, n
      bitcnt = nbyte
      index=nbit/8+1
      ibit=mod(nbit,8)
      nbit = nbit + nbyte + nskip

      ! first byte
      tbit = min(bitcnt,8-ibit)
      ! read (in(index),'(z8)') inint
      inint = in(index)
      !  print*, inint
      itmp = iand(inint, ones(8-ibit))
      if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
      index = index + 1
      bitcnt = bitcnt - tbit

      ! now transfer whole bytes
      do while (bitcnt.ge.8)
        !  read (in(index),'(z8)') inint
        inint = in(index)
        !  print*, inint
        itmp = ior(ishft(itmp,8),inint)
        !   print*,inint
        bitcnt = bitcnt - 8
        index = index + 1
      enddo

      ! get data from last byte
      if (bitcnt.gt.0) then
        !  read (in(index),'(z8)') inint
        inint = in(index)
        !  print*, inint
        itmp = ior(ishft(itmp,bitcnt),iand(ishft(inint,&
            -(8-bitcnt)),ones(bitcnt)))
      endif

      iout(i) = itmp
      ! print*,'itmp', itmp
    enddo

    return
  end subroutine gbytes


  subroutine comunpack(cpack,len,lensec,idrsnum,idrstmpl,ndpts, fld)
    !$$$  SUBPROGRAM DOCUMENTATION BLOCK
    !                .      .    .                                       .
    ! SUBPROGRAM:    comunpack
    !   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2000-06-21
    !
    ! ABSTRACT: This subroutine unpacks a data field that was packed using a
    !   complex packing algorithm as defined in the GRIB2 documention,
    !   using info from the GRIB2 Data Representation Template 5.2 or 5.3.
    !   Supports GRIB2 complex packing templates with or without
    !   spatial differences (i.e. DRTs 5.2 and 5.3).
    !
    ! PROGRAM HISTORY LOG:
    ! 2000-06-21  Gilbert
    ! 2004-12-29  Gilbert  -  Added test ( provided by Arthur Taylor/MDL )
    !                         to verify that group widths and lengths are
    !                         consistent with section length.
    ! 2016-02-26              update unpacking for template 5.3
    !
    ! USAGE:    CALL comunpack(cpack,len,lensec,idrsnum,idrstmpl,ndpts,fld,ier)
    !   INPUT ARGUMENT LIST:
    !     cpack    - The packed data field (character*1 array)
    !     len      - length of packed field cpack().
    !     lensec   - length of section 7 (used for error checking).
    !     idrsnum  - Data Representation Template number 5.N
    !                Must equal 2 or 3.
    !     idrstmpl - Contains the array of values for Data Representation
    !                Template 5.2 or 5.3
    !     ndpts    - The number of data values to unpack
    !
    !   OUTPUT ARGUMENT LIST:
    !     fld()    - Contains the unpacked data values
    !     ier      - Error return:
    !                  0 = OK
    !                  1 = Problem - inconsistent group lengths of widths.
    !
    ! REMARKS: None
    !
    ! ATTRIBUTES:
    !   LANGUAGE: XL Fortran 90
    !   MACHINE:  IBM SP
    !
    !$$$

    integer,intent(in) :: cpack(len)
    integer,intent(in) :: ndpts,len
    integer,intent(in) :: idrstmpl(*)
    real,intent(out) :: fld(ndpts)

    integer,allocatable :: ifld(:),ifldmiss(:)
    integer(4) :: ieee
    integer,allocatable :: gref(:),gwidth(:),glen(:)
    real :: ref,bscale,dscale,rmiss1,rmiss2
    ! real :: fldo(6045)
    integer :: totBit, totLen, non
    !f2py intent(in) cpack
    !f2py intent(in) len
    !f2py intent(in) lensec
    !f2py intent(in) idrsnum
    !f2py intent(in) idrstmpl
    !f2py intent(in) ndpts
    !f2py intent(out) fld

    ier=0
    ! print *,'IDRSTMPL: ',(idrstmpl(j),j=1,16)
    ieee = idrstmpl(1)
    call rdieee(ieee, ref, 1)
    bscale = 2.0**real(idrstmpl(2))
    dscale = 10.0**real(-idrstmpl(3))
    nbitsgref = idrstmpl(4)
    itype = idrstmpl(5)
    ngroups = idrstmpl(10)
    nbitsgwidth = idrstmpl(12)
    nbitsglen = idrstmpl(16)
    if (idrsnum.eq.3) then
      nbitsd=idrstmpl(18)*8
    endif
    ! print*, 'nbitsd', nbitsd
    ! Constant field

    if (ngroups.eq.0) then
      do j=1,ndpts
        fld(j)=ref
      enddo
      return
    endif

    iofst=0
    allocate(ifld(ndpts),stat=is)
    ! print *,'ALLOC ifld: ',is,ndpts
    allocate(gref(ngroups),stat=is)
    ! print *,'ALLOC gref: ',is, ngroups
    allocate(gwidth(ngroups),stat=is)
    ! print *,'ALLOC gwidth: ',is, ngroups
    !
    !  Get missing values, if supplied
    !
    if ( idrstmpl(7).eq.1 ) then
      if (itype.eq.0) then
          call rdieee(idrstmpl(8),rmiss1,1)
      else
          rmiss1=real(idrstmpl(8))
      endif
    elseif ( idrstmpl(7).eq.2 ) then
      if (itype.eq.0) then
          call rdieee(idrstmpl(8),rmiss1,1)
          call rdieee(idrstmpl(9),rmiss2,1)
      else
          rmiss1=real(idrstmpl(8))
          rmiss2=real(idrstmpl(9))
      endif
    endif
    ! print *,'RMISSs: ',rmiss1,rmiss2,ref
    !
    !  Extract Spatial differencing values, if using DRS Template 5.3
    !
    ! print*, 'idrsnum:', idrsnum
    ! print*, nbitsd
    ! print*, idrstmpl(17)
    if (idrsnum.eq.3) then
      if (nbitsd.ne.0) then
        call gbytes(cpack,ival1,iofst,nbitsd,0,1)
        iofst=iofst+nbitsd
        if (idrstmpl(17).eq.2) then
          call gbytes(cpack,ival2,iofst,nbitsd, 0, 1)
          iofst=iofst+nbitsd
        endif
        call gbytes(cpack,isign,iofst,1,0,1)
        iofst=iofst+1
        call gbytes(cpack,minsd,iofst,nbitsd-1, 0, 1)
        iofst=iofst+nbitsd-1
        if (isign.eq.1) minsd=-minsd
      else
        ival1=0
        ival2=0
        minsd=0
      endif
      ! print *,'SDu ',ival1,ival2,minsd,nbitsd
    endif
    !
    !  Extract Each Group's reference value
    !
    ! print *,'SAG1: ',nbitsgref,ngroups,iofst
    if (nbitsgref.ne.0) then
      call gbytes(cpack,gref,iofst,nbitsgref,0,ngroups)
      itemp=nbitsgref*ngroups
      iofst=iofst+(itemp)
      if (mod(itemp,8).ne.0) iofst=iofst+(8-mod(itemp,8))
    else
      gref(1:ngroups)=0
    endif
    ! write(78,*)'GREFs: ',(gref(j),j=1,ngroups)
    !
    !  Extract Each Group's bit width
    !
    ! print *,'SAG2: ',nbitsgwidth,ngroups,iofst,idrstmpl(11)
    if (nbitsgwidth.ne.0) then
      call gbytes(cpack,gwidth,iofst,nbitsgwidth,0,ngroups)
      itemp=nbitsgwidth*ngroups
      iofst=iofst+(itemp)
      if (mod(itemp,8).ne.0) iofst=iofst+(8-mod(itemp,8))
    else
      gwidth(1:ngroups)=0
    endif
    do j=1,ngroups
      gwidth(j)=gwidth(j)+idrstmpl(11)
    enddo
    ! write(78,*)'GWIDTHs: ',(gwidth(j),j=1,ngroups)
    !
    !  Extract Each Group's length (number of values in each group)
    !
    allocate(glen(ngroups),stat=is)
    ! print *,'ALLOC glen: ',is
    ! print *,'SAG3: ',nbitsglen,ngroups,iofst,idrstmpl(14),idrstmpl(13)
    if (nbitsglen.ne.0) then
      call gbytes(cpack,glen,iofst,nbitsglen,0,ngroups)
      itemp=nbitsglen*ngroups
      iofst=iofst+(itemp)
      if (mod(itemp,8).ne.0) iofst=iofst+(8-mod(itemp,8))
    else
      glen(1:ngroups)=0
    endif
    do j=1,ngroups
      glen(j)=(glen(j)*idrstmpl(14))+idrstmpl(13)
    enddo
    glen(ngroups)=idrstmpl(15)
    ! write(78,*)'GLENs: ',(glen(j),j=1,ngroups)
    ! print *,'GLENsum: ',sum(glen)
    ! print*, glen(:8)
    !
    !  Test to see if the group widths and lengths are consistent with number of
    !  values, and length of section 7.
    !
    totBit = 0
    totLen = 0
    do j=1,ngroups
      totBit = totBit + (gwidth(j)*glen(j));
      totLen = totLen + glen(j);
    enddo
    if (totLen .NE. ndpts) then
      ier=1
      ! print*, totLen, ".NE.", ndpts
      return
    endif
    if ( (totBit/8) .GT. lensec) then
      ier=1
      ! print*, totBit/8, ".GT.", lensec
      return
    endif
    !
    !  For each group, unpack data values
    !
    if ( idrstmpl(7).eq.0 ) then        ! no missing values
      n=1
      do j=1,ngroups
      ! write(78,*)'NGP ',j,gwidth(j),glen(j),gref(j)
        if (gwidth(j).ne.0) then
          call gbytes(cpack,ifld(n),iofst,gwidth(j),0,glen(j))
          do k=1,glen(j)
            ifld(n)=ifld(n)+gref(j)
            n=n+1
          enddo
        else
          ifld(n:n+glen(j)-1)=gref(j)
          n=n+glen(j)
        endif
        iofst=iofst+(gwidth(j)*glen(j))
      enddo
    elseif ( idrstmpl(7).eq.1.OR.idrstmpl(7).eq.2 ) then
      ! missing values included
      allocate(ifldmiss(ndpts))
      ! ifldmiss=0
      n=1
      non=1
      do j=1,ngroups
        ! print *,'SAGNGP ',j,gwidth(j),glen(j),gref(j)
        if (gwidth(j).ne.0) then
          msng1=(2**gwidth(j))-1
          msng2=msng1-1
          call gbytes(cpack,ifld(n),iofst,gwidth(j),0,glen(j))
          iofst=iofst+(gwidth(j)*glen(j))
          do k=1,glen(j)
            if (ifld(n).eq.msng1) then
              ifldmiss(n)=1
            elseif (idrstmpl(7).eq.2.AND.ifld(n).eq.msng2) then
              ifldmiss(n)=2
            else
              ifldmiss(n)=0
              ifld(non)=ifld(n)+gref(j)
              non=non+1
            endif
            n=n+1
          enddo
        else
          msng1=(2**nbitsgref)-1
          msng2=msng1-1
          if (gref(j).eq.msng1) then
            ifldmiss(n:n+glen(j)-1)=1
            !ifld(n:n+glen(j)-1)=0
          elseif (idrstmpl(7).eq.2.AND.gref(j).eq.msng2) then
            ifldmiss(n:n+glen(j)-1)=2
            !ifld(n:n+glen(j)-1)=0
          else
            ifldmiss(n:n+glen(j)-1)=0
            ifld(non:non+glen(j)-1)=gref(j)
            non=non+glen(j)
          endif
          n=n+glen(j)
        endif
      enddo
    endif
    !write(78,*)'IFLDs: ',(ifld(j),j=1,ndpts)

    if ( allocated(gref) ) deallocate(gref)
    if ( allocated(gwidth) ) deallocate(gwidth)
    if ( allocated(glen) ) deallocate(glen)
    !
    !  If using spatial differences, add overall min value, and
    !  sum up recursively
    !
    ! print*,' idrstmpl(17)', idrstmpl(17)
    if (idrsnum.eq.3) then         ! spatial differencing
      if (idrstmpl(17).eq.1) then      ! first order
          ifld(1)=ival1
          if ( idrstmpl(7).eq.0 ) then        ! no missing values
            itemp=ndpts
          else
            itemp=non-1
          endif
          do n=2,itemp
            ifld(n)=ifld(n)+minsd
            ifld(n)=ifld(n)+ifld(n-1)
          enddo
      elseif (idrstmpl(17).eq.2) then    ! second order
          ifld(1)=ival1
          ifld(2)=ival2
          if ( idrstmpl(7).eq.0 ) then        ! no missing values
            itemp=ndpts
          else
            itemp=non-1
          endif
          do n=3,itemp
            ifld(n)=ifld(n)+minsd
            ifld(n)=ifld(n)+(2*ifld(n-1))-ifld(n-2)
          enddo
      endif
    !write(78,*)'IFLDs: ',(ifld(j),j=1,ndpts)
    endif
    !
    !  Scale data back to original form
    !
    ! print*, MAXVAL(ifld), MINVAL(ifld)
    ! print *,'SAGT: ',ref,bscale,dscale
    if ( idrstmpl(7).eq.0 ) then        ! no missing values
      do n=1,ndpts
        fld(n)=((real(ifld(n))*bscale)+ref)*dscale
        !write(78,*)'SAG ',n,fld(n),ifld(n),bscale,ref,1./dscale
      enddo
    elseif ( idrstmpl(7).eq.1.OR.idrstmpl(7).eq.2 ) then
      ! missing values included
      non=1
      do n=1,ndpts
        if ( ifldmiss(n).eq.0 ) then
          fld(n)=((real(ifld(non))*bscale)+ref)*dscale
          !print *,'SAG ',n,fld(n),ifld(non),bscale,ref,dscale
          non=non+1
        elseif ( ifldmiss(n).eq.1 ) then
          fld(n)=rmiss1
        elseif ( ifldmiss(n).eq.2 ) then
          fld(n)=rmiss2
        endif
      enddo
      if ( allocated(ifldmiss) ) deallocate(ifldmiss)
    endif

    if ( allocated(ifld) ) deallocate(ifld)

    !open(10,form='unformatted',recl=24180,access='direct')
    !read(10,rec=1) (fldo(k),k=1,6045)
    !do i =1,6045
    !  print *,i,fldo(i),fld(i),fldo(i)-fld(i)
    !enddo

    return
  end subroutine comunpack


  subroutine rdieee(rieee, a, num)
    !                .      .    .                                       .
    ! SUBPROGRAM:    rdieee
    !   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-09
    !
    ! ABSTRACT: This subroutine reads a list of real values in
    !   32-bit IEEE floating point format.
    !
    ! PROGRAM HISTORY LOG:
    ! 2000-05-09  Gilbert
    !
    ! USAGE:    CALL rdieee(rieee,a,num)
    !   INPUT ARGUMENT LIST:
    !     rieee    - Input array of floating point values in 32-bit IEEE format.
    !     num      - Number of floating point values to convert.
    !
    !   OUTPUT ARGUMENT LIST:
    !     a        - Output array of real values.
    !
    ! REMARKS: None
    !
    ! ATTRIBUTES:
    !   LANGUAGE: Fortran 90
    !   MACHINE:  IBM SP
    !
    !$$$

    real(4),intent(in) :: rieee(num)
    real,intent(out) :: a(num)
    integer,intent(in) :: num

    integer(4) :: ieee

    real,parameter :: two23=scale(1.0,-23)
    real,parameter :: two126=scale(1.0,-126)

    do j=1,num
    !
    !  Transfer IEEE bit string to integer variable
    !
      ieee=transfer(rieee(j),ieee)
    !
    !  Extract sign bit, exponent, and mantissa
    !
      isign=ibits(ieee,31,1)
      iexp=ibits(ieee,23,8)
      imant=ibits(ieee,0,23)
      sign=1.0
      if (isign.eq.1) sign=-1.0

      if ( (iexp.gt.0).and.(iexp.lt.255) ) then
        temp=2.0**(iexp-127)
        a(j)=sign*temp*(1.0+(two23*real(imant)))

      elseif ( iexp.eq.0 ) then
        if ( imant.ne.0 ) then
          a(j)=sign*two126*two23*real(imant)
        else
          a(j)=sign*0.0
        endif

      elseif ( iexp.eq.255 ) then
        a(j)=sign*huge(a(j))

      endif

    enddo
    return
  end subroutine rdieee
