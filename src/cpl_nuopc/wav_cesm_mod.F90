module wav_cesm_mod

#ifdef CESMCOUPLED
  
  use wav_kind_mod, only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod , only : shr_sys_abort

  implicit none
  private ! except

  public :: cesm_rest_filename
  public :: cesm_hist_filename
  public :: handle_err

  ! if a run is a startup or branch run, then initfile is used
  ! to construct the initial file and used in W3IORSMD
  character(len=256), public :: initfile

  ! if a run is a continue run, then casename is used to construct
  ! the restart filename in W3IORSMD
  character(len=256), public :: casename

  logical, public :: rstwr   ! true => write restart
  logical, public :: histwr  ! true => write history file (snapshot)

  integer, public :: outfreq ! output frequency in hours

  integer, public           :: inst_index  ! number of current instance (ie. 1)
  character(len=16), public :: inst_name   ! fullname of current instance (ie. "wav_0001")
  character(len=16), public :: inst_suffix ! char string associated with instance

!===============================================================================
contains
!===============================================================================
  
  subroutine cesm_rest_filename(lwrite, time, fname) 

    use wav_shr_methods, only : runtype, stdout, root_task

    ! input/output variables
    logical, intent(in)           :: lwrite
    integer, intent(in)           :: time(2)
    character(len=*), intent(out) :: fname

    ! local variables
    integer :: yy,mm,dd,hh,mn,ss,totsec
    logical :: exists
    !----------------------------------------------

    yy =  time(1)/10000
    mm = (time(1)-yy*10000)/100
    dd = (time(1)-yy*10000-mm*100)
    hh = time(2)/10000
    mn = (time(2)-hh*10000)/100
    ss = (time(2)-hh*10000-mn*100)
    totsec = hh*3600+mn*60+ss

    if (len_trim(inst_suffix) > 0) then
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5)') &
            trim(casename)//&
            &'.ww3'//trim(inst_suffix)//'.r.',yy,'-',mm,'-',dd,'-',totsec
    else
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5)') &
            trim(casename)//'.ww3.r.',yy,'-',mm,'-',dd,'-',totsec
    endif

    if (len_trim(inst_suffix) > 0) then
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5)') &
            trim(casename)//&
            &'.ww3'//trim(inst_suffix)//'.r.',yy,'-',mm,'-',dd,'-',totsec
    else
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5)') &
            trim(casename)//'.ww3.r.',yy,'-',mm,'-',dd,'-',totsec
    endif

    ! if read
    if (lwrite) then
       if ( root_task ) then
          write (stdout,'(a)') ' writing restart file '//trim(fname)
       end if
    else
       if (runtype /= 'continue') fname = initfile
       inquire( file=fname, exist=exists)
       if (.not. exists ) then
          call shr_sys_abort("required initial/restart file " // trim(fname) // "does not exist")
       else
          if ( root_task) then
             write (stdout,'(a)') ' reading initial/restart file '//trim(fname)
          end if
       end if
    end if

  end subroutine cesm_rest_filename

  !===============================================================================
  subroutine cesm_hist_filename(nx, ny, noswll, time, e3df, ncid, dimid)

    use wav_shr_methods, only : runtype, stdout, root_task
    use netcdf

    integer, intent(in)  :: nx, ny    ! Discrete dimensions of spatial grid.
    integer, intent(in)  :: noswll    ! Number of swell fields from part.
    integer, intent(in)  :: time(2)
    integer, intent(in)  :: e3df(:,:) ! freq. indices for 3D output
    integer, intent(out) :: ncid
    integer, intent(out) :: dimid(4)

    ! local variables
    character(len=cl) :: fname
    integer           :: ef_len
    integer           :: yy,mm,dd,hh,mn,ss,totsec
    logical           :: exists
    integer           :: ierr
    !----------------------------------------------
    

    yy =  time(1)/10000
    mm = (time(1)-yy*10000)/100
    dd = (time(1)-yy*10000-mm*100)
    hh = time(2)/10000
    mn = (time(2)-hh*10000)/100
    ss = (time(2)-hh*10000-mn*100)
    totsec = hh*3600+mn*60+ss
    ef_len = e3df(3,1)-e3df(2,1)+1

    if (len_trim(inst_suffix) > 0) then
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)') &
            trim(casename)//'.ww3'//trim(inst_suffix)//'.hi.',yy,'-',mm,'-',dd,'-',totsec,'.nc'
    else
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)') &
            trim(casename)//'.ww3.hi.',yy,'-',mm,'-',dd,'-',totsec,'.nc'
    endif

    ! write(*, *) 'w3iogomd: writing history ', fname
    inquire(file=trim(fname),exist=exists)
    if (.not. exists) then
       ierr = nf90_create(trim(fname),nf90_clobber,ncid)
       call handle_err(ierr,'create')
       ierr = nf90_def_dim(ncid,'nx',nx,dimid(1))
       call handle_err(ierr,'def_dimid1')
       ierr = nf90_def_dim(ncid,'ny',ny,dimid(2))
       call handle_err(ierr,'def_dimid2')
       ierr = nf90_def_dim(ncid,'noswll',noswll+1,dimid(3))
       call handle_err(ierr,'def_dimid3')
       ierr = nf90_def_dim(ncid,'freq', ef_len,dimid(4)) !ef_len=25
       call handle_err(ierr,'def_dimid4')
    else
       ierr = nf90_open(trim(fname),nf90_write,ncid)
       call handle_err(ierr,'open')
       ierr = nf90_inq_dimid(ncid,'nx',dimid(1))
       call handle_err(ierr,'inq_dimid1')
       ierr = nf90_inq_dimid(ncid,'ny',dimid(2))
       call handle_err(ierr,'inq_dimid2')
       ierr = nf90_inq_dimid(ncid,'noswll',dimid(3))
       call handle_err(ierr,'inq_dimid3')
       ierr = nf90_inq_dimid(ncid,'freq',dimid(4)) !ef_len=25
       call handle_err(ierr,'inq_dimid4')
    endif

  end subroutine cesm_hist_filename

  !===============================================================================
  subroutine handle_err(ierr,string)
    use netcdf
    integer         ,intent(in) :: ierr
    character(len=*),intent(in) :: string
    if (ierr /= nf90_noerr) then
       call shr_sys_abort("*** wavewatch iii netcdf error: "//trim(string)//':'//trim(nf90_strerror(ierr)))
    endif
  end subroutine handle_err

#endif

end module wav_cesm_mod
