module wav_cesm_mod

#ifdef CESMCOUPLED
  
  use wav_kind_mod, only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod , only : shr_sys_abort

  implicit none
  private ! except

  public :: cesm_rest_filename

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
#endif

end module wav_cesm_mod
