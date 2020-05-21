!-----------------------------------------------------------------------
!
	MODULE netcdf
!
!-----------------------------------------------------------------------
!  Purpose: 
!     Provides a module interface for netcdf include file and error 
!     handling function.
!
!  Scope: all variables and routines are public
!
!  Bugs:
!-----------------------------------------------------------------------

        implicit none

        include 'netcdf.inc'

	CONTAINS

!-----------------------------------------------------------------------
!
        SUBROUTINE handle_error (ncerr, message)
!
!-----------------------------------------------------------------------
!  Purpose:
!    Deals with netcdf errors.
!    Prints the type of error and exits.
!
!  Scope: public subroutine 
!    Module variables used:
!       netcdf error codes (from netcdf.inc)
!
!-----------------------------------------------------------------------

        integer, intent(in) :: ncerr
        character(len=*), intent(in), optional :: message

        if (ncerr /= NF_NOERR) then

	  print *, "NETCDF ERROR: ", nf_strerror(ncerr)
	  if (present(message)) print *, message

	  if (critical()) then
	     print *
	     print *, "CRITICAL NETCDF ERROR: STOP"
	     stop
          end if

        endif


        CONTAINS

!-----------------------------------------------------------------------
!
        FUNCTION critical ()
!
!-----------------------------------------------------------------------
! Purpose:
!    Used to determine whether a netcdf error is critical (i.e., warrants
!    program termination.)
!    Currently, all errors are treated as critical.
!
!  Scope: private, contained within subroutine handle_error
!    Module variables used:
!       various netcdf error codes
!    Variables from containing scope (handle_error):
!       ncerr (someday...)
!
!-----------------------------------------------------------------------

        logical :: critical

! FIXME - determine whether error is critical here...
!         for testing purposes, all errors are critical.
        critical = .true.

        END FUNCTION critical


        END SUBROUTINE handle_error

	END MODULE netcdf

