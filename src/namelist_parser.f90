!*******************************************************************************
!> author: Jacob Williams
!  date: August 20, 2017
!
!  A module for parsing Fortran namelists.

    module namelist_parser_module

    use json_module
    use iso_fortran_env, only: real64,int32

    implicit none

    private

    integer,parameter,public :: wp = real64  !! default real kind

    ! variable types:
    integer,parameter,public :: type_string  = 1  !! a character string variable
    integer,parameter,public :: type_double  = 2  !! a `real(wp)` variable
    integer,parameter,public :: type_integer = 3  !! an `integer` variable
    integer,parameter,public :: type_logical = 4  !! a logical variable

    public :: parse_namelist

    contains
!*******************************************************************************

!*******************************************************************************
!>
!  Parse a Fortran namelist into a `json_value` structure.
!
!  The file may contains multiple namelists, and multiple
!  instances of the same namelist (this case is handled by
!  creating an array).
!
!  The structure of the JSON file will be:
!```javascript
!   {
!       "namelist1":{ "var1":val1, "var2":val2, ... },
!       "namelist2":{ "var1":val1, "var2":val2, ... },
!       "namelist_array":[{ "var1":val1, "var2":val2, ... },
!                         { "var1":val1, "var2":val2, ... }]
!   }
!```
!
!@note Currently it only works for a subset of valid namelists,
!      specifically, where each variable/value assignment is on a separate
!      line with the form 'var = val'. It also works for derived types and
!      arrays, but each element of an array must be on a separate line.

    subroutine parse_namelist(filename,p_namelist,status_ok)

    implicit none

    character(len=*),intent(in) :: filename  !! the namelist file to parse
    type(json_value),pointer :: p_namelist   !! the resulting JSON structure
    logical,intent(out) :: status_ok         !! true if there were no errors

    type(json_value),pointer :: p
    type(json_value),pointer :: p_array
    character(len=:),allocatable :: line
    character(len=:),allocatable :: namelist_name
    character(len=:),allocatable :: path
    character(len=:),allocatable :: val
    type(json_core) :: json     !! for manipulation the JSON pointers
    integer :: iunit            !! file unit number
    integer :: istat            !! file `iostat` flag
    integer :: status           !! status code during parsing
    integer :: i                !! index of `=` in a line
    integer :: iline            !! line number counter
    integer :: var_type         !! type of a JSON variable
    integer :: n_children       !! number of elements in an array of namelists
    character(len=256) :: istr  !! for integer to string conversion
    integer :: itype            !! type of the value in a line
    integer :: int_val          !! integer value
    logical :: logical_val      !! logical value
    real(wp) :: real_val        !! double value
    logical :: found            !! to check if namelist with same name already processed
    integer :: len_val          !! length of value string
    logical :: eof              !! end of file flag when reading namelist file

    ! namelist characters:
    character(len=1),parameter :: nml_start_char = '&'
    character(len=1),parameter :: nml_end_char   = '/'
    character(len=1),parameter :: comment_char   = '!'
    character(len=1),parameter :: equals_char    = '='
    character(len=1),parameter :: sep            = '%'
    character(len=1),parameter :: comma          = ','

    ! use fortran style path separators:
    call json%initialize(path_separator='%')

    ! create root object:
    call json%create_object(p_namelist,'')

    ! open the namelist file:
    open(newunit=iunit,file=trim(filename),status='OLD',iostat=istat)
    if (istat==0) then

        status = 0 ! looking for namelist
        iline = 0  ! line counter
        eof = .false.
        do

            if (eof) exit ! finished

            ! read a line from the file
            iline = iline + 1
            call read_line_from_file(iunit,line,eof)
            line = trim(adjustl(line))
            if (line(1:1)==comment_char .or. line == '') cycle ! skip it

            select case (status)

            case(0) ! looking for a namelist

                if (line(1:1)==nml_start_char) then
                    ! a new namelist has been found
                    status = 1 ! parsing a namelist
                    namelist_name = lowercase_string(line(2:))
                    ! does it exist already?
                    call json%get(p_namelist,namelist_name,p,found)
                    if (found) then
                        ! we need to turn it into an array if
                        ! it isn't already one
                        call json%info(p,var_type=var_type,n_children=n_children)
                        if (var_type == json_array) then
                            ! add the array index to the name:
                            write(istr,'(I256)') n_children + 1
                            namelist_name = namelist_name//'('//trim(adjustl(istr))//')'
                        elseif (var_type == json_object) then
                            ! replace this with an array where the
                            ! current object is the first element
                            call json%create_array(p_array,namelist_name)
                            call json%replace(p,p_array,destroy=.false.)
                            call json%add(p_array,p)
                            namelist_name = namelist_name//'(2)'
                        else
                            call line_parse_error(iline,line,'invalid type')
                        end if
                    end if

                end if

            case(1) ! in the process of parsing a namelist

                if (line(1:1)==nml_end_char) then
                    ! finished with this namelist
                    status = 0 ! looking for namelist
                else
                    ! continue parsing this namelist

                    ! we are assuming that:
                    !  * each line contains an equal sign: `var = val`
                    !  * it maybe ends in a comma: `var = val,`
                    !  * val can be enclosed in quotes: `var = "val",`
                    i = index(line,equals_char)

                    if (i<=1) then
                        call line_parse_error(iline,line,'invalid line')
                    else

                        ! full path, including the namelist name
                        ! [also convert to lower case]
                        path = lowercase_string(namelist_name//sep//line(1:i-1))

                        ! the value after the equal sign:
                        val = trim(adjustl(line(i+1:)))

                        ! remove the comma after the value if necessary:
                        len_val = len(val)
                        if (val(len_val:len_val)==comma) then
                            val = val(1:len_val-1)
                        end if

                        ! remove quotes around strings if necessary:
                        ! [either type of quotes is allowed]
                        len_val = len(val)
                        if ((val(1:1)=='"' .and. val(len_val:len_val)=='"') .or. &
                            (val(1:1)=="'" .and. val(len_val:len_val)=="'")) then
                            if (len_val>2) then
                                val = val(2:len_val-1)
                            else
                                val = ''
                            end if
                        end if

                        ! have to determine what kind of value it is:
                        call infer_variable_type(val,itype)

                        ! add the variable to the json structure:
                        select case (itype)
                        case(type_string)
                            call json%add_by_path(p_namelist,path,val)
                        case(type_double)
                            call to_real(val,real_val,status_ok)
                            call json%add_by_path(p_namelist,path,real_val)
                        case(type_integer)
                            call to_integer(val,int_val,status_ok)
                            call json%add_by_path(p_namelist,path,int_val)
                        case(type_logical)
                            call to_logical(val,logical_val,status_ok)
                            call json%add_by_path(p_namelist,path,logical_val)
                        case default
                            call line_parse_error(iline,line,'invalid variable type')
                        end select

                    end if

                end if

            case default
                call line_parse_error(iline,line,'invalid status flag')
            end select

        end do

        status_ok = .true.
        close(unit=iunit,iostat=istat)  ! close the namelist file

    else
        status_ok = .false.
    end if

    contains

        subroutine line_parse_error(iline,line,error_msg)

        !!  throw an error and stop the program.

        implicit none

        integer,intent(in)          :: iline
        character(len=*),intent(in) :: line
        character(len=*),intent(in) :: error_msg

        write(*,*) ''
        write(*,*) error_msg
        write(*,*) ''
        write(*,*) '  line number: ',iline
        write(*,*) '  line:        '//line
        write(*,*) ''
        error stop 'fatal error'

        end subroutine line_parse_error

    end subroutine parse_namelist
!*******************************************************************************

!*******************************************************************************
!>
!  Read a single line from a file.

    subroutine read_line_from_file(iunit,line,eof)

    implicit none

    integer,intent(in) :: iunit  !! the file unit (assumed to be opened)
    character(len=:),allocatable,intent(out) :: line !! the line read
    logical,intent(out) :: eof   !! if end of file reached

    integer,parameter :: buffer_size = 256  !! the size of the read buffer [arbitrary]

    integer :: nread  !! character count specifier for read statement
    integer :: istat  !! file read `iostat` flag
    character(len=buffer_size) :: buffer !! the file read buffer

    nread  = 0
    buffer = ''
    line   = ''
    eof    = .false.

    do
        ! read in the next block of text from the line:
        read(iunit,fmt='(A)',advance='NO',size=nread,iostat=istat) buffer
        if (IS_IOSTAT_END(istat)) then
            ! add the last block of text before the end of the file
            if (nread>0) line = line//buffer(1:nread)
            eof = .true.
            exit
        elseif (IS_IOSTAT_EOR(istat)) then
            ! add the last block of text before the end of record
            if (nread>0) line = line//buffer(1:nread)
            exit
        elseif (istat==0) then ! all the characters were read
            line = line//buffer ! add this block of text to the string
        else  ! some kind of error
            write(*,*) 'istat=',istat
            error stop 'Read error.'
        end if
    end do

    end subroutine read_line_from_file
!*******************************************************************************

!*****************************************************************************************
!>
!  Infers the variable type, assuming the following precedence:
!
!  * integer
!  * double
!  * logical
!  * character

    pure subroutine infer_variable_type(str,itype)

    implicit none

    character(len=*),intent(in) :: str
    integer,intent(out) :: itype

    real(wp) :: rval      !! a real value
    integer  :: ival      !! an integer value
    logical  :: lval      !! a logical value
    logical  :: status_ok !! status flag

    call to_integer(str,ival,status_ok)
    if (status_ok) then
        itype = type_integer
        return
    end if

    call to_real(str,rval,status_ok)
    if (status_ok) then
        itype = type_double
        return
    end if

    call to_logical(str,lval,status_ok)
    if (status_ok) then
        itype = type_logical
        return
    end if

    ! default is string:
    itype = type_string

    end subroutine infer_variable_type
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string to a `real(wp)`

    pure elemental subroutine to_real(str,val,status_ok)

    implicit none

    character(len=*),intent(in) :: str
    real(wp),intent(out) :: val
    logical,intent(out) :: status_ok

    integer :: istat  !! read `iostat` error code

    read(str,fmt=*,iostat=istat) val
    if (istat==0) then
        status_ok = .true.
    else
        status_ok = .false.
        val = 0.0_wp
    end if

    end subroutine to_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string to an `integer`

    pure elemental subroutine to_integer(str,val,status_ok)

    implicit none

    character(len=*),intent(in) :: str
    integer,intent(out) :: val
    logical,intent(out) :: status_ok

    integer :: istat  !! read `iostat` error code

    character(len=*),parameter :: default_int_fmt  = '(I256)'

    read(str,fmt=default_int_fmt,iostat=istat) val
    if (istat==0) then
        status_ok = .true.
    else
        status_ok = .false.
        val = 0
    end if

    end subroutine to_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string to a `logical`
!
!  * Evaluates to `.true.`  for strings ['t','true','.true.']
!  * Evaluates to `.false.` for strings ['f','false','.false.']
!
!  The string match is not case sensitive.

    pure elemental subroutine to_logical(str,val,status_ok)

    implicit none

    character(len=*),intent(in) :: str
    logical,intent(out) :: val
    logical,intent(out) :: status_ok

    character(len=:),allocatable :: tmp

    ! True and False options (all lowercase):
    character(len=*),dimension(3),parameter :: true_str  = ['t     ',&
                                                            'true  ',&
                                                            '.true.']
    character(len=*),dimension(3),parameter :: false_str = ['f      ',&
                                                            'false  ',&
                                                            '.false.']

    tmp = lowercase_string(str)
    if ( any(tmp==true_str) ) then
        val = .true.
        status_ok = .true.
    else if ( any(tmp==false_str) ) then
        val = .false.
        status_ok = .true.
    else
        val = .false.
        status_ok = .false.
    end if

    end subroutine to_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Return the lowercase version of the character.

    pure elemental function lowercase_character(c) result(c_lower)

    implicit none

    character(len=1),intent(in) :: c
    character(len=1)            :: c_lower

    integer :: i  !! index in uppercase array

    character(len=*),parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !! uppercase characters
    character(len=*),parameter :: lower = 'abcdefghijklmnopqrstuvwxyz' !! lowercase characters

    i = index(upper,c)
    c_lower = merge(lower(i:i),c,i>0)

    end function lowercase_character
!*****************************************************************************************

!*******************************************************************************
!>
!  Returns lowercase version of the string.

    pure elemental function lowercase_string(str) result(s_lower)

    implicit none

    character(len=*),intent(in) :: str      !! input string
    character(len=(len(str)))   :: s_lower  !! lowercase version of the string

    integer :: i  !! counter
    integer :: n  !! length of input string

    s_lower = ''
    n = len_trim(str)

    if (n>0) then
        do concurrent (i=1:n)
            s_lower(i:i) = lowercase_character(str(i:i))
        end do
    end if

    end function lowercase_string
!*******************************************************************************

!*******************************************************************************
    end module namelist_parser_module
!*******************************************************************************
