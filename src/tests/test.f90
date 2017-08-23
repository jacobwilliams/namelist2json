!*******************************************************************************
!>
!  Example converting a namelist to a JSON file

    program main

    use namelist_parser_module
    use json_module
    use iso_fortran_env, only: output_unit

    implicit none

    character(len=*),parameter :: filename     = 'test.nml'
    character(len=*),parameter :: filename_out = 'test.json'
    type(json_value),pointer :: p_namelist
    logical :: status_ok
    type(json_core) :: json

    call parse_namelist(filename,p_namelist,status_ok)

    if (status_ok) then
        call json%initialize(compress_vectors=.true.)
        call json%print(p_namelist,filename_out)
        call json%print(p_namelist,output_unit)
    else
        write(*,*) 'error'
    end if

    call json%destroy(p_namelist)

    end program main
!*******************************************************************************
