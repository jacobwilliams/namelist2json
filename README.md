## Description

A basic module for parsing Fortran namelists and converting them to a JSON data structure. It is written in modern Fortran.

Note that the parser is very simple and only works on a subset of valid namelist files. It may be expanded in the future.

The namelist must be formatted like so:

```fortran
 &NAMELIST_NAME
 var1%A = 3,
 var1%B = true,
 var1%C(1) = '1',
 var1%C(2) = '1',
 var1%C(3) = '1',
 var1%E = 'SNOPTA',
 var1%F%G = 0,
 var1%H(7)%I = 2.0,
 /
```

Each variable must be on only one line. The value to the right of the `=` sign must be an integer, real, logical, or string. Multiple namelines per file are allowed.

## Building

A [FoBiS](https://github.com/szaghi/FoBiS) configuration file (`namelist2json.fobis`) is provided that can build the library and examples. Use the `mode` flag to indicate what to build. For example:

* To build all the examples using gfortran: `FoBiS.py build -f namelist2json.fobis -mode tests-gnu`
* To build all the examples using ifort: `FoBiS.py build -f namelist2json.fobis -mode tests-intel`
* To build a static library using gfortran: `FoBiS.py build -f namelist2json.fobis -mode static-gnu`
* To build a static library using ifort: `FoBiS.py build -f namelist2json.fobis -mode static-intel`

The full set of modes are:

* `static-gnu`
* `static-gnu-debug`
* `static-intel`
* `static-intel-debug`
* `shared-gnu`
* `shared-gnu-debug`
* `shared-intel`
* `shared-intel-debug`
* `tests-gnu`
* `tests-gnu-debug`
* `tests-intel`
* `tests-intel-debug`

## Third-Party Requirements

This project requires [json-fortran](https://github.com/jacobwilliams/json-fortran), which is included in `src` as a git submodule.

## See also

 * To do a similar thing using Python, you could use [f90nml](https://github.com/marshallward/f90nml).
 * [JSON-Fortran](https://github.com/jacobwilliams/json-fortran)
 * [fortran-csv-module](https://github.com/jacobwilliams/fortran-csv-module)

## License

This library is released under a permissive BSD-3 license.
