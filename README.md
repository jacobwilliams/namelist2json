### Description

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
 var1%E = 'a string',
 var1%F%G = 0,
 var1%H(2)%I = 2.0,
 var2%J%K = F
 /
```

Each variable must be on only one line. The value to the right of the `=` sign must be an integer, real, logical, or string. Multiple namelines per file are allowed.

The example above would be converted into the JSON file:
```javascript
{
  "namelist_name": {
    "var1": {
      "a": 3,
      "b": true,
      "c": ["1", "1", "1"],
      "e": "a string",
      "f": {
        "g": 0
      },
      "h": [
        null,
        {
          "i": 0.2E+1
        }
      ]
    },
    "var2": {
      "j": {
        "k": false
      }
    }
  }
}
```

## Compiling

A `fmp.toml` file is provided for compiling namelist2json with the [Fortran Package Manager](https://github.com/fortran-lang/fpm). For example, to build:

```
fpm build --profile release
```

To run the unit tests:

```
fpm test
```

To use `namelist2json` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
namelist2json = { git="https://github.com/jacobwilliams/namelist2json.git" }
```

or, to use a specific version:
```toml
[dependencies]
namelist2json = { git="https://github.com/jacobwilliams/namelist2json.git", tag = "1.0.0"  }
```

To generate the documentation using [ford](https://github.com/Fortran-FOSS-Programmers/ford), run: `ford ford.md`

### Third-Party requirements

This project requires [json-fortran](https://github.com/jacobwilliams/json-fortran), which will be downloaded by `FPM`.

## Documentation

The latest API documentation for the `master` branch can be found [here](https://jacobwilliams.github.io/namelist2json/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### Development

This project is hosted on GitHub at: https://github.com/jacobwilliams/namelist2json

### License

This library is released under a permissive BSD-3 license.

### See also

 * To do a similar thing using Python, you could use [f90nml](https://github.com/marshallward/f90nml).
 * [fast-namelist](https://github.com/jacobwilliams/fast-namelist)
 * [JSON-Fortran](https://github.com/jacobwilliams/json-fortran)
 * [fortran-csv-module](https://github.com/jacobwilliams/fortran-csv-module)
