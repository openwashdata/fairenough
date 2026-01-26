# Build License

A unified interface to usethis license functions. Uses match_license()
to map license input to a supported license, then calls the appropriate
usethis function.

## Usage

``` r
build_license(license, ...)
```

## Arguments

- license:

  Character string specifying the license name

- ...:

  Additional arguments passed to underlying usethis function

## Value

Invisible TRUE if successful
