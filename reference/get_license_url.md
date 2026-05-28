# Get License URL

Look up a license's reference URL from `LICENSE_CONFIG`. Accepts any
input recognized by
[`match_license()`](https://openwashdata.github.io/fairenough/reference/match_license.md)
(canonical name, DESCRIPTION-style spelling such as `"CC-BY-4.0"` or
`"CC BY 4.0"`, etc.) and returns the canonical URL.

## Usage

``` r
get_license_url(license)
```

## Arguments

- license:

  Character string identifying a license.

## Value

Character URL, or `NULL` if the license is unrecognized or has no
canonical URL (e.g. `"Proprietary"`).
