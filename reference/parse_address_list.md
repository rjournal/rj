# Parse a string into a rfc822 address list.

EBNF at <http://tools.ietf.org/html/rfc2822#section-3.4>

## Usage

``` r
parse_address_list(x)
```

## Arguments

- x:

  string to parse

## Value

a list of
[`address`](https://rjournal.github.io/rj/reference/address.md)es

## Examples

``` r
parse_address_list("<a@b.com> Alison, <c@d.com> Colin")
#> <a@b.com>,
#>   <c@d.com> 
```
