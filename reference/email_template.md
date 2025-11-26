# Send an email template.

Interpolate article values into a template, and create a new (unsent)
email in your default mail client. The email is created using
[`browseURL`](https://rdrr.io/r/utils/browseURL.html) and the mailto
protocol, so it must be relatively brief.

## Usage

``` r
email_template(article, template)
```

## Arguments

- article:

  An article id, e.g. `"2013-01"`

- template:

  The name of a template (without extension) found in `inst/templates`.

## Text format

The template should be divided into header and body with `---`. The
header should contain fields and values separated by `:` - only a
limited

## Template parameters

The templates use whisker to insert template values. These have the form
`{{field_name}}`. You can use any field from the description as well as
the following special fields:

- name: the name of the first author

- email: email address of first author

- editor: name of editor

- me: your name, as determine by envvar `RJ_NAME`
