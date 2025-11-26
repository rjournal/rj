# Generate an email template.

Generate an email template.

## Usage

``` r
email_text(text, means = getOption("RJ_EMAIL_TOOL", "mailto"))
```

## Arguments

- text:

  character vector, text of the e-mail (including headers)

- means:

  string, one of "mailto", "browser" (both open mailto: URL), "show"
  (file.show), "edit" (file.edit), "open" (shell open) or "clip" (system
  clipboard). Defaults to RJ_EMAIL_TOOL environment variable.
