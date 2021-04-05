# rj 0.2.19

2021-04-05
 - fix `get_articles_path()` to use the repostiroy root if not
   defined by `set_articles_path()`. Also check the validity of
   the path set by `set_articles_path()` and ignore it (with a
   warning) if it does not exist.

 - re-factor printing in `summarise_articles()`. This also fixes
   the duplication of entries with "revision received" status.

 - added `summarise_articles(..., other=TRUE)` option which if
   set will show articles which have a status not covered by any of
   the other sections (typically entries like accepted, proofed,
   online) - possibly useful to check for status errors.

2021-03-22
 - `RJ_EMAIL_TOOL` environment variable can be set to change the tool
   used to handle generated e-mails. It can have one of the following
   values:
   - "mailto" or "browser"
     opens the system browser with _mailto:_ URL, on most
     systems this will open the e-mail client as defined
     in the system (default)
   - "show" uses `file.show()` function on the generated text file
   - "edit" uses `file.edit()` function on the generated text file
   - "open" calls `open` system command on the generated text file
     (on macOS this opens your system text editor)
   - "clip" copies the text of the e-mail into the system
     clipboard. Note that on systems other that macOS or Windows this
     requires you to have either `xsel` or `xclip` helper tool
     installed.

 - Warn about the "browser" option (typically adulterated by
   RStudio which tends to prevent _mailto:_ URLs from working) only if
   it is a function.

2021-03-15
 - modified the `email_template()` function to use the same text as
   `valid_status()` for major and minor revision 


# rj 0.2.18

2021-02-22
 - Exported `get_articles_path()`

2021-02-22
 - getting report function to work, almost there

