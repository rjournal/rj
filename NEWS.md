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

 - added _experimental_ `actionable_articles()` function which
   lists articles with potential actions such as "needs reviewers",
   "review overdue" or "invite overdue". It requires reviewer
   entries to use well-defined comments such as
   `[Invited 2020-12-18; Agreed 2020-12-21; Minor 2020-12-29]`
   Currently only looks at articles with status "out for review",
   "acknowledged" and "submitted". The supported review states are:
    - Invited
    - Agreed, Declined, Abandoned
    - Revision (subsequent rounds of reviews after Major)
    - Minor, Major, Reject (final)

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

