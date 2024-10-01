# rj 0.2.4
- Changed proofing template email to Simon's

# rj 0.2.31
- added vignettes on conflict of interest and roles and responsibilities

# rj 0.2.29

- added a line for AE: decisions for summarise_articles()

# rj 0.2.28

- Add `check_in_submission_folder()` to verify the article is currently active for actions (major_revision, minor_revision, accept, reject). 
 
# rj 0.2.27

- Added two new functions to create summary plots `article_status_plot()` and `time_to_accept_plot()`

# rj 0.2.26

- Expand `get_assignments()` and `actionable_articles()` to search on AEs and 
  print out articles in status "with AE". 
- `match_keywords()` now requires authentication everytime to avoid false positive PERMISSION_DENIED


# rj 0.2.25

- Updated the template for "accept" to reflect new format, check list, and four issues per year.

# rj 0.2.24

- Changes to `summarise_articles()`: 
   - include the unassigned articles in the beginning, if any
   - now use `tabulate_article()` to produce the output

# rj 0.2.23

- Changed the `ae_workload()` to 
  - have a default period of 365 days
  - return all AEs where those without assignments as 0
- `summarise_articles()` now first prints out article(s) not assigned to any editor


# rj 0.2.22

2022-01-31
  - fixes to `ae_workload()`; now handles "" empty AE field without failing,
    and will show the AE initials, name, and email address alongside the
    assignment count. Now works as previously documented, and `day_back`
    argument works again (but note the meaning of this is not what users might
    expect.)

# rj 0.2.21

2021-11-01
  - fixes to erroring in summarise_articles
  - make anonymous example for add_reviewer()
  - email_template also updates status

# rj 0.2.20

2021-10-04
 - revised add_ae function
 
2021-09-06
 - Added a function and template to acknowledge revisions

2021-08-25 
  - add_ae function added
  - fixes to get_submissions  
  - new function to tabulate_articles which helps to count keywords, reviewer loads, ...
  - updates to vignettes on operations

2021-08-10
  - fix to match_keywords
  
# rj 0.2.19

2021-06-21
 - new function to list articles that are accepted but not online
2021-06-17
 - added `git(..)` helper function to call `git` properly.
   It honors the `GIT` environment variable.
 - fixed git actions to work when executed in a subdirectory
   of "articles" by using `get_articles_path()` for the target.
 - add `as.characer.id()` so simple things like
   `warning("Article ", article$id, " has issues")` works.
 - `update_status` will update the last entry (with a warning)
   instead of appending if it is the same status.
2021-06-07
 - removed check on RJournal.sty file being in article directory
 - updated template for minor revision to say "tentatively" accepted
 - Only build article pdf if it doesn't exist already
 
2021-04-19
 - added a new function to handle format reject

2021-04-07
 - fix file path quoting in `git mv` commands (spaces in names
   would break it)

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

