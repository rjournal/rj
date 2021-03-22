# rj 0.2.19

2021-03-22
 - RJ_EMAIL_TOOL environment variable can be set to change the tool
   used to handle generated e-mails. If can have one of the
   following values:
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

2021-02-22: Exported get_articles_path()
2021-02-22: getting report function to work, almost there

