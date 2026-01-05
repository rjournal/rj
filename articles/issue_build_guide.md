# Guide to building an issue

## Select articles

- Identify articles to include in the new issue. Normally, all articles
  accepted up to the publication dates: Mar 31, Jun 30, Oct 31, Dec 31,
  should be included.

- Set the status of each included article to “proofed” using

  ``` r
  rj::update_status("<article_id>", "proofed")`
  ```

- For each article, check that the supplementary file list is correct.

- Copy included articles into `Proofs` folder using

  ``` r
  rj:::make_proof(<issue number>)
  ```

  The `issue number` is of the form `YYYY-N` where `N` is the issue
  number within the year. e.g., `2026-1` for the first issue of 2026.

- For each article, this will also create a zip file of supplementary
  material from the list of files in the DESCRIPTION.

## News items

- Contact the following people for news items (give them each a couple
  of weeks):

  - Changes on CRAN: \<Achim.Zeileis at r-project.org\>
  - News from R Core: \<Tomas.Kalibera at r-project.org\>
  - BioConductor: \<maintainer at bioconductor.org\>
  - R Foundation: \<Torsten.Hothorn at r-project.org\>
  - R Forwards: \<Heather.Turner at r-project.org\>

- Copy received news items into the news folder of the issue. Only a
  subset of the above will be received for each issue. Most will send
  news every second issue.

## Editorial

- Write the editorial for the issue. This should be saved as
  `<issue>/news/editorial/editorial.Rmd`.

## Preparation

For each article in the issue:

- Move any correspondence to correspondence folder (e.g., motivation
  letter, rejoinder, etc.)

- Copyedit using `r rj::llm_copyedit(<article_id>)` This needs an OpenAI
  API key stored in `.Renviron` as `OPENAI_API_KEY`. Check the
  copyediting carefully, as the AI can make mistakes. It is usually
  pretty good on the main article, but often messes up the bib file.

- For articles with an Rmd file, make sure it renders to both pdf and
  html. output should be set to

  ``` yaml
  output:
    rjtools::rjournal_article:
      toc: no
      self_contained: yes
  ```

- For articles containing a tex file, but no Rmd file, use texor to
  create the Rmd

  ``` r
  texor::latex_to_web("<path to tex file>")
  ```

  - The conversion to Rmd can be fragile, especially if authors have
    defined their own LaTeX macros, or use additional packages. Also
    tables, equations and equation cross-referencing can need fixing.
  - This should only produce an html output. The original tex file is
    used to produce the pdf output.

- If you encounter problems that you can’t solve yourself, ask the
  Technical Editor for assistance.

## Assembling the issue

- When an article is ready, copy to the `rjournal.github.io` repo using
  `r rj::publish_article("<article_id>", <volume>, <issue>)` This will
  also assign a “slug” (the id of the published article), which is added
  to the DESCRIPTION file. This slug will be used for the folder name in
  the `rjournal.github.io` “\_articles” folder.
- For any news item:
  `r rj::publish_news("<folder path>", <volume>, <issue>)`
- When all articles and news items are copied over, build the issue
  using `r rj::publish_issue("<issue id>")` This will create the issue
  folder in the `rjournal.github.io` repo, and generates the issue page
  with links to all articles and news items.
- Build the issue by rendering `_issues/<issue id>.Rmd` in the
  `rjournal.github.io` repo. This publishes to the `_web` folder.
- Commit and push the changes to the `rjournal.github.io` repo, to
  automatically trigger deployment to `rjournal.github.io`

## Checking

- Check the issue carefully yourself at
  <https://rjournal.github.io/issues/>.
- Ask the executive editors to check the issue.
- Ask each author to check their own proofs.
- Fix any issues found. Ask the Technical Editor for help where
  necessary.

## Publishing the issue

- The Technical Editor will issue a pull request to merge the master
  branch of the repo into the published branch. Once approved, this will
  then trigger a cron job to update <https://journal.r-project.org>.

## Final steps

- Once papers are online, register DOIs with <https://doi.crossref.org/>
  using username `rfou` and password provided by previous editor. Click
  on “Upload submissions”, then “Metadata” and upload the `doi.xml` file
  located in the `_issues/<issue_id>` folder.
- Update `RJournal.bib` using `update_bib.R` in the `rjournal.github.io`
  repo.
- Announce the new issue on any social media channels used by the R
  Journal.
