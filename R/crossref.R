## code to create XML file to submit an issue
## code hevaily based on jss tools for DOI submission
##
## install.packages("jss", repos="http://R-Forge.R-project.org")
##
## Main function is rj:::upload_dois_from_issue()
## EIC will need login credentials in SOPS-editor.md



## Constants
doi_prefix <- "10.32614"
rjournal_url <- "https://journal.r-project.org/archive"

depositor <- list(
  depositor_name = "R Foundation",
  depositor_email = "R-foundation@r-project.org"
)


journal_metadata <- list(
  journal_title = "The R Journal",
  journal_abbreviated_title = "The R Journal",
  coden = NULL, # JSS is "JSSOBK", we don't have one?
  issn = "2073-4859" # 1548-7660"
)

doi_batch_id_tpl <- "<doi_batch_id>{{ID}}</doi_batch_id>\n"
print_doi_batch_id <- function(file) {
  x <- as.POSIXlt(Sys.time())
  mon <- if (nchar(x$mon) == 1) paste("0", x$mon, sep = "") else x$mon
  mday <- if (nchar(x$mday) == 1) paste("0", x$mday, sep = "") else x$mday
  hour <- if (nchar(x$hour) == 1) paste("0", x$hour, sep = "") else x$hour
  min <- if (nchar(x$min) == 1) paste("0", x$min, sep = "") else x$min
  sec <- if (nchar(round(x$sec)) == 1) paste("0", round(x$sec), sep = "") else round(x$sec)
  ID <- paste(x$year, mon, mday, hour, min, sec, sep = "")
  txt <- whisker.render(doi_batch_id_tpl, list(ID = ID))
  cat(txt, file = file, append = TRUE)
}


timestamp_tpl <- "<timestamp>{{TIMESTAMP}}</timestamp>\n"
print_timestamp <- function(file) {
  x <- as.POSIXlt(Sys.time())
  mon <- if (nchar(x$mon) == 1) paste("0", x$mon, sep = "") else x$mon
  mday <- if (nchar(x$mday) == 1) paste("0", x$mday, sep = "") else x$mday
  hour <- if (nchar(x$hour) == 1) paste("0", x$hour, sep = "") else x$hour
  min <- if (nchar(x$min) == 1) paste("0", x$min, sep = "") else x$min
  sec <- if (nchar(round(x$sec)) == 1) paste("0", round(x$sec), sep = "") else round(x$sec)
  TIMESTAMP <- paste(x$year, mon, mday, hour, min, sec, sep = "")
  txt <- whisker.render(timestamp_tpl, list(TIMESTAMP = TIMESTAMP))
  cat(txt, file = file, append = TRUE)
}


depositor_tpl <- "
<depositor>
  <depositor_name>{{depositor_name}}</depositor_name>
  <email_address>{{depositor_email}}</email_address>
</depositor>
"

print_depositor <- function(file) {
  txt <- whisker.render(depositor_tpl, depositor)
  cat(txt, file = file, append = TRUE)
}

journal_metadata_tpl <- "

<journal_metadata language=\"en\">
  <full_title>{{journal_title}}</full_title>
  <abbrev_title>{{journal_abbreviated_title}}</abbrev_title>
  {{#issn}}<issn media_type=\"electronic\">{{issn}}</issn>{{/issn}}
  {{#coden}}<coden>{{coden}}</coden>{{/coden}}
</journal_metadata>

"

print_journal_metadata <- function(file) {
  txt <- whisker.render(journal_metadata_tpl, journal_metadata)
  cat(txt, file = file, append = TRUE)
}

## jv no {{sf}}{{volume}} here
journal_issue_tpl <- "
<journal_issue>
<publication_date media_type=\"online\">
  <year>{{year}}</year>
</publication_date>
<journal_volume>
  <volume>{{volume}}</volume>
</journal_volume>
<issue>{{issue}}</issue>
</journal_issue>

"

print_journal_issue <- function(x, file) { # type, file) {

  data <- list(
    year = x$year,
    volume = x$volume,
    number = x$number,
    issue = x$issue
  )

  txt <- whisker.render(journal_issue_tpl, data)
  cat(txt, file = file, append = TRUE)
}

## from c("a1", "a2", ...) create list(family, given)
author_list <- function(auts) {
  auts <- strsplit(auts, "\\s+")
  lapply(auts, function(x) list(family = x[length(x)], given = paste(x[-length(x)], sep = " ")))
}

author_tpl <- "
<person_name sequence=\"{{author_order}}\" contributor_role=\"author\">
  <given_name>{{given}}</given_name>
  <surname>{{family}}</surname>
  {{#suffix}}<suffix>{{suffix}}</suffix}{{/suffix}}
  {{#affiliation}}<affiliation>{{affiliation}}</affiliation>{{/affiliation}}
  {{#ORCID}}<ORCID>{{ORCID}}</ORCID>{{/ORCID}}
</person_name>
"

get_authors <- function(x) {
  out <- ""
  for (i in 1:length(x$authors)) {
    aut <- x$authors[[i]]
    l <- list(
      given = tth::tth(aut$given),
      family = tth::tth(aut$family),
      suffix = if (is.null(aut$suffix)) NULL else tth::tth(aut$suffix),
      affiliation = aut$affiliation,
      ORCID = aut$ORCID,
      author_order = ifelse(i == 1, "first", "additional")
    )
    out <- paste(out, whisker.render(author_tpl, l), sep = "\n")
  }
  out
}


## our DOI is just the slug
doi_tpl <- "{{doi_prefix}}/{{slug}}"

get_doi <- function(x) {
  data <- list(
    doi_prefix = doi_prefix,
    slug = x$slug
  )
  whisker.render(doi_tpl, data)
}

url_tpl <- "{{rjournal_url}}/{{year}}/{{slug}}/index.html"

get_url <- function(x) {
  data <- list(
    rjournal_url = rjournal_url,
    year = x$year,
    slug = x$slug
  )
  whisker.render(url_tpl, data)
}


journal_article_tpl <- "
<journal_article publication_type=\"full_text\">
  <titles>
    <title>{{title}}</title>
  </titles>
  <contributors>
    {{{authors}}}
  </contributors>
  <publication_date media_type=\"online\">
    <year>{{year}}</year>
  </publication_date>
  <pages>
    <first_page>{{first_page}}</first_page>
  </pages>
  <publisher_item>
    <identifier id_type=\"doi\">{{DOI}}</identifier>
  </publisher_item>
  <doi_data>
    <doi>{{DOI}}</doi>
    <resource>{{url}}</resource>\
  </doi_data>
</journal_article>
"

print_journal_article <- function(x, file) {
  data <- list(
    title = x$title,
    authors = get_authors(x),
    year = x$year,
    first_page = x$pages[1],
    last_page = x$pages[2],
    DOI = get_doi(x),
    url = get_url(x)
  )
  txt <- whisker.render(journal_article_tpl, data)
  cat(txt, file = file, append = TRUE)
}

## Could add citations
##  by areading.bib file and creating from there
## <citation_list>  / a citation list for the content being registered
##     <citation key="key-10.9876/S0003695199034166-1">
##       <issn>0027-8424</issn>
##       <journal_title>Proc. Natl. Acad. Sci. U.S.A.</journal_title>
##       <author>West</author>
##       <volume>98</volume>
##       <issue>20</issue>
##       <first_page>11024</first_page>
##       <cYear>2001</cYear>
##     </citation>
##     <citation key="key-10.9876/S0003695199034166-2">
##      <journal_title>Space Sci. Rev.</journal_title>
##      <author>Heber</author>
##      <volume>97</volume>
##      <first_page>309</first_page>
##      <cYear>2001</cYear>
##     </citation>


print_citation <- function(cit, file) {
  ## how to do this for different types...
}

print_citations <- function(art, file) {
  cat("<citation_list>\n", out = file, append = TRUE)
  for (cit in art$citations) {
    print_citation(cit, file)
  }
  cat("</citation_list>\n", out = file, append = TRUE)
}







doi_batch_header_old <- '<doi_batch xmlns="http://www.crossref.org/schema/4.4.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="4.4.0" xsi:schemaLocation="http://www.crossref.org/schema/4.4.0 http://www.crossref.org/schemas/crossref4.4.0.xsd">'

doi_batch_header <- '<doi_batch xmlns="http://www.crossref.org/schema/4.4.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="4.4.0" xsi:schemaLocation="http://www.crossref.org/schema/4.4.0 http://www.crossref.org/schemas/crossref4.4.0.xsd">\n'

newDeposit <- function(x, out = "") {
  cat(doi_batch_header, file = out)
  cat("<head>\n", file = out, append = TRUE)

  print_doi_batch_id(out)
  print_timestamp(out)
  print_depositor(out)
  cat("<registrant>CrossRef</registrant> \n </head>\n
	<body>\n", file = out, append = TRUE)
  ## this generates xml for all bib items stored in a bib file
  for (i in 1:length(x$articles)) {
    cat("<journal>\n", file = out, append = TRUE)
    print_journal_metadata(out)
    print_journal_issue(x, out)
    print_journal_article(x$articles[[i]], out)
    cat("</journal>\n\n\n", file = out, append = TRUE)
  }
  cat("
</body>
</doi_batch>
", file = out, append = TRUE)
  ## test via
  ## http://test.crossref.org, see http://help.crossref.org/verifying_your_xml
  ## make real deposits with https://doi.crossref.org/servlet/deposit

  # system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://test.crossref.org/servlet/deposit", sep=""))

  #    system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://doi.crossref.org/servlet/deposit", sep=""))
}


## we need to get into format for crossref
## require(yaml)
## cfg = yaml.load_file("path/to/_config.yml")
## i = cfg[[ind]]
##
process_config_yml_component <- function(i) {
  articles <- Filter(function(i) !is.null(i$slug), i$articles)
  articles <- Filter(function(l) grepl("^RJ", l$slug), articles)

  out <- list()
  out$year <- i$year
  out$volume <- i$volume
  out$issue <- i$num
  out$articles <- list()
  for (j in 1:length(articles)) {
    a <- articles[[j]]
    l <- list()
    l$slug <- a$slug
    l$year <- i$year
    l$pages <- a$pages
    l$title <- a$title # strip italics? ...
    l$authors <- author_list(a$author)
    # other?
    out$articles[[a$slug]] <- l
  }

  out
}



load_issues <- function() {
  if (!grepl("/RJournal/articles$", getwd())) {
    warning("Not in proper directory: start in RJournal/articles")
    return()
  }
  cfg <- yaml.load_file("../rjournal.github.io/_config.yml")
  cfg$issues
}

## Filter out a specific volume and number:
## issues = load_issues()
## issue = find_volume_num(issues, 9, 2)
## create_doi_from_issue(issue)
find_volume_num <- function(issues, volume, num) {
  out <- Filter(function(i) i$volume == volume && i$num == num, issues)
  if (length(out) == 1) {
    out[[1]]
  } else {
    warning("Not uniquely identified")
    return()
  }
}


## system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://test.crossref.org/servlet/deposit", sep=""))
## system(paste("curl -F \'operation=doMDUpload\' -F \'login_id=fast\' -F \'login_passwd=fast_827\' -F \'fname=@", out, "\' ", "https://doi.crossref.org/servlet/deposit", sep=""))

## upload DOIs from an issue
## issues = rj:::load_issues()
## issue = rj:::find_volume_num(issues, 9, 2)
## rj:::upload_dois_from_issue(pissue, passwd="", url="test")
##
##
## Issues: in the 9-2 volume, an empty given_name is generated,
## causing an error. See
## https://doi.crossref.org/servlet/submissionAdmin?sf=detail&submissionID=1442846045
## to check on a submission, where the ID is shown in the queue.
upload_dois_from_issue <- function(issue,
                                   login = "rfou", passwd = "see_EIC_instructions",
                                   out = sprintf("/tmp/DOI_%s_%s.xml", issue$volume, issue$num),
                                   url = "test" # or live
) {
  pissue <- process_config_yml_component(issue)
  newDeposit(pissue, out)

  ## test via
  ## http://test.crossref.org, see http://help.crossref.org/verifying_your_xml
  ## make real deposits with https://doi.crossref.org/servlet/deposit

  q_url <- list(
    "test" = "https://test.crossref.org/servlet/submissionAdmin?sf=showQ",
    "live" = "https://doi.crossref.org/servlet/submissionAdmin?sf=showQ"
  )[[url]]

  u_url <- list(
    "test" = "https://test.crossref.org/servlet/deposit",
    "live" = "https://doi.crossref.org/servlet/deposit"
  )[[url]]


  cmd <- sprintf("curl -F 'operation=doMDUpload' -F 'login_id=%s' -F 'login_passwd=%s' -F 'fname=@%s' %s", login, passwd, out, u_url)

  system(cmd)
  resp <- system(cmd)
  print(resp)
  browseURL(q_url)
}

## create a DOI from the an issue component of the config$issues
##
## require(yaml)
## cfg = yaml.load_file("../rjournal.github.io/_config.yml")
## issue = cfg$issue[[length(cfg$issue)]] # last one
##
## or
##
## issues = load_issues()
## issue = find_volume_num(issues, 10, 2)
##
## then
##
## create_doi_from_issue(issue)
## Then, log onto https://doi.crossref.org
## upload this new xml file through upload; cross fingers
create_doi_from_issue <- function(issue, outdir = "/tmp/DOI_XML/") {
  if (is.null(issue$volume)) {
    warning("No volume for this issue")
    return()
  }
  out <- paste0(outdir, "DOI-", issue$volume, "-", issue$num, ".xml")
  issue <- process_config_yml_component(issue)
  newDeposit(issue, out)
}

## create all DOI entries for initial upload
create_doi_xmls <- function(outdir = "/tmp/DOI_XML/", config = "../rjournal.github.io/_config.yml") {
  cfg <- yaml.load_file(config)
  ## identify volume, volnum
  dir.create(outdir)
  sapply(cfg$issues, create_doi_from_issue, outdir = outdir)
}
