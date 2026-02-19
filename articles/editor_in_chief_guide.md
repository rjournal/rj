# Editor-in-Chief's Guide

## New submissions and revisions

### Downloading new submissions and revisions

Articles are submitted through a [Google
form](https://forms.gle/Eqkf6cFJM3mjuxZUA) which populates a [Google
sheet](https://docs.google.com/spreadsheets/d/1Snt4SDkfyceoX_PPGm-9cB29DAw-dvnCjWHHOt5jS38),
and a [Google
drive](https://drive.google.com/drive/folders/1azJouPrcOS9HxI2LCjBLHTWj6dx_OBqxiVgW30Zu7uUHD2c-Ge7ras_cLJlxT93NlaxpMd8i).
This sheet contains details of the submission, and also a zip file with
the necessary files;
[`get_submissions()`](https://rjournal.github.io/rj/reference/get_submissions.md)
will authenticate against both.

Pre-2026 form, sheet and submission files can be found in the following
links: [Google form](https://forms.gle/ykj6QcoGQD5ctnA5A), [Google
sheet](https://docs.google.com/spreadsheets/d/15Tem82ikjHhNVccZGrHFVdzVjoxAmwaiNwaBE95wu6k/edit#gid=1671813370),
and [Google
drive](https://drive.google.com/drive/folders/0B2aFIue-Ar8dfmdVdHlpTnRndWhxQnNtR0o2YnoycDU1cVVzRlRaM0VSQWU1YUlnZlkyZEk?resourcekey=0-pzsHNE5mUiKuzB-VFmGD7g).
These ones were owned by different individuals, so it is advised not to
use these and should not be required unless searching for historical
submissions. Note that if you are creating a new form, then you must
manually add a column in Google sheet named “Submission ID”, change
“Email Address” to “Email address” and the sheet name “Form Responses 1”
to “Form responses 1” for the
[`get_submissions()`](https://rjournal.github.io/rj/reference/get_submissions.md)
to work. It is sensitive cases and changes to the column nmaes.

On a regular (at least weekly) basis, download newly submitted items:

``` r
get_submissions()
```

This will create a new folder in the `Submissions` directory of the
`articles` repo, and a `DESCRIPTION` file with the meta data.

You should check that this file has been constructed correctly, and that
the zip file has unpacked into the top level of the folder. Common
missing items are the `Suppl:` line which should list the `.R` files and
any data needed to reproduce the paper.

### Check relevance and reproducibility

First check that the paper fits the [scope of the
journal](https://journal.r-project.org/#article-types). If not,
[`reject()`](https://rjournal.github.io/rj/reference/action.md) the
paper.

Then check that the paper is reproducible by either: (a) compiling the
`Rmd` file; or (b) compiling both the LaTeX file and all `.R` files. If
there are any problems, request a
[`resubmission()`](https://rjournal.github.io/rj/reference/action.md)
from the authors.

### Acknowledge submission and assigning a new article to an EE

For revisions, use
[`acknowledge_revision()`](https://rjournal.github.io/rj/reference/acknowledge_revision.md)
to send an acknowledgement email to the authors, cc’ing the handling
editor.

For new submissions, choose one of the three active EE’s (i.e., not the
previous EIC). To balance EE load, use
[`assignments()`](https://rjournal.github.io/rj/reference/assignments.md)
to see how many papers each EE is currently handling, and what has been
assigned to them in recent months. As far as possible, take account of
the relative expertise of each EE so that papers are assigned to the
most appropriate person. Use `acknowledge_submission(editor = "XX")`. to
send an acknowledgement email to the authors, cc’ing the chosen handling
editor.

### Check progress of all papers

The function
[`report()`](https://rjournal.github.io/rj/reference/report.md) shows
the status of all current papers being handled, by author and by status.
Anything with 3 stars needs urgent attention.

## Meetings

The editors meet roughly every month. The EIC is responsible for
organizing these at mutually convenient times, taking account of the
different timezones.

The associate editors meet roughly every 3–4 months. The EIC is also
responsible for organizing these meetings. It is best to organize two
meetings, 12 hours apart, to allow for different timezones.

## New editors

### New executive editor

The EIC is responsible to navigating the search for a new EE. The search
for a new editorial board member should begin in the middle of the year,
giving enough time to find a replacement before the outgoing member
leaves at the end of December. Nominations for the new member are
discussed by the editorial board first, and then preferably also with
the advisory committee so that they can provide historical perspective.

Once a shortlist of candidates is created, but before approaching anyone
on the list, it should be sent to the R Foundation Board members
(<R-foundation-board@r-project.org>) for their feedback and approval.
People on the list are then approached by the EIC or an EE, and once
someone agrees, the EIC informs the members of the R Foundation
(<R-foundation-members@r-project.org>). The new editor is formally
appointed by the R Foundation president.

Once a new EE is found, the following steps take place:

- The new EE is added to the Editors team, with role changed as owner,
  at `https://github.com/rjournal` and to the `rjournal.slack.com`
  organisation.
- The new EE is added to the `editors.csv` file in the `inst` folder of
  the `rj` package.
- Onboard the new EE.

The EIC may take on outstanding handling editor duties of the outgoing
member.

### New associate editors

The EIC is responsible for recruiting new AEs, after discussion with all
editors. Ideally, keywords of submissions over the past year are
summarised, and compared with keywords of current AEs. New AEs should be
recruited for topics where there is the most need.

To invite a new AE, the EIC should send an email to the candidate, with
a brief description of the journal and the role, asking if they would be
interested in joining the editorial board as an AE. If they agree, the
EIC should ask them to complete the onboarding form
[here](https://github.com/rjournal/ae-articles-template). This form is
associated with the <rjournal.submission@gmail.com> and responses can be
found
[here](https://docs.google.com/spreadsheets/d/1_J7rawwkXjjO3BbhTwoSBiwwevHEEJWaSQZ2RMdcA0Y/edit?resourcekey=&gid=914313391#gid=914313391).

Once a new AE is appointed, the following steps take place:

- The EIC informs the other EEs.
- The EIC sets up a GitHub repo of the form ae-articles-xx where xx is
  the initials of the AE using the template at
  <https://github.com/rjournal/ae-articles-template>
- The new AE is added to the `associate-editors.csv` file in the `inst`
  folder of the `rj` package.

## Handover to new Editor-in-Chief

- Update the editorial board on the [R Journal
  website](https://rjournal.github.io/editors.html).
- Update the [R Journal Wikipedia
  page](https://en.wikipedia.org/wiki/The_R_Journal) with the change of
  editors.
- Ask Martin Maechler to forward <r-journal@r-project.org> to the new
  EIC’s preferred email address.
- Grant permission to the new EIC to access the [Google
  sheet](https://docs.google.com/spreadsheets/d/1Snt4SDkfyceoX_PPGm-9cB29DAw-dvnCjWHHOt5jS38),
  and [Google
  drive](https://drive.google.com/drive/folders/1azJouPrcOS9HxI2LCjBLHTWj6dx_OBqxiVgW30Zu7uUHD2c-Ge7ras_cLJlxT93NlaxpMd8i)
  used for submissions.
- Hand over the `rjournal.submission` Google account. This account owns
  the google forms and spreadsheets used by the editors.
- Update the front page of the [R Journal
  website](https://journal.r-project.org/) with the latest graphics.

## Archives

\[Not done since 2019. Is it needed?\]

In the `articles` repo, the `Proofs` folder contains all the supporting
files of Accepted articles. The `Rejected` folder contains all of the
supporting files for rejected submisisons.

From time to time, papers with dates older than two years should be
moved to the `archive` repo, to make the `articles` repo smaller.
Recommend that this is done at the hand-over of the EiC role at the end
of each year.
