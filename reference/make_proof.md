# Make a proof of an issue

The \`make_proof()\` function is the first step to creating an issue. It
moves the 'proofed' articles from the \`Accepted\` folder and news
articles from \`News_items/{id}\` into \`Proofs/{id}\`.

## Usage

``` r
make_proof(id, exec = FALSE)
```

## Arguments

- id:

  The id of the issue to proof

- exec:

  Set to TRUE to make the proof, the default (FALSE) allows a preview of
  which articles will be moved where.

## Details

After the proof is made with this function, \`publish_issue()\` can be
used to publish these articles into the \`rjournal.github.io\`
repository.
