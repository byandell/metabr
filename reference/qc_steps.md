# Title

Missing values might be \`NA\` or \`0\` values.

Batch corrected values are transposed so steps are different.

## Usage

``` r
qc_steps(dirpath, filename, exclude_first = FALSE, Batch_Plate = NULL)

calc_cf(rawobject, exclude_first = FALSE)

replace_missing_ave_cf(qcobject, drop_fewer = TRUE)

correct_raw_cf(rawobject, qcobject)

read_batch_metab(dirpath, filename, sheet = 4)

read_raw_metab(dirpath, filename, sheet = 1, skip = 0, Batch_Plate = NULL)
```

## Arguments

- dirpath:

  path to directory

- filename:

  name of file

- exclude_first:

  exclude first QC if \`TRUE\`

- Batch_Plate:

  add \`Batch\` and \`Plate\` if not \`NULL\`.

- rawobject:

- qcobject:

- drop_fewer:

  drop compounds with fewer than all QC entries

- sheet:

  sheet number if excel type file

- skip:

  number of lines to skip

- object:

  object from \`calc_cf()\`

## Value

data frame
