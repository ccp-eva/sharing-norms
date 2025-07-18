# The social learning and development of intra- and inter-ethnic sharing norms in the Congo Basin

## Repository Structure

### Data

Raw data is found in the `data` directory.  It consists of experimental data
(`data/raw_data_anonymized.csv`) and interview data (split across four files
`data/Interview_Data*.csv`).

The anonymised experimental data is derived from a non-shared raw data file,
using the script `data/anonymize.R`.

### Analysis

A logistic model is fit to the experimental data, implemented in Stan.  The Stan
model definitions are in `analysis/stan_code*.c` and the R script which fits
them is in `analysis/fit_model.R`.

Two Bayesian GLMMs are fit to the interview data, implemented in brms.  This
code is in `analysis/Interviews.R`.

### Results

The `analysis/results.Rmd` RMarkdown document loads the fitted stan models and
presents tables of results as reported in the manuscript.  The helper script
`analysis/knit.sh` knits this document to a PDF file.
