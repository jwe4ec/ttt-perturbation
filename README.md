# ttt-perturbation
This repository contains analysis code for this project on the Open Science Framework: https://osf.io/rqej3/.

## Inputs From [ttt-p1-main-analysis](https://github.com/jwe4ec/ttt-p1-main-analysis)

### Data

#### From `/02_networks/code/02_further_clean_data_align_obs.R`

- `/final_clean/data_var.RDS` as of 10/15/24

### Results

#### From `/02_networks/code/03_run_analyses.R`

For idiographic VAR models run in Mplus on 10/6/24:

- `/raw/mplus_var/` outputs
  - 53 DAT files, 53 INP files, and 53 OUT files (one of each type per participant)
  - `Mplus Run Models.txt`
- `/raw/varfit.RDS`

#### From `/02_networks/code/04_extract_temporal_results.R`

For idiographic VAR model results extracted on 10/28/24:

- `/extracted/results_var.RDS`
- `/extracted/thres_adj_mats_var.RDS`

## Code

# TODO