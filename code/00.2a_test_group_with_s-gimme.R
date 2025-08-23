# ---------------------------------------------------------------------------- #
# Test S-GIMME grouping algorithm -----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load package and set seed

groundhog.library("gimme", groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Test S-GIMME ----
# ---------------------------------------------------------------------------- #

# Tutorial: https://tarheels.live/gimme/subgrouping-gimme/

# Call the data from the "gimme" package into the environment

data("simData", package = "gimme") 

# Run gimme using "simData" (takes about 40 min). Console says:
# - "Number of subgroups = 2"
# - "Modularity = 0.19335"

s_gimme_path <- "./results/test_s-gimme/"

gimme_output <- gimme(data        = simData,
                      out         = s_gimme_path,
                      subgroup    = TRUE,
                      sub_feature = "lag & contemp", # TODO: Consider grouping on only lagged relations
                      sub_method  = "Walktrap", 
                      groupcutoff = .75,
                      subcutoff   = .51)

# Save results

save(gimme_output, file = paste0(s_gimme_path, "gimme_output.RData"))