# ---------------------------------------------------------------------------- #
# Perturb S-GIMME groups -----
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

# Load packages and set seed

pkgs <- c("gimme", "perturbR", "tictoc")
groundhog.library(pkgs, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Evaluate S-GIMME groups ----
# ---------------------------------------------------------------------------- #

# Tutorials
# - S-GIMME:    https://tarheels.live/gimme/subgrouping-gimme/
# - "perturbR": https://cran.r-project.org/web/packages/perturbR/vignettes/perturbR-vignette.html

# Load S-GIMME output

load("./results/test_s-gimme/gimme_output.RData")

# Evaluate S-GIMME groups with "perturbR" (takes 4.5 min) and save plots (plots
# can't be obtained from output object, despite its slots for ARI and VI plots)

perturbR_path <- "./results/test_s-gimme/perturbR/"
dir.create(perturbR_path)

tic()

pdf(paste0(perturbR_path, "perturbR_plots.pdf"))
perturbRout <- perturbR(sym.matrix = gimme_output$sim_matrix,
                        plot       = TRUE,
                        resolution = 0.01,
                        reps       = 100,
                        errbars    = TRUE) # Show +/- 1 SE from mean values across 
                                           # repetitions at given resolution (alpha)
dev.off()

toc()

# Save results

save(perturbRout, file = paste0(perturbR_path, "perturbRout.RData"))