# ---------------------------------------------------------------------------- #
# Evalute S-GIMME results -----
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

pkgs <- c("gimme", "perturbR", "corrplot")
groundhog.library(pkgs, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Load S-GIMME and "perturbR" output ----
# ---------------------------------------------------------------------------- #

load("./results/test_s-gimme/gimme_output.RData")
load("./results/test_s-gimme/perturbR/perturbRout.RData")

# ---------------------------------------------------------------------------- #
# Evaluate S-GIMME results ----
# ---------------------------------------------------------------------------- #

# Tutorials
# - S-GIMME:    https://tarheels.live/gimme/subgrouping-gimme/
# - "perturbR": https://cran.r-project.org/web/packages/perturbR/vignettes/perturbR-vignette.html

# TODO (continue and think more deeply about results): Evaluate results

a <- cor(gimme_output$sim_matrix)

perturbR_path <- "./results/test_s-gimme/perturbR/"

pdf(paste0(perturbR_path, "corrplot.pdf"))
corrplot(a, method = "color")
dev.off()




