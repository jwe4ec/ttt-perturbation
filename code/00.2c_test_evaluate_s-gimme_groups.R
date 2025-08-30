# ---------------------------------------------------------------------------- #
# Evaluate S-GIMME subgroups -----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# Tutorials
# - S-GIMME:    https://tarheels.live/gimme/subgrouping-gimme/
# - "perturbR": https://cran.r-project.org/web/packages/perturbR/vignettes/perturbR-vignette.html

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
# Evaluate subgroups by visually inspecting correlogram of similarity matrix ----
# ---------------------------------------------------------------------------- #

# Similarity matrix shows the number of sig. (a) group-level paths and (b) candidate
# paths (based on expected parameter change for each modification index after fitting
# group-level model for each person) that each person shares with every other person, 
# but what are these correlations showing? Somehow rescaling?

a <- cor(gimme_output$sim_matrix)

perturbR_path <- "./results/test_s-gimme/perturbR/"

pdf(paste0(perturbR_path, "corrplot.pdf"))
corrplot(a, method = "color")
dev.off()

# ---------------------------------------------------------------------------- #
# Evaluate robustness of subgroups by comparing subgroup assignments ----
# ---------------------------------------------------------------------------- #

# Note: Per Karrer et al. (2008; https://doi.org/10.1103/PhysRevE.77.046119), subgroups are robust if 
# >= 20% of edges in similarity matrix can be perturbed (i.e., alpha >= 20%) before similarity values 
# cross similarity value when 20% of participants (nodes in similarity matrix) are randomly assigned 
# to different subgroups (see "perturbR-vignette.html" above)

  # TODO: Why would you compare similarity when perturbing edges of similarity matrix to similarity 
  # when perturbing participants' subgroups? The similarity matrix doesn't contain information about
  # what subgroup participants are in, correct? So how does perturbing participants' subgroups work?

# For Adjusted Rand Index (ARI; higher values reflect greater similarity)

ari20mark <- perturbRout$ari20mark

round(ari20mark, 2) == .33 # Bottom line in ARI plot (when 20% of subgroup assignments are swapped)
perturbRout$percent[min(which(colMeans(perturbRout$ARI) < ari20mark))] = .61 # Alpha at intersection

(ari_at_20 <- round(mean(perturbRout$ARI[, which(round(perturbRout$percent, 2) == .20)]), 2)) == .88 # ARI at alpha of 20%

ari_at_20 > ari20mark # Thus, subgroups are robust per ARI

# For Variation of Information (VI; lower values reflect greater similarity)

vi20mark <- perturbRout$vi20mark

round(vi20mark, 2) == 1.40 # Top line in VI plot (when 20% of subgroup assignments are swapped)
perturbRout$percent[min(which(colMeans(perturbRout$VI) > vi20mark))] == .61 # Alpha at intersection

(vi_at_20 <- round(mean(perturbRout$VI[, which(round(perturbRout$percent, 2) == .20)]), 2)) == .26 # VI at alpha of 20%

vi_at_20 < vi20mark # Thus, subgroups are robust per VI

# ---------------------------------------------------------------------------- #
# Evaluate robustness of subgroups by comparing modularity values ----
# ---------------------------------------------------------------------------- #

# Note: See Gates et al. (2017, p. 142) for limitations of using modularity

cutoff <- perturbRout$cutoff

round(cutoff, 2) == 0.12 # Upper 5th percentile of modularity values from random matrices

(observed <- round(perturbRout$modularity[1, 1], 2)) == 0.19 # Modularity for observed similarity matrix

observed > cutoff # Thus, subgroups are robust per modularity

pdf(paste0(perturbR_path, "modularity_plot.pdf"))
hist(perturbRout$modularity[, which(round(perturbRout$percent, digits = 2) == 1.00)],
     main = "Modularity of Observed Similarity Matrix vs. Random Matrices",
     xlim = c(0, 1),
     xlab = "Modularity")
abline(v = cutoff, col = "black", lty = 2)
abline(v = perturbRout$modularity[1, 1], col = "red")
dev.off()

# TODO: Maybe we could consider a homogeneity test to reject null of one cluster
# - Steinley and Brusco (2011)’s lower bound ratio (LBR) test, which determines 
#   whether the ratio of the within-cluster sum of squares for the 2-means solution 
#   to the sum-of-squares total (SSE^2 / SST) is < the lower bound of the ratio that 
#   is obtainable for splitting a multivariate normal distribution in half
# - Duda-Hart test (https://www.wiley.com/en-kr/Pattern+Classification%2C+2nd+Edition-p-9780471056690), 
#   which compares the same ratio (SSE^2 / SST) to a critical value of an approximate 
#   asymptotic distribution of the ratio under a multivariate normal null distribution