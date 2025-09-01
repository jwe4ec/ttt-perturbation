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
# group-level model for each person) that each person shares with every other person.
# Correlogram shows correlations among participants' numbers of sig. shared paths.

a <- cor(gimme_output$sim_matrix)

perturbR_path <- "./results/test_s-gimme/perturbR/"

pdf(paste0(perturbR_path, "corrplot.pdf"))
corrplot(a, method = "color")
dev.off()

# ---------------------------------------------------------------------------- #
# Evaluate robustness of subgroups by comparing subgroup assignments ----
# ---------------------------------------------------------------------------- #

# Note: "perturbR-vignette.html" above says that, per Karrer et al. (2008; https://doi.org/10.1103/PhysRevE.77.046119), 
# subgroups are robust if >= 20% of edges in the similarity matrix can be perturbed (i.e., alpha >= 20%) before the
# similarity value between the perturbed similarity matrix and the original, unperturbed matrix crosses the similarity 
# value when 20% of participants are randomly assigned to different subgroups

  # However, rather than emphasizing alpha >= 20% per se, Karrer et al. actually emphasize whether (a) the alpha when
  # the similarity value between the perturbed matrix and the original matrix crosses the similarity value when 20% of
  # participants are randomly assigned to different subgroups is greater than (b) the alpha when the similarity value
  # between the perturbed random matrix and the original random matrix crosses this value (see pp. 6-8). If so, then 
  # Karrer et al. consider the subgroups robust. Moreover, Karrer et al. also consider the differences between the two
  # similarity curves overall; even if the alpha at which both curves cross this value is similar, if the curve for the
  # perturbed observed matrix is distinct from the curve for the perturbed random matrix at higher values of alpha, then
  # Karrer et al. consider "some portion of the community structure found is relatively robust" (p. 8 and Figure 2d).

  # Note: Given that the similarity matrix doesn't directly specify what subgroup participants are in, "perturbR()" finds
  # the "true" subgroup membership of original, unperturbed similarity matrix using "walktrap.community()". It then
  # perturbs the network and finds the subgroup membership of perturbed similarity matrix using "walktrap.community()"
  # again. Both ARI and VI are computed by comparing the two sets of group memberships (using "arandi()" and "vi.dist()").

# For Adjusted Rand Index (ARI; higher values reflect greater similarity)

ari20mark <- perturbRout$ari20mark

round(ari20mark, 2) == .33 # Bottom line in ARI plot (when 20% of subgroup assignments are swapped)
perturbRout$percent[min(which(colMeans(perturbRout$ARI) < ari20mark))] = .61 # Alpha at intersection for observed matrix

  # "perturbR-vignette.html" emphasizes this comparison

(ari_at_20 <- round(mean(perturbRout$ARI[, which(round(perturbRout$percent, 2) == .20)]), 2)) == .88 # ARI at alpha of 20%

ari_at_20 > ari20mark # Thus, subgroups are robust to perturbation per ARI

  # But Karrer et al. emphasize that alpha at intersection for obs. matrix (.61) > alpha at intersection for random matrix 

# For Variation of Information (VI; lower values reflect greater similarity)

vi20mark <- perturbRout$vi20mark

round(vi20mark, 2) == 1.40 # Top line in VI plot (when 20% of subgroup assignments are swapped)
perturbRout$percent[min(which(colMeans(perturbRout$VI) > vi20mark))] == .61 # Alpha at intersection for observed matrix

  # "perturbR-vignette.html" emphasizes this comparison

(vi_at_20 <- round(mean(perturbRout$VI[, which(round(perturbRout$percent, 2) == .20)]), 2)) == .26 # VI at alpha of 20%

vi_at_20 < vi20mark # Thus, subgroups are robust to perturbation per VI

  # But Karrer et al. emphasize that alpha at intersection for obs. matrix (.61) > alpha at intersection for random matrix
  # (in this case, similarity curve for random matrix is never below the top line in VI plot, so never even intersects it)

# ---------------------------------------------------------------------------- #
# Evaluate robustness of subgroups by comparing modularity values ----
# ---------------------------------------------------------------------------- #

# Note: See Gates et al. (2017, p. 142) and Karrer et al. (2008, discussion of
# z-scores on pp. 2 and 8) for limitations of using modularity

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

# TODO: Maybe we could also consider a homogeneity test to reject null of one cluster
# - Steinley and Brusco (2011)’s lower bound ratio (LBR) test, which determines 
#   whether the ratio of the within-cluster sum of squares for the 2-means solution 
#   to the sum-of-squares total (SSE^2 / SST) is < the lower bound of the ratio that 
#   is obtainable for splitting a multivariate normal distribution in half
# - Duda-Hart test (https://www.wiley.com/en-kr/Pattern+Classification%2C+2nd+Edition-p-9780471056690), 
#   which compares the same ratio (SSE^2 / SST) to a critical value of an approximate 
#   asymptotic distribution of the ratio under a multivariate normal null distribution