# ---------------------------------------------------------------------------- #
# Group with "semtree" -----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# Resource (source of example code below): https://brandmaier.github.io/semtree/
# - Note: Articles tab above seems more up to date than "browseVignettes("semtree")"
# - See papers at https://brandmaier.github.io/semtree/index.html
# - For plot of implied mixture model from another simulated data example, see
# https://brandmaier.github.io/semtree/articles/score-based-tests.html

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

groundhog.library("semtree", groundhog_day)

# ---------------------------------------------------------------------------- #
# Simulate data ----
# ---------------------------------------------------------------------------- #

# Simulate some data from a linear latent growth curve model (that is, a random intercept 
# and random slope over time). The dataset will be called growth.data. The dataset contains
# five observations for each individual (X1 to X5) and one predictor P1. The predictor is 
# dichotomous and predicts a (quite large) difference in mean slope.

set.seed(23)

N <- 1000
M <- 5
icept <- rnorm(N, 10, sd = 4)
slope <- rnorm(N, 3, sd = 1.2)
p1 <- sample(c(0, 1), size = N, replace = TRUE)
loadings <- 0:4

x <- (slope + p1 * 5) %*% t(loadings) + 
     matrix(rep(icept, each = M), byrow = TRUE, ncol = M) + 
     rnorm(N * M, sd = .08)

growth.data <- data.frame(x, factor(p1))
names(growth.data) <- c(paste0("X", 1:M), "P1")

# ---------------------------------------------------------------------------- #
# Specify an OpenMx model ----
# ---------------------------------------------------------------------------- #

# Specify a linear latent growth curve model using OpenMx’s path specification. The model has 
# five observed variables. Residual variances are assumed to be identical over time.

# Note: binary P1 covariate is not part of the model

manifests <- names(growth.data)[1:5]

growthCurveModel <- mxModel("Linear Growth Curve Model Path Specification",
                            type = "RAM",
                            manifestVars = manifests,
                            latentVars = c("intercept","slope"),
                            mxData(growth.data, type = "raw"),
                            # Residual variances
                            mxPath(
                              from = manifests,
                              arrows = 2,
                              free = TRUE,
                              values = c(.1, .1, .1, .1, .1),
                              labels = c("residual","residual","residual","residual","residual")
                            ),
                            # Latent variances and covariance
                            mxPath(
                              from = c("intercept","slope"),
                              arrows = 2,
                              connect = "unique.pairs",
                              free = TRUE,
                              values = c(2, 0, 1),
                              labels = c("vari", "cov", "vars")
                            ),
                            # Intercept loadings
                            mxPath(
                              from = "intercept",
                              to = manifests,
                              arrows = 1,
                              free = FALSE,
                              values = c(1, 1, 1, 1, 1)
                            ),
                            # Slope loadings
                            mxPath(
                              from = "slope",
                              to = manifests,
                              arrows = 1,
                              free = FALSE,
                              values = c(0, 1, 2, 3, 4)
                            ),
                            # Manifest means
                            mxPath(
                              from = "one",
                              to = manifests,
                              arrows = 1,
                              free = FALSE,
                              values = c(0, 0, 0, 0, 0)
                            ),
                            # Latent means
                            mxPath(
                              from = "one",
                              to = c("intercept", "slope"),
                              arrows = 1,
                              free = TRUE,
                              values = c(1, 1),
                              labels = c("meani", "means")
                            ))

# Fit the model to the entire dataset

growthCurveModel <- mxRun(growthCurveModel)

# ---------------------------------------------------------------------------- #
# Run a tree using default "naive" selection method ----
# ---------------------------------------------------------------------------- #

# Grow a SEM tree using the semtree function, which takes the model and the dataset as input. 
# If not specified otherwise, SEM tree will assume that all variables in the dataset which 
# are not observed variables [included in the SEM] are potential predictors

# Per R documentation, this creates a tree that recursively partitions a dataset such that the 
# partitions maximally differ with respect to the model-predicted distributions. Each resulting 
# subgroup (represented as a leaf in the tree) is represented by a SEM with a distinct set of 
# parameter estimates. Predictors can be of any data type.

# Use default "naive" selection method, which compares all possible split values to one 
# another over all predictors included in the dataset (see "semtree.control()" defaults)

tree_naive <- semtree(model = growthCurveModel, data = growth.data,
                      predictors = NULL) # By default, all variables that are in dataset and
                                         # not part of model are potential predictors

# Examine tree

summary(tree_naive)
plot(tree_naive)
tree_naive$control

tree_naive$model # Single-group SEM fit to full sample (N = 1000)
round(tree_naive$lr, 2) == 1683.14 # LR comparing single-group SEM to multigroup SEM (split per binary P1 covariate)
tree_naive$p            == 0       # p value for LR test

tree_naive$left_child$model  # SEM fit to left child subgroup (where P1 == 1; N = 526)
tree_naive$right_child$model # SEM fit to right child subgroup (where P1 == 0; N = 474)

# ---------------------------------------------------------------------------- #
# Run a tree using "score" selection method ----
# ---------------------------------------------------------------------------- #

# Tutorial: https://brandmaier.github.io/semtree/articles/score-based-tests.html

# Score-based tests for variable and split-point selection are preferable because 
# they are fast to compute, perform unbiased variable selection, and have better 
# statistical power than some other selection algorithms proposed earlier. See
# Arnold et al. (2021; https://doi.org/10.3389/fpsyg.2020.564403)

# Use "score" selection method and Bonferroni-correction to adjust for multiple 
# testing of predictors

ctrl <- semtree.control(method = "score", bonferroni = TRUE)

tree_score <- semtree(model = growthCurveModel, data = growth.data, 
                      control = ctrl)

# Examine tree

summary(tree_score)
plot(tree_score)
tree_score$control

# TODO: LRT is listed below, but don't score-based tests not use LRTs? Check Arnold et al. above.





tree_score$model # Single-group SEM fit to full sample (N = 1000)
round(tree_score$lr, 2) == 818.54 # LR comparing single-group SEM to multigroup SEM (split per binary P1 covariate)
tree_score$p.uncorrected # 1.52e-173 (p value before Bonferroni correction)
tree_score$p            == 0      # p value for LR test

tree_score$left_child$model  # SEM fit to left child subgroup (where P1 == 1; N = 526)
tree_score$right_child$model # SEM fit to right child subgroup (where P1 == 0; N = 474)

# Note: We'd have to specify the template SEM in OpenMx using either OpenMx or lavaan 
# syntax, and that SEM would be fit in all potential subgroups (allowing the parameter
# estimates to differ in each subgroup) split based on covariates not in the networks 
# (e.g., demographics). By contrast, S-GIMME identifies subgroups without covariates
# and conducts subgroup-specific model searches.