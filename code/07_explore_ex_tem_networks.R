# ---------------------------------------------------------------------------- #
# Explore Example Temporal Networks From Actual Data -----
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

pkgs <- c("qgraph", "randomcoloR", "tidyr", "ggplot2", "cowplot")
groundhog.library(pkgs, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data, selected results, and adjacency matrices ----
# ---------------------------------------------------------------------------- #

# Data

  # Note: "_d" variables were centered by removing linear trend in "ttt-p1-main-analysis" 
  # repo, whereas "_d2" variables were centered by removing linear trend and weekend effect in
  # "recenter_data.R" of present repo

load("./data/recentered/data_var_perturb2.RDS")

dat <- data_var_perturb2

# Results from idiographic VAR models

  # Temporal results from Mplus using "_d" variables

load("./results/from_ttt-p1-main-analysis/extracted/results_var.RDS")

  # All results from GIMME using "_d2" variables

gimme_var_res_ls     <- readRDS("./results/gimme/raw/var_res_ls.RDS")
gimme_var_res_ls_std <- readRDS("./results/gimme/raw_std/var_res_ls_std.RDS") # TODO: Likely remove this





# Adjacency matrices for temporal results from idiographic VAR models

adj_mats_path <- "./results/adj_mats/"

  # From Mplus using "_d" variables

load(paste0(adj_mats_path, "thres_adj_mats_var.Rdata"))
load(paste0(adj_mats_path, "satur_adj_mats_var.Rdata"))

  # From GIMME using "_d2" variables

load(paste0(adj_mats_path, "gimme_adj_mats_var.Rdata"))
load(paste0(adj_mats_path, "gimme_adj_mats_var_std.Rdata")) # TODO: Likely remove this





# ---------------------------------------------------------------------------- #
# Explore number of significant autoregressive and cross-lagged effects ----
# ---------------------------------------------------------------------------- #

# Most participants have few significant edges

n_sig_edges <- unlist(lapply(thres_adj_mats_var, function(x) sum(x != 0)))

mean(n_sig_edges)   # 3.51
median(n_sig_edges) # 3
sd(n_sig_edges)     # 3.15

table(n_sig_edges)

hist(n_sig_edges,
     breaks = 21, right = FALSE,
     main = "Distribution of Significant Edges",
     xlab = "Number of Significant Edges",
     ylab = "Number of Participants",
     xlim = c(0, 20))

# Order of participants by number of significant edges

sort(n_sig_edges)

# Randomly select example participants with (a) 1 and (b) 3-4 significant edges.
# Example participant with 10 significant edges was manually chosen.

low_sig_edges_ids <- names(n_sig_edges[n_sig_edges == 1])
med_sig_edges_ids <- names(n_sig_edges[n_sig_edges %in% c(3, 4)])

low_sig_edges_ex_id <- sample(low_sig_edges_ids, 1)
med_sig_edges_ex_id <- sample(med_sig_edges_ids, 1)

if (low_sig_edges_ex_id != "999341") {
  stop("low_sig_edges_ex_id should be 999341. Restart R and rerun script.")
}
if (med_sig_edges_ex_id != "861114") {
  stop("med_sig_edges_ex_id should be 861114. Restart R and rerun script.")
}

high_sig_edges_ex_id <- "326177"
med_sig_edges_ex_id  <- "861114"
low_sig_edges_ex_id  <- "999341"

n_sig_edges[med_sig_edges_ex_id] == 3 # "861114" has 3 significant edges

# Determine number of participants for which each edge is significant

thres_adj_mats_var_sig_edges <- lapply(thres_adj_mats_var, function(x) (x != 0))

thres_adj_mats_var_sig_freq <- Reduce("+", thres_adj_mats_var_sig_edges)

range(thres_adj_mats_var_sig_freq) == c(0, 8)

# Determine percentage of participants for which each edge is significant

n <- length(thres_adj_mats_var)

thres_adj_mats_var_sig_incl_perc <- round((thres_adj_mats_var_sig_freq / n) * 100, 1)

range(thres_adj_mats_var_sig_incl_perc) == c(0.0, 15.1)

# Show number (and percentage) of participants for which each edge is significant

thres_adj_mats_var_sig_freq_incl_perc <- thres_adj_mats_var_sig_freq
thres_adj_mats_var_sig_freq_incl_perc[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_freq_incl_perc)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_freq_incl_perc)) {
    thres_adj_mats_var_sig_freq_incl_perc[i, j] <- paste0(thres_adj_mats_var_sig_freq[i, j],
                                                          " (",
                                                          format(thres_adj_mats_var_sig_incl_perc[i, j], 
                                                                 nsmall = 1, trim = TRUE),
                                                          ")")
  }
}

thres_adj_mats_var_sig_freq_incl_perc

# Determine range of significant edge weights across participants

thres_adj_mats_var_ever_sig <- thres_adj_mats_var_sig_freq > 0

thres_adj_mats_var_sig_range <- thres_adj_mats_var_ever_sig
thres_adj_mats_var_sig_range[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_range)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_range)) {
    if (thres_adj_mats_var_ever_sig[i, j] == TRUE) {
      # Obtain significant edge weights
      
      element_values <- sapply(thres_adj_mats_var, function(x) x[i, j])
      
      element_values <- element_values[element_values != 0]
      
      # Compute range of significant edge weights
      
      range <- format(round(range(element_values), 2),
                      nsmall = 2, trim = TRUE)
      
      thres_adj_mats_var_sig_range[i, j] <- paste(range, collapse = ", ")
    }
  }
}

thres_adj_mats_var_sig_range

# Show number of participants for which each edge is significant along with
# range of significant edges

thres_adj_mats_var_sig_freq_range <- thres_adj_mats_var_sig_freq
thres_adj_mats_var_sig_freq_range[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_freq_range)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_freq_range)) {
    thres_adj_mats_var_sig_freq_range[i, j] <- paste0(thres_adj_mats_var_sig_freq[i, j],
                                                      " (",
                                                      thres_adj_mats_var_sig_range[i, j], 
                                                      ")")
  }
}

thres_adj_mats_var_sig_freq_range

# ---------------------------------------------------------------------------- #
# Explore number of observations ----
# ---------------------------------------------------------------------------- #

dat <- as.data.frame(dat)

# Identify approximate number of observations

sum(!is.na(dat[dat$lifepak_id == high_sig_edges_ex_id, "bad"])) == 98
sum(!is.na(dat[dat$lifepak_id == med_sig_edges_ex_id, "bad"])) == 93
sum(!is.na(dat[dat$lifepak_id == low_sig_edges_ex_id, "bad"])) == 82

# Identify distribution of approximate number of observations (range: 62-102, median: 86)

dat_ls <- split(dat, dat$lifepak_id)

hist(unlist(lapply(dat_ls, function(x) sum(!is.na(x$bad_d)))),
     main = "Distribution of Number of Observations",
     xlab = "Number of Observations",
     ylab = "Number of Participants",
     xlim = c(60, 105))

all(range(unlist(lapply(dat_ls, function(x) sum(!is.na(x$bad_d)))))      == c(62, 102))
median(unlist(lapply(dat_ls,    function(x) sum(!is.na(x$bad_d)))))      == 86

# ---------------------------------------------------------------------------- #
# Investigate missing data patterns ----
# ---------------------------------------------------------------------------- #

# Plot observations over time for participants in first quartile of number of observations (74)

q1 <- quantile(unlist(lapply(dat_ls,  function(x) sum(!is.na(x$bad_d)))), .25)
q1 == 74

q1_ids <- names(dat_ls)[unlist(lapply(dat_ls, function(x) sum(!is.na(x$bad_d)))) < q1]

# Define function to create presence/absence plot with row for each node

plot_presence <- function(part_data, lifepak_id) {
  target_cols <- paste0(c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad"), "_d")
  
  plot_data <- part_data[, c("lifepak_id", "bin_no_adj", target_cols)]
  
  plot_data <- pivot_longer(plot_data, all_of(target_cols), names_to = "variable")
  
  plot_data$present <- !(is.na(plot_data$value))
  
  ggplot(plot_data, aes(x = bin_no_adj, y = variable, fill = present)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("#F0F0F0", "black")) +
    theme_classic() +
    labs(x = "Time", y = "Variable", fill = "Present",
         title = paste0("ID ", lifepak_id))
}

# Run function for participants in first quartile of number of observations

missing_data_plots_path <- "./results/missing_data_plots/"

dir.create(missing_data_plots_path)

pdf(file = paste0(missing_data_plots_path, "presence_q1_ids.pdf"))

dat_ls_q1_ids <- dat_ls[q1_ids]

plot_ls <- lapply(names(dat_ls_q1_ids), function(lifepak_id) {
  plot_presence(dat_ls_q1_ids[[lifepak_id]], lifepak_id)
})

  # Extract legend from first plot for shared legend

legend_bottom <- get_legend(plot_ls[[1]] + 
                              guides(color = guide_legend(nrow = 1)) +
                              theme(legend.position = "bottom"))

plot_ls <- lapply(plot_ls, function(plot) {
  plot <- plot + theme(legend.position = "none")
})

  # TODO (Condense code; "for" loop didn't work): Include 6 plots per page

pages_with_plots <- split(1:length(plot_ls), ceiling(1:length(plot_ls) / 6))

plots <- plot_grid(plotlist = plot_ls[pages_with_plots[[1]]], ncol = 2, nrow = 3)
plot_grid(plots, legend_bottom, ncol = 1, rel_heights = c(1, .1))

plots <- plot_grid(plotlist = plot_ls[pages_with_plots[[2]]], ncol = 2, nrow = 3)
plot_grid(plots, legend_bottom, ncol = 1, rel_heights = c(1, .1))

plots <- plot_grid(plotlist = plot_ls[pages_with_plots[[3]]], ncol = 2, nrow = 3)
plot_grid(plots, legend_bottom, ncol = 1, rel_heights = c(1, .1))

dev.off()

# TODO (Consider computing without missing observations at night): Define function 
# to compute overall mean of each node's mean units of "bin_no_adj" between each 
# observation (including missing observations at night)

compute_bin_no_adj_present_diff_m_overall <- function(part_data) {
  target_cols <- paste0(c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad"), "_d")
  
  bin_no_adj_present <- list()
  
  for (target_col in target_cols) {
    bin_no_adj_present[[target_col]] <- part_data$bin_no_adj[!is.na(part_data[, target_col])]
  }
  
  bin_no_adj_present_diff <- lapply(bin_no_adj_present, diff)
  
  bin_no_adj_present_diff_m <- lapply(bin_no_adj_present_diff, mean)
  
  bin_no_adj_present_diff_m_overall <- mean(unlist(bin_no_adj_present_diff_m))
  
  return(bin_no_adj_present_diff_m_overall)
}

# Run function for all participants

bin_no_adj_present_diff_m_overall <- unlist(lapply(dat_ls, 
                                                   compute_bin_no_adj_present_diff_m_overall))

# Plot distribution. Participants with fewest observations (< Q1) are same as those
# with greatest distance (> Q3) between each observation

hist(bin_no_adj_present_diff_m_overall)

q3 <- quantile(bin_no_adj_present_diff_m_overall, .75)
round(q3, 5) == 2.69863

q3_ids <- names(bin_no_adj_present_diff_m_overall)[bin_no_adj_present_diff_m_overall > q3]

all(q1_ids %in% q3_ids)

# ---------------------------------------------------------------------------- #
# Restrict to example participants ----
# ---------------------------------------------------------------------------- #

# TODO: Temporarily restrict sample until ready to analyze full sample





retain_ids <- c(high_sig_edges_ex_id = high_sig_edges_ex_id,
                med_sig_edges_ex_id  = med_sig_edges_ex_id, 
                low_sig_edges_ex_id  = low_sig_edges_ex_id)

dat_ls <- dat_ls[retain_ids]

thres_adj_mats_var <- thres_adj_mats_var[retain_ids]
satur_adj_mats_var <- satur_adj_mats_var[retain_ids]

# TODO: Temporarily save for use in prior script "fit_gimme_var_models.R"

dir.create("./data/temp/")

save(retain_ids, file = "./data/temp/retain_ids.RDS")






# ---------------------------------------------------------------------------- #
# Test whether weekend indicator predicts missingness ----
# ---------------------------------------------------------------------------- #

# TODO: Define function to test whether weekend indicator predicts missingness. For 
# now, use presence/absence of "bad_d" as a proxy for that of all node variables.

test_wend_miss <- function(part_data) {
  part_data$bad_miss <- NA
  
  part_data$bad_miss <- as.integer(is.na(part_data$bad))
  
  # TODO: First need to fill in "response_time_wday" and "response_time_wend" above

  # TODO: Test whether weekend indicator (0 for weekday, 1 for weekend) predicts missingness
  
}





# TODO: Run function

lapply(dat_ls, test_wend_miss)





# ---------------------------------------------------------------------------- #
# Explore item distributions ----
# ---------------------------------------------------------------------------- #

# For example participant with high number of significant edges

# Before detrending

# par(mfrow = c(4, 2))
# hist(dat_ls[[high_sig_edges_ex_id]]$bad)
# hist(dat_ls[[high_sig_edges_ex_id]]$control)
# hist(dat_ls[[high_sig_edges_ex_id]]$energy)
# hist(dat_ls[[high_sig_edges_ex_id]]$focus)
# hist(dat_ls[[high_sig_edges_ex_id]]$fun)
# hist(dat_ls[[high_sig_edges_ex_id]]$interest)
# hist(dat_ls[[high_sig_edges_ex_id]]$movement)
# hist(dat_ls[[high_sig_edges_ex_id]]$sad)
# par(mfrow = c(1, 1))

# After detrending

# par(mfrow = c(4, 2))
# hist(dat_ls[[high_sig_edges_ex_id]]$bad_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$control_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$energy_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$focus_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$fun_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$interest_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$movement_d)
# hist(dat_ls[[high_sig_edges_ex_id]]$sad_d)
# par(mfrow = c(1, 1))

# Not done for example participants with medium or low numbers of significant edges

# ---------------------------------------------------------------------------- #
# Plot temporal networks ----
# ---------------------------------------------------------------------------- #

# Define function to plot temporal network

plot_network <- function(tem, lifepak_id, thres) {
  # Get node variables and rename as labels
  
  labels <- row.names(tem)
  
  labels[labels == "bad"]      <- "Bad\nSelf"
  labels[labels == "control"]  <- "Lack\nControl"
  labels[labels == "energy"]   <- "Fatigue"
  labels[labels == "focus"]    <- "Lack\nFocus"
  labels[labels == "fun"]      <- "Inaction"
  labels[labels == "interest"] <- "Lack\nInterest"
  labels[labels == "movement"] <- "Slower\nor \nFidgety"
  labels[labels == "sad"]      <- "Sad"
  
  # Define plotting options
  
    # Include edge labels only for thresholded networks (too cluttered in saturated networks)
  
  if (thres == "satur") {
    edge_labels <- FALSE
  } else {
    edge_labels <- TRUE
  }
  
    tem_to_plot <- tem
    max_for_plot <- max(abs(tem))
    
    tem_title <- paste0("Temporal (ID ", lifepak_id, ")")

    plots_path <- "./results/network_plots/"
    
    tem_plot_filename  <- paste0(plots_path, lifepak_id, "_tem_plot_", thres)

  # Plot circle graphs
  
  tem_plot <- qgraph(tem_to_plot,
                     edge.labels = edge_labels,
                     edge.label.color = "black",
                     edge.label.margin = .01,
                     edge.label.cex = .9,
                     layout = "circle", 
                     labels = labels, 
                     theme = "colorblind",
                     asize = 7, 
                     vsize = 8, 
                     label.cex = c(rep(.8, 6), .7), 
                     mar = rep(6, 4), 
                     title = tem_title,
                     label.scale = FALSE,
                     maximum = max_for_plot,
                     esize = 10)
  
  # Export plot
  
  dir.create(plots_path, recursive = TRUE, showWarnings = FALSE)
  
  qgraph(tem_plot,
         filetype = "pdf",
         filename = tem_plot_filename)
  
  invisible()
}

# Run function for thresholded and saturated networks

dir.create("./results/network_plots/")

lapply(names(thres_adj_mats_var), function(lifepak_id) {
  plot_network(thres_adj_mats_var[[lifepak_id]], lifepak_id, "thres_a05")
})

lapply(names(satur_adj_mats_var), function(lifepak_id) {
  plot_network(satur_adj_mats_var[[lifepak_id]], lifepak_id, "satur")
})

# ---------------------------------------------------------------------------- #
# Compute standardized detrended variables ----
# ---------------------------------------------------------------------------- #

# Define function to standardize detrended variables

compute_d_std_vars <- function(part_data) {
  d_cols <- paste0(c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad"), "_d") # TODO: HERE
  
  for (d_col in d_cols) {
    d_std_col <- paste0(d_col, "_std")
    
    part_data[, d_std_col] <- as.numeric(scale(part_data[, d_col]))
  }
  
  return(part_data)
}

# Run function

dat_ls <- lapply(dat_ls, compute_d_std_vars)

# ---------------------------------------------------------------------------- #
# Define function to compute predicted values ----
# ---------------------------------------------------------------------------- #

# TODO: Try streamlining this by multiplying adjacency matrix by vector of starting values





# Define function to compute predicted values over desired time points from adjacency 
# matrix and desired starting values

compute_pred <- function(adj_mat, n_timepoints, start_list) {
  init_vec      <- rep(NA, n_timepoints)
  
  bad_pred      <- init_vec
  control_pred  <- init_vec
  energy_pred   <- init_vec
  focus_pred    <- init_vec
  fun_pred      <- init_vec
  interest_pred <- init_vec
  movement_pred <- init_vec
  sad_pred      <- init_vec
  
  for (i in 1:n_timepoints) {
    if (i == 1) {
      bad_pred[i]      <- start_list$bad
      control_pred[i]  <- start_list$control
      energy_pred[i]   <- start_list$energy
      focus_pred[i]    <- start_list$focus
      fun_pred[i]      <- start_list$fun
      interest_pred[i] <- start_list$interest
      movement_pred[i] <- start_list$movement
      sad_pred[i]      <- start_list$sad
    } else if (i > 1) {
      bad_l1      <- bad_pred[i - 1]
      control_l1  <- control_pred[i - 1]
      energy_l1   <- energy_pred[i - 1]
      focus_l1    <- focus_pred[i - 1]
      fun_l1      <- fun_pred[i - 1]
      interest_l1 <- interest_pred[i - 1]
      movement_l1 <- movement_pred[i - 1]
      sad_l1      <- sad_pred[i - 1]
      
      bad_pred[i]      <- bad_l1*adj_mat["bad", "bad"]      + control_l1*adj_mat["control", "bad"]      + energy_l1*adj_mat["energy", "bad"]      + focus_l1*adj_mat["focus", "bad"]      + fun_l1*adj_mat["fun", "bad"]      + interest_l1*adj_mat["interest", "bad"]      + movement_l1*adj_mat["movement", "bad"]      + sad_l1*adj_mat["sad", "bad"]
      control_pred[i]  <- bad_l1*adj_mat["bad", "control"]  + control_l1*adj_mat["control", "control"]  + energy_l1*adj_mat["energy", "control"]  + focus_l1*adj_mat["focus", "control"]  + fun_l1*adj_mat["fun", "control"]  + interest_l1*adj_mat["interest", "control"]  + movement_l1*adj_mat["movement", "control"]  + sad_l1*adj_mat["sad", "control"]
      energy_pred[i]   <- bad_l1*adj_mat["bad", "energy"]   + control_l1*adj_mat["control", "energy"]   + energy_l1*adj_mat["energy", "energy"]   + focus_l1*adj_mat["focus", "energy"]   + fun_l1*adj_mat["fun", "energy"]   + interest_l1*adj_mat["interest", "energy"]   + movement_l1*adj_mat["movement", "energy"]   + sad_l1*adj_mat["sad", "energy"]
      focus_pred[i]    <- bad_l1*adj_mat["bad", "focus"]    + control_l1*adj_mat["control", "focus"]    + energy_l1*adj_mat["energy", "focus"]    + focus_l1*adj_mat["focus", "focus"]    + fun_l1*adj_mat["fun", "focus"]    + interest_l1*adj_mat["interest", "focus"]    + movement_l1*adj_mat["movement", "focus"]    + sad_l1*adj_mat["sad", "focus"]
      fun_pred[i]      <- bad_l1*adj_mat["bad", "fun"]      + control_l1*adj_mat["control", "fun"]      + energy_l1*adj_mat["energy", "fun"]      + focus_l1*adj_mat["focus", "fun"]      + fun_l1*adj_mat["fun", "fun"]      + interest_l1*adj_mat["interest", "fun"]      + movement_l1*adj_mat["movement", "fun"]      + sad_l1*adj_mat["sad", "fun"]
      interest_pred[i] <- bad_l1*adj_mat["bad", "interest"] + control_l1*adj_mat["control", "interest"] + energy_l1*adj_mat["energy", "interest"] + focus_l1*adj_mat["focus", "interest"] + fun_l1*adj_mat["fun", "interest"] + interest_l1*adj_mat["interest", "interest"] + movement_l1*adj_mat["movement", "interest"] + sad_l1*adj_mat["sad", "interest"]
      movement_pred[i] <- bad_l1*adj_mat["bad", "movement"] + control_l1*adj_mat["control", "movement"] + energy_l1*adj_mat["energy", "movement"] + focus_l1*adj_mat["focus", "movement"] + fun_l1*adj_mat["fun", "movement"] + interest_l1*adj_mat["interest", "movement"] + movement_l1*adj_mat["movement", "movement"] + sad_l1*adj_mat["sad", "movement"]
      sad_pred[i]      <- bad_l1*adj_mat["bad", "sad"]      + control_l1*adj_mat["control", "sad"]      + energy_l1*adj_mat["energy", "sad"]      + focus_l1*adj_mat["focus", "sad"]      + fun_l1*adj_mat["fun", "sad"]      + interest_l1*adj_mat["interest", "sad"]      + movement_l1*adj_mat["movement", "sad"]      + sad_l1*adj_mat["sad", "sad"]
    }
  }
  
  pred <- data.frame(t             = 1:n_timepoints,
                     bad_pred      = bad_pred,
                     control_pred  = control_pred,
                     energy_pred   = energy_pred,
                     focus_pred    = focus_pred,
                     fun_pred      = fun_pred,
                     interest_pred = interest_pred,
                     movement_pred = movement_pred,
                     sad_pred      = sad_pred)
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Define function for defining starting values from detrended values in data ----
# ---------------------------------------------------------------------------- #

# Define function for defining starting values from participant's standardized 
# detrended values in data at a given time point "j"

define_start <- function(part_data, j) {                                       # TODO: HERE
  start_list <- list(bad      = part_data[j, "bad_d_std"],
                     control  = part_data[j, "control_d_std"],
                     energy   = part_data[j, "energy_d_std"],
                     focus    = part_data[j, "focus_d_std"],
                     fun      = part_data[j, "fun_d_std"],
                     interest = part_data[j, "interest_d_std"],
                     movement = part_data[j, "movement_d_std"],
                     sad      = part_data[j, "sad_d_std"])
}

# ---------------------------------------------------------------------------- #
# Compute all predicted values starting from participant's detrended values at baseline ----
# ---------------------------------------------------------------------------- #

# Compute number of study time points for each participant

n_study_timepoints <- lapply(dat_ls, nrow)

# Define starting values for each participant from detrended values at baseline

start_list_bl <- lapply(dat_ls, define_start, 1)                                  # TODO: HERE

# Compute predicted values for thresholded and saturated networks (a) over study 
# period and (b) into future 

pred_study_bl_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], start_list_bl[[lifepak_id]])
})
pred_400_bl_thres_a05   <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], 400,                              start_list_bl[[lifepak_id]])
})

pred_study_bl_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], start_list_bl[[lifepak_id]])
})
pred_400_bl_satur       <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], 400,                              start_list_bl[[lifepak_id]])
})

names(pred_study_bl_thres_a05) <- names(thres_adj_mats_var)
names(pred_400_bl_thres_a05)   <- names(thres_adj_mats_var)

names(pred_study_bl_satur)     <- names(satur_adj_mats_var)
names(pred_400_bl_satur)       <- names(satur_adj_mats_var)                       # TODO: HERE

# ---------------------------------------------------------------------------- #
# Define function to compute "k" predicted values starting from each time point ----
# ---------------------------------------------------------------------------- #

# Define function to compute "k" predicted values over desired time points ("iterations") from
# adjacency matrix, starting each iteration from observed value at that iteration's time point

compute_k_pred <- function(part_data, adj_mat, k, iterations) {
  pred <- data.frame()
  
  for (iter in 1:iterations) {
    start_list <- define_start(part_data, iter)                # TODO: HERE
    
    iter_pred <- compute_pred(adj_mat, k, start_list)
    
    names(iter_pred)[names(iter_pred) == "t"] <- "iter_t"
    
    iter_pred$iter <- iter
    iter_pred <- iter_pred[, c("iter", names(iter_pred)[names(iter_pred) != "iter"])]
    
    pred <- rbind(pred, iter_pred)
  }
  
  pred$t <- pred$iter + pred$iter_t - 1
  pred <- pred[, c("t", names(pred)[names(pred) != "t"])]
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Define function to compute signed prediction errors ----
# ---------------------------------------------------------------------------- #

# Define function to compute signed prediction errors for "k" predicted values
# starting from each time point

compute_pred_error <- function(part_data, pred) {
  names(part_data)[names(part_data) == "bin_no_adj"] <- "t"
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  target_cols <- paste0(vars, "_d_std")                               # TODO: HERE
  part_data_tmp <- part_data[, c("t", target_cols)]
  
  pred <- merge(pred, part_data_tmp, by = "t", all.x = TRUE)
  pred <- pred[order(pred$iter, pred$iter_t), ]
  
  # Compute signed prediction error
  
  for (var in vars) {
    pred[, paste0(var, "_error")] <- pred[, paste0(var, "_d_std")] - pred[, paste0(var, "_pred")]      # TODO: HERE
  }
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Define function to compute "diff_over_obs_sd" at each "iter_t" ----
# ---------------------------------------------------------------------------- #

# Define function to compute "diff_over_obs_sd", or (SD of observed data - SD of
# signed prediction errors) / SD of observed data, at each "iter_t" for "k" predicted 
# values starting from each time point

compute_diff_over_obs_sd <- function(pred_error) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  iter_t_values <- unique(pred_error$iter_t)
  
  res <- data.frame(iter_t = iter_t_values)
  
  for (iter_t in iter_t_values) {
    pred_error_iter_t <- pred_error[pred_error$iter_t == iter_t, ]
    
    for (var in vars) {
      obs_col   <- paste0(var, "_d_std")                              # TODO: HERE
      error_col <- paste0(var, "_error")
      
      # Restrict to rows where variable's prediction errors could be computed
      
      pred_error_iter_t_var <- pred_error_iter_t[!is.na(pred_error_iter_t[, error_col]), ]
      
      n_errors <- nrow(pred_error_iter_t_var)
      
      obs_sd   <- sd(pred_error_iter_t_var[, obs_col])
      error_sd <- sd(pred_error_iter_t_var[, error_col])
      
      diff_over_obs_sd <- (obs_sd - error_sd) / obs_sd
      
      res[res$iter_t == iter_t, paste0(var, "_n_errors")]         <- n_errors
      res[res$iter_t == iter_t, paste0(obs_col,   "_sd")]         <- obs_sd
      res[res$iter_t == iter_t, paste0(error_col, "_sd")]         <- error_sd
      res[res$iter_t == iter_t, paste0(var, "_diff_over_obs_sd")] <- diff_over_obs_sd
    }
  }
  
  res <- round(res, 3)

  return(res)
}

# ---------------------------------------------------------------------------- #
# Compute 4 predicted values starting from participant's detrended values at each time point ----
# ---------------------------------------------------------------------------- #

# Compute predicted values for saturated and thresholded networks over study period

pred_study_4_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], satur_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]])
})

pred_study_4_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], thres_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]])
})

names(pred_study_4_satur)     <- names(satur_adj_mats_var)
names(pred_study_4_thres_a05) <- names(thres_adj_mats_var)                           # TODO: HERE

# Compute signed prediction error

pred_error_study_4_satur     <- lapply(names(dat_ls), function(lifepak_id) {
  compute_pred_error(dat_ls[[lifepak_id]], pred_study_4_satur[[lifepak_id]])
})

pred_error_study_4_thres_a05 <- lapply(names(dat_ls), function(lifepak_id) {
  compute_pred_error(dat_ls[[lifepak_id]], pred_study_4_thres_a05[[lifepak_id]])
})

names(pred_error_study_4_satur)     <- names(dat_ls)
names(pred_error_study_4_thres_a05) <- names(dat_ls)                                # TODO: HERE

# TODO (some "diff_over_obs_sd" values are negative because sometimes "error_sd" is 
# greater than "obs_sd"): Compute "diff_over_obs_sd" at each "iter_t"

diff_over_obs_sd_study_4_satur     <- lapply(pred_error_study_4_satur,     compute_diff_over_obs_sd)
diff_over_obs_sd_study_4_thres_a05 <- lapply(pred_error_study_4_thres_a05, compute_diff_over_obs_sd)      # TODO: HERE





# ---------------------------------------------------------------------------- #
# Compute all predicted values starting from participant's max for one node and 0 for others ----
# ---------------------------------------------------------------------------- #

# Define various starting values for each participant

define_start_max_one_0_others <- function(part_data) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  start_list_max_one_0_others_element <- vector("list", length = length(vars))
  names(start_list_max_one_0_others_element) <- vars
  start_list_max_one_0_others_element[ ] <- 0
  
  start_list_max_one_0_others <- vector("list", length = length(vars))
  names(start_list_max_one_0_others) <- paste0("max_", vars, "_d_std")
  start_list_max_one_0_others[ ] <- list(start_list_max_one_0_others_element)
  
  start_list_max_one_0_others$max_bad_d_std$bad           <- max(part_data["bad_d_std"],      na.rm = TRUE)
  start_list_max_one_0_others$max_control_d_std$control   <- max(part_data["control_d_std"],  na.rm = TRUE)
  start_list_max_one_0_others$max_energy_d_std$energy     <- max(part_data["energy_d_std"],   na.rm = TRUE)
  start_list_max_one_0_others$max_focus_d_std$focus       <- max(part_data["focus_d_std"],    na.rm = TRUE)
  start_list_max_one_0_others$max_fun_d_std$fun           <- max(part_data["fun_d_std"],      na.rm = TRUE)
  start_list_max_one_0_others$max_interest_d_std$interest <- max(part_data["interest_d_std"], na.rm = TRUE)
  start_list_max_one_0_others$max_movement_d_std$movement <- max(part_data["movement_d_std"], na.rm = TRUE)
  start_list_max_one_0_others$max_sad_d_std$sad           <- max(part_data["sad_d_std"],      na.rm = TRUE)
  
  return(start_list_max_one_0_others)
}

start_list_max_one_0_others <- lapply(dat_ls, define_start_max_one_0_others)

# Define function to compute predicted values from various starting values

compute_pred_various_start <- function(adj_mats_var, n_timepoints, various_start_lists, various_start_list_names) {
  various_pred_lists <- vector("list", length(various_start_list_names))
  names(various_pred_lists) <- various_start_list_names
  
  for (start_list_name in various_start_list_names) {
    various_pred_lists[[start_list_name]] <- lapply(names(adj_mats_var), function(lifepak_id) {
      if (length(n_timepoints) > 1) {
        compute_pred(adj_mats_var[[lifepak_id]], n_timepoints[[lifepak_id]], various_start_lists[[lifepak_id]][[start_list_name]])
      } else if (length(n_timepoints == 1)) {
        compute_pred(adj_mats_var[[lifepak_id]], n_timepoints              , various_start_lists[[lifepak_id]][[start_list_name]])
      }
    })
    
    names(various_pred_lists[[start_list_name]]) <- names(adj_mats_var)
  }
  
  return(various_pred_lists)
}

# Run function to compute predicted values for thresholded and saturated networks 
# (a) over study period and (b) into future

start_list_max_one_0_others_names <- paste0("max_", c("bad_d_std", "control_d_std", "energy_d_std", "focus_d_std",
                                                      "fun_d_std", "interest_d_std", "movement_d_std", "sad_d_std"))

pred_study_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, n_study_timepoints, start_list_max_one_0_others, start_list_max_one_0_others_names)
pred_400_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, 400,                start_list_max_one_0_others, start_list_max_one_0_others_names)

pred_study_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, n_study_timepoints, start_list_max_one_0_others, start_list_max_one_0_others_names)
pred_400_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, 400,                start_list_max_one_0_others, start_list_max_one_0_others_names)

# TODO: Adapt as needed and run for GIMME results





# ---------------------------------------------------------------------------- #
# Plot predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted values starting from one time point or "k" predicted
# values starting many time points (pred_start_t_points = "one" or "many"). Predicted values 
# are plotted as (green) lines, points, or both (pred_plot_type = "l", "p", or "b"); observed
# values are plotted as (black) points. Options include (a) restricting displayed range of 
# time points (view_t_min and view_t_max) and, for "k" predicted values starting from
# many time points, (b) using a different color for each iteration (iter_colors = TRUE)

plot_pred_obs <- function(pred_df, obs_df, pred_start_t_points, pred_plot_type, plot_name, plot_title,
                          view_t_min = NULL, view_t_max = NULL, iter_colors = NULL) {
  obs_df$t <- 1:nrow(obs_df)
  
  # Optionally restrict displayed range of time points
  
  if (!is.null(view_t_min) & !is.null(view_t_max)) {
    obs_df  <- obs_df[obs_df$t >= view_t_min & obs_df$t <= view_t_max, ]
    pred_df <- pred_df[pred_df$t >= view_t_min & pred_df$t <= view_t_max, ]
  }
  
  # Define plot settings
  
  xlab <- "Time"
  ylab <- "Detrended Value"
  ylim_ll <- -10
  ylim_ul <- 10
  lwd <- 1.5
  pch <- 16
  
  if (pred_start_t_points == "one") {
    color_pred <- "green"
  } else if (pred_start_t_points == "many") {
    n_iterations <- max(pred_df$iter)
    
    if (is.null(iter_colors)) {
      color_pred <- rep("green", n_iterations)
    } else if (iter_colors == TRUE) {
      color_pred <- distinctColorPalette(n_iterations)
    }
  }
  
  vars       <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  pred_cols <- paste0(vars, "_pred")
  obs_cols  <- paste0(vars, "_d_std")                                          # TODO: HERE
  
    # Check that y-axis spans range of predicted and observed values
  
  pred_cols_min <- min(pred_df[, pred_cols], na.rm = TRUE)
  pred_cols_max <- max(pred_df[, pred_cols], na.rm = TRUE)
  
  obs_cols_min  <- min(obs_df[, obs_cols],   na.rm = TRUE)
  obs_cols_max  <- max(obs_df[, obs_cols],   na.rm = TRUE)
  
  if (pred_cols_min < ylim_ll | obs_cols_min < ylim_ll) {
    stop(paste0("Make lower limit of 'ylim' <= ", min(pred_cols_min, obs_cols_min)))
  }
  if (pred_cols_max > ylim_ul | obs_cols_max > ylim_ul) {
    stop(paste0("Make upper limit of 'ylim' >= ", max(pred_cols_max, obs_cols_max)))
  }

  # Create plots
  
  pdf(paste0("./results/pred_values/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    pred_col  <- pred_cols[i]
    obs_col   <- obs_cols[i]
    var_label <- var_labels[i]
    
    # Start with empty plot
    
    plot(pred_df$t, pred_df[, pred_col], main = var_label, 
         type = "n", xlab = xlab, ylab = ylab, ylim = c(ylim_ll, ylim_ul))
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    # Plot predicted values
    
    if (pred_start_t_points == "one") {
      if (pred_plot_type        == "l") {
        lines(pred_df$t,  pred_df[, pred_col], lwd = lwd, col = color_pred)
      } else if (pred_plot_type == "p") {
        points(pred_df$t, pred_df[, pred_col], pch = pch, col = color_pred)
      } else if (pred_plot_type == "b") {
        lines(pred_df$t,  pred_df[, pred_col], lwd = lwd, col = color_pred)
        points(pred_df$t, pred_df[, pred_col], pch = pch, col = color_pred)
      }
    } else if (pred_start_t_points == "many") {
      for (j in 1:n_iterations) {
        iter_pred  <- pred_df[pred_df$iter == j, ]
        iter_color <- color_pred[j]
        
        if (pred_plot_type        == "l") {
          lines(iter_pred$t,  iter_pred[, pred_col], lwd = lwd, col = iter_color)
        } else if (pred_plot_type == "p") {
          points(iter_pred$t, iter_pred[, pred_col], pch = pch, col = iter_color)
        } else if (pred_plot_type == "b") {
          lines(iter_pred$t,  iter_pred[, pred_col], lwd = lwd, col = iter_color)
          points(iter_pred$t, iter_pred[, pred_col], pch = pch, col = iter_color)
        }
      }
    }
    
    # Plot observed values as points
    
    points(obs_df$t, obs_df[, obs_col])
    
    # TODO (Consider making this optional): Overlay weekend indicator at upper limit of "ylim" to explore weekend effects
    
    text(x = 0, y = ylim_ul, labels = "W:", cex = .5)
    
    obs_df$response_time_wend[obs_df$response_time_wend == 1] <- ylim_ul
    
    points(obs_df$t, obs_df$response_time_wend, pch = 95, cex = .6)
  }
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

dir.create("./results/pred_values/")

  # For all predicted values starting from observed baseline values

lapply(names(pred_study_bl_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_bl_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "one", "l",
                paste0("pred_study_bl_thres_a05_", lifepak_id),
                paste0("Through Study for Thresholded Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_thres_a05),   function(lifepak_id) {
  plot_pred_obs(pred_400_bl_thres_a05[[lifepak_id]],   dat_ls[[lifepak_id]], "one", "l",
                paste0("pred_400_bl_thres_a05_",   lifepak_id),
                paste0("Through 400 for Thresholded Starting From Obs. Baseline Values (ID ",   lifepak_id, ")"))
})

lapply(names(pred_study_bl_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_bl_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "one", "l",
                paste0("pred_study_bl_satur_",     lifepak_id),
                paste0("Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_satur),       function(lifepak_id) {
  plot_pred_obs(pred_400_bl_satur[[lifepak_id]],       dat_ls[[lifepak_id]], "one", "l",
                paste0("pred_400_bl_satur_",       lifepak_id),
                paste0("Through 400 for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})

  # For 4 predicted values starting from each time point

lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_",                      lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})
lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_iter_colors_",          lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})
lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_1-50_iter_colors_",     lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                view_t_min = 1, view_t_max = 50, iter_colors = TRUE)
})

lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_",                  lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})
lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_iter_colors_",      lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})
lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_1-50_iter_colors_", lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                view_t_min = 1, view_t_max = 50, iter_colors = TRUE)
})                                                                                                                         # TODO: HERE

# Define function to plot predicted values from various baseline starting points

  # TODO: Adapt as needed and run for GIMME results





plot_pred_obs_various_bl_start <- function(various_pred_lists, various_pred_lists_focal_var_labels, dat_ls, 
                                           plot_name_stem, thres, plot_title_stem) {
  for (i in 1:length(various_pred_lists)) {
    pred_list <- various_pred_lists[[i]]
    pred_list_name <- names(various_pred_lists)[i]
    pred_list_focal_var_label <- various_pred_lists_focal_var_labels[i]
    
    pred_list_plot_title_stem <- sub("pred_list_focal_var_label", pred_list_focal_var_label, plot_title_stem)
    
    lapply(names(pred_list), function(lifepak_id) {
      plot_pred_obs(pred_list[[lifepak_id]], dat_ls[[lifepak_id]], "one", "l",
                    paste0(plot_name_stem, "_", pred_list_name, "_", thres, "_", lifepak_id),
                    paste0(pred_list_plot_title_stem, lifepak_id, ")"))
    })
  }
}

  # Define label of focal variable for each list of starting values (use list names in
  # "pred_study_max_one_0_others_thres_a05" as paradigmatic)

pred_max_one_0_others_focal_var_labels <- names(pred_study_max_one_0_others_thres_a05)

pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_bad_d_std"]      <- "Bad Self"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_control_d_std"]  <- "Lack Control"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_energy_d_std"]   <- "Fatigue"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_focus_d_std"]    <- "Lack Focus"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_fun_d_std"]      <- "Inaction"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_interest_d_std"] <- "Lack Interest"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_movement_d_std"] <- "Slower or Fidgety"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_sad_d_std"]      <- "Sad"

  # Run "plot_pred_obs_various_start()" function

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_thres_a05, pred_max_one_0_others_focal_var_labels, dat_ls,
                               "pred_study_max_one_0_others", "thres_a05",
                               'Through Study for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_thres_a05,   pred_max_one_0_others_focal_var_labels, dat_ls,
                               "pred_400_max_one_0_others",   "thres_a05",
                               'Through 400 for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_satur,     pred_max_one_0_others_focal_var_labels, dat_ls,
                               "pred_study_max_one_0_others", "satur",
                               'Through Study for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_satur,       pred_max_one_0_others_focal_var_labels, dat_ls,
                               "pred_400_max_one_0_others",   "satur",
                               'Through 400 for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

# ---------------------------------------------------------------------------- #
# Plot signed prediction errors at each "iter_t" ----
# ---------------------------------------------------------------------------- #

# Define function to plot signed prediction errors at each "iter_t" for "k" predicted
# values starting from each time point

plot_pred_error <- function(pred_error, plot_name, plot_title) {
  pred_error$t_from_start <- NA
  pred_error$t_from_start <- pred_error$iter_t - 1
  
  # Define plot settings
  
  t_from_start_values <- unique(pred_error$t_from_start)
  
  xlab <- "Time Points From Starting Time Point"
  ylab <- "Prediction Error"
  ylim_ll <- -10
  ylim_ul <- 10
  col  <- "red"
  pch  <- 16
  
  vars       <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
    # Check that y-axis spans range of predicted and observed values
  
  pred_error_cols_min <- min(pred_error[, paste0(vars, "_error")], na.rm = TRUE)
  pred_error_cols_max <- max(pred_error[, paste0(vars, "_error")], na.rm = TRUE)
  
  if (pred_error_cols_min < ylim_ll) {
    stop(paste0("Make lower limit of 'ylim' <= ", pred_error_cols_min))
  }
  if (pred_error_cols_max > ylim_ul) {
    stop(paste0("Make upper limit of 'ylim' >= ", pred_error_cols_max))
  }
  
  # Create plots
  
  pdf(paste0("./results/pred_error/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    var       <- vars[i]
    var_label <- var_labels[i]
    
    plot(pred_error$t_from_start, pred_error[, paste0(var, "_error")], main = var_label,
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = c(ylim_ll, ylim_ul), xaxt = "n")
    axis(1, at = t_from_start_values, labels = t_from_start_values)

    mtext(plot_title, side = 3, line = -1, outer = TRUE)
  }
  
  par(mfrow = c(1, 1))

  dev.off()
}

# Run function

dir.create("./results/pred_error/")

  # For 4 predicted values starting from each time point

lapply(names(pred_error_study_4_satur),     function(lifepak_id) {
  plot_pred_error(pred_error_study_4_satur[[lifepak_id]],
    paste0("pred_study_4_satur_error_", lifepak_id),
    paste0("Errors for Next 3 Through Study for Satur. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

lapply(names(pred_error_study_4_thres_a05), function(lifepak_id) {
  plot_pred_error(pred_error_study_4_thres_a05[[lifepak_id]],
    paste0("pred_study_4_thres_a05_error_", lifepak_id),
    paste0("Errors for Next 3 Through Study for Thres. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})                                                                                                                # TODO: HERE

# ---------------------------------------------------------------------------- #
# Plot "diff_over_obs_sd" at each "iter_t" ----
# ---------------------------------------------------------------------------- #

# Define function to plot "diff_over_obs_sd" at each "iter_t" for "k" predicted
# values starting from each time point

plot_diff_over_obs_sd <- function(diff_over_obs_sd, plot_name, plot_title) {
  diff_over_obs_sd$t_from_start <- NA
  diff_over_obs_sd$t_from_start <- diff_over_obs_sd$iter_t - 1
  
  t_from_start_values <- unique(diff_over_obs_sd$t_from_start)
  
  xlab <- "Time Points From Starting Time Point"
  ylab <- expression((italic("SD")["Data"] - italic("SD")["Pred. Errors"]) / italic("SD")[Data])
  ylim <- c(-2, 2)

  vars       <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  pdf(paste0("./results/diff_over_obs_sd/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    var       <- vars[i]
    var_label <- var_labels[i]
    
    x <- diff_over_obs_sd$t_from_start
    y <- diff_over_obs_sd[, paste0(var, "_diff_over_obs_sd")]
    
    plot(x, y, main = var_label,
         type = "b", xlab = xlab, ylab = ylab, ylim = ylim, pch = 16, xaxt = "n")
    axis(1, at = t_from_start_values, labels = t_from_start_values)
    text(x, y, y, pos = 3, cex = 0.55, col = "blue")
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
  }

  par(mfrow = c(1, 1))
  
  dev.off()
}

# Run function

dir.create("./results/diff_over_obs_sd/")

  # For 4 predicted values starting from each time point

lapply(names(diff_over_obs_sd_study_4_satur),     function(lifepak_id) {
  plot_diff_over_obs_sd(diff_over_obs_sd_study_4_satur[[lifepak_id]],
    paste0("diff_over_obs_sd_study_4_satur_", lifepak_id),
    paste0("Fit for Next 3 Through Study for Satur. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

lapply(names(diff_over_obs_sd_study_4_thres_a05), function(lifepak_id) {
  plot_diff_over_obs_sd(diff_over_obs_sd_study_4_thres_a05[[lifepak_id]],
    paste0("diff_over_obs_sd_study_4_thres_a05_", lifepak_id),
    paste0("Fit for Next 3 Through Study for Thres. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})                                                                                                                # TODO: HERE

# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





