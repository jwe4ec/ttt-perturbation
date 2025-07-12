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

pkgs <- c("qgraph", "randomcoloR", "tidyr", "ggplot2", "cowplot", "colorspace", "DescTools")
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

round(mean(n_sig_edges), 2) == 3.58
median(n_sig_edges)         == 3
round(sd(n_sig_edges), 2)   == 3.12

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

  # Note: Prior to refitting Mplus networks on clean data, the randomly sampled
  # participant with 1 edge was "999341". However, this participant now has 2 edges, 
  # so a new participant with 1 edge was randomly sampled ("272769").

# low_sig_edges_ex_id <- sample(low_sig_edges_ids, 1)
low_sig_edges_ex_id <- "272769"

  # Note: Prior to refitting Mplus networks on clean data, the randomly sampled
  # participant with 3-4 edges was "861114". Given that this participant still has
  # 3 edges, we retain this participant rather than sampling a new one.

# med_sig_edges_ex_id <- sample(med_sig_edges_ids, 1)
med_sig_edges_ex_id <- "861114"
n_sig_edges[med_sig_edges_ex_id] == 3 # 3 edges

high_sig_edges_ex_id <- "326177"
n_sig_edges[high_sig_edges_ex_id] == 10 # 10 edges

# Determine number of participants for which each edge is significant

thres_adj_mats_var_sig_edges <- lapply(thres_adj_mats_var, function(x) (x != 0))

thres_adj_mats_var_sig_freq <- Reduce("+", thres_adj_mats_var_sig_edges)

range(thres_adj_mats_var_sig_freq) == c(0, 7)

# Determine percentage of participants for which each edge is significant

n <- length(thres_adj_mats_var)

thres_adj_mats_var_sig_incl_perc <- round((thres_adj_mats_var_sig_freq / n) * 100, 1)

range(thres_adj_mats_var_sig_incl_perc) == c(0.0, 18.4)

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
sum(!is.na(dat[dat$lifepak_id == low_sig_edges_ex_id, "bad"])) == 88

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

  # TODO: Test creation of time variables

test <- dat

test$notification_time <- format(test$notification_datetime, "%H:%M:%S")
test$response_end_time <- format(test$response_end_datetime, "%H:%M:%S")

max(test$notification_time, na.rm = TRUE) == "22:58:56"
min(test$response_end_time, na.rm = TRUE) == "07:32:26"

  # TODO: May need to analyze this in data prior to making it evenly spaced in time





test_wend_miss <- function(part_data) {
  part_data$bad_miss <- NA
  
  part_data$bad_miss <- as.integer(is.na(part_data$bad))
  
  # TODO: First need to fill in "response_wday" and "response_wend" above

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
# Compute standard deviation-scaled centered variables ----
# ---------------------------------------------------------------------------- #

# Define function to scale centered variables by their SDs (without centering)

compute_scl_vars <- function(part_data) {
  node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  d_cols  <- paste0(node_vars, "_d")
  d2_cols <- paste0(node_vars, "_d2")
  
  target_cols <- c(d_cols, d2_cols)
  
  for (target_col in target_cols) {
    scl_col <- paste0(target_col, "_scl")
    
    target_col_sd <- sd(part_data[, target_col], na.rm = TRUE)
    
    part_data[, scl_col] <- part_data[, target_col] / target_col_sd
  }
  
  return(part_data)
}

# Run function

dat_ls <- lapply(dat_ls, compute_scl_vars)

# TODO: Testing as SD-scaled centered value does not plot with equilibrium 0

median(dat_ls$"326177"$bad, na.rm = TRUE)
median(dat_ls$"326177"$bad_d, na.rm = TRUE)
median(dat_ls$"326177"$bad_d_scl, na.rm = TRUE)





# ---------------------------------------------------------------------------- #
# Find first time point with complete observations in data ----
# ---------------------------------------------------------------------------- #

# Define function for finding first time point with complete observations in data

find_first_compl_row_idx <- function(part_data, obs_var_suf) {
  node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

  target_data <- part_data[paste0(node_vars, obs_var_suf)]
  
  first_compl_row_idx <- which(complete.cases(target_data))[1]

  return(first_compl_row_idx)
}

# Run function

first_compl_row_idx_d_scl  <- lapply(dat_ls, find_first_compl_row_idx, "_d_scl")
first_compl_row_idx_d2_scl <- lapply(dat_ls, find_first_compl_row_idx, "_d2_scl")

# ---------------------------------------------------------------------------- #
# Define function for defining starting values from centered values in data ----
# ---------------------------------------------------------------------------- #

# Define function for defining starting values from participant's SD-scaled
# centered values in data at a given time point "j"

define_start <- function(part_data, j, obs_var_suf) {
  node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

  start_list <- lapply(node_vars, function(node_var) part_data[j, paste0(node_var, obs_var_suf)])
  names(start_list) <- node_vars
  
  return(start_list)
}

# ---------------------------------------------------------------------------- #
# Define function to compute predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to compute predicted values over desired number of time points
# ("n_timepoints") from adjacency matrix and desired starting values
# - Start from a given time point ("start_timepoint"; 1 by default)
# - By default, assume an adjacency matrix whose rows are inputs and columns are
#   outputs, with option to use adjacency matrix whose columns are inputs and rows 
#   are outputs ("adj_mat_cols_as_inputs = TRUE")

compute_pred <- function(adj_mat, n_timepoints, start_list, start_timepoint = 1,
                         adj_mat_cols_as_inputs = FALSE) {
  # Initialize columns with NA for desired number of time points
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  vars_pred <- paste0(vars, "_pred")
  
  pred_mat <- matrix(NA, nrow = n_timepoints, ncol = 8, dimnames = list(NULL, vars_pred))
  
  # Compute predicted values for desired number of time points, starting from given time point

  for (i in start_timepoint:n_timepoints) {
    if (i == start_timepoint) {
      pred_mat[i, "bad_pred"]      <- start_list$bad
      pred_mat[i, "control_pred"]  <- start_list$control
      pred_mat[i, "energy_pred"]   <- start_list$energy
      pred_mat[i, "focus_pred"]    <- start_list$focus
      pred_mat[i, "fun_pred"]      <- start_list$fun
      pred_mat[i, "interest_pred"] <- start_list$interest
      pred_mat[i, "movement_pred"] <- start_list$movement
      pred_mat[i, "sad_pred"]      <- start_list$sad
    } else if (i > start_timepoint) {
      vars_l1 <- paste0(vars, "_l1")
      
      l1_mat <- matrix(NA, nrow = 1, ncol = 8, dimnames = list(NULL, vars_l1))
      
      l1_mat[1, "bad_l1"]      <- pred_mat[i - 1, "bad_pred"]
      l1_mat[1, "control_l1"]  <- pred_mat[i - 1, "control_pred"]
      l1_mat[1, "energy_l1"]   <- pred_mat[i - 1, "energy_pred"]
      l1_mat[1, "focus_l1"]    <- pred_mat[i - 1, "focus_pred"]
      l1_mat[1, "fun_l1"]      <- pred_mat[i - 1, "fun_pred"]
      l1_mat[1, "interest_l1"] <- pred_mat[i - 1, "interest_pred"]
      l1_mat[1, "movement_l1"] <- pred_mat[i - 1, "movement_pred"]
      l1_mat[1, "sad_l1"]      <- pred_mat[i - 1, "sad_pred"]
      
      if (adj_mat_cols_as_inputs == FALSE) {
        # Pre-multiply coefficients as 8-by-8 matrix by prior time point's values as 
        # 1-by-8 matrix to get current time point's predicted values as 1-by-8 matrix
        
        pred_mat[i, ] <- l1_mat %*% adj_mat
        
      } else if (adj_mat_cols_as_inputs == TRUE) {
        # Post-multiply coefficients as 8-by-8 matrix by prior time point's values as 
        # 8-by-1 matrix to get current time point's predicted values as 8-by-1 matrix
        
        l1_mat_one_col <- t(l1_mat)
        
        pred_mat_i_one_col <- adj_mat %*% l1_mat_one_col
        
        pred_mat[i, ] <- t(pred_mat_i_one_col)
      }
    }
  }
  
  pred <- data.frame(t = 1:n_timepoints,
                     as.data.frame(pred_mat))
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Compute all predicted values starting from participant's centered values at baseline ----
# ---------------------------------------------------------------------------- #

# Compute number of study time points for each participant

n_study_timepoints <- lapply(dat_ls, nrow)

# Define starting values for each participant from SD-scaled centered values at 
# baseline (i.e., using first time point with complete observations)

start_list_bl_d_scl  <- lapply(names(dat_ls), function(lifepak_id) {
  define_start(dat_ls[[lifepak_id]], first_compl_row_idx_d_scl[[lifepak_id]],  "_d_scl")
})
start_list_bl_d2_scl <- lapply(names(dat_ls), function(lifepak_id) {
  define_start(dat_ls[[lifepak_id]], first_compl_row_idx_d2_scl[[lifepak_id]], "_d2_scl")
})

names(start_list_bl_d_scl)  <- names(dat_ls)
names(start_list_bl_d2_scl) <- names(dat_ls)

# Compute predicted values for thresholded and saturated Mplus networks (a) over study 
# period and (b) into future

pred_study_bl_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], 
               start_list_bl_d_scl[[lifepak_id]], first_compl_row_idx_d_scl[[lifepak_id]])
})
pred_400_bl_thres_a05   <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], 400,
               start_list_bl_d_scl[[lifepak_id]], first_compl_row_idx_d_scl[[lifepak_id]])
})

pred_study_bl_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], 
               start_list_bl_d_scl[[lifepak_id]], first_compl_row_idx_d_scl[[lifepak_id]])
})
pred_400_bl_satur       <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], 400,                              
               start_list_bl_d_scl[[lifepak_id]], first_compl_row_idx_d_scl[[lifepak_id]])
})

names(pred_study_bl_thres_a05) <- names(thres_adj_mats_var)
names(pred_400_bl_thres_a05)   <- names(thres_adj_mats_var)

names(pred_study_bl_satur)     <- names(satur_adj_mats_var)
names(pred_400_bl_satur)       <- names(satur_adj_mats_var)

# Compute predicted values for GIMME networks over study period

pred_study_bl_gimme     <- lapply(names(gimme_adj_mats_var), function(lifepak_id) {
  compute_pred(gimme_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], 
               start_list_bl_d2_scl[[lifepak_id]], first_compl_row_idx_d2_scl[[lifepak_id]])
})

names(pred_study_bl_gimme)     <- names(gimme_adj_mats_var)

# ---------------------------------------------------------------------------- #
# Define function to compute "k" predicted values starting from each time point ----
# ---------------------------------------------------------------------------- #

# Define function to compute "k" predicted values over desired time points ("iterations") from
# adjacency matrix, starting each iteration from observed value at that iteration's time point

compute_k_pred <- function(part_data, adj_mat, k, iterations, obs_var_suf) {
  pred <- data.frame()
  
  for (iter in 1:iterations) {
    start_list <- define_start(part_data, iter, obs_var_suf)
    
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

compute_pred_error <- function(part_data, pred, obs_var_suf) {
  names(part_data)[names(part_data) == "bin_no_adj"] <- "t"
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  target_cols <- paste0(vars, obs_var_suf)
  part_data_tmp <- part_data[, c("t", target_cols)]
  
  pred <- merge(pred, part_data_tmp, by = "t", all.x = TRUE)
  pred <- pred[order(pred$iter, pred$iter_t), ]
  
  # Compute signed prediction error
  
  for (var in vars) {
    pred[, paste0(var, "_error")] <- pred[, paste0(var, obs_var_suf)] - pred[, paste0(var, "_pred")]
  }
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Define function to compute "diff_over_obs_sd" at each "iter_t" ----
# ---------------------------------------------------------------------------- #

# Define function to compute "diff_over_obs_sd", or (SD of observed data - SD of
# signed prediction errors) / SD of observed data, at each "iter_t" for "k" predicted 
# values starting from each time point

compute_diff_over_obs_sd <- function(pred_error, obs_var_suf) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  iter_t_values <- unique(pred_error$iter_t)
  
  res <- data.frame(iter_t = iter_t_values)
  
  for (iter_t in iter_t_values) {
    pred_error_iter_t <- pred_error[pred_error$iter_t == iter_t, ]
    
    for (var in vars) {
      obs_col   <- paste0(var, obs_var_suf)
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
# Compute 4 predicted values for Mplus networks starting from participant's centered values at each time point ----
# ---------------------------------------------------------------------------- #

# Compute predicted values for saturated and thresholded Mplus networks over study period

pred_study_4_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], satur_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]], "_d_scl")
})

pred_study_4_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], thres_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]], "_d_scl")
})

names(pred_study_4_satur)     <- names(satur_adj_mats_var)
names(pred_study_4_thres_a05) <- names(thres_adj_mats_var)

# Compute signed prediction error

pred_error_study_4_satur     <- lapply(names(dat_ls), function(lifepak_id) {
  compute_pred_error(dat_ls[[lifepak_id]], pred_study_4_satur[[lifepak_id]],     "_d_scl")
})

pred_error_study_4_thres_a05 <- lapply(names(dat_ls), function(lifepak_id) {
  compute_pred_error(dat_ls[[lifepak_id]], pred_study_4_thres_a05[[lifepak_id]], "_d_scl")
})

names(pred_error_study_4_satur)     <- names(dat_ls)
names(pred_error_study_4_thres_a05) <- names(dat_ls)

# TODO (some "diff_over_obs_sd" values are negative because sometimes "error_sd" is 
# greater than "obs_sd"): Compute "diff_over_obs_sd" at each "iter_t". Also sometimes 0.

diff_over_obs_sd_study_4_satur     <- lapply(pred_error_study_4_satur,     compute_diff_over_obs_sd, "_d_scl")
diff_over_obs_sd_study_4_thres_a05 <- lapply(pred_error_study_4_thres_a05, compute_diff_over_obs_sd, "_d_scl")





# ---------------------------------------------------------------------------- #
# Compute 4 predicted values for GIMME networks starting from participant's centered values at each time point ----
# ---------------------------------------------------------------------------- #

# Compute predicted values for GIMME networks over study period

pred_study_4_gimme     <- lapply(names(gimme_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], gimme_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]], "_d2_scl")
})

names(pred_study_4_gimme) <- names(gimme_adj_mats_var)

# Compute signed prediction error

pred_error_study_4_gimme     <- lapply(names(dat_ls), function(lifepak_id) {
  compute_pred_error(dat_ls[[lifepak_id]], pred_study_4_gimme[[lifepak_id]], "_d2_scl")
})

names(pred_error_study_4_gimme) <- names(dat_ls)

# TODO (some "diff_over_obs_sd" values are negative because sometimes "error_sd" is 
# greater than "obs_sd"): Compute "diff_over_obs_sd" at each "iter_t"

diff_over_obs_sd_study_4_gimme <- lapply(pred_error_study_4_gimme, compute_diff_over_obs_sd, "_d2_scl")





# ---------------------------------------------------------------------------- #
# Compute predicted values starting from various perturbed starting values ----
# ---------------------------------------------------------------------------- #

# TODO: Consider combining these functions together and with define_start() above





# Define various perturbed starting values for each participant based on participant's 
# max for one node and 0 for others

define_start_max_one_0_others <- function(part_data, obs_var_suf) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  start_list_max_one_0_others_element <- vector("list", length = length(vars))
  names(start_list_max_one_0_others_element) <- vars
  start_list_max_one_0_others_element[ ] <- 0
  
  start_list_max_one_0_others <- vector("list", length = length(vars))
  names(start_list_max_one_0_others) <- paste0("max_", vars, obs_var_suf)
  start_list_max_one_0_others[ ] <- list(start_list_max_one_0_others_element)
  
  start_list_max_one_0_others[[paste0("max_bad",      obs_var_suf)]]$bad      <- max(part_data[paste0("bad",      obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_control",  obs_var_suf)]]$control  <- max(part_data[paste0("control",  obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_energy",   obs_var_suf)]]$energy   <- max(part_data[paste0("energy",   obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_focus",    obs_var_suf)]]$focus    <- max(part_data[paste0("focus",    obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_fun",      obs_var_suf)]]$fun      <- max(part_data[paste0("fun",      obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_interest", obs_var_suf)]]$interest <- max(part_data[paste0("interest", obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_movement", obs_var_suf)]]$movement <- max(part_data[paste0("movement", obs_var_suf)], na.rm = TRUE)
  start_list_max_one_0_others[[paste0("max_sad",      obs_var_suf)]]$sad      <- max(part_data[paste0("sad",      obs_var_suf)], na.rm = TRUE)
  
  return(start_list_max_one_0_others)
}

  # Note: Run for only Mplus results so far

start_list_max_one_0_others_d_scl <- lapply(dat_ls, define_start_max_one_0_others, "_d_scl")

# Define various perturbed starting values for each participant based on participant's max for one 
# node and baseline values (i.e., using first time point with complete observations) for others

define_start_max_one_bl_others <- function(part_data, obs_var_suf, first_compl_row_idx) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  j <- first_compl_row_idx
  
  start_list_max_one_bl_others_element <- list(bad      = part_data[j, paste0("bad",      obs_var_suf)],
                                               control  = part_data[j, paste0("control",  obs_var_suf)],
                                               energy   = part_data[j, paste0("energy",   obs_var_suf)],
                                               focus    = part_data[j, paste0("focus",    obs_var_suf)],
                                               fun      = part_data[j, paste0("fun",      obs_var_suf)],
                                               interest = part_data[j, paste0("interest", obs_var_suf)],
                                               movement = part_data[j, paste0("movement", obs_var_suf)],
                                               sad      = part_data[j, paste0("sad",      obs_var_suf)])
  
  start_list_max_one_bl_others <- vector("list", length = length(vars))
  names(start_list_max_one_bl_others) <- paste0("max_", vars, obs_var_suf)
  start_list_max_one_bl_others[ ] <- list(start_list_max_one_bl_others_element)
  
  start_list_max_one_bl_others[[paste0("max_bad",      obs_var_suf)]]$bad      <- max(part_data[paste0("bad",      obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_control",  obs_var_suf)]]$control  <- max(part_data[paste0("control",  obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_energy",   obs_var_suf)]]$energy   <- max(part_data[paste0("energy",   obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_focus",    obs_var_suf)]]$focus    <- max(part_data[paste0("focus",    obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_fun",      obs_var_suf)]]$fun      <- max(part_data[paste0("fun",      obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_interest", obs_var_suf)]]$interest <- max(part_data[paste0("interest", obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_movement", obs_var_suf)]]$movement <- max(part_data[paste0("movement", obs_var_suf)], na.rm = TRUE)
  start_list_max_one_bl_others[[paste0("max_sad",      obs_var_suf)]]$sad      <- max(part_data[paste0("sad",      obs_var_suf)], na.rm = TRUE)
  
  return(start_list_max_one_bl_others)
}

  # Note: Run for only GIMME results so far

start_list_max_one_bl_others_d2_scl <- lapply(names(dat_ls), function(lifepak_id) {
  define_start_max_one_bl_others(dat_ls[[lifepak_id]], "_d2_scl", first_compl_row_idx_d2_scl[[lifepak_id]])
})

names(start_list_max_one_bl_others_d2_scl) <- names(dat_ls)

# Define function to compute predicted values from various perturbed starting values
# (starting from a given time point; 1 by default)

compute_pred_various_start <- function(adj_mats_var, n_timepoints, various_start_lists, start_timepoints = NULL) {
  # Get unique names of "various_start_lists" (should be same for each participant)
  
  various_start_list_names <- unique(unlist(lapply(various_start_lists, names)))
  
  # Compute predicted values from various perturbed starting values
  
  various_pred_lists <- vector("list", length(various_start_list_names))
  names(various_pred_lists) <- various_start_list_names
  
  for (start_list_name in various_start_list_names) {
    various_pred_lists[[start_list_name]] <- lapply(names(adj_mats_var), function(lifepak_id) {
      adj_mat_var <- adj_mats_var[[lifepak_id]]
      start_list  <- various_start_lists[[lifepak_id]][[start_list_name]]
      
      if (length(n_timepoints) > 1) {
        n_timepts <- n_timepoints[[lifepak_id]]
      } else if (length(n_timepoints == 1)) {
        n_timepts <- n_timepoints
      }
        
      if (!is.null(start_timepoints)) {
        start_timepoint <- start_timepoints[[lifepak_id]]
      } else {
        start_timepoint <- 1
      }
      
      compute_pred(adj_mat_var, n_timepts, start_list, start_timepoint)
    })
    
    names(various_pred_lists[[start_list_name]]) <- names(adj_mats_var)
  }
  
  return(various_pred_lists)
}

# Run function

  # To compute predicted values for thresholded and saturated networks (a) over 
  # study period and (b) into future (run for only Mplus results so far), based
  # on participant's max for one node and 0 for others

pred_study_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, n_study_timepoints, start_list_max_one_0_others_d_scl)
pred_400_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, 400,                start_list_max_one_0_others_d_scl)

pred_study_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, n_study_timepoints, start_list_max_one_0_others_d_scl)
pred_400_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, 400,                start_list_max_one_0_others_d_scl)

  # To compute predicted values for GIMME networks over study period, based on 
  # participant's max for one node and baseline for others

pred_study_max_one_bl_others_gimme <- 
  compute_pred_various_start(gimme_adj_mats_var, n_study_timepoints,
                             start_list_max_one_bl_others_d2_scl, first_compl_row_idx_d2_scl)

# ---------------------------------------------------------------------------- #
# Plot predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted values
# - Starting from one time point or (for "k" predicted values) starting from many 
#   (pred_start_t_points = "one" or "many")
# - Predicted values are plotted as (green) lines, points, or both
#   (pred_plot_type = "l", "p", or "b"); observed values are plotted as (black) points
# - Optional: If starting from one time point and "pred_df2" is given, plot two sets of 
#   predicted values, one set in green from "pred_df" (e.g., from perturbed starting 
#   values) and one set in blue from "pred_df2" (e.g., from unperturbed starting values)
# - Optional: Restrict displayed range of time points ("view_t_min" and "view_t_max").
#   Otherwise, plot will start at 0 and end at maximum "t" of "pred_df1".
# - Optional: For "k" predicted values starting from many time points, use a different 
#   color for each iteration ("iter_colors = TRUE")

plot_pred_obs <- function(pred_df1, obs_df, obs_var_suf, pred_start_t_points, pred_plot_type,
                          plot_dir, plot_name, plot_title,
                          pred_df2 = NULL, view_t_min = NULL, view_t_max = NULL, iter_colors = NULL) {
  obs_df$t <- 1:nrow(obs_df)
  
  # Define plot settings
  
  xlab <- "Time"
  ylab <- expression(paste("Centered Value / ", italic("SD")))
  xlim_ll <- 0
  xlim_ul <- max(pred_df1$t)
  ylim_ll <- -10
  ylim_ul <- 10
  lwd <- 1.5
  pch <- 16
  
    # Optionally restrict displayed range of time points
  
  if (!is.null(view_t_min) & !is.null(view_t_max)) {
    xlim_ll <- ifelse(view_t_min == 1, 0, view_t_min)
    xlim_ul <- view_t_max
  }
  
  if (pred_start_t_points == "one") {
    color_pred1 <- "green"
    
    if (!is.null(pred_df2)) {
      color_pred2 <- "blue"
    }
  } else if (pred_start_t_points == "many") {
    n_iterations <- max(pred_df1$iter)
    
    if (is.null(iter_colors)) {
      color_pred <- rep("green", n_iterations)
    } else if (iter_colors == TRUE) {
      set.seed(1234)
      color_pred <- distinctColorPalette(n_iterations)
    }
  }
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  pred_cols    <- paste0(vars, "_pred")
  obs_cols     <- paste0(vars, obs_var_suf)
  raw_obs_cols <- vars
  
    # Check that y-axis spans range of observed and predicted values
  
  obs_cols_min   <- min(obs_df[, obs_cols],    na.rm = TRUE)
  obs_cols_max   <- max(obs_df[, obs_cols],    na.rm = TRUE)
  
  pred1_cols_min <- min(pred_df1[, pred_cols], na.rm = TRUE)
  pred1_cols_max <- max(pred_df1[, pred_cols], na.rm = TRUE)
  
  overall_min <- min(obs_cols_min, pred1_cols_min)
  overall_max <- max(obs_cols_max, pred1_cols_max)
  
  if (!is.null(pred_df2)) {
    pred2_cols_min <- min(pred_df2[, pred_cols], na.rm = TRUE)
    pred2_cols_max <- max(pred_df2[, pred_cols], na.rm = TRUE)
    
    overall_min <- min(overall_min, pred2_cols_min)
    overall_max <- max(overall_max, pred2_cols_max)
  }
  
  if (overall_min < ylim_ll) {
    stop(paste0("Make lower limit of 'ylim' <= ", overall_min))
  }
  if (overall_max > ylim_ul) {
    stop(paste0("Make upper limit of 'ylim' >= ", overall_max))
  }

  # Create plots
  
  pdf(paste0(plot_dir, plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    pred_col    <- pred_cols[i]
    obs_col     <- obs_cols[i]
    raw_obs_col <- raw_obs_cols[i]
    var_label   <- var_labels[i]
    
    # Start with empty plot
    
    plot(pred_df1$t, pred_df1[, pred_col], main = var_label, type = "n", 
         xlab = xlab, ylab = ylab, xlim = c(xlim_ll, xlim_ul), ylim = c(ylim_ll, ylim_ul))
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    # Plot predicted values
    
    if (pred_start_t_points == "one") {
      if (pred_plot_type        == "l") {
        lines(pred_df1$t,    pred_df1[, pred_col], lwd = lwd, col = color_pred1)
      } else if (pred_plot_type == "p") {
        points(pred_df1$t,   pred_df1[, pred_col], pch = pch, col = color_pred1)
      } else if (pred_plot_type == "b") {
        lines(pred_df1$t,    pred_df1[, pred_col], lwd = lwd, col = color_pred1)
        points(pred_df1$t,   pred_df1[, pred_col], pch = pch, col = color_pred1)
      }
      
      if (!is.null(pred_df2)) {
        if (pred_plot_type        == "l") {
          lines(pred_df2$t,  pred_df2[, pred_col], lwd = lwd, col = color_pred2)
        } else if (pred_plot_type == "p") {
          points(pred_df2$t, pred_df2[, pred_col], pch = pch, col = color_pred2)
        } else if (pred_plot_type == "b") {
          lines(pred_df2$t,  pred_df2[, pred_col], lwd = lwd, col = color_pred2)
          points(pred_df2$t, pred_df2[, pred_col], pch = pch, col = color_pred2)
        }
      }
    } else if (pred_start_t_points == "many") {
      for (j in 1:n_iterations) {
        iter_pred  <- pred_df1[pred_df1$iter == j, ]
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
    
    # TODO (Consider making this optional): Overlay weekend indicator just below
    # upper limit of "ylim" (i.e., "ylim_ul" - 0.5) to explore weekend effects
    
    text(x = xlim_ll, y = ylim_ul, labels = "W:", cex = .5)
    
    obs_df$response_wend[obs_df$response_wend == 1] <- ylim_ul - 0.5
    
    points(obs_df$t, obs_df$response_wend, pch = 95, cex = .6)
    
    # TODO (Consider making this optional): Print median of all raw observed values
    
    raw_obs_col_median <- median(obs_df[, raw_obs_col], na.rm = TRUE)
    
    if (raw_obs_col_median %in% c(0, 100)) {
      old_detrend_label <- "(Not Detrended Per Prior Criterion)"
    } else {
      old_detrend_label <- NULL
    }
    
    mtext(paste("Raw Uncentered Obs. Mdn:", raw_obs_col_median, old_detrend_label),
          side = 3, line = 1, adj = 0, cex = .5)
    
    # TODO: Print mode of all raw observed values
    
      # Compute mode
    
    raw_obs_col_mo <- Mode(obs_df[, raw_obs_col], na.rm = TRUE)
    
      # If mode is singular and at a pole (i.e., 0 or 100), compute proportion of
      # data at mode and determine whether that proportion is nontrivial (>= 25%)
    
    prop_at_single_mode_at_pole <- NA
    single_nontriv_mode_at_pole <- NA
    
    if (length(raw_obs_col_mo) == 1) {
      if (is.na(raw_obs_col_mo)) {
        single_nontriv_mode_at_pole <- FALSE
      } else if (raw_obs_col_mo %in% c(0, 100)) {
        prop_at_single_mode_at_pole <- sum(obs_df[, raw_obs_col] == raw_obs_col_mo, na.rm = TRUE) / 
                                         sum(!is.na(obs_df[, raw_obs_col]))
        
        single_nontriv_mode_at_pole <- prop_at_single_mode_at_pole >= .25
      } else {
        single_nontriv_mode_at_pole <- FALSE
      }
    } else if (length(raw_obs_col_mo) > 1) {
      single_nontriv_mode_at_pole <- FALSE
    }
      
      # Print mode(s) and, if singular and at pole, print percentage of data at mode
    
    modes_label <- paste(raw_obs_col_mo, collapse = ", ")
    detrend_label <- if (single_nontriv_mode_at_pole) "(Not Detrended)" else NULL
    
    mtext(paste("Raw Uncentered Obs. Mode(s):", modes_label),
          side = 3, line = 0.5, adj = 0, cex = .5)
    
    mtext(paste("% Raw Uncentered Obs. at Single Polar Mode:",
                round(prop_at_single_mode_at_pole * 100, 1), detrend_label),
          side = 3, line = 0, adj = 0, cex = .5)
    
    # TODO (Remove this if not used): Print percentage of observed values +/- 3 units from median
    
    # mdn_thres_ll <- raw_obs_col_median - 3
    # mdn_thres_ul <- raw_obs_col_median + 3
    # 
    # prop_in_mdn_thres <-
    #   sum(obs_df[, raw_obs_col] >= mdn_thres_ll & obs_df[, raw_obs_col] <= mdn_thres_ul, na.rm = TRUE) / 
    #   sum(!is.na(obs_df[, raw_obs_col]))
    # 
    # perc_in_mdn_thres <- round(prop_in_mdn_thres * 100, 1)
    # 
    # if (prop_in_mdn_thres >= .7) {
    #   detrend_label <- "(Not Detrended)"
    # } else {
    #   detrend_label <- NULL
    # }
    # 
    # mtext(paste("% Raw Uncentered Obs. \u00B1 3 From Mdn:", perc_in_mdn_thres, detrend_label),
    #       side = 3, line = 0, adj = 0, cex = .5)
  }
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

# Run function

pred_values_path <- "./results/pred_values/"
dir.create(pred_values_path)

  # For all predicted values starting from observed baseline values

pred_values_from_bl_path <- paste0(pred_values_path, "from_bl/")
dir.create(pred_values_from_bl_path)

    # For Mplus networks

lapply(names(pred_study_bl_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_bl_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "_d_scl", "one", "l",
                pred_values_from_bl_path, paste0("pred_study_bl_thres_a05_", lifepak_id),
                paste0("Through Study for Thresholded Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_thres_a05),   function(lifepak_id) {
  plot_pred_obs(pred_400_bl_thres_a05[[lifepak_id]],   dat_ls[[lifepak_id]], "_d_scl", "one", "l",
                pred_values_from_bl_path, paste0("pred_400_bl_thres_a05_",   lifepak_id),
                paste0("Through 400 for Thresholded Starting From Obs. Baseline Values (ID ",   lifepak_id, ")"))
})

lapply(names(pred_study_bl_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_bl_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "_d_scl", "one", "l",
                pred_values_from_bl_path, paste0("pred_study_bl_satur_",     lifepak_id),
                paste0("Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_satur),       function(lifepak_id) {
  plot_pred_obs(pred_400_bl_satur[[lifepak_id]],       dat_ls[[lifepak_id]], "_d_scl", "one", "l",
                pred_values_from_bl_path, paste0("pred_400_bl_satur_",       lifepak_id),
                paste0("Through 400 for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})

    # For GIMME neworks

lapply(names(pred_study_bl_gimme),     function(lifepak_id) {
  plot_pred_obs(pred_study_bl_gimme[[lifepak_id]],     dat_ls[[lifepak_id]], "_d2_scl", "one", "l",
                pred_values_from_bl_path, paste0("pred_study_bl_gimme_",     lifepak_id),
                paste0("Through Study for GIMME Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})

  # For 4 predicted values starting from each time point

pred_values_from_each_t_path <- paste0(pred_values_path, "from_each_t/")
dir.create(pred_values_from_each_t_path)

    # For Mplus networks

lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     dat_ls[[lifepak_id]], "_d_scl", "many", "l",
                pred_values_from_each_t_path, paste0("pred_study_4_satur_iter_colors_",          lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})

lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], dat_ls[[lifepak_id]], "_d_scl", "many", "l",
                pred_values_from_each_t_path, paste0("pred_study_4_thres_a05_iter_colors_",      lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})

    # For GIMME neworks

lapply(names(pred_study_4_gimme),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_gimme[[lifepak_id]],     dat_ls[[lifepak_id]], "_d2_scl", "many", "l",
                pred_values_from_each_t_path, paste0("pred_study_4_gimme_iter_colors_",          lifepak_id),
                paste0("Next 3 Through Study for GIMME Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})

# ---------------------------------------------------------------------------- #
# Plot predicted values from various perturbed baseline starting values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted values from various perturbed baseline starting 
# values ("various_pred_lists") in green
# - Optional: Overlay predicted values from unperturbed baseline starting values
#   ("pred_bl") in blue
# - Optional: Restrict displayed range of time points ("view_t_min" and "view_t_max")

plot_pred_obs_various_bl_start <- function(various_pred_lists, dat_ls, obs_var_suf, 
                                           plot_dir, plot_name_stem, thres, plot_title_stem,
                                           pred_bl = NULL, view_t_min = NULL, view_t_max = NULL) {
  # Define label for the focal variable of each "pred_list"
  
  various_pred_lists_focal_var_labels <- names(various_pred_lists)
  
  various_pred_lists_focal_var_labels[grepl(paste0("bad",      obs_var_suf), various_pred_lists_focal_var_labels)] <- "Bad Self"
  various_pred_lists_focal_var_labels[grepl(paste0("control",  obs_var_suf), various_pred_lists_focal_var_labels)] <- "Lack Control"
  various_pred_lists_focal_var_labels[grepl(paste0("energy",   obs_var_suf), various_pred_lists_focal_var_labels)] <- "Fatigue"
  various_pred_lists_focal_var_labels[grepl(paste0("focus",    obs_var_suf), various_pred_lists_focal_var_labels)] <- "Lack Focus"
  various_pred_lists_focal_var_labels[grepl(paste0("fun",      obs_var_suf), various_pred_lists_focal_var_labels)] <- "Inaction"
  various_pred_lists_focal_var_labels[grepl(paste0("interest", obs_var_suf), various_pred_lists_focal_var_labels)] <- "Lack Interest"
  various_pred_lists_focal_var_labels[grepl(paste0("movement", obs_var_suf), various_pred_lists_focal_var_labels)] <- "Slower or Fidgety"
  various_pred_lists_focal_var_labels[grepl(paste0("sad",      obs_var_suf), various_pred_lists_focal_var_labels)] <- "Sad"
  
  # Plot predicted values from various baseline starting points
  
  for (i in 1:length(various_pred_lists)) {
    pred_list <- various_pred_lists[[i]]
    pred_list_name <- names(various_pred_lists)[i]
    pred_list_focal_var_label <- various_pred_lists_focal_var_labels[i]
    
    pred_list_plot_title_stem <- sub("pred_list_focal_var_label", pred_list_focal_var_label, plot_title_stem)
    
    lapply(names(pred_list), function(lifepak_id) {
      plot_pred_obs(pred_list[[lifepak_id]], dat_ls[[lifepak_id]], obs_var_suf, "one", "l",
                    plot_dir, paste0(plot_name_stem, "_", pred_list_name, "_", thres, "_", lifepak_id),
                    paste0(pred_list_plot_title_stem, lifepak_id, ")"),
                    pred_bl[[lifepak_id]], view_t_min, view_t_max)
    })
  }
}

# Run function

  # For participant's max for one node and 0 for others (run for only Mplus results so far)

pred_values_max_one_0_others_path <- paste0(pred_values_path, "max_one_0_others/")
dir.create(pred_values_max_one_0_others_path)

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_thres_a05, dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_study_max_one_0_others",      "thres_a05",
                               'Through Study for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_study_bl_thres_a05)
plot_pred_obs_various_bl_start(pred_study_max_one_0_others_thres_a05, dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_study_max_one_0_others_1-20", "thres_a05",
                               'Through 20 for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_study_bl_thres_a05, 1, 20)
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_thres_a05,   dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_400_max_one_0_others",        "thres_a05",
                               'Through 400 for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_400_bl_thres_a05)

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_satur,     dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_study_max_one_0_others",      "satur",
                               'Through Study for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_study_bl_satur)
plot_pred_obs_various_bl_start(pred_study_max_one_0_others_satur,     dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_study_max_one_0_others_1-20", "satur",
                               'Through 20 for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_study_bl_satur, 1, 20)
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_satur,       dat_ls, "_d_scl",
                               pred_values_max_one_0_others_path, "pred_400_max_one_0_others",        "satur",
                               'Through 400 for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ',
                               pred_400_bl_satur)

  # For participant's max for one node and baseline for others (run for only GIMME results so far)

pred_values_max_one_bl_others_path <- paste0(pred_values_path, "max_one_bl_others/")
dir.create(pred_values_max_one_bl_others_path)

plot_pred_obs_various_bl_start(pred_study_max_one_bl_others_gimme,    dat_ls, "_d2_scl",
                               pred_values_max_one_bl_others_path, "pred_study_max_one_bl_others_1-20", "gimme",
                               'Through 20 for GIMME Starting From Max "pred_list_focal_var_label" and BL Otherwise (ID ',
                               pred_study_bl_gimme, 1, 20)

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
  
    # Check that y-axis spans range of prediction error values
  
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

    # For Mplus networks

lapply(names(pred_error_study_4_satur),     function(lifepak_id) {
  plot_pred_error(pred_error_study_4_satur[[lifepak_id]],
    paste0("pred_study_4_satur_error_", lifepak_id),
    paste0("Errors for Next 3 Through Study for Satur. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

lapply(names(pred_error_study_4_thres_a05), function(lifepak_id) {
  plot_pred_error(pred_error_study_4_thres_a05[[lifepak_id]],
    paste0("pred_study_4_thres_a05_error_", lifepak_id),
    paste0("Errors for Next 3 Through Study for Thres. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

    # For GIMME networks

lapply(names(pred_error_study_4_gimme),     function(lifepak_id) {
  plot_pred_error(pred_error_study_4_gimme[[lifepak_id]],
    paste0("pred_study_4_gimme_error_", lifepak_id),
    paste0("Errors for Next 3 Through Study for GIMME Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

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
  ylab <- expression((italic("SD")["Data"] - italic("SD")["Pred. Errors"]) / italic("SD")["Data"])
  ylim <- c(-2, 2)

  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
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

    # For Mplus networks

lapply(names(diff_over_obs_sd_study_4_satur),     function(lifepak_id) {
  plot_diff_over_obs_sd(diff_over_obs_sd_study_4_satur[[lifepak_id]],
    paste0("diff_over_obs_sd_study_4_satur_", lifepak_id),
    paste0("Fit for Next 3 Through Study for Satur. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

lapply(names(diff_over_obs_sd_study_4_thres_a05), function(lifepak_id) {
  plot_diff_over_obs_sd(diff_over_obs_sd_study_4_thres_a05[[lifepak_id]],
    paste0("diff_over_obs_sd_study_4_thres_a05_", lifepak_id),
    paste0("Fit for Next 3 Through Study for Thres. Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

    # For GIMME networks

lapply(names(diff_over_obs_sd_study_4_gimme),     function(lifepak_id) {
  plot_diff_over_obs_sd(diff_over_obs_sd_study_4_gimme[[lifepak_id]],
                        paste0("diff_over_obs_sd_study_4_gimme_", lifepak_id),
                        paste0("Fit for Next 3 Through Study for GIMME Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})

# ---------------------------------------------------------------------------- #
# Create plots to explore weekend effect ----
# ---------------------------------------------------------------------------- #

# Compute 20 predicted values for saturated Mplus networks and for GIMME networks
# (about 2 days) starting from participant's centered values at each time point

pred_study_20_satur <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], satur_adj_mats_var[[lifepak_id]], 20, n_study_timepoints[[lifepak_id]], "_d_scl")
})
pred_study_20_gimme <- lapply(names(gimme_adj_mats_var), function(lifepak_id) {
  compute_k_pred(dat_ls[[lifepak_id]], gimme_adj_mats_var[[lifepak_id]], 20, n_study_timepoints[[lifepak_id]], "_d2_scl")
})

names(pred_study_20_satur) <- names(satur_adj_mats_var)
names(pred_study_20_gimme) <- names(gimme_adj_mats_var)

# Define function to prepare data for plots to explore weekend effect

prep_dat_wend_plots <- function(part_data, pred, obs_var_suf, wday1, wday2) {
  # Merge predicted values with observed data
  
  names(part_data)[names(part_data) == "bin_no_adj"] <- "t"
  
  target_meta_cols <- c("t", "notification_datetime", "response_end_datetime", "response_wday", "response_wend")
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  target_var_cols <- paste0(vars, obs_var_suf)
  
  part_data_tmp <- part_data[, c(target_meta_cols, target_var_cols)]
  
  pred <- merge(pred, part_data_tmp, by = "t", all.x = TRUE)
  pred <- pred[order(pred$iter, pred$iter_t), ]
  
  # Label each iteration's predicted values with weekday of its starting value
  
  tmp_df <- pred[pred$iter_t == 1, c("iter", "iter_t", "response_wday")]
  tmp_df$iter_start_wday <- tmp_df$response_wday
  tmp_df <- tmp_df[c("iter", "iter_start_wday")]
  
  pred <- merge(pred, tmp_df, "iter", all.x = TRUE)
  
  # Restrict to iterations with starting values on "wday1" or "wday2"
  
  wend_plot_df <- pred[pred$iter_start_wday %in% c(wday1, wday2), ]
  
  return(wend_plot_df)
}

# Run function

  # For saturated Mplus networks

wend_plot_dfs_satur_fri_mon <- lapply(names(dat_ls), function(lifepak_id) {
  prep_dat_wend_plots(dat_ls[[lifepak_id]], pred_study_20_satur[[lifepak_id]], "_d_scl", "Friday", "Monday")
})
wend_plot_dfs_satur_fri_sun <- lapply(names(dat_ls), function(lifepak_id) {
  prep_dat_wend_plots(dat_ls[[lifepak_id]], pred_study_20_satur[[lifepak_id]], "_d_scl", "Friday", "Sunday")
})
wend_plot_dfs_satur_fri_sat <- lapply(names(dat_ls), function(lifepak_id) {
  prep_dat_wend_plots(dat_ls[[lifepak_id]], pred_study_20_satur[[lifepak_id]], "_d_scl", "Friday", "Saturday")
})

names(wend_plot_dfs_satur_fri_mon) <- names(dat_ls)
names(wend_plot_dfs_satur_fri_sun) <- names(dat_ls)
names(wend_plot_dfs_satur_fri_sat) <- names(dat_ls)

  # For GIMME networks

wend_plot_dfs_gimme_fri_mon <- lapply(names(dat_ls), function(lifepak_id) {
  prep_dat_wend_plots(dat_ls[[lifepak_id]], pred_study_20_gimme[[lifepak_id]], "_d2_scl", "Friday", "Monday")
})

names(wend_plot_dfs_gimme_fri_mon) <- names(dat_ls)

# Define function to create plots to explore weekend effect
# - Starting from one time point or (for "k" predicted values) starting from many 
#   (pred_start_t_points = "one" or "many")
# - Optional: For "k" predicted values starting from many time points, use a different 
#   color for each iteration ("iter_colors = TRUE")

create_wend_plot <- function(wend_plot_df, obs_var_suf, pred_start_t_points, wday1, wday2,
                             plot_name, plot_title, iter_colors_by_wday = NULL) {
  df <- wend_plot_df
  
  df$t_from_start <- NA
  df$t_from_start <- df$iter_t - 1
  
  # Define plot settings
  
  t_from_start_values <- unique(df$t_from_start)
  
  xlab <- "Time Points From Starting Time Point"
  ylab <- expression(paste("Centered Value / ", italic("SD")))
  xlim_ll <- 0
  xlim_ul <- max(t_from_start_values)
  ylim_ll <- -10
  ylim_ul <- 10
  lwd <- 1.5
  pch <- 16
  
  if (pred_start_t_points == "one") {
    color_pred_wday1 <- "#3B809A"
    color_pred_wday2 <- "#F17B51"
  } else if (pred_start_t_points == "many") {
    n_iterations_wday1 <- length(unique(df$iter[df$iter_start_wday == wday1]))
    n_iterations_wday2 <- length(unique(df$iter[df$iter_start_wday == wday2]))
    
    if (is.null(iter_colors_by_wday)) {
      color_pred_wday1 <- rep("#3B809A", n_iterations_wday1)
      color_pred_wday2 <- rep("#F17B51", n_iterations_wday2)
    } else if (iter_colors_by_wday == TRUE) {
      set.seed(1234)
      color_pred_wday1 <- sequential_hcl(n_iterations_wday1, palette = "Teal")
      color_pred_wday2 <- sequential_hcl(n_iterations_wday2, palette = "Peach")
    }
    
    color_pred_wday1 <- adjust_transparency(color_pred_wday1, .5)
    color_pred_wday2 <- adjust_transparency(color_pred_wday2, .5)
  }
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  pred_cols    <- paste0(vars, "_pred")
  obs_cols     <- paste0(vars, obs_var_suf)

  # Check that y-axis spans range of observed and predicted values
  
  obs_cols_min  <- min(df[, obs_cols],  na.rm = TRUE)
  obs_cols_max  <- max(df[, obs_cols],  na.rm = TRUE)
  
  pred_cols_min <- min(df[, pred_cols], na.rm = TRUE)
  pred_cols_max <- max(df[, pred_cols], na.rm = TRUE)
  
  overall_min   <- min(obs_cols_min, pred_cols_min)
  overall_max   <- max(obs_cols_max, pred_cols_max)
  
  if (overall_min < ylim_ll) {
    stop(paste0("Make lower limit of 'ylim' <= ", overall_min))
  }
  if (overall_max > ylim_ul) {
    stop(paste0("Make upper limit of 'ylim' >= ", overall_max))
  }
  
  # Create plots
  
  pdf(paste0("./results/pred_values/wend_effect/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    pred_col    <- pred_cols[i]
    obs_col     <- obs_cols[i]
    var_label   <- var_labels[i]
    
    # Start with empty plot
    
    plot(df$t_from_start, df[, pred_col], main = var_label, type = "n", 
         xlab = xlab, ylab = ylab, xlim = c(xlim_ll, xlim_ul), ylim = c(ylim_ll, ylim_ul))
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    # Create separate data frame for each weekday
    
    df_wday1 <- df[df$iter_start_wday == wday1, ]
    df_wday2 <- df[df$iter_start_wday == wday2, ]
    
    # Plot predicted values
    
    if (pred_start_t_points == "one") {
      lines(df_wday1$t_from_start, df_wday1[, pred_col], lwd = lwd, col = color_pred_wday1)
      lines(df_wday2$t_from_start, df_wday2[, pred_col], lwd = lwd, col = color_pred_wday2)
    } else if (pred_start_t_points == "many") {
      iters_wday1 <- unique(df_wday1$iter)
      iters_wday2 <- unique(df_wday2$iter)
      
      for (j in 1:n_iterations_wday1) {
        iter_pred  <- df_wday1[df_wday1$iter == iters_wday1[j], ]
        iter_color <- color_pred_wday1[j]

        lines(iter_pred$t_from_start, iter_pred[, pred_col], lwd = lwd, col = iter_color)
      }
      for (j in 1:n_iterations_wday2) {
        iter_pred  <- df_wday2[df_wday2$iter == iters_wday2[j], ]
        iter_color <- color_pred_wday2[j]
        
        lines(iter_pred$t_from_start, iter_pred[, pred_col], lwd = lwd, col = iter_color)
      }
    }
    
    # TODO (consider removing): Plot observed values as points
    
    # points(df$t_from_start, df[, obs_col])
    
    # TODO (maybe put in plot): Add legend
    
    mtext(paste(wday1, "Starts: Teal;", wday2, "Starts: Peach"),
          side = 3, line = 0, adj = 0, cex = .5)
  }
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

# Run function

dir.create("./results/pred_values/wend_effect/")

  # For saturated Mplus networks

    # For 20 predicted values starting from each time point on a Friday or Monday

lapply(names(wend_plot_dfs_satur_fri_mon), function(lifepak_id) {
  create_wend_plot(wend_plot_dfs_satur_fri_mon[[lifepak_id]], "_d_scl", "many", "Friday", "Monday",
                   paste0("wend_pred_study_20_satur_fri_mon_iter_colors_", lifepak_id),
                   paste0("Next 20 Through Study for Satur. Starting From Each Obs. Value on Fri. or Mon. (ID ", lifepak_id, ")"),
                   iter_colors_by_wday = TRUE)
})

  # TODO (keep these?): For 20 predicted values starting from each time point on a Friday or Sunday

lapply(names(wend_plot_dfs_satur_fri_sun), function(lifepak_id) {
  create_wend_plot(wend_plot_dfs_satur_fri_sun[[lifepak_id]], "_d_scl", "many", "Friday", "Sunday",
                   paste0("wend_pred_study_20_satur_fri_sun_iter_colors_", lifepak_id),
                   paste0("Next 20 Through Study for Satur. Starting From Each Obs. Value on Fri. or Sun. (ID ", lifepak_id, ")"),
                   iter_colors_by_wday = TRUE)
})

  # TODO (keep these?): For 20 predicted values starting from each time point on a Friday or Saturday

lapply(names(wend_plot_dfs_satur_fri_sat), function(lifepak_id) {
  create_wend_plot(wend_plot_dfs_satur_fri_sat[[lifepak_id]], "_d_scl", "many", "Friday", "Saturday",
                   paste0("wend_pred_study_20_satur_fri_sat_iter_colors_", lifepak_id),
                   paste0("Next 20 Through Study for Satur. Starting From Each Obs. Value on Fri. or Sat. (ID ", lifepak_id, ")"),
                   iter_colors_by_wday = TRUE)
})





  # For GIMME networks

    # For 20 predicted values starting from each time point on a Friday or Monday

lapply(names(wend_plot_dfs_gimme_fri_mon), function(lifepak_id) {
  create_wend_plot(wend_plot_dfs_gimme_fri_mon[[lifepak_id]], "_d2_scl", "many", "Friday", "Monday",
                   paste0("wend_pred_study_20_gimme_fri_mon_iter_colors_", lifepak_id),
                   paste0("Next 20 Through Study for GIMME Starting From Each Obs. Value on Fri. or Mon. (ID ", lifepak_id, ")"),
                   iter_colors_by_wday = TRUE)
})

# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





