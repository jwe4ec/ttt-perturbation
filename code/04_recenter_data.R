# ---------------------------------------------------------------------------- #
# Recenter Data -----
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/from_ttt-p1-main-analysis/final_clean/data_var_perturb.RDS")

dat <- data_var_perturb

# ---------------------------------------------------------------------------- #
# Compute day of "response_end_datetime" and weekend indicator ----
# ---------------------------------------------------------------------------- #

dat$response_wday <- weekdays(as.Date(dat$response_end_datetime))

# Compute weekend indicator

wend_days <- c("Saturday", "Sunday")

dat$response_wend[dat$response_wday %in% wend_days]    <- 1
dat$response_wend[!(dat$response_wday %in% wend_days)] <- 0
dat$response_wend[is.na(dat$response_wday)]            <- NA

# ---------------------------------------------------------------------------- #
# Center each variable separately for each participant  ----
# ---------------------------------------------------------------------------- #

# In "ttt-p1-main-analysis" repo, linear trends were removed (creating "_d" variables).
# Now try centering by both (a) removing linear trend if most (i.e., >= 70%) of observations
# are not close (i.e., +/- 3 units from median; otherwise, remove median) and (b) removing 
# weekend effect (creating "_d2" variables). In prior version of "_d2" variables, we removed 
# linear trend if median was not 0 or 100 (as such variables seemed unipolar), and removed 
# weekend effect, but a variable could still be unipolar if its median is not exactly 0 or 
# 100 (thus our new method independent of median's value).

vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

for (var in vars) {
  # Create new column for each variable's residuals and initialize with NA
  
  residual_col <- paste0(var, "_d2")
  
  dat[[residual_col]] <- NA
  
  for (participant in unique(dat$lifepak_id)) {
    # Subset data for current participant
    
    part_data <- subset(dat, lifepak_id == participant)
    
    # Compute proportion of observations +/- 3 units from median     # TODO: Finalize thresholds
    
    mdn <- median(part_data[[var]], na.rm = TRUE)
    
    mdn_thres_ll <- mdn - 3
    mdn_thres_ul <- mdn + 3
    
    prop_in_mdn_thres <-
      sum(part_data[[var]] >= mdn_thres_ll & part_data[[var]] <= mdn_thres_ul, na.rm = TRUE) / 
      sum(!is.na(part_data[[var]]))
    
    # Fit linear model
    
    if (prop_in_mdn_thres >= .7) {                                   # TODO: Finalize thresholds
      # Print median for relevant example participants and variables
      
      if (participant %in% c("272769", "861114", "326177")) {
        cat("Mdn for example lifepak_id", participant, "variable", var, ":", mdn, "\n")
      }
      
      # TODO: Print median for all relevant participants and variables
      
      # cat("Mdn for lifepak_id", participant, "variable", var, ":", mdn, "\n")
      
      # Remove median and weekend effect
      
      part_data$var_mdn_rm <- NA
      part_data$var_mdn_rm <- part_data[[var]] - mdn
      
      fit <- lm(part_data$var_mdn_rm ~ part_data$response_wend, data = part_data)
    } else {
      # Remove linear trend and weekend effect
      
      fit <- lm(part_data[[var]] ~ part_data$bin_no_adj + part_data$response_wend, data = part_data)
    }
    
    # Calculate residuals for non-NA values
    
    residuals <- residuals(fit)
    
    dat[dat$lifepak_id == participant & !is.na(dat[[var]]), residual_col] <- residuals
  }
}

# ---------------------------------------------------------------------------- #
# Save data ----
# ---------------------------------------------------------------------------- #

data_var_perturb2 <- dat

recentered_path <- "./data/recentered/"

dir.create(recentered_path)

save(data_var_perturb2, file = paste0(recentered_path, "data_var_perturb2.RDS"))