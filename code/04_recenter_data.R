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
# Compute day of "response_time" and weekend indicator ----
# ---------------------------------------------------------------------------- #

dat$response_time_wday <- weekdays(as.Date(dat$response_time))

# Compute weekend indicator

wend_days <- c("Saturday", "Sunday")

dat$response_time_wend[dat$response_time_wday %in% wend_days]    <- 1
dat$response_time_wend[!(dat$response_time_wday %in% wend_days)] <- 0
dat$response_time_wend[is.na(dat$response_time_wday)]            <- NA

# ---------------------------------------------------------------------------- #
# Center each variable separately for each participant  ----
# ---------------------------------------------------------------------------- #

# In "ttt-p1-main-analysis" repo, linear trends were removed (creating "_d" variables).
# Now try centering by both (a) removing linear trend if median is not 0 or 100 (otherwise, 
# the variable seems unipolar) and (b) removing weekend effect (creating "_d2" variables).

vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

for (var in vars) {
  # Create new column for each variable's residuals and initialize with NA
  
  residual_col <- paste0(var, "_d2")
  
  dat[[residual_col]] <- NA
  
  for (participant in unique(dat$lifepak_id)) {
    # Subset data for current participant
    
    part_data <- subset(dat, lifepak_id == participant)
    
    # Fit linear model
    
    median <- median(part_data[[var]], na.rm = TRUE)
    
    if (median %in% c(0, 100)) {
      fit <- lm(part_data[[var]] ~ part_data$response_time_wend, data = part_data)
    } else {
      fit <- lm(part_data[[var]] ~ part_data$bin_no_adj + part_data$response_time_wend, data = part_data)
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