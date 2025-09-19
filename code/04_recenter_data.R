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
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./code/01_define_functions.R")

# Check R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load package

groundhog.library(DescTools, groundhog_day)

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
# Now try centering by both (a) removing linear trend if mode (if it represents nontrivial 
# proportion, >= 25%, of data) is not 0 or 100 (as such variables seem unipolar, in which 
# case we remove the mode) and (b) removing weekend effect (creating "_d2" variables).
# - In a prior version of "_d2" variables, we removed linear trend if median was not 
# 0 or 100, and removed weekend effect, but a variable could still be unipolar if its 
# median is not exactly 0 or 100. Thus, in a second prior version, we removed linear 
# trend if most (i.e., >= 70%) of observations are not close (i.e., +/- 3 units from 
# median; otherwise, remove median), and removed weekend effect, but this still did not 
# capture variables that seem unipolar. In a third prior version, we removed linear
# trend if mode is not 0 or 100, but given the many response options (0-100) the mode
# can be 0 or 100 for a trivial proportion of data (thus our current approach).

vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

for (var in vars) {
  # Create new column for each variable's residuals and initialize with NA
  
  residual_col <- paste0(var, "_d2")
  
  dat[[residual_col]] <- NA
  
  for (participant in unique(dat$lifepak_id)) {
    # Subset data for current participant
    
    part_data <- subset(dat, lifepak_id == participant)
    
    # Compute mode
    
    mo <- Mode(part_data[[var]], na.rm = TRUE)
    
    # If mode is singular and at a pole (i.e., 0 or 100), compute proportion of
    # data at mode and determine whether that proportion is nontrivial (>= 25%)
    
    prop_at_single_mode_at_pole <- NA
    single_nontriv_mode_at_pole <- NA
    
    if (length(mo) == 1) {
      if (is.na(mo)) {
        single_nontriv_mode_at_pole <- FALSE
        
        warning(paste0(participant, "'s mode for '", var, "' is NA"))
      } else if (mo %in% c(0, 100)) {
        prop_at_single_mode_at_pole <- sum(part_data[[var]] == mo, na.rm = TRUE) / 
                                         sum(!is.na(part_data[[var]]))
        
        single_nontriv_mode_at_pole <- prop_at_single_mode_at_pole >= .25
        
        if (single_nontriv_mode_at_pole) {
          print(paste0(participant, " has mode of ", mo, " representing nontrivial ",
                       round(prop_at_single_mode_at_pole * 100, 1), 
                       "% of data for '", var, "'"))
        }
      } else {
        single_nontriv_mode_at_pole <- FALSE
      }
    } else if (length(mo) > 1) {
      single_nontriv_mode_at_pole <- FALSE
      
      # Note: Several participants have multiple nodes
      
      # warning(paste0(participant, " has multiple modes for '", var, "'"))
    }
    
    # Fit linear model
    
    if (single_nontriv_mode_at_pole) {
      # Remove mode and weekend effect
      
      part_data$var_mo_rm <- NA
      part_data$var_mo_rm <- part_data[[var]] - mo
      
      fit <- lm(part_data$var_mo_rm ~ part_data$response_wend, data = part_data)
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