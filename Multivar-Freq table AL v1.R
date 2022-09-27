############################################################################################################################################
################### Code for a function that generates variable counts combined with multivariable regression odds ratios ##################
############################################################################################################################################
#### A. Lench 2022 #########################################################################################################################
############################################################################################################################################
# This code is for a function which will carry out a multivariable linear regression and construct a combined frequency/odds ratio table. 
# The inputs are an outcome variable, the explanatory variables a dataframe containing the data with these variables -
# and another instance of the dataframe with baselines specified for the regression if required.
# The output is a dataframe containing formatted counts and multivariable OR +/- 95%CI.
# This function is suited for analyses which involve multiple multivariable (without univariate) regressions.
# The code has been developed from methods in the epi r handbook (epirhandbook.com, September 2022).
############################################################################################################################################

# Packages
library(tidyverse)
library(broom)

############################################################################################################################################

### Main code for function

Multivar_Freq_func <- function(func_data, func_data_baselines, func_outcome, func_explanatory_vars) {

  # Compensate if baselines not set
  func_data_baselines <- if(is.data.frame(func_data_baselines)) func_data_baselines else func_data
  
  # Convert to factors
  func_data <- func_data %>%
    mutate_at(c(func_explanatory_vars),as.factor)
  func_data_baselines <- func_data_baselines %>%
    mutate_at(c(func_explanatory_vars),as.factor)
  
  # Drop rows of data with NA values in the outcome/explanatory variables, these are not included - assign values prior to analysis if needed 
  func_data <- func_data %>%
    drop_na(any_of(c(func_outcome,func_explanatory_vars)))
  func_data_baselines <- func_data_baselines %>%
    drop_na(any_of(c(func_outcome,func_explanatory_vars)))
  
  # Counts
  outcome_counts <- func_explanatory_vars %>% 
    map(.f = 
          ~{func_data %>%
              group_by(.data[[func_outcome[]]]) %>%     
              count(.data[[.x]]) %>%    
              pivot_wider(              
                names_from = .data[[func_outcome[]]],
                values_from = n) %>% 
              drop_na(.data[[.x]]) %>%         
              rename("category" = all_of(.x)) %>% 
              mutate(category = as.character(category)) %>%
              mutate(variable = .x)}) %>%
    
    bind_rows() %>%
    mutate(term = paste0(variable,category)) %>%
    rename(outcome_N = "0", outcome_Y = "1") %>%
    mutate(outcome_total = outcome_N + outcome_Y)
  
  # Multivariable regression
  outcome_multi <- func_explanatory_vars %>%
    str_c(collapse = "+") %>%
    str_c(paste0(func_outcome," ~ "), .) %>%
    glm(family = "binomial",
        data = func_data_baselines) %>%
    tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    mutate(outcome_CI=paste0(sprintf("%.2f", conf.low),"-", sprintf("%.2f", conf.high))) %>%
    rename(outcome_OR=estimate, outcome_CI_low=conf.low, outcome_CI_high=conf.high)
  
   # Join count and regression data
  multivar_freq <- outcome_counts %>%
    left_join(outcome_multi, by = "term")
  
  # Generate final fields and refining data frame
  multivar_freq <- multivar_freq %>%
    mutate(outcome_total = format(round(as.numeric(outcome_total), 0), nsmall=0, big.mark=",")) %>%
    mutate(outcome_Y = format(round(as.numeric(outcome_Y), 0), nsmall=0, big.mark=",")) %>%
    mutate(outcome_OR_CI = paste0(sprintf(outcome_OR, fmt = '%#.2f'), " (", outcome_CI, ")")) %>%
    select(variable, category, outcome_total, outcome_Y, outcome_OR_CI) %>%
    mutate_all(~replace(., . == "NA (NA)", "*")) %>%
    rename(variable=variable,category=category,n=outcome_total) %>%
    rename(!!paste0(func_outcome,"_n"):=outcome_Y) %>%
    rename(!!paste0(func_outcome,"_OR"):=outcome_OR_CI)
  
  return(multivar_freq)
  
}

############################################################################################################################################

### Example (see https://epirhandbook.com/en/univariate-and-multivariable-regression.html)

# packages
library(rio)
library(flextable)
library(officer)

# import linelist
linelist <- import("linelist_cleaned.rds")

# define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")

# convert dichotomous variables to 0/1 
linelist <- linelist %>%  
  mutate(across(                                      
    .cols = all_of(c(explanatory_vars, "outcome")),
    .fns = ~case_when(
      . %in% c("m", "yes", "Death")   ~ 1,
      . %in% c("f", "no",  "Recover") ~ 0,
      TRUE                            ~ NA_real_)))

# add in age_category to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "age_cat")

# pass to function and convert to flextable
test <- Multivar_Freq_func(linelist, NA, "outcome", explanatory_vars) %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  font(fontname = 'Calibri', part = "all") %>%
  fontsize(part = "all", size = 9) %>%
  height_all(height = 0.25, part = "body") %>%
  hrule(rule = "exact", part = "body")

# output
doc = read_docx() %>%
  body_add_flextable(test)
dfile = tempfile(fileext=".docx")
print(doc, target = dfile)
if(interactive()) browseURL(dfile)
