## This script runs frequency and Fisher's exact tests.
## Frequency tests were used to quantify the unevenness of resource distribution among
## the forecasting topics within each curriculum level.
## Fisher's exact tests were used to compare the distribution of resources within
## forecasting topics between forecasting courses and open-access, online resources and 
## between forecasting courses and forecasting-adjacent courses.
## We assign the expected frequency of each topic as the frequencies in the forecasting courses
## in both cases, while the observed is either the resources from open-access, online resources
## or forecasting-adjacent courses.
## The purpose of running this test is to statistically determine whether
## there is a difference in the curriculum landscape between forecasting course lessons and
## open-access, online resources and between forecasting course lessons and forecasting-adjacent
## courses.

## Authors: AM Willson & H Gallo
## Date modified: 28 February 2022

library(tidyverse)
library(freqtables)

rm(list = ls())

#### Load data ####
load('Data/cleaned_data_EF_course.RData')
EF_data = data
load('Data/cleaned_data_online.RData')
online_data = data
load('Data/cleaned_data.RData')
FA_data = data
rm(data)

#### Frequency (Pearson) tests ####

## These test whether the distribution of resources among forecasting topics
## is significantly different from an even distribution among all topics

# Forecasting course lessons
EF_data %>%
  freq_table(Sub.topic) %>%
  freq_test()

# Forecasting-adjacent courses
FA_data %>%
  freq_table(Sub.topic) %>%
  freq_test()

# Open-access, online resources
online_data %>%
  freq_table(Category) %>%
  freq_test()

## All three curriculum levels have resources unevenly distributed

#### Manipulating source function ####

## This is source code for the freq_test() function in the freqtables package
## The code was changed so that the p-value for the Fisher's test is now simulated
## with 2000 iterations

freq_test.freq_table_two_way <- function(.data, ...) {
  
  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n_row = n_col = n_total = n_expected = chi2_contrib = r = pchisq = NULL
  chi2_pearson = df = col_cat = n = row_cat = NULL
  
  # Check to make sure .data is a freq_table_two_way
  # --------------------------------------------
  if (!("freq_table_two_way" %in% class(.data))) {
    stop(".data must be of class freq_table_two_way. It is currently: ", class(.data))
  }
  
  # Calculate Pearson's Chi-square test
  # Test whether population is equally distributed across categories of .data
  # ---------------------------------------------------------------------
  out <- .data %>%
    dplyr::group_by(col_cat) %>%
    dplyr::mutate(n_col = sum(n)) %>%  # Find marginal totals for "columns"
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_expected     = (n_row * n_col) / n_total,
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      r              = unique(row_cat) %>% length(),
      c              = unique(col_cat) %>% length(),
      df             = (r -1) * (c - 1),
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )
  
  # Test for expected cell counts <= 5
  # ----------------------------------
  if ( min(out$n_expected) <= 5 ) {
    message(paste0("One or more expected cell counts are <= 5. Therefore, ",
                   "Fisher's Exact Test was used."))
    
    # Add Fisher's Exact Test
    # -----------------------
    # Convert .data to a matrix
    n_s  <- dplyr::pull(.data, n)
    mx   <- matrix(n_s, nrow = 2, byrow = TRUE)
    
    # Use R's built-in fisher.test
    fisher <- stats::fisher.test(mx, simulate.p.value = T, B = 2000)
    
    # Add Fisher's p_value to out
    out <- out %>%
      dplyr::mutate(p_fisher = fisher$p.value)
  }
  
  # Add class to out that describes the information it contains
  # -----------------------------------------------------------
  class(out) <- c("freq_table_two_way", class(out))
  
  # Return tibble of results
  out
}
#### Preparing data for Fisher's ####

EF_data_f = EF_data %>%
  select(Sub.topic) %>%
  mutate(Level = rep('EF', length(Sub.topic)))

FA_data_f = FA_data %>%
  select(Sub.topic) %>%
  mutate(Level = rep('FA', length(Sub.topic)))

online_data_f = online_data %>%
  select(Category) %>%
  mutate(Level = rep('O', length(Category)))
colnames(online_data_f)[1] = 'Sub.topic'

comb_f = rbind(EF_data_f, FA_data_f, online_data_f)

#### Fisher's exact tests ####

# Between all three
ft_three = comb_f %>%
  freq_table(Sub.topic, Level) %>%
  freq_test.freq_table_two_way()
ft_three

# Between forecasting course lessons and open-access, online resources
ft_EF_O = comb_f %>%
  filter(Level %in% c('EF', 'O')) %>%
  freq_table(Sub.topic, Level) %>%
  freq_test.freq_table_two_way()
ft_EF_O

# Between forecasting course lessons and forecasting-adjacent courses
ft_EF_FA = comb_f %>%
  filter(Level %in% c('EF', 'FA')) %>%
  freq_table(Sub.topic, Level) %>%
  freq_test.freq_table_two_way()
ft_EF_FA
