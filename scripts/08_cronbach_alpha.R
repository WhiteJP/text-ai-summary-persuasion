# =============================================================================
# 08 · Internal Consistency: Cronbach's Alpha
# =============================================================================
# Computes Cronbach's alpha for each composite scale at pre- and
# post-treatment timepoints.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(psych)

d <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# Composite item definitions
# =============================================================================

composite_items <- list(
  `IRS Approval` = list(
    pre  = c("trust_irs_pre_1", "fav_irs_pre_1", "agents_pre_1"),
    post = c("trust_irs_post_1", "fav_irs_post_1", "agents_post_1")
  ),
  `Civil Service` = list(
    pre  = c("servants_pre_1", "fav_cs_pre_1"),
    post = c("servants_post_1", "fav_cs_post_1")
  ),
  `Material Values Scale` = list(
    pre  = paste0("mvs", 1:6, "_pre"),
    post = paste0("mvs", 1:6, "_post")
  )
)

# =============================================================================
# Compute alpha for each composite x timepoint
# =============================================================================

cat("\n=== Internal Consistency: Cronbach's Alpha ===\n\n")

alpha_results <- imap_dfr(composite_items, function(items, composite_name) {
  imap_dfr(items, function(cols, timepoint) {
    item_data <- d[, cols] |> drop_na()
    n_items <- length(cols)
    n_obs <- nrow(item_data)

    a <- psych::alpha(item_data, check.keys = TRUE, discrete = FALSE)

    inter_item_r <- if (n_items == 2) {
      cor(item_data[[1]], item_data[[2]], use = "complete.obs")
    } else {
      NA_real_
    }

    tibble(
      Composite = composite_name,
      Timepoint = str_to_title(timepoint),
      N_items = n_items,
      N_obs = n_obs,
      Alpha = a$total$raw_alpha,
      Mean_inter_item_r = a$total$average_r,
      Inter_item_r = inter_item_r
    )
  })
})

# Print formatted table
print_tbl <- alpha_results %>%
  mutate(
    Alpha = sprintf("%.3f", Alpha),
    Mean_inter_item_r = sprintf("%.3f", Mean_inter_item_r),
    Inter_item_r = ifelse(is.na(Inter_item_r), "\u2014",
                          sprintf("%.3f", Inter_item_r))
  )

print(as.data.frame(print_tbl), row.names = FALSE)

cat("\nNote: Inter-item r reported for 2-item composites.\n")

