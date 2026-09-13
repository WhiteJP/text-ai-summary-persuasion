# =============================================================================
# 10 · Internal Consistency: Cronbach's Alpha
# =============================================================================
# Prints Cronbach's alpha for each composite (IRS Favorability, Civil Service,
# Material Values Scale) at pre, immediate post (T1), and 2-month follow-up
# (T2). Console only.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(psych)
})

d <- readRDS(here::here("data", "d.rds"))

composite_items <- list(
  `IRS Favorability` = list(
    pre = c("trust_irs_pre_1", "fav_irs_pre_1", "agents_pre_1"),
    t1 = c("trust_irs_post_1", "fav_irs_post_1", "agents_post_1"),
    t2 = c("trust_irs_post_1_fu", "fav_irs_post_1_fu", "agents_post_1_fu")
  ),
  `Civil Service Favorability` = list(
    pre = c("servants_pre_1", "fav_cs_pre_1"),
    t1 = c("servants_post_1", "fav_cs_post_1"),
    t2 = c("servants_post_1_fu", "fav_cs_post_1_fu")
  ),
  `Material Values Scale` = list(
    pre = paste0("mvs", 1:6, "_pre"),
    t1 = paste0("mvs", 1:6, "_post"),
    t2 = paste0("mvs", 1:6, "_post_fu")
  )
)

time_labels <- c(pre = "Pre", t1 = "T1 post", t2 = "T2 (2-month)")

alpha_one <- function(dat, cols, composite_name, timepoint) {
  missing_cols <- setdiff(cols, names(dat))
  if (length(missing_cols) > 0L) {
    return(tibble(
      Composite = composite_name,
      Timepoint = time_labels[[timepoint]],
      N_items = length(cols),
      N_obs = NA_integer_,
      Alpha = NA_real_,
      Mean_inter_item_r = NA_real_,
      Inter_item_r = NA_real_
    ))
  }

  item_data <- dat[, cols, drop = FALSE] |> drop_na()
  n_items <- length(cols)
  n_obs <- nrow(item_data)
  if (n_obs < 3L) {
    return(tibble(
      Composite = composite_name,
      Timepoint = time_labels[[timepoint]],
      N_items = n_items,
      N_obs = n_obs,
      Alpha = NA_real_,
      Mean_inter_item_r = NA_real_,
      Inter_item_r = NA_real_
    ))
  }

  a <- psych::alpha(item_data, check.keys = TRUE, discrete = FALSE)
  inter_item_r <- if (n_items == 2) {
    cor(item_data[[1]], item_data[[2]], use = "complete.obs")
  } else {
    NA_real_
  }

  tibble(
    Composite = composite_name,
    Timepoint = time_labels[[timepoint]],
    N_items = n_items,
    N_obs = n_obs,
    Alpha = a$total$raw_alpha,
    Mean_inter_item_r = a$total$average_r,
    Inter_item_r = inter_item_r
  )
}

cat("\n=== Internal Consistency: Cronbach's Alpha ===\n\n")

alpha_t1 <- imap_dfr(composite_items, function(items, composite_name) {
  bind_rows(
    alpha_one(d, items$pre, composite_name, "pre"),
    alpha_one(d, items$t1, composite_name, "t1")
  )
})

fu_path <- here::here("data", "d_with_followup.rds")
alpha_t2 <- NULL
n_t2 <- NA_integer_
if (file.exists(fu_path)) {
  d_fu <- readRDS(fu_path) |>
    filter(!is.na(.data$ResponseId_fu), .data$fu_completed_outcomes == 1L)
  n_t2 <- nrow(d_fu)
  alpha_t2 <- imap_dfr(composite_items, function(items, composite_name) {
    alpha_one(d_fu, items$t2, composite_name, "t2")
  })
} else {
  cat("Skipped T2 alpha: data/d_with_followup.rds not found.\n")
}

alpha_results <- bind_rows(alpha_t1, alpha_t2) |>
  mutate(Timepoint = factor(Timepoint, levels = unname(time_labels))) |>
  arrange(Composite, Timepoint)

print_tbl <- alpha_results %>%
  mutate(
    Alpha = ifelse(is.na(Alpha), "\u2014", sprintf("%.3f", Alpha)),
    Mean_inter_item_r = ifelse(
      is.na(Mean_inter_item_r), "\u2014", sprintf("%.3f", Mean_inter_item_r)
    ),
    Inter_item_r = ifelse(
      is.na(Inter_item_r), "\u2014", sprintf("%.3f", Inter_item_r)
    )
  )

print(as.data.frame(print_tbl), row.names = FALSE)

cat("\nNote: Inter-item r reported for 2-item composites.\n")
cat("T1 uses wave-1 completers (N = ", nrow(d), ").", sep = "")
if (!is.na(n_t2)) {
  cat(" T2 uses follow-up completers (N = ", n_t2, ").", sep = "")
}
cat("\n")
