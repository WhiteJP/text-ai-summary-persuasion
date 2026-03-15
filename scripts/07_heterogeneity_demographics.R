# =============================================================================
# 07 · Treatment-Effect Heterogeneity by Demographics
# =============================================================================
# Tests whether treatment effects vary across demographic moderators.
# Model: change ~ text_treat * moderator + pre_score_z (no format).
# Holm correction applied across moderators within each DV.
# Outputs a summary table of moderator p-values (raw and Holm-corrected).

library(dplyr)
library(purrr)
library(estimatr)
library(broom)

d <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# DV specifications
# =============================================================================

dv_specs <- tribble(
  ~change_dv,             ~pre_dv,             ~label,                    ~text_treatment,
  "irs_approval_change",  "irs_approval_pre",  "IRS Approval Composite",  "Lewis",
  "civil_service_change", "civil_service_pre", "Civil Service Composite", "Lewis",
  "doge_change",          "doge_pre_1",        "DOGE Approval",           "Lewis",
  "trump_change",         "trump_pre_1",       "Trump Approval",          "Lewis",
  "enforce_change",       "enforce_pre_1",     "IRS Enforcement",         "Lewis",
  "fav_ssa_change",       "fav_ssa_pre_1",     "SSA Agents Favorability", "Lewis",
  "mvs_change",           "mvs_pre",           "Material Values Scale",   "Haidt"
)

# =============================================================================
# Moderator set and labels
# =============================================================================

moderators <- c(
  "gender_recoded", "age", "educ_cat_clean", "income_cat_clean",
  "race_white_fct", "strong_republican", "strong_conservative"
)

moderator_labels <- c(
  gender_recoded      = "Gender",
  age                 = "Age",
  educ_cat_clean      = "Education (categorical)",
  income_cat_clean    = "Household Income",
  race_white_fct      = "Race (White vs Non-White)",
  strong_republican   = "Strong Republican",
  strong_conservative = "Strong Conservative"
)

# =============================================================================
# Core heterogeneity test function
# =============================================================================

estimate_het_one_mod <- function(data, change_dv, pre_dv, text_treatment, moderator) {
  d_mod <- data %>%
    filter(
      !is.na(.data[[change_dv]]),
      !is.na(.data[[pre_dv]]),
      !is.na(.data[[moderator]])
    ) %>%
    mutate(
      .outcome = .data[[change_dv]],
      .treat = as.numeric(text == text_treatment),
      .pre_z = as.numeric(scale(.data[[pre_dv]]))
    )

  is_numeric_mod <- is.numeric(d_mod[[moderator]])

  if (is_numeric_mod) {
    d_mod$.mod <- as.numeric(d_mod[[moderator]])
  } else {
    d_mod$.mod <- factor(d_mod[[moderator]])
  }

  het_model <- lm_robust(.outcome ~ .treat * .mod + .pre_z, data = d_mod)

  int_terms <- grep("^\\.treat:\\.mod", names(coef(het_model)), value = TRUE)

  if (length(int_terms) > 0) {
    beta_int <- coef(het_model)[int_terms]
    V_int <- vcov(het_model)[int_terms, int_terms, drop = FALSE]
    wald_stat <- tryCatch(
      as.numeric(t(beta_int) %*% solve(V_int) %*% beta_int),
      error = function(e) NA_real_
    )
    df1 <- length(int_terms)
    df2 <- het_model$df.residual
    f_stat <- wald_stat / df1
    het_p <- pf(f_stat, df1, df2, lower.tail = FALSE)
  } else {
    f_stat <- NA_real_; df1 <- NA_integer_; df2 <- NA_integer_; het_p <- NA_real_
  }

  tibble(
    moderator = moderator,
    mod_label = moderator_labels[moderator],
    f_statistic = f_stat, df1 = df1, df2 = df2, p_value = het_p
  )
}

# =============================================================================
# Run all DVs x moderators
# =============================================================================

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
  )
}

cat("\n=== Treatment-Effect Heterogeneity by Demographics ===\n\n")

all_het_tests <- list()

for (dv_idx in seq_len(nrow(dv_specs))) {
  spec <- dv_specs[dv_idx, ]

  dv_tests <- map_dfr(moderators, function(mod) {
    estimate_het_one_mod(d, spec$change_dv, spec$pre_dv, spec$text_treatment, mod)
  }) %>%
    mutate(dv = spec$label, .before = 1)

  dv_tests$p_holm <- p.adjust(dv_tests$p_value, method = "holm")
  all_het_tests[[spec$label]] <- dv_tests
}

het_summary <- bind_rows(all_het_tests)

# =============================================================================
# Summary table
# =============================================================================

summary_table <- het_summary %>%
  transmute(
    DV = dv,
    Moderator = mod_label,
    F = sprintf("%.3f", f_statistic),
    df1, df2,
    p = format.pval(p_value, digits = 3),
    p_holm = format.pval(p_holm, digits = 3),
    sig = sigstars(p_holm)
  )

print(as.data.frame(summary_table), row.names = FALSE)

sig_count <- sum(het_summary$p_holm < 0.05, na.rm = TRUE)
if (sig_count > 0) {
  cat(sprintf("\n%d significant after Holm correction (p_holm < 0.05).\n", sig_count))
} else {
  cat("\nNo significant heterogeneity after Holm correction.\n")
}


