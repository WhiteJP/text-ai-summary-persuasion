# =============================================================================
# 05 · Main Analyses
# =============================================================================
# ATE estimation for all 7 preregistered DVs, Bayes factors for format
# equivalence, and Figure 1 (3-panel forest plot).

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(estimatr)
library(broom)
library(BayesFactor)
library(patchwork)
library(ggh4x)
library(ggtext)

d <- readRDS(here::here("data", "d.rds"))
fs::dir_create(here::here("output", "figures"))

# =============================================================================
# Analysis function
# =============================================================================
# Per preregistration: treatment coding for text, effect coding for format,
# z-scored pre-treatment measure, all interactions.

persuasion_analysis <- function(change_dv, pre_dv, data,
                                label, text_treatment = "Lewis") {
  data_model <- data %>%
    mutate(
      text_treat   = ifelse(text == text_treatment, 1, 0),
      format_eff   = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z  = scale(.data[[pre_dv]])[, 1]
    )

  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod    <- lm_robust(as.formula(formula_str), data = data_model)
  lm_mod <- lm(as.formula(formula_str), data = data_model)

  coefs            <- tidy(mod, conf.int = TRUE)
  text_coef        <- coefs |> filter(term == "text_treat")
  interaction_coef <- coefs |> filter(term == "text_treat:format_eff")

  pooled_sd <- sd(data[[pre_dv]], na.rm = TRUE)
  std_ate   <- text_coef$estimate / pooled_sd

  list(
    model = mod, lm_model = lm_mod, coefs = coefs,
    text_coef = text_coef, interaction_coef = interaction_coef,
    pooled_sd = pooled_sd, std_ate = std_ate
  )
}

# =============================================================================
# Run analyses for all 7 DVs
# =============================================================================

irs_approval   <- persuasion_analysis("irs_approval_change",  "irs_approval_pre",  d, "IRS Approval",     "Lewis")
civil_service  <- persuasion_analysis("civil_service_change", "civil_service_pre", d, "Civil Service",     "Lewis")
doge_approval  <- persuasion_analysis("doge_change",          "doge_pre_1",        d, "DOGE Approval",    "Lewis")
trump_approval <- persuasion_analysis("trump_change",         "trump_pre_1",       d, "Trump Approval",   "Lewis")
irs_enforce    <- persuasion_analysis("enforce_change",       "enforce_pre_1",     d, "IRS Enforcement",  "Lewis")
fav_ssa        <- persuasion_analysis("fav_ssa_change",       "fav_ssa_pre_1",     d, "SSA Favorability", "Lewis")
mvs            <- persuasion_analysis("mvs_change",           "mvs_pre",           d, "Material Values",  "Haidt")

# =============================================================================
# ATE summary table
# =============================================================================

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
  )
}

all_analyses <- list(irs_approval, civil_service, doge_approval,
                     trump_approval, irs_enforce, fav_ssa, mvs)
all_labels   <- c("IRS Approval", "Civil Service", "DOGE Approval",
                   "Trump Approval", "IRS Enforcement", "SSA Favorability",
                   "Material Values")

cat("\n=== Average Treatment Effects (ATE) ===\n\n")

ate_table <- map2_dfr(all_analyses, all_labels, function(a, lab) {
  tc <- a$text_coef
  ic <- a$interaction_coef
  tibble::tibble(
    DV              = lab,
    `ATE (b)`       = sprintf("%.3f%s", tc$estimate, sigstars(tc$p.value)),
    `Cohen's d`     = sprintf("%.2f", a$std_ate),
    `95% CI`        = sprintf("[%.3f, %.3f]", tc$conf.low, tc$conf.high),
    p               = round(tc$p.value, 4),
    `Interaction`   = sprintf("%.3f%s", ic$estimate, sigstars(ic$p.value)),
    `Int. p`        = round(ic$p.value, 4)
  )
})

print(as.data.frame(ate_table), row.names = FALSE)

# =============================================================================
# Bayes factors for format equivalence
# =============================================================================

cat("\n\n=== Bayes Factors for Format Equivalence (BF01) ===\n\n")

compute_bf_focused <- function(change_dv, pre_dv, text_treatment, data,
                               rscale = sqrt(2) / 4) {
  data_model <- data |>
    mutate(
      text_treat  = ifelse(text == text_treatment, 1, 0),
      format_eff  = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z = as.numeric(scale(.data[[pre_dv]]))
    ) |>
    select(outcome = all_of(change_dv), text_treat, format_eff, pre_score_z) |>
    drop_na()

  df <- as.data.frame(data_model)

  # Full model (with text:format interaction)
  bf_with_int <- lmBF(
    outcome ~ text_treat + format_eff + pre_score_z +
      text_treat:format_eff + text_treat:pre_score_z + format_eff:pre_score_z,
    data = df, rscaleCont = rscale
  )

  # Restricted model (without text:format interaction)
  bf_no_int <- lmBF(
    outcome ~ text_treat + format_eff + pre_score_z +
      text_treat:pre_score_z + format_eff:pre_score_z,
    data = df, rscaleCont = rscale
  )

  bf_01 <- bf_no_int / bf_with_int
  extractBF(bf_01)$bf
}

bf_specs <- tribble(
  ~change_dv,             ~pre_dv,              ~text_treatment, ~label,
  "irs_approval_change",  "irs_approval_pre",   "Lewis",         "IRS Favorability",
  "civil_service_change", "civil_service_pre",  "Lewis",         "Civil Service Favorability",
  "doge_change",          "doge_pre_1",         "Lewis",         "DOGE Disapproval",
  "trump_change",         "trump_pre_1",        "Lewis",         "Trump Disapproval",
  "enforce_change",       "enforce_pre_1",      "Lewis",         "IRS Funding Support",
  "fav_ssa_change",       "fav_ssa_pre_1",      "Lewis",         "SSA Agent Favorability",
  "mvs_change",           "mvs_pre",            "Haidt",         "Material Values"
)

bf_primary <- pmap_dfr(bf_specs, function(change_dv, pre_dv, text_treatment, label) {
  tibble::tibble(label = label, bf_01 = compute_bf_focused(change_dv, pre_dv, text_treatment, d))
})

print(as.data.frame(bf_primary |> mutate(bf_01 = round(bf_01, 2))))

# =============================================================================
# Figure 1: Main Results (ATE + Interaction + Bayes Factors)
# =============================================================================

cat("\nGenerating main results figure...\n")

# Helper: extract, standardize, and flip a coefficient for the forest plot
extract_forest_coef <- function(analysis, label, pre_dv, panel, coef_type, flip) {
  pre_sd <- sd(d[[pre_dv]], na.rm = TRUE)
  tc <- if (coef_type == "ate") analysis$text_coef else analysis$interaction_coef
  if (nrow(tc) == 0) return(NULL)
    tibble::tibble(
    label   = label,
    panel   = panel,
    effect  = coef_type,
    d       = flip * tc$estimate / pre_sd,
    se_d    = tc$std.error / pre_sd,
    ci_low  = flip * (if (flip ==  1) tc$conf.low  else tc$conf.high) / pre_sd,
    ci_high = flip * (if (flip ==  1) tc$conf.high else tc$conf.low)  / pre_sd,
    p       = tc$p.value
  )
}

lewis_panel <- "Trt Text: 'Cyber Sleuth'"
haidt_panel <- "Trt Text: 'Pursuit of Happiness'"

# ate_flip: 1 = keep direction, -1 = reverse so positive = persuaded as intended
forest_specs <- list(
  list(a = irs_approval,   label = "IRS Favorability",                          pre = "irs_approval_pre",  ate_flip =  1),
  list(a = civil_service,  label = "Civil Service Favorability",                pre = "civil_service_pre", ate_flip =  1),
  list(a = doge_approval,  label = "DOGE Disapproval<sup>\u2020</sup>",         pre = "doge_pre_1",        ate_flip = -1),
  list(a = trump_approval, label = "Trump Disapproval<sup>\u2020</sup>",        pre = "trump_pre_1",       ate_flip = -1),
  list(a = irs_enforce,    label = "IRS Funding Support",                       pre = "enforce_pre_1",     ate_flip =  1),
  list(a = fav_ssa,        label = "SSA Agent Favorability",                    pre = "fav_ssa_pre_1",     ate_flip =  1)
)

# Panel A: Overall ATE (positive = persuaded in predicted direction)
ate_data <- bind_rows(
  map_dfr(forest_specs, ~ extract_forest_coef(
    .x$a, .x$label, .x$pre, lewis_panel, "ate", .x$ate_flip)),
  extract_forest_coef(mvs, "Reduced Material Values<sup>\u2020</sup>",
                      "mvs_pre", haidt_panel, "ate", -1)
) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

# Panel B: Interaction (positive = Full > Summary)
int_data <- bind_rows(
  map_dfr(forest_specs, ~ extract_forest_coef(
    .x$a, .x$label, .x$pre, lewis_panel, "interaction", .x$ate_flip * -1)),
  extract_forest_coef(mvs, "Reduced Material Values<sup>\u2020</sup>",
                      "mvs_pre", haidt_panel, "interaction", -1 * -1)
) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

p_ate <- ggplot(ate_data, aes(x = d, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), size = 0.4) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = c(6, 1.5)) +
  labs(x = "Overall ATE", y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_markdown())

p_int <- ggplot(int_data, aes(x = d, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), size = 0.4) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = c(6, 1.5)) +
  labs(x = "Full text ATE \u2013 AI summary ATE", y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Panel C: Bayes factors
bf_panel_labels <- c(
  lewis_panel = "'Cyber Sleuth'",
  haidt_panel = "'Pursuit of Happiness'"
)

# Assign display labels matching the ATE/interaction panels
bf_display_labels <- c(
  "IRS Favorability", "Civil Service Favorability",
  "DOGE Disapproval<sup>\u2020</sup>", "Trump Disapproval<sup>\u2020</sup>",
  "IRS Funding Support", "SSA Agent Favorability",
  "Reduced Material Values<sup>\u2020</sup>"
)

bf_plot_data <- bf_primary |>
  mutate(
    label = bf_display_labels,
    panel = if_else(
      label == "Reduced Material Values<sup>\u2020</sup>",
      bf_panel_labels[["haidt_panel"]],
      bf_panel_labels[["lewis_panel"]]
    ),
    label = factor(label, levels = levels(int_data$label)),
    panel = factor(panel, levels = unname(bf_panel_labels))
  )

p_bf <- ggplot(bf_plot_data, aes(x = bf_01, y = label)) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey45") +
  geom_point() +
  geom_text(aes(label = sprintf("%.2f", bf_01)), hjust = -0.225, size = 2.8) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.30))) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = c(6, 1.5)) +
  labs(
    x = expression("Format equivalence" ~ (BF["01"])),
    y = NULL
  ) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Combine panels
p_main_results <- p_ate + p_int + p_bf +
  plot_layout(widths = c(1, 1, 0.75)) +
  plot_annotation(tag_levels = "A") &
  theme(
    strip.text    = element_text(size = 8),
    axis.text.x   = element_text(size = 8),
    axis.title.x  = element_text(size = 8.5)
  )

ggsave(
  here::here("output", "figures", "main-results.png"),
  p_main_results,
  width = 7.5, height = 3.5, dpi = 300, bg = "white"
)

cat("Saved: output/figures/main-results.png\n")
