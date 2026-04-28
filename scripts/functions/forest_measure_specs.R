# Single source of truth for main-figure forest outcomes (panels A & B, T1 and T2).
#
# Regression treatment coding (see forest_plot_estimates.R::persuasion_analysis):
#   text_treat = 1 for the focal book, 0 for the other. For MVS / Pursuit of Happiness
#   that means Haidt = 1, Lewis = 0 (text_treatment = "Haidt").
#
# `ate_flip` does NOT change that coding; it only rescales the reported Cohen's d so the
# figure uses one persuasion direction (same as original main analyses):
#   +1: raw text_treat coef already matches the figure sign convention
#   -1: flip the coef for the figure (e.g. DOGE/Trump disapproval coding; MVS so d aligns
#       with the Lewis-panel “Lewis − Haidt” style for comparability across facets)
#
# Interaction panel B uses flip `ate_flip * -1` for every row (including MVS).
#
# Cohen's d uses SD(pre) from wave-1 completers `d_ref` (see forest_plot_estimates.R).

forest_measure_specs <- function() {
  tibble::tribble(
    ~label, ~pre_dv, ~ate_flip, ~facet, ~change_dv_t1, ~change_dv_t2, ~text_treatment,
    "IRS Favorability", "irs_approval_pre", 1L, "lewis", "irs_approval_change", "irs_change_t2", "Lewis",
    "IRS Funding Support", "enforce_pre_1", 1L, "lewis", "enforce_change", "enforce_change_t2", "Lewis",
    "Civil Service Favorability", "civil_service_pre", 1L, "lewis", "civil_service_change", "civil_service_change_t2", "Lewis",
    "SSA Agent Favorability", "fav_ssa_pre_1", 1L, "lewis", "fav_ssa_change", "fav_ssa_change_t2", "Lewis",
    "DOGE Disapproval", "doge_pre_1", -1L, "lewis", "doge_change", "doge_change_t2", "Lewis",
    "Trump Disapproval", "trump_pre_1", -1L, "lewis", "trump_change", "trump_change_t2", "Lewis",
    "Reduced Material Values", "mvs_pre", -1L, "haidt", "mvs_change", "mvs_change_t2", "Haidt"
  )
}

forest_panel_labels <- function() {
  list(
    lewis = "Trt Text: 'Cyber Sleuth'",
    haidt = "Trt Text: 'Pursuit of Happiness'"
  )
}
