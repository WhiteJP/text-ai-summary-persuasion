# =============================================================================
# 07 · Paper figures
# =============================================================================
# Figure 1: IRS pre/post + ATE forest → output/figures/irs-and-ates.png
# Figure 2: format-interaction forest → output/figures/format-interactions.png
# Figure 3: Bayes-factor equivalence → output/figures/bf-equivalence.png
#
# Forest inputs: forest_t1.rds / forest_t2.rds (produced by 05/06).
# Control-text (material values) results are reported in text only.
#
# Figure 3 loads bf01_t1.csv / bf01_t2.csv written by 05/06
# (same forest_measure_specs() labels and seeds).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tidyr)
  library(purrr)
  library(readr)
})
has_ggh4x <- requireNamespace("ggh4x", quietly = TRUE)

fs::dir_create(here::here("output", "figures"))
fs::dir_create(here::here("output", "tables"))

# PNAS two-column width: 17.8 cm. cairo writes a PNG pHYs chunk at this dpi
# so pdfTeX uses the intended physical size when \includegraphics is called
# without a width.
pnas_2col_cm <- 17.8

d <- readRDS(here::here("data", "d.rds"))

# -----------------------------------------------------------------------------
# Figure 1A · IRS favorability pre/post
# -----------------------------------------------------------------------------
format_col_main <- c("Full" = "#4E79A7", "Summary" = "#F28E2B")

irs_cond <- d |>
  filter(text == "Lewis") |>
  select(format, pre = irs_approval_pre, post = irs_approval_post) |>
  mutate(pre_bin = factor(round(pre))) |>
  tidyr::drop_na(pre_bin, post)

irs_diag_line <- tibble::tibble(x = factor(1:7), y = 1:7)

# Jitter seed so the individual-level scatter is reproducible across runs.
set.seed(20250831)

paper_theme <- theme_bw() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, hjust = 1, margin = margin(r = 3)),
    axis.ticks.length = unit(2.5, "pt"),
    axis.title.x = element_text(size = 10, margin = margin(t = 3, b = 0)),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.7, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.spacing.y = unit(0.06, "cm"),
    legend.margin = margin(t = 2, r = 0, b = 0, l = 0, unit = "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(4, "pt"),
    plot.margin = margin(3, 6, 2, 3)
  )

stacked_colour_legend <- guide_legend(
  ncol = 1,
  override.aes = list(size = 0.5, linewidth = 0.6)
)

p_irs_prepost <- ggplot(irs_cond, aes(x = pre_bin, y = post)) +
  geom_line(
    data = irs_diag_line, aes(x = x, y = y, group = 1), inherit.aes = FALSE,
    linetype = "dashed", colour = "grey50", linewidth = 0.35
  ) +
  geom_violin(
    fill = "grey80", alpha = 0.55, linewidth = 0.25,
    scale = "width", colour = "grey50"
  ) +
  geom_point(
    aes(colour = format), alpha = 0.5, size = 1.1,
    position = position_jitter(width = 0.12, height = 0.08)
  ) +
  scale_colour_manual(
    name = NULL,
    values = format_col_main,
    breaks = c("Full", "Summary"),
    labels = c("Full text", "AI summary")
  ) +
  scale_y_continuous(breaks = 1:7) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 2, alpha = 1))) +
  labs(
    x = "Pre-treatment (rounded)",
    y = "Post-treatment"
  ) +
  paper_theme +
  theme(aspect.ratio = 1)

# -----------------------------------------------------------------------------
# Figure 1B / Figure 2 · ATE and format-interaction forests
# -----------------------------------------------------------------------------

forest_t1_path <- here::here("output", "intermediate", "forest_t1.rds")
forest_t2_path <- here::here("output", "intermediate", "forest_t2.rds")
if (!file.exists(forest_t1_path) || !file.exists(forest_t2_path)) {
  stop("Missing forest inputs; run 05_t1_analyses.R and 06_t2_analyses.R first.")
}

# Legend order (immediate first). Dodge order: second factor level is offset upward on y.
time_legend <- c("Immediately post-treatment", "~2 months later")
time_levels_dodge <- c("~2 months later", "Immediately post-treatment")

t1 <- readRDS(forest_t1_path)
t2 <- readRDS(forest_t2_path)

lewis_panel <- t1$lewis_panel

stopifnot(identical(lewis_panel, t2$lewis_panel))

col_time <- c(
  `Immediately post-treatment` = "#333333",
  `~2 months later` = "#E15759"
)

ate_all <- bind_rows(
  t1$ate |> mutate(time = factor(time_legend[[1]], levels = time_levels_dodge)),
  t2$ate |> mutate(time = factor(time_legend[[2]], levels = time_levels_dodge))
) |>
  filter(panel == lewis_panel) |>
  droplevels()

int_all <- bind_rows(
  t1$int |> mutate(time = factor(time_legend[[1]], levels = time_levels_dodge)),
  t2$int |> mutate(time = factor(time_legend[[2]], levels = time_levels_dodge))
) |>
  filter(panel == lewis_panel) |>
  droplevels()

p_ate <- ggplot(ate_all, aes(x = d, y = label, colour = time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = 0.55),
    size = 0.4,
    linewidth = 0.4
  ) +
  scale_x_continuous(breaks = scales::breaks_width(0.2), expand = expansion(mult = 0.06)) +
  scale_colour_manual(values = col_time, name = NULL, breaks = time_legend) +
  guides(colour = stacked_colour_legend) +
  labs(title = "(B) Average Attitude Change", x = "Overall ATE", y = NULL) +
  paper_theme

p_int <- ggplot(int_all, aes(x = d, y = label, colour = time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = 0.55),
    size = 0.4,
    linewidth = 0.4
  ) +
  scale_x_continuous(breaks = scales::breaks_width(0.3), expand = expansion(mult = 0.06)) +
  scale_colour_manual(values = col_time, name = NULL, breaks = time_legend) +
  guides(colour = stacked_colour_legend) +
  labs(
    title = NULL,
    x = "Full text ATE \u2013 AI summary ATE",
    y = NULL
  ) +
  paper_theme

# Figure 1: drop the standalone square constraint so the two panels share a
# common canvas height.
p_irs_panel <- p_irs_prepost +
  labs(title = "(A) IRS Favorability") +
  theme(aspect.ratio = NULL)

fig1_widths <- c(1, 1.15)
fig1_height_cm <- 8.8
# Figure 2 matches Figure 1A's plotting-panel size (not the full 1B slot).
fig1a_panel_w_cm <- 5.38
fig1a_panel_h_cm <- 5.83
fig2_width_cm <- 4.0 + fig1a_panel_w_cm + 0.5
fig2_height_cm <- 0.5 + fig1a_panel_h_cm + 2.0

p_fig1 <- p_irs_panel + p_ate +
  plot_layout(widths = fig1_widths, guides = "keep")

ggsave(
  here::here("output", "figures", "irs-and-ates.png"),
  p_fig1,
  width = pnas_2col_cm,
  height = fig1_height_cm,
  units = "cm",
  dpi = 300,
  type = "cairo",
  bg = "white"
)

# Figure 2: same plot-panel size as Figure 1A, plus room for y-axis labels.
p_format <- p_int +
  labs(title = NULL) +
  theme(
    axis.text.y = element_text(size = 9, hjust = 1, margin = margin(r = 3)),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(2.5, "pt")
  )
if (has_ggh4x) {
  p_format <- p_format +
    ggh4x::force_panelsizes(
      rows = unit(fig1a_panel_h_cm, "cm"),
      cols = unit(fig1a_panel_w_cm, "cm")
    )
}

ggsave(
  here::here("output", "figures", "format-interactions.png"),
  p_format,
  width = fig2_width_cm,
  height = fig2_height_cm,
  units = "cm",
  dpi = 300,
  type = "cairo",
  bg = "white"
)

cat("Saved: output/figures/irs-and-ates.png\n")
cat("Saved: output/figures/format-interactions.png\n")

# -----------------------------------------------------------------------------
# Figure 3 · Bayes-factor equivalence (T1 + T2, treatment text only)
# -----------------------------------------------------------------------------

source(here::here("scripts", "functions", "forest_measure_specs.R"))

outcome_order <- forest_measure_specs() |>
  filter(facet == "lewis") |>
  pull(label)

t1_path <- here::here("output", "tables", "bf01_t1.csv")
t2_path <- here::here("output", "tables", "bf01_t2.csv")
if (!file.exists(t1_path) || !file.exists(t2_path)) {
  stop(
    "Missing bf01_t1.csv / bf01_t2.csv. Run scripts/05_t1_analyses.R and ",
    "scripts/06_t2_analyses.R first.",
    call. = FALSE
  )
}
bf_t1 <- read_csv(t1_path, show_col_types = FALSE)
bf_t2 <- read_csv(t2_path, show_col_types = FALSE)
cat("Loaded:", t1_path, "\n")
cat("Loaded:", t2_path, "\n")

# Dodge vertically only when T1/T2 BF values are close enough that points/labels
# would overlap on the same line (threshold in BF01 units).
overlap_thresh <- 0.55
dodge_half <- 0.26

y_levels <- rev(outcome_order)  # bottom → top

bf_plot <- bind_rows(
  bf_t1 |> mutate(time = time_legend[[1]]),
  bf_t2 |> mutate(time = time_legend[[2]])
) |>
  filter(label %in% outcome_order) |>
  mutate(
    label = factor(label, levels = y_levels),
    time = factor(time, levels = time_levels_dodge),
    bf_plot = pmin(bf_01, 10),
    bf_lab = sprintf("%.2f", bf_01),
    lab_x = bf_plot + 0.22,
    y_base = as.numeric(label)
  ) |>
  group_by(label) |>
  mutate(
    needs_dodge = (max(bf_plot) - min(bf_plot)) < overlap_thresh,
    y_off = dplyr::case_when(
      !needs_dodge ~ 0,
      time == time_legend[[1]] ~ dodge_half,
      TRUE ~ -dodge_half
    ),
    y = y_base + y_off
  ) |>
  ungroup()

band_labs <- tibble::tibble(
  x = c(0.5, 2, 6.5),
  lab = c(
    "Favors\ndifference",
    "Anecdotal evidence\nfor equivalence",
    "Moderate evidence\nfor equivalence"
  )
)

hline_df <- bf_plot |> distinct(y_base)
lewis_breaks <- as.numeric(factor(outcome_order, levels = y_levels))

p_bf <- ggplot(bf_plot, aes(x = bf_plot, y = y, colour = time)) +
  geom_hline(
    data = hline_df,
    aes(yintercept = y_base),
    inherit.aes = FALSE,
    colour = "grey88",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = c(1, 3),
    colour = "grey55",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_point(size = 2.6) +
  geom_text(
    aes(x = lab_x, label = bf_lab),
    hjust = 0,
    size = 3.0,
    show.legend = FALSE
  ) +
  geom_text(
    data = band_labs,
    aes(x = x, y = Inf, label = lab),
    inherit.aes = FALSE,
    vjust = 1.2,
    size = 2.55,
    fontface = "bold",
    colour = "grey35",
    lineheight = 0.95
  ) +
  scale_y_continuous(
    breaks = lewis_breaks,
    labels = outcome_order,
    expand = expansion(add = c(0.35, 1.25))
  ) +
  scale_colour_manual(values = col_time, name = NULL, breaks = time_legend) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = c(0, 1, 3, 5, 7, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = expression(paste("Bayes factors for format equivalence (", BF["01"], ")")),
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1, margin = margin(r = 4)),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(5, "pt"),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10.5, margin = margin(t = 4, b = 0)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "lines"),
    legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(8, "pt"),
    legend.spacing.y = unit(0, "pt"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(4, 4, 2, 1)
  )

fig3_height_cm <- 6.5

ggsave(
  here::here("output", "figures", "bf-equivalence.png"),
  p_bf,
  width = pnas_2col_cm,
  height = fig3_height_cm,
  units = "cm",
  dpi = 300,
  type = "cairo",
  bg = "white"
)
cat("Saved: output/figures/bf-equivalence.png\n")
print(as.data.frame(bf_t1 |> mutate(bf_01 = round(bf_01, 2))), row.names = FALSE)
print(as.data.frame(bf_t2 |> mutate(bf_01 = round(bf_01, 2))), row.names = FALSE)
