# =============================================================================
# 07 · Main results figure (Figure 1)
# =============================================================================
# Panel A: IRS pre/post (wave-1) from data/d.rds.
# Panels B–C: forest plots from output/intermediate/forest_t1.rds and
# forest_t2.rds (produced by 05_t1_analyses.R and 06_t2_analyses.R).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(ggh4x)
})

fs::dir_create(here::here("output", "figures"))

# Legend order (immediate first). Dodge order: second factor level is offset upward on y.
time_legend <- c("Immediately post-treatment", "~2 months later")
time_levels_dodge <- c("~2 months later", "Immediately post-treatment")

t1 <- readRDS(here::here("output", "intermediate", "forest_t1.rds"))
t2 <- readRDS(here::here("output", "intermediate", "forest_t2.rds"))

lewis_panel <- t1$lewis_panel
haidt_panel <- t1$haidt_panel

stopifnot(identical(lewis_panel, t2$lewis_panel), identical(haidt_panel, t2$haidt_panel))

d <- readRDS(here::here("data", "d.rds"))

col_time <- c(
  `Immediately post-treatment` = "#333333",
  `~2 months later` = "#E15759"
)

ate_all <- bind_rows(
  t1$ate |> mutate(time = factor(time_legend[[1]], levels = time_levels_dodge)),
  t2$ate |> mutate(time = factor(time_legend[[2]], levels = time_levels_dodge))
)

int_all <- bind_rows(
  t1$int |> mutate(time = factor(time_legend[[1]], levels = time_levels_dodge)),
  t2$int |> mutate(time = factor(time_legend[[2]], levels = time_levels_dodge))
)

p_ate <- ggplot(ate_all, aes(x = d, y = label, colour = time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = 0.55),
    size = 0.35,
    linewidth = 0.35
  ) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = c(6, 1.5)) +
  scale_colour_manual(values = col_time, name = NULL, breaks = time_legend) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 0.4, linewidth = 0.5)
    )
  ) +
  labs(title = "(B) Average Attitude Change", x = "Overall ATE", y = NULL) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.55, "lines"),
    legend.key.width = unit(1.1, "lines"),
    legend.margin = margin(t = 10, r = 0, b = 2, l = 125, unit = "pt"),
    legend.box.spacing = unit(0, "pt")
  )

p_int <- ggplot(int_all, aes(x = d, y = label, colour = time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = 0.55),
    size = 0.35,
    linewidth = 0.35
  ) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = c(6, 1.5)) +
  scale_colour_manual(values = col_time, name = NULL, breaks = time_legend) +
  labs(
    title = "(C) Full Text vs. AI Summary",
    x = "Full text ATE \u2013 AI summary ATE",
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

format_col_main <- c("Full" = "#4E79A7", "Summary" = "#F28E2B")

irs_cond <- d |>
  filter(text == "Lewis") |>
  select(format, pre = irs_approval_pre, post = irs_approval_post) |>
  mutate(pre_bin = factor(round(pre))) |>
  tidyr::drop_na(pre_bin, post)

irs_no_change_ref <- irs_cond |>
  distinct(pre_bin) |>
  mutate(y = as.numeric(as.character(pre_bin)))

irs_diag_line <- tibble::tibble(x = factor(1:7), y = 1:7)

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
    aes(colour = format), alpha = 0.5, size = 0.9,
    position = position_jitter(width = 0.12, height = 0.08)
  ) +
  scale_colour_manual(
    name = NULL,
    values = format_col_main,
    breaks = c("Full", "Summary"),
    labels = c("Full text", "AI summary")
  ) +
  scale_y_continuous(breaks = 1:7) +
  labs(
    title = "(A) IRS Favorability",
    x = "Pre-treatment (rounded)",
    y = "Post-treatment"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = "center",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.y = unit(0.12, "cm"),
    legend.margin = margin(t = 4, r = 0, b = 0, l = 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0, "pt")
  )

fig_theme_bc <- theme(
  strip.text = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.x = element_text(size = 8.5),
  plot.margin = margin(4, 3, 4, 2)
)

p_bc <- (p_ate + p_int + plot_layout(widths = c(1, 1))) & fig_theme_bc

a_bc_gutter_r_pt <- 4
p_irs_prepost <- p_irs_prepost +
  fig_theme_bc +
  theme(
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 8.5, margin = margin(t = 2, b = 4)),
    axis.title.y = element_text(size = 8.5),
    plot.margin = margin(4, 3 + a_bc_gutter_r_pt, 2, 2)
  )

p_main_results <-
  (patchwork::free(p_irs_prepost, type = "panel", side = "tb") | p_bc) +
  plot_layout(widths = c(1, 2))

ggsave(
  here::here("output", "figures", "main-results.png"),
  p_main_results,
  width = 7.1, height = 3.5, dpi = 300, bg = "white"
)

cat("Saved: output/figures/main-results.png\n")
