# =============================================================================
# 01 · Data Wrangling
# =============================================================================
# Loads raw Qualtrics CSV, creates composite measures, change scores,
# timing, comprehension, and demographics. Defines and saves three
# analysis samples: d_all, d_itt, d (completers).

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)

source(here::here("scripts", "functions", "read_csv_qualtrics.R"))

# --- Load raw data -----------------------------------------------------------

d_raw <- read_csv_qualtrics(
  here::here("data", "text-ai-summary-qualtrics-data.csv"),
  col_types = cols(attn_check = col_character())
) |>
  filter(StartDate >= as.POSIXct("2026-01-27"))

# --- Race one-hot encoding helper -------------------------------------------

race_codes <- c(
  "1" = "white", "2" = "black", "3" = "native_american",
  "4" = "asian", "5" = "pacific_islander", "6" = "other"
)

multi_hot_df <- function(x, codes, prefix, na_zero = TRUE) {
  s <- as.character(x)
  s <- gsub("[^0-9]", "", s)
  s[is.na(x)] <- NA_character_
  mat <- sapply(names(codes), function(dig) str_detect(s, fixed(dig)))
  if (is.null(dim(mat))) mat <- cbind(mat)
  if (na_zero) mat[is.na(mat)] <- FALSE
  storage.mode(mat) <- "integer"
  colnames(mat) <- paste0(prefix, make.names(unname(codes)))
  tibble::as_tibble(mat)
}

# --- Wrangle -----------------------------------------------------------------

d_all <- d_raw %>%
  # Condition assignment
  mutate(
    text = factor(
      ifelse(lewis == 1, "Lewis", "Haidt"),
      levels = c("Lewis", "Haidt")
    ),
    format = factor(
      ifelse(full_piece == 1, "Full", "Summary"),
      levels = c("Full", "Summary")
    ),
    condition = interaction(text, format, sep = "_"),
    treated = !is.na(lewis) | !is.na(full_piece)
  ) %>%

  # Attention checks
  mutate(
    attncheck1_passed = att_check == "3",
    attncheck2_passed = attn_check == "4,5",
    attncheck_passed = attncheck1_passed & attncheck2_passed
  ) %>%

  # IRS approval composite (3 items)
  mutate(
    irs_approval_pre  = rowMeans(select(., trust_irs_pre_1,  fav_irs_pre_1,  agents_pre_1),  na.rm = TRUE),
    irs_approval_post = rowMeans(select(., trust_irs_post_1, fav_irs_post_1, agents_post_1), na.rm = TRUE)
  ) %>%

  # Civil service composite (2 items)
  mutate(
    civil_service_pre  = rowMeans(select(., servants_pre_1,  fav_cs_pre_1),  na.rm = TRUE),
    civil_service_post = rowMeans(select(., servants_post_1, fav_cs_post_1), na.rm = TRUE)
  ) %>%

  # Material Values Scale composite
  mutate(
    mvs_pre  = rowMeans(select(., matches("^mvs\\d+_pre$")),  na.rm = TRUE),
    mvs_post = rowMeans(select(., matches("^mvs\\d+_post$")), na.rm = TRUE)
  ) %>%

  # Change scores
  mutate(
    irs_approval_change  = irs_approval_post  - irs_approval_pre,
    civil_service_change = civil_service_post - civil_service_pre,
    trust_irs_change = trust_irs_post_1 - trust_irs_pre_1,
    fav_irs_change   = fav_irs_post_1   - fav_irs_pre_1,
    agents_change    = agents_post_1    - agents_pre_1,
    servants_change  = servants_post_1  - servants_pre_1,
    fav_cs_change    = fav_cs_post_1    - fav_cs_pre_1,
    enforce_change   = enforce_post_1   - enforce_pre_1,
    doge_change      = doge_post_1      - doge_pre_1,
    trump_change     = trump_post_1     - trump_pre_1,
    fav_ssa_change   = fav_ssa_post_1   - fav_ssa_pre_1,
    mvs_change       = mvs_post         - mvs_pre
  ) %>%

  # Timing: aggregate page-level timers for the text reading portion only
  mutate(
    text_total_time = rowSums(
      select(., matches("(lewis|haidt)_(full|ai)_p\\d+_timer_Page Submit$")),
      na.rm = TRUE
    ),
    text_total_clicks = rowSums(
      select(., matches("(lewis|haidt)_(full|ai)_p\\d+_timer_Click Count$")),
      na.rm = TRUE
    ),
    text_pages_completed = rowSums(
      !is.na(select(., matches("(lewis|haidt)_(full|ai)_p\\d+_timer_Page Submit$"))),
      na.rm = TRUE
    ),
    text_pages_total = ifelse(format == "Full", 25, 5),
    text_pages_pct   = text_pages_completed / text_pages_total
  ) %>%

  # Comprehension scoring
  mutate(
    lewis_comp1_correct = as.integer(lewis_comp1 == 1),
    lewis_comp2_correct = as.integer(lewis_comp2 == 1),
    lewis_comp3_correct = as.integer(lewis_comp3 == 1),
    haidt_comp1_correct = as.integer(haidt_comp1 == 1),
    haidt_comp2_correct = as.integer(haidt_comp2 == 1),
    haidt_comp3_correct = as.integer(haidt_comp3 == 1)
  ) %>%
  mutate(
    lewis_comp_total = ifelse(
      !is.na(lewis_comp1) | !is.na(lewis_comp2) | !is.na(lewis_comp3),
      rowSums(select(., lewis_comp1_correct, lewis_comp2_correct, lewis_comp3_correct), na.rm = TRUE),
      NA_integer_
    ),
    haidt_comp_total = ifelse(
      !is.na(haidt_comp1) | !is.na(haidt_comp2) | !is.na(haidt_comp3),
      rowSums(select(., haidt_comp1_correct, haidt_comp2_correct, haidt_comp3_correct), na.rm = TRUE),
      NA_integer_
    ),
    comp_total       = ifelse(text == "Lewis", lewis_comp_total, haidt_comp_total),
    comp_pct         = comp_total / 3,
    comp_all_correct = comp_total == 3
  ) %>%

  # Drop per-page timer columns (no longer needed)
  select(-matches("(lewis|haidt).*_timer_")) |>

  # Drop Prolific-provided demographics (keep survey-collected versions)
  select(-c(rid, age, gender...568, hhi, ethnicity, education,
            political_party, region, zip)) |>

  # Recode demographics
  mutate(
    gender = `gender...45`,
    gender = factor(gender, levels = 1:3, labels = c("Male", "Female", "Non-Binary")),
    yob = ifelse(yob <= 7, 2000 + yob, 1900 + yob),
    age = year(StartDate) - yob,
    educ_cat = factor(educ, 1:6, c("< HS", "HS", "Some College",
                                    "2yr College", "4yr College", "Post-grad")),
    income_cat = factor(income, 1:6, c("<10K", "10-30K", "30-60K",
                                       "60-100K", "100-250K", ">250K")),
    party_id = factor(party_id, 1:4,
                      c("Democrat", "Republican", "Independent", "Independent")),
    dem_rep_num = case_when(
      party_id == "Democrat"    ~ dem_strength,
      party_id == "Independent" & party_lean == 2 ~ 3,
      party_id == "Independent" & party_lean == 3 ~ 4,
      party_id == "Independent" & party_lean == 1 ~ 5,
      party_id == "Republican"  ~ 5 + rep_strength,
    ),
    ideology_cat = factor(ideo, 1:4,
                          c("Liberal", "Conservative", "Moderate", "Don't know")),
    lib_con_num = case_when(
      ideology_cat == "Liberal"      ~ lib_lean,
      ideology_cat == "Moderate" & ideo_lean == 1 ~ 3,
      ideology_cat == "Moderate" & ideo_lean == 3 ~ 4,
      ideology_cat == "Moderate" & ideo_lean == 2 ~ 5,
      ideology_cat == "Conservative" ~ 5 + con_lean,
    ),
  ) |>
  mutate(multi_hot_df(race, race_codes, "race_"))

# --- Define analysis samples -------------------------------------------------
# 1. d_all      = everyone who started (after date filter)
# 2. d_itt      = treated + passed attention check 2 (preregistered)
# 3. d          = ITT participants who finished the survey (completers)

d_passed_attn1 <- d_all |> filter(attncheck1_passed)
d_treated      <- d_all |> filter(treated)
d_itt          <- d_treated |> filter(attncheck2_passed)
d              <- d_itt |> filter(Finished == "1")

# --- Save analysis datasets --------------------------------------------------

saveRDS(d_all, here::here("data", "d_all.rds"))
saveRDS(d_itt, here::here("data", "d_itt.rds"))
saveRDS(d,     here::here("data", "d.rds"))

cat(sprintf(
  paste0("Data wrangling complete.\n",
         "  Started survey:       %d\n",
         "  Passed attn check 1:  %d\n",
         "  Received treatment:   %d\n",
         "  ITT sample:           %d\n",
         "  Final (completers):   %d\n"),
  nrow(d_all), nrow(d_passed_attn1), nrow(d_treated), nrow(d_itt), nrow(d)
))
