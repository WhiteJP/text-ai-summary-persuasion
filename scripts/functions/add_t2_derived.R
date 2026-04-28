#' Add all follow-up (T2) derived columns to a joined wave-1 + follow-up dataset
#'
#' Intended to be called once in 01b_link_followup_data.R on both d_w and
#' d_itt_w *before* saveRDS, so every downstream script sees the same columns.
#'
#' Creates:
#'   Participation indicators -- fu_started, fu_finished, fu_started_not_finished
#'   T2 composites           -- irs_approval_t2, civil_service_t2, mvs_t2
#'   T2 comprehension        -- lewis_comp*_correct_fu, haidt_comp*_correct_fu,
#'                              lewis_comp_fu_total, haidt_comp_fu_total,
#'                              comp_fu_total, comp_change
#'   T2 change scores        -- irs_change_t2, civil_service_change_t2,
#'                              doge_change_t2, trump_change_t2,
#'                              enforce_change_t2, fav_ssa_change_t2,
#'                              mvs_change_t2
#'
#' @param dat A data frame produced by left-joining wave-1 data with follow-up
#'   Qualtrics exports (columns ending in `_fu`).
#' @return The same data frame with the new columns appended.
#' @noRd
add_t2_derived <- function(dat) {
  dat |>
    dplyr::mutate(
      # --- Follow-up participation indicators --------------------------------
      fu_started = as.integer(!is.na(.data$ResponseId_fu)),
      fu_finished = as.integer(
        !is.na(.data$fu_completed_outcomes) & .data$fu_completed_outcomes == 1L
      ),
      fu_started_not_finished = as.integer(
        .data$fu_started == 1L & .data$fu_finished == 0L
      ),

      # --- T2 composite scores -----------------------------------------------
      irs_approval_t2 = rowMeans(
        dplyr::across(c(
          .data$trust_irs_post_1_fu,
          .data$fav_irs_post_1_fu,
          .data$agents_post_1_fu
        )),
        na.rm = TRUE
      ),
      civil_service_t2 = rowMeans(
        dplyr::across(c(
          .data$servants_post_1_fu,
          .data$fav_cs_post_1_fu
        )),
        na.rm = TRUE
      ),
      mvs_t2 = rowMeans(
        dplyr::across(c(
          .data$mvs1_post_fu, .data$mvs2_post_fu, .data$mvs3_post_fu,
          .data$mvs4_post_fu, .data$mvs5_post_fu, .data$mvs6_post_fu
        )),
        na.rm = TRUE
      ),

      # --- T2 comprehension --------------------------------------------------
      lewis_comp1_correct_fu = as.integer(.data$lewis_comp1_fu == 1L),
      lewis_comp2_correct_fu = as.integer(.data$lewis_comp2_fu == 1L),
      lewis_comp3_correct_fu = as.integer(.data$lewis_comp3_fu == 1L),
      haidt_comp1_correct_fu = as.integer(.data$haidt_comp1_fu == 1L),
      haidt_comp2_correct_fu = as.integer(.data$haidt_comp2_fu == 1L),
      haidt_comp3_correct_fu = as.integer(.data$haidt_comp3_fu == 1L),
      lewis_comp_fu_total = ifelse(
        .data$text == "Lewis" &
          !is.na(.data$lewis_comp1_fu) &
          !is.na(.data$lewis_comp2_fu) &
          !is.na(.data$lewis_comp3_fu),
        .data$lewis_comp1_correct_fu +
          .data$lewis_comp2_correct_fu +
          .data$lewis_comp3_correct_fu,
        NA_real_
      ),
      haidt_comp_fu_total = ifelse(
        .data$text == "Haidt" &
          !is.na(.data$haidt_comp1_fu) &
          !is.na(.data$haidt_comp2_fu) &
          !is.na(.data$haidt_comp3_fu),
        .data$haidt_comp1_correct_fu +
          .data$haidt_comp2_correct_fu +
          .data$haidt_comp3_correct_fu,
        NA_real_
      ),
      comp_fu_total = ifelse(
        .data$text == "Lewis",
        .data$lewis_comp_fu_total,
        .data$haidt_comp_fu_total
      ),

      # --- T2 change scores --------------------------------------------------
      irs_change_t2           = .data$irs_approval_t2    - .data$irs_approval_pre,
      civil_service_change_t2 = .data$civil_service_t2   - .data$civil_service_pre,
      doge_change_t2          = .data$doge_post_1_fu     - .data$doge_pre_1,
      trump_change_t2         = .data$trump_post_1_fu    - .data$trump_pre_1,
      enforce_change_t2       = .data$enforce_post_1_fu  - .data$enforce_pre_1,
      fav_ssa_change_t2       = .data$fav_ssa_post_1_fu  - .data$fav_ssa_pre_1,
      mvs_change_t2           = .data$mvs_t2             - .data$mvs_pre,
      comp_change             = .data$comp_fu_total      - .data$comp_total
    )
}
