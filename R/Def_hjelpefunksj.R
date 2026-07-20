#' mapping_navn
#'
#' Denne funksjonen lager mapping mellom variabler som har vært gjennom prePros()
#' og variabler som kommer fra rådataen
#'
#' @param data rådata (data som ikke har vært gjennom prepros)
#' @param x_var valgt variabel i UI-delen
#'
#' @examples
#' \donttest{
#' try(mapping_navn(raw_regdata, "BMI_kategori"))
#' }
#'
#' @export

mapping_navn <- function(data, x_var) {
  ny_data <- data.frame(
    gammelt_navn =
      c(
        "Helsetilstand",
        "Helsetilstand_3mnd",
        "Helsetilstand_12mnd",
        "Helsetilstand_60mnd",
        "SRS22_spm22_3mnd",
        "SRS22_spm22_12mnd",
        "SRS22_spm22_60mnd",
        "SRS22_spm21_3mnd",
        "SRS22_spm21_12mnd",
        "SRS22_spm21_60mnd",
        "Alder",
        "BMI_kategori",
        "Kurve_preGr",
        "Kurve_post",
        "Diff_prosent_kurve",
        "Knivtid",
        "Liggetid",
        "Blodtap_100",
        "Blodtap_200",
        "SRS22_total",
        "SRS22_total_3mnd",
        "SRS22_total_12mnd",
        "SRS22_total_60mnd",
        "SRS22_funksjon",
        "SRS22_funksjon_3mnd",
        "SRS22_funksjon_12mnd",
        "SRS22_funksjon_60mnd",
        "SRS22_smerte",
        "SRS22_smerte_3mnd",
        "SRS22_smerte_12mnd",
        "SRS22_smerte_60mnd",
        "SRS22_selvbilde",
        "SRS22_selvbilde_3mnd",
        "SRS22_selvbilde_12mnd",
        "SRS22_selvbilde_60mnd",
        "SRS22_mhelse",
        "SRS22_mhelse_3mnd",
        "SRS22_mhelse_12mnd",
        "SRS22_mhelse_60mnd",
        "SRS22_fornoyd_3mnd",
        "SRS22_fornoyd_12mnd",
        "SRS22_fornoyd_60mnd"
      ),
    nytt_navn = c(
      "HELSETILSTAND_SCALE",
      "HEALTH_CONDITION_SCALE",
      "HEALTH_CONDITION_SCALE_patient12mths",
      "HEALTH_CONDITION_SCALE_patient_60_mths",
      "SRS22_22",
      "SRS22_22_patient12mths",
      "SRS22_22_patient60mths",
      "SRS22_21",
      "SRS22_21_patient12mths",
      "SRS22_21_patient60mths",
      "Alder_num",
      "BMI",
      "PRE_MAIN_CURVE",
      "POST_MAIN_CURVE",
      "Diff_prosent_kurve_raw",
      "knivtid_min",
      "BED_DAYS_POSTOPERATIVE",
      "PER_BLOOD_LOSS_VALUE",
      "PER_BLOOD_LOSS_VALUE",
      "SRS22_MAIN_SCORE",
      "SRS22_FULL_SCORE",
      "SRS22_FULL_SCORE_patient12mths",
      "SRS22_FULL_SCORE_patient60mths",
      "SRS22_FUNCTION_SCORE",
      "SRS22_FUNCTION_SCORE_patient3mths",
      "SRS22_FUNCTION_SCORE_patient12mths",
      "SRS22_FUNCTION_SCORE_patient60mths",
      "SRS22_PAIN_SCORE",
      "SRS22_PAIN_SCORE_patient3mths",
      "SRS22_PAIN_SCORE_patient12mths",
      "SRS22_PAIN_SCORE_patient60mths",
      "SRS22_SELFIMAGE_SCORE",
      "SRS22_SELFIMAGE_SCORE_patient3mths",
      "SRS22_SELFIMAGE_SCORE_patient12mths",
      "SRS22_SELFIMAGE_SCORE_patient60mths",
      "SRS22_MENTALHEALTH_SCORE",
      "SRS22_MENTALHEALTH_SCORE_patient3mths",
      "SRS22_MENTALHEALTH_SCORE_patient12mths",
      "SRS22_MENTALHEALTH_SCORE_patient60mths",
      "SRS22_SATISFACTION_SCORE",
      "SRS22_SATISFACTION_SCORE_patient12mths",
      "SRS22_SATISFACTION_SCORE_patient60mths"
    )
  )

  valgt_data <- ny_data |>
    dplyr::filter(.data$gammelt_navn == {{ x_var }}) |>
    dplyr::select("nytt_navn")

  valgt_variabel <- valgt_data[1, 1]

  return(valgt_variabel)
}
