################################################################################
## FUNKSJON SOM LAGER TABELL MED REGISTRERINGER PR OPERASJONSDATO ##
################################################################################

#' @title tabell registreringer
#' @param date1 dato-valg (min) gjort av brukeren
#' @param date2 dato-valg (max) gjort av brukeren
#' @param data datasett -> datasett som har vært gjennom prePros()
#' @examples
#' \donttest{
#' try(tbl_reg("01-09-2024", "01-01-2025", RegData))
#' }
#'
#' @export

tbl_reg <- function(date1, date2, data) {
  d1 <- as.Date(date1, format = "%d-%m-%Y")
  d2 <- as.Date(date2, format = "%d-%m-%Y")

  data <- data |>
    dplyr::filter(dplyr::between(.data$SURGERY_DATE, d1, d2))

  data <- data |>
    dplyr::group_by(lubridate::year(.data$SURGERY_DATE), lubridate::month(.data$SURGERY_DATE)) |>
    dplyr::count(.data$ShNavn) |>
    # nolint start object_usage_linter
    dplyr::rename(
      mnd = `lubridate::month(.data$SURGERY_DATE)`,
      aar = `lubridate::year(.data$SURGERY_DATE)`
    )
  # nolint end

  reg_tbl <- data |>
    tidyr::pivot_wider(names_from = c("mnd", "aar"), names_sep = "-", values_from = "n") |>
    dplyr::mutate_all(~ replace(., is.na(.), 0)) |>
    dplyr::mutate(Totalt = rowSums(dplyr::across(dplyr::where(is.numeric))))

  return(reg_tbl)
}


################################################################################
## FUNKSJON SOM LAGER OVERSIKT OVER REGISTRERINGER PR SKJEMA ##
################################################################################

#' @title tabell registreringer pr skjema
#' @param date1 dato-valg (min) gjort av brukeren
#' @param date2 dato-valg (max) gjort av brukeren
#' @param data data -> data som har vært gjennom prePros()
#' @examples
#' \donttest{
#' try(tbl_skjema_reg("01-09-2024", "01-01-2025", RegData))
#' }
#'
#' @export

tbl_skjema_reg <- function(date1, date2, data) {
  tbl_skjema <- data |>
    dplyr::filter(dplyr::between(
      .data$SURGERY_DATE,
      as.Date({{ date1 }}, format = "%d-%m-%Y"),
      as.Date({{ date2 }}, format = "%d-%m-%Y")
    )) |>
    dplyr::group_by(.data$ShNavn) |>
    dplyr::mutate(
      personopplysninger = sum(!is.na(.data$REGISTERED_DATE)),
      Skjema_1a_Pasientoppl_preop = sum(!is.na(.data$FILLING_DATE)),
      Skjema_2a_Sykepleier_lege_preop = sum(!is.na(.data$SURGERY_DATE)),
      Skjema_1a_Pasientoppl_3mnd = sum(!is.na(.data$FOLLOWUP)),
      Skjema_2a_Sykepleier_lege_3mnd = sum(!is.na(.data$FOLLOWUP_surgeon3mths)),
      Skjema_1a_Pasientoppl_12mnd = sum(!is.na(.data$FOLLOWUP_patient12mths)),
      Skjema_2a_Sykepleier_lege_12mnd = sum(!is.na(.data$FOLLOWUP_surgeon12mths)),
      Skjema_1a_Pasientoppl_60mnd = sum(!is.na(.data$FOLLOWUP_patient60mths))
    ) |>
    dplyr::select(
      c(
        "ShNavn",
        "personopplysninger",
        "Skjema_1a_Pasientoppl_preop",
        "Skjema_2a_Sykepleier_lege_preop",
        "Skjema_1a_Pasientoppl_3mnd",
        "Skjema_2a_Sykepleier_lege_3mnd",
        "Skjema_1a_Pasientoppl_12mnd",
        "Skjema_2a_Sykepleier_lege_12mnd",
        "Skjema_1a_Pasientoppl_60mnd"
      )
    ) |>
    unique()

  return(tbl_skjema)
}
