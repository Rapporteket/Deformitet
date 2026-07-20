################################################################################
################################ LAG  Tabell ###################################
################################################################################


#' @title lagTabell
#'
#' @param data data som har vært gjennom prepVar()
#' @param var_reshid reshID (valg kun tilgj. som SC-bruker)
#' @param visning valg av visning
#'
#' @examples
#' \donttest{
#' try(lagTabell(data, 103240, "egen enhet"))
#' }
#'
#' @export


lagTabell <- function(data, var_reshid, visning) {
  if (visning != "hver enhet") {
    data_sykeh <- data |>
      dplyr::select(-c("CURRENT_SURGERY")) |>
      dplyr::filter(.data$CENTREID == {{ var_reshid }})

    data_sykeh <- data_sykeh |>
      dplyr::select(-c("CENTREID", "Kjonn")) |>
      dplyr::add_tally(name = "n") |>
      dplyr::group_by(data_sykeh[3]) |>
      dplyr::add_count(name = "by_var") |>
      dplyr::mutate(Prosent = round(.data$by_var / .data$n * 100, 2)) |>
      dplyr::rename("n pr variabel" = .data$by_var) |>
      dplyr::distinct()

    data_sykeh <- data_sykeh |>
      dplyr::relocate(.data$Prosent, .before = .data$n)
  }

  if (visning == "hver enhet") {
    data_sykeh_alle <- data

    data_sykeh_alle <- data_sykeh_alle |>
      dplyr::select(-c("CENTREID", "Kjonn", "CURRENT_SURGERY")) |>
      dplyr::group_by(.data$ShNavn) |>
      dplyr::add_tally(name = "n") |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$ShNavn, data_sykeh_alle[3]) |>
      dplyr::add_count(name = "by_var") |>
      dplyr::mutate(Prosent = round(.data$by_var / .data$n * 100, 2)) |>
      dplyr::rename("n pr variabel" = .data$by_var) |>
      dplyr::distinct()

    data_sykeh_alle <- data_sykeh_alle |>
      dplyr::relocate(.data$Prosent, .before = .data$n)
  } else {
    data_alle <- data

    data_alle <- data_alle |>
      dplyr::select(-c("CENTREID", "Kjonn", "CURRENT_SURGERY")) |>
      dplyr::mutate(ShNavn = dplyr::replace_values(
        .data$ShNavn,
        "Haukeland" ~ "Alle",
        "Rikshospitalet" ~ "Alle",
        "St.Olav" ~ "Alle"
      )) |>
      dplyr::add_tally(name = "n") |>
      dplyr::group_by(data_alle[3]) |>
      dplyr::add_count(name = "by_var") |>
      dplyr::mutate(Prosent = round(.data$by_var / .data$n * 100, 2)) |>
      dplyr::rename("n pr variabel" = .data$by_var) |>
      dplyr::distinct()


    data_komplett <- dplyr::full_join(data_sykeh, data_alle)

    data_komplett <- data_komplett |>
      dplyr::relocate(.data$Prosent, .before = .data$n)
  }


  if (visning == "hver enhet") {
    return(data_sykeh_alle)
  }

  if (visning == "egen enhet") {
    return(data_sykeh)
  }
  if (visning == "hele landet, uten sammenligning") {
    data_alle <- data_alle |>
      dplyr::relocate(.data$Prosent, .before = .data$n)

    return(data_alle)
  } else {
    return(data_komplett)
  } # => hele landet med sammenligning
}


################################################################################
############################## LAG GJEN TABELL #################################
################################################################################


#' gjen_var_til_data
#'
#' Denne funksjonen tar ei kolonne fra ei dataramme og legger den til
#' ei annen dataramme. Dette gjøres pr. forløps-id. Kolonnen som legges til
#' får navnet "gjen_var"
#'
#' @param raw_data rå-RegData
#' @param data RegData som har vært gjennom prepros
#' @param gjen_var valgt variabel
#'
#' @examples
#' \donttest{
#' try(gjen_var_til_data(raw_regdata, RegData, "Diff_prosent_kurve"))
#' }
#'
#' @export

gjen_var_til_data <- function(raw_data, data, gjen_var) {
  if (gjen_var %in% c("Alder", "Knivtid", "Diff_prosent_kurve")) {
    gjen_data <- data |>
      dplyr::mutate(gjen_var = dplyr::case_when(
        {{ gjen_var }} == "Alder" ~ Alder_num,
        {{ gjen_var }} == "Knivtid" ~ knivtid_min,
        {{ gjen_var }} == "Diff_prosent_kurve" ~ Diff_prosent_kurve
      ))

    return(gjen_data)
  } else {
    valg_raw <- raw_data |>
      dplyr::select(dplyr::all_of(gjen_var), "MCEID") # kun velge variablene vi er interessert i og forløps-id

    gjen_data <- dplyr::left_join(data, valg_raw, by = "MCEID")

    colnames(gjen_data)[ncol(gjen_data)] <- "gjen_var"

    return(gjen_data)
  }
}


#' @title lag_gjen_tabell
#'
#' @param data data som har vært gjennom prepVar()
#'
#' @examples
#' \donttest{
#' try(lag_gjen_tabell(data))
#' }
#'
#' @export


lag_gjen_tabell <- function(data) {
  gjen <- data |>
    dplyr::filter(!is.na(.data$gjen_var))

  gjen_pr_sykehus <- gjen |>
    dplyr::group_by(.data$ShNavn) |>
    dplyr::summarise(
      gjennomsnitt = round(mean(.data$gjen_var), 2),
      median = median(.data$gjen_var)
    ) |>
    dplyr::ungroup()


  gjen_total <- gjen |>
    dplyr::summarize(
      "gjennomsnitt nasjonalt" = round(mean(.data$gjen_var), 2),
      "median nasjonalt" = median(.data$gjen_var)
    )

  gjen_tabell <- merge(gjen_pr_sykehus, gjen_total)


  gjen_n <- gjen |>
    dplyr::group_by(.data$ShNavn) |>
    dplyr::tally(n = "antall") |>
    dplyr::mutate("antall nasjonalt" = sum(.data$antall))

  gjen_tabell2 <- merge(gjen_tabell, gjen_n)

  gjen_tabell2 <- gjen_tabell2 |>
    dplyr::relocate(.data$antall, .before = "gjennomsnitt nasjonalt")

  return(gjen_tabell2)
}
