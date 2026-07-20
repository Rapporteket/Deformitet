## GJENNOMSNITT OVER TID ##
## Hvilke variabler som er inkludert her bør gjennomgås kritisk (nå er bare alle
## inkludert og det er ikke bra!)


#' @title Tabell - gjennomsnitt over tid
#'
#' @param data data som har vært gjennom mapping_navn() og prepVar()
#' @param var brukervalg av variabel
#' @param map_data mapping-data mellom reshId og sykehusnavn
#' @param tidsenhet brukervalg av tid
#' @param visning brukervalg av visning
#' @param userUnidIt brukerens enhetstilhørighet
#'
#' @examples
#' \donttest{
#' try(tabell_gjen_tid(prep_data, var, map_db_resh, "kvartal", "hver enhet", 111961))
#' }
#'
#' @export

tabell_gjen_tid <- function(
  data,
  var,
  map_data,
  tidsenhet = "kvartal",
  visning = "hele landet",
  userUnitId
) {
  data$PasientID <- as.character(data$PasientID)

  if (tidsenhet == "kvartal") {
    data <- data |>
      dplyr::rename(
        mine = {{ var }},
        date = 6
      ) |>
      dplyr::mutate(
        quarter = lubridate::floor_date(.data$date, unit = "quarter"),
        tid = lubridate::ymd(.data$quarter)
      )
  } else {
    data <- data |>
      dplyr::rename(
        mine = {{ var }},
        date = 6
      ) |>
      dplyr::mutate(
        aar = lubridate::floor_date(.data$date, unit = "year"),
        tid = format(.data$aar, "%Y")
      )
  }

  ## Pr. sykehus

  data_sykehus <- data |>
    dplyr::filter(!is.na(.data$mine)) |>
    dplyr::group_by(.data$ShNavn, .data$tid) |>
    dplyr::summarize(gjen = mean(.data$mine)) |>
    dplyr::select(c(.data$ShNavn, .data$tid, .data$gjen))

  data_tally <- data |>
    dplyr::group_by(.data$ShNavn, .data$tid) |>
    dplyr::add_tally(n = "antall") |>
    dplyr::select(.data$ShNavn, .data$tid, .data$antall) |>
    unique()

  data_sykehus <- dplyr::left_join(data_sykehus, data_tally)

  ## Nasjonalt

  data_nasjonalt <- data |>
    dplyr::filter(!is.na(.data$mine)) |>
    dplyr::select(-.data$ShNavn) |>
    dplyr::mutate(ShNavn = "Nasjonalt") |>
    dplyr::group_by(.data$ShNavn, .data$tid) |>
    dplyr::summarize(gjen = mean(.data$mine)) |>
    dplyr::select(c(.data$ShNavn, .data$tid, .data$gjen))

  data_nasjonalt_tally <- data |>
    dplyr::select(-.data$ShNavn) |>
    dplyr::mutate(ShNavn = "Nasjonalt") |>
    dplyr::group_by(.data$ShNavn, .data$tid) |>
    dplyr::add_tally(n = "antall") |>
    dplyr::select(.data$ShNavn, .data$tid, .data$antall) |>
    unique()

  data_nasjonalt <- dplyr::left_join(data_nasjonalt, data_nasjonalt_tally)

  data <- rbind(data_sykehus, data_nasjonalt) # Bind disse to sammen

  map_data <- map_data |>
    dplyr::add_row(UnitId = "0", orgname = "Nasjonalt")

  data <- merge(data, map_data, by.x = "ShNavn", by.y = "orgname")

  data <- data |>
    dplyr::filter(dplyr::case_when(
      {{ visning }} == "hele landet, uten sammenligning" ~
        ShNavn == "Nasjonalt",
      {{ visning }} == "egen enhet" ~
        UnitId == {{ userUnitId }},
      {{ visning }} == "hver enhet" ~
        ShNavn != "Nasjonalt",
      .default = .data$ShNavn == .data$ShNavn
    ))

  data <- data |>
    dplyr::select(-c(.data$UnitId))


  ##### GJØR DET MULIG Å PRINTE KVARTAL PENT #####

  if (tidsenhet == "kvartal") {
    data <- data |>
      dplyr::mutate(
        tid1 = lubridate::year(.data$tid),
        tid_as_character = as.character(.data$tid),
        tid = dplyr::case_when(
          str_detect(tid, "01-01") == TRUE ~ paste(tid1, "1", sep = "-"),
          str_detect(tid, "04-01") == TRUE ~ paste(tid1, "2", sep = "-"),
          str_detect(tid, "07-01") == TRUE ~ paste(tid1, "3", sep = "-"),
          str_detect(tid, "10-01") == TRUE ~ paste(tid1, "4", sep = "-")
        )
      )

    data$tid <- as.factor(data$tid)

    data <- data |>
      dplyr::select(-c("tid1", "tid_as_character"))
  }
  return(data)
}


#' @title Plot - gjennomsnitt over tid
#'
#' @param data data som har vært gjennom tabell_gjen_tid()
#' @param visning valgt visning
#' @param gg_data data fra prepVar()[2]
#' @param map_var data som mapper mellom variabler (faktornivå og "kontinuerlig variabel")
#' @param tidsenhet valgt tidsenhet
#' @param data_var datasett med brukerens UI-valg
#'
#' @examples
#' \donttest{
#' try(over_tid_plot(data, "egen enhet", gg_data, map_var, "kvartal"))
#' }
#'
#' @export

over_tid_plot <- function(
  data,
  visning,
  gg_data,
  map_var,
  tidsenhet,
  data_var
) {
  data$ShNavn <- as.factor(data$ShNavn)

  if (visning == "hele landet") {
    data$ShNavn <- relevel(data$ShNavn, "Nasjonalt")
  } else {
    data <- data
  }

  if (tidsenhet == "aar") {
    tid <- "År"
  } else {
    tid <- "Kvartal"
  }

  limits <- y_limits_gjen(map_var)

  tid_plot <-
    ggplot2::ggplot(data, ggplot2::aes(
      x = .data$tid, y = .data$gjennomsnitt,
      color = .data$ShNavn, group = .data$ShNavn
    )) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.2)

  ## GENERELLE TILPASNINGER AV PLOT ##
  tid_plot <- tid_plot +
    ggplot2::theme_bw(base_size = 16) +


    ggplot2::xlab(tid) +
    ggplot2::ylab(gg_data$xlab) +
    ggplot2::ggtitle("Gjennomsnitt over tid") +
    ggplot2::labs(caption = paste0(
      "**Valgte variabler:**", "\n", data_var[1, ], ", Kjønn: ", data_var[2, ], "\n",
      "Dato: ", data_var[3, ], "-", data_var[4, ], "\n",
      "Alder: ", data_var[5, ], "-", data_var[6, ], "\n",
      "Type operasjon: ", data_var[7, ]
    )) +

    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.y = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(
        color = "#87189D", # add caption
        face = "italic"
      )
    ) +

    ggplot2::scale_color_manual(
      values = # adding chosen colors
        c("darkblue", "#6CACE4", "#ADDFB3", "#87189D")
    ) +

    ggplot2::ylim(limits$ymin, limits$ymax)

  return(tid_plot)
}

#' @title Y-axis limits
#' Funksjon som setter fornuftige øvre og nedre grenser på yaksen.
#' @param var variabel som velges av bruker i UI-delen
#' @examples
#' \donttest{
#' try(ylimits_gjen(SRS22_MAIN_SCORE))
#' }
#'
#' @export

y_limits_gjen <- function(var) {
  data <- data.frame(ymin = "", ymax = "")
  data <- data |>
    dplyr::mutate(
      ymin = dplyr::case_when(
        stringr::str_detect(var, "SRS22") == TRUE ~ 1,
        stringr::str_detect(var, "SCALE") == TRUE ~ 0,
        stringr::str_detect(var, "Alder") == TRUE ~ 5,
        stringr::str_detect(var, "BMI") == TRUE ~ 10,
        stringr::str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 20,
        stringr::str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 0,
        stringr::str_detect(var, "kurve") == TRUE ~ 0,
        stringr::str_detect(var, "tid") == TRUE ~ 0,
        stringr::str_detect(var, "BED") == TRUE ~ 0,
        stringr::str_detect(var, "BLOOD") == TRUE ~ 100
      ),
      ymax = dplyr::case_when(
        stringr::str_detect(var, "SRS22") == TRUE ~ 5,
        stringr::str_detect(var, "SCALE") == TRUE ~ 100,
        stringr::str_detect(var, "Alder") == TRUE ~ 80,
        stringr::str_detect(var, "BMI") == TRUE ~ 35,
        stringr::str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 110,
        stringr::str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 50,
        stringr::str_detect(var, "kurve") == TRUE ~ 110,
        stringr::str_detect(var, "tid") == TRUE ~ 600,
        stringr::str_detect(var, "BED") == TRUE ~ 12,
        stringr::str_detect(var, "BLOOD") == TRUE ~ 1000
      )
    )
}


#' @title Sjekke antall
#' Funksjon for å sjekke størrelse på datagrunnlaget. Funksjonen gjør at man
#' dropper å vise variabelen om det ikke finnes noen registreringer på gitt variabel
#' i hoveddataen og om det er na i datasettet som er regnet om med tabell_gjen_tid()
#' @param data datasett som ikke har blitt omgjort
#' @param data1 datasett som har vært gjennom tabell_gjen_tid()
#' @param date1 brukerens valg for første dato
#' @param date2 brukerens valg for siste dato
#' @param tidsenhet brukerens valg av tidsenhet (kvartal vs. år)
#'
#' @examples
#' \donttest{
#' try(sjekk_antall(RegData, tabell_gjen_tid, "2024-01-01", "2025-01-01", "aar"))
#' }
#' @export

sjekk_antall <- function(data, data1, date1, date2, tidsenhet) {
  if (tidsenhet == "kvartal") {
    true_data <- data |>
      dplyr::filter(dplyr::between(.data$.data$SURGERY_DATE, as.Date(date1), as.Date(date2))) |>
      dplyr::mutate(quarter = lubridate::floor_date(.data$SURGERY_DATE, unit = "quarter")) |>
      dplyr::select(.data$quarter) |>
      unique() |>
      dplyr::add_tally(n = "n_quarter") |>
      dplyr::select(.data$quarter, .data$n_quarter)

    true_quarter <- true_data$n_quarter[1]

    sample_data <- data1 |>
      dplyr::group_by(.data$ShNavn) |>
      dplyr::add_tally(n = "n_quarter") |>
      dplyr::mutate(check = dplyr::if_else(.data$n_quarter == true_quarter, TRUE, FALSE))

    sample_quarter <- sample_data$n_quarter[1]

    check <- dplyr::if_else(
      true_quarter == 0, "Drop",
      dplyr::if_else(is.na(sample_quarter), "Drop", "Keep")
    )
    return(check)
  } else {
    true_data <- data |>
      dplyr::filter(dplyr::between(.data$.data$SURGERY_DATE, as.Date(date1), as.Date(date2))) |>
      dplyr::mutate(year = lubridate::floor_date(.data$SURGERY_DATE, unit = "year")) |>
      dplyr::select(.data$year) |>
      unique() |>
      dplyr::add_tally(n = "n_year") |>
      dplyr::select(.data$year, .data$n_year)

    true_year <- true_data$n_year[1]

    sample_data <- data1 |>
      dplyr::group_by(.data$ShNavn) |>
      dplyr::add_tally(n = "n_year") |>
      dplyr::mutate(check = dplyr::if_else(.data$n_year == true_year, TRUE, FALSE))

    sample_year <- sample_data$n_year[1]

    check <- dplyr::if_else(
      true_year == 0, "Drop",
      dplyr::if_else(is.na(sample_year), "Drop", "Keep")
    )

    return(check)
  }
}
