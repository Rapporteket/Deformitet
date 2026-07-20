#' Funksjon som gjør utvalg i datagrunnlaget til registreringene
#'
#' @param datoFra Operasjonsdato, fra og med. Standard: '2023-01-01'
#' @param datoTil Operasjonsdato, til og med. Standard: Sys.Date()
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: 9
#' Alt annet enn 0 og 1 gir begge
#' @param op_type  1: Primæroperasjon, 2: Reoperasjon,
#' 3:Planlagt strekking av vektstav, 4: Planlagt to-seanse. Andre tall: alle
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#' @param fargepalett - Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export

utvalgEnh <- function(RegData, datoFra = "2023-01-01", datoTil = Sys.Date(),
                      minald = 0, maxald = 110, erMann = 9, aar = 0,
                      op_type = 9,
                      enhetsUtvalg = 0, reshID = 0, fargepalett = "BlaaOff") {
  "%i%" <- intersect
  data <- RegData

    # Gruppenavn
  indEgen1 <- match(reshID, RegData$ReshId)
  # Hvis ikke egne data eller reshID=0:
  enhetsUtvalg <- ifelse(reshID == 0 | is.na(indEgen1), 0, enhetsUtvalg)

  if (enhetsUtvalg %in% c(1, 2)) { # Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[indEgen1])
  } else {
    hovedgrTxt <- "Hele landet"
  }

  # Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
  # trengs ikke data for hele landet:
  if (enhetsUtvalg == 2) {
    RegData <- RegData[which(RegData$ReshId == as.numeric(reshID)), ] # kun egen enhet
  }

  if (op_type %in% 1:4) {
  RegData <- RegData |> dplyr::filter( CURRENT_SURGERY %in% op_type) }

  RegData <- RegData |>
    dplyr::filter(
      dplyr::between(SURGERY_DATE,
                     as.Date({{ datoFra }}),
                     as.Date({{ datoTil }})))

   RegData <- RegData |>
    dplyr::filter(dplyr::between(Alder,{{ minald }}, {{ maxald }}))

  if (erMann %in% 0:1) {
    RegData <- dplyr::filter( RegData$ErMann == erMann) }

  N <- dim(RegData)[1]

  utvalgTxt <- if (N == 0) {paste0("Operasjonsdato: ", datoFra, 'til ', datoTil)
    } else {
    c(paste0("Operasjonsdato: ", min(RegData$OpDato, na.rm = TRUE),
      " til ",
      max(RegData$OpDato, na.rm = TRUE)),
    if ((minald > 0) | (maxald < 110)) {
      paste0(
        "Pasienter fra ", min(RegData$Alder, na.rm = TRUE),
        " til ",
          max(RegData$Alder, na.rm = TRUE), " år")},
    if (erMann %in% 0:1) {
      paste0("Kjønn: ", c("Kvinner", "Menn")[erMann + 1])}
  )}

  # Enhetsutvalg:

  ind <- list(Hoved = 0, Rest = NULL)
  if (enhetsUtvalg %in% c(0, 2)) { # Ikke sammenlikning
    medSml <- 0
    smltxt <- "" #
    ind$Hoved <- seq_len(nrow(RegData)) #
  }
  if (enhetsUtvalg == 1) { # Involverer egen enhet
    medSml <- 1
    smltxt <- "landet forøvrig"
    ind$Hoved <- which(as.numeric(RegData$ReshId) == reshID)
    ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
  }


  UtData <- list(
    RegData = RegData, utvalgTxt = utvalgTxt, fargepalett = fargepalett, ind = ind,
    medSml = medSml, hovedgrTxt = hovedgrTxt, smltxt = smltxt
  ) # GronnHNpms624,
  return(invisible(UtData))
}


#' Filtrer data for module data dump
#' Funksjonen filtrerer data
#'
#' @title Filtrer datadump
#'
#' @param data datasett
#' @param dato1 brukervalg - dato min
#' @param dato2 brukervalg - dato max
#' @param userRole brukerrolle
#' @param userUnitId brukertilhørighet
#' @return datasett filtrert på brukervalg
#' @export


filtrer_datadump <- function(data, dato1, dato2, userRole, userUnitId) { #


  # Funksjonene under er fra Ingrid. Tror den er basert på preprossesseringa
  # prepVar
  # FASES UT
  data <- data |>
    dplyr::filter(dplyr::between(.data$SURGERY_DATE, as.Date({{ dato1 }}), as.Date({{ dato2 }})))


  if (userRole != "SC") {
    data <- data |>
      dplyr::select(-dplyr::contains(c("mths", "mnd"))) |>
      dplyr::filter(.data$CENTREID == userUnitId)
  }

  return(data)
}


#' @title Utvalgsfunksjon
#'
#' @export

utvalg_basic <- function(data, user_unit, gender, type_op, tid1, tid2, alder1, alder2, bruk_av_funk) {
  # Filter by unit (if desirable)

  # FASES UT..........

  if (bruk_av_funk != "ikke_filtrer_reshId") {
    data <- data |>
      dplyr::filter(.data$CENTREID == user_unit)
  } else {
    data <- data
  }

  # Filter by gender

  data <- data |>
    dplyr::filter(.data$Kjonn == dplyr::case_when(
      {{ gender }} == "kvinne" ~ "kvinne",
      {{ gender }} == "mann" ~ "mann",
      {{ gender }} != "kvinne" | {{ gender }} != "mann" ~ Kjonn
    ))

  # Filter by operation type

  data <- data |>
    dplyr::filter(dplyr::case_when(
      {{ type_op }} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
      {{ type_op }} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
      {{ type_op }} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)
    ))

  # Add filter on surgery date--------------------------------------------------

  data <- data |>
    dplyr::filter(dplyr::between(
      .data$SURGERY_DATE,
      as.Date({{ tid1 }}),
      as.Date({{ tid2 }})
    ))

  # Add filter on age-----------------------------------------------------------

  # Using column "Alder_num" in which alder is given as an integer
  data <- data |>
    dplyr::filter(dplyr::between(
      .data$Alder_num,
      {{ alder1 }},
      {{ alder2 }}
    ))


  return(data)
}

# nolint start
## TEST AT DET FUNGERER:
##
##
## g <- utvalg_basic(RegData, 111961, "mann", "Primæroperasjon", "2023-01-01", "2025-12-01", 1, 100, "ikke_filtrer_reshId")

# nolint end
