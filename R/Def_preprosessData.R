#' Preprosesser data fra NKR Deformitet
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @param RegData dataramme med registerets data
#'
#' @return RegData En dataramme med det preprosesserte datasettet
#'
#' @export

preprosData <- function(RegData = RegData, egneVarNavn = 0) {

  ##### This function is run on RegData after RegData is returned from
  ##### the function alleRegData()
  ##### This function returns a complete data frame (no choices at this stage)



  # Kun ferdigstilte registreringer:
  #  RegData <- RegData[which(RegData$LegeskjemaStatus == 1), ]  #Vi ønsker kun ferdigstilte legeskjema

  if (egneVarNavn == 0) {
    RegData <- dplyr::rename(RegData,
      OpDato = SURGERY_DATE,
      Kjonn = GENDER,
      PasientID = PATIENT_ID
    )
  }

  #Beregnede variabler
  #Tid fra operasjon til 1.kontroll og 2.kontroll
  RegData$DagerOpKtr1 <- as.numeric(difftime(as.Date(RegData$FILLING_DATE_surgeon3mths),
                                             RegData$OpDato, units = 'days'))
  RegData$DagerOpKtr2 <- as.numeric(difftime(as.Date(RegData$FILLING_DATE_surgeon12mths),
                                             RegData$OpDato, units = 'days'))


  # Kjønnsvariabel:ErMann
  RegData <- dplyr::mutate(RegData, ErMann = abs(.data$Kjonn - 2))
  RegData$Alder <- as.numeric((as.Date(RegData$OpDato) - as.Date(RegData$BIRTH_DATE))) / 365.25
  RegData$Alder_num <- as.integer(RegData$Alder)


  # Riktig datoformat. Hoveddato = OpDato NB: OpDato er navnet om til Inndato i nakke.
  # InnDato brukes her om innleggelsesdato
  RegData$MndNum <- as.POSIXlt(RegData$OpDato, format = "%Y-%m-%d")$mon + 1
  RegData$Kvartal <- ceiling(RegData$MndNum / 3)
  RegData$Halvaar <- ceiling(RegData$MndNum / 6)
  RegData$Aar <- 1900 + as.POSIXlt(RegData$OpDato, format = "%Y-%m-%d")$year
  RegData$MndAar <- format(RegData$OpDato, "%b%y")

  # RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$ForstLukketMed), RegData$UtDato,units = 'days'))

  RegData <- dplyr::rename(RegData,
    ReshId = CENTREID
  )
  class(RegData$ReshId) <- "numeric"
  RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) # Fjerner mellomrom etter navn

  # Tomme sykehusnavn får resh som navn:
  indTom <- which(is.na(RegData$ShNavn) | RegData$ShNavn == "")
  RegData$ShNavn[indTom] <- RegData$ReshId[indTom]

  # Sjekker om alle resh har egne enhetsnavn
  dta <- unique(RegData[, c("ReshId", "ShNavn")])
  duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId) > 1)])
  duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn) > 1)])

  if (length(c(duplSh, duplResh)) > 0) {
    ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
    RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind], " (", RegData$ReshId[ind], ")")
  }


#Knivtid
  RegData <- RegData |>
    tidyr::unite("my_start", KNIFE_TIME_START_HOUR:KNIFE_TIME_START_MIN, sep = ":") |>
    tidyr::unite("my_end", KNIFE_TIME_END_HOUR:KNIFE_TIME_END_MIN, sep = ":")

  RegData$my_start <- strptime(RegData$my_start, "%H:%M")
  RegData$my_end <- strptime(RegData$my_end, "%H:%M")

  RegData <- RegData |>
    dplyr::mutate(
      my_time_mins = my_end - my_start,
      my_time_mins = gsub("mins", "", my_time_mins),
      my_time_mins = as.numeric(my_time_mins),
      my_time_mins = tidyr::replace_na(my_time_mins, 0),
      KNIFE_TIME_EXACT_TIMER = tidyr::replace_na(KNIFE_TIME_EXACT_TIMER, 0),
      KNIFE_TIME_EXACT_MIN = tidyr::replace_na(KNIFE_TIME_EXACT_MIN, 0),
      KNIFE_TIME_EXACT_TIMER = KNIFE_TIME_EXACT_TIMER * 60,
      my_time_mins2 = KNIFE_TIME_EXACT_TIMER + KNIFE_TIME_EXACT_MIN,
      my_time_mins2 = tidyr::replace_na(my_time_mins2, 0),
      kniv_tid_min = my_time_mins + my_time_mins2
    ) |>
    dplyr::select(
      -"KNIFE_TIME_EXACT_MIN", -"KNIFE_TIME_EXACT_TIMER", -"my_time_mins",
      -"my_time_mins2", -"my_start", -"my_end"
    )



  #--------fra annen funksj-----------------
  # RegData <- RegData |>
  #   dplyr::rename(PID = PasientID)

  RegData <- RegData |>
    dplyr::select(-dplyr::starts_with("USERCOMMENT"))


# -----------Tilrettelegge variabler-----------------------
# Kode under skal flyttes til "tilretteleggeVar"

  # KJØNN:
  #RegData$Kjonn <- as.character(RegData$GENDER)
  RegData <- RegData |>
    dplyr::mutate(Kjonn = dplyr::replace_values(as.character(Kjonn),
                                                "1" ~ "mann", "2" ~ "kvinne"))

  # PRE-OPERATIV KURVE:
  RegData$PRE_MAIN_CURVE <- as.numeric(RegData$PRE_MAIN_CURVE)

  # b. Make curve groups (for easier visualization)
  RegData <- RegData |>
    dplyr::mutate(Kurve_preGr = cut(PRE_MAIN_CURVE,
                                  breaks = c(0, 40, 44, 49, 54, 59, 64, 70, 110),
                                  right = FALSE,
                                  labels = c("< 40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70+")
    ))


  # POST-OPERATIV KURVE:
 # RegData$POST_MAIN_CURVE <- as.numeric(RegData$POST_MAIN_CURVE)

  # Make curve groups (for easier visualization)
  RegData <- RegData |>
    dplyr::mutate(Kurve_postGr = cut(.data$POST_MAIN_CURVE,
                                   breaks = c(0, 10, 20, 30, 40, 110),
                                   right = FALSE,
                                   labels = c("0-9", "10-19", "20-29", "30-39", "40+")
    ))

  # KURVE-DIFFERANSE PROSENT:
  # RegData <- RegData |>
  #   dplyr::mutate(
  #     Diff_prosent_kurve = (((.data$PRE_MAIN_CURVE - .data$POST_MAIN_CURVE) / .data$PRE_MAIN_CURVE) * 100),
  #     Diff_prosent_kurve_raw = round(.data$Diff_prosent_kurve, digits = 0),
  #     Diff_prosent_kurve = cut(
  #       .data$Diff_prosent_kurve_raw,
  #       breaks = c(0, 45, 55, 65, 75, 80, 85, 90, 95, 105),
  #       labels = c("0-44", "45-54", "55-64", "65-74", "75-79", "80-84", "85-89", "90-94", "95-100")
  #     )
  #   )
  RegData <- RegData |>
    dplyr::mutate(
      Diff_prosent_kurve = 100*(PRE_MAIN_CURVE - POST_MAIN_CURVE) / PRE_MAIN_CURVE,
      KurveDiffGr = cut(Diff_prosent_kurve,
                          breaks = c(0, 45, 55, 65, 75, 80, 85, 90, 95, 100),
                         # labels = c("0-45", "45-55", "55-65", "65-75", "75-80", "80-85", "85-90", "90-95", "95-100"),
                         right = TRUE,
                         ordered=TRUE))

  # LIGGETID:
  #RegData$BED_DAYS_POSTOPERATIVE <- as.numeric(RegData$BED_DAYS_POSTOPERATIVE)
  RegData <- RegData |>
    dplyr::mutate(LiggetidGr = cut(.data$BED_DAYS_POSTOPERATIVE,
                                 breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 40),
                                 labels = c("1", "2", "3", "4", "5", "6", "7", "> 7")
    ))

  #  KNIVTID:
RegData$KnivtidGr <- cut(RegData$kniv_tid/60,
                         breaks = c(seq(0,6, 0.5), 100))
levels(RegData$KnivtidGr)[length(levels(RegData$KnivtidGr))] <- "> 6"

  # BLODTAP:
  RegData <- RegData |>
    dplyr::mutate(
      Blodtap_100 = cut(PER_BLOOD_LOSS_VALUE,
        breaks = c(-1, 99, 199, 299, 399, 499, 599, 699, 799, 899,
          999, 1099, 1199, 1299, 1399, 1499, 5000),
        labels = c("< 100", "100-199", "200-299", "300-399", "400-499",
          "500-599", "600-699", "700-799", "800-899", "900-999",
          "1000-1099", "1100-1199", "1200-1299", "1300-1399",
          "1400-1499", "> 1500")
      ))

  RegData <- RegData |>
    dplyr::mutate(
      Blodtap_200 = cut(PER_BLOOD_LOSS_VALUE,
        breaks = c(-1, 199, 399, 599, 799, 999, 1199, 1399, 1599, 2000),
        labels = c("< 200", "200-399", "400-599", "600-799", "800-999",
          "1000-1199", "1200-1399", "1400-1599", "> 1600")
      ))

#Ikke rettet disse:

  # (ix) FOR SRS22
  # MAIN FORM
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_total = cut(
        .data$SRS22_MAIN_SCORE,
        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  RegData <- RegData |>
    dplyr::mutate(
      SRS22_total_3mnd = cut(.data$SRS22_FULL_SCORE,
                             breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                             labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  RegData <- RegData |>
    dplyr::mutate(SRS22_total_12mnd = cut(.data$SRS22_FULL_SCORE_patient12mths,
                                          breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                          labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
    ))

  RegData <- RegData |>
    dplyr::mutate(SRS22_total_60mnd = cut(.data$SRS22_FULL_SCORE_patient12mths,
                                          breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                          labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
    ))


  # FUNCTION
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_funksjon = cut(.data$SRS22_FUNCTION_SCORE,
                           breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                           labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_funksjon_3mnd = cut(.data$SRS22_FUNCTION_SCORE_patient3mths,
                                breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_funksjon_12mnd = cut(.data$SRS22_FUNCTION_SCORE_patient12mths,
                                 breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                 labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_funksjon_60mnd = cut(.data$SRS22_FUNCTION_SCORE_patient60mths,
                                 breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                 labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  # PAIN
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_smerte = cut(.data$SRS22_PAIN_SCORE,
                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_smerte_3mnd = cut(.data$SRS22_PAIN_SCORE_patient3mths,
                              breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                              labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_smerte_12mnd = cut(.data$SRS22_PAIN_SCORE_patient12mths,
                               breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                               labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_smerte_60mnd = cut(.data$SRS22_PAIN_SCORE_patient60mths,
                               breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                               labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  # SELF IMAGE
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_selvbilde = cut(.data$SRS22_SELFIMAGE_SCORE,
                            breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                            labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_selvbilde_3mnd = cut(.data$SRS22_SELFIMAGE_SCORE_patient3mths,
                                 breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                 labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_selvbilde_12mnd = cut(.data$SRS22_SELFIMAGE_SCORE_patient12mths,
                                  breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                  labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_selvbilde_60mnd = cut(.data$SRS22_SELFIMAGE_SCORE_patient60mths,
                                  breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                  labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  # MENTAL HEALTH
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_mhelse = cut(.data$SRS22_MENTALHEALTH_SCORE,
                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_mhelse_3mnd = cut(.data$SRS22_MENTALHEALTH_SCORE_patient3mths,
                              breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                              labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", " 3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_mhelse_12mnd = cut(.data$SRS22_MENTALHEALTH_SCORE_patient12mths,
                               breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                               labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_mhelse_60mnd = cut(.data$SRS22_MENTALHEALTH_SCORE_patient60mths,
                               breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                               labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      )
    )

  # SRS22 - SATISFACTION (21 og 21) - from patient followup
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
  RegData <- RegData |>
    dplyr::mutate(
      SRS22_fornoyd_3mnd = cut(.data$SRS22_SATISFACTION_SCORE,
                               breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                               labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_fornoyd_12mnd = cut(.data$SRS22_SATISFACTION_SCORE_patient12mths,
                                breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),
      SRS22_fornoyd_60mnd = cut(.data$SRS22_SATISFACTION_SCORE_patient60mths,
                                breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")
      ),

      # Question 22
      SRS22_spm21_3mnd = cut(
        .data$SRS22_21,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd",
          "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt"
        )
      ),
      SRS22_spm21_12mnd = cut(
        .data$SRS22_21_patient12mths,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd",
          "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt"
        )
      ),
      SRS22_spm21_60mnd = cut(
        .data$SRS22_21_patient60mths,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd",
          "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt"
        )
      ),

      # Questions 21
      SRS22_spm22_3mnd = cut(
        .data$SRS22_22,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja",
          "Definitivt ja", "Ikke utfylt"
        )
      ),
      SRS22_spm22_12mnd = cut(
        .data$SRS22_22_patient12mths,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja",
          "Definitivt ja", "Ikke utfylt"
        )
      ),
      SRS22_spm22_60mnd = cut(
        .data$SRS22_22_patient60mths,
        breaks = c(0, 1, 2, 3, 4, 5, 10),
        labels = c(
          "Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja",
          "Definitivt ja", "Ikke utfylt"
        )
      )
    )

  # (x) FOR EQ5D


  # (xi) FOR HELSETILSTAND
  RegData <- RegData |>
    dplyr::mutate(
      Helsetilstand = cut(.data$HELSETILSTAND_SCALE,
                          breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                          labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")
      ),
      # 3-6 mnd
      Helsetilstand_3mnd = cut(.data$HEALTH_CONDITION_SCALE,
                               breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                               labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")
      ),
      # 12 mnd
      Helsetilstand_12mnd = cut(.data$HEALTH_CONDITION_SCALE_patient12mths,
                                breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")
      ),
      # 60 mnd
      Helsetilstand_60mnd = cut(.data$HEALTH_CONDITION_SCALE_patient60mths,
                                breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")
      )
    )


  # (xii) FOR KOMPLIKASJONER
  # Procedure complications as indicated by patient
  RegData$PROCEDURE_COMPLICATIONS <- as.character(RegData$PROCEDURE_COMPLICATIONS)
  RegData$PROCEDURE_COMPLICATIONS_patient12mths <- as.character(RegData$PROCEDURE_COMPLICATIONS_patient12mths)
  RegData$PROCEDURE_COMPLICATIONS_patient60mths <- as.character(RegData$PROCEDURE_COMPLICATIONS_patient60mths)

  RegData <- RegData |>
    dplyr::mutate(
      Komplikasjoner_3mnd = dplyr::replace_values(
        .data$PROCEDURE_COMPLICATIONS,
        "0" ~ "Nei", "1" ~ "Ja", "9" ~ "Ikke utfylt"
      )
    )

  RegData <- RegData |>
    dplyr::mutate(
      Komplikasjoner_12mnd = dplyr::replace_values(
        .data$PROCEDURE_COMPLICATIONS_patient12mths,
        "0" ~ "Nei", "1" ~ "Ja", "9" ~ "Ikke utfylt"
      )
    )

  RegData <- RegData |>
    dplyr::mutate(
      Komplikasjoner_60mnd = dplyr::replace_values(
        .data$PROCEDURE_COMPLICATIONS_patient60mths,
        "0" ~ "Nei", "1" ~ "Ja", "9" ~ "Ikke utfylt"
      )
    )



  return(invisible(RegData))
}


#-------------------------- ALT UNDER HER SKAL FASES UT-------------------------------------------------
# Filtrering og tilrettelegging gjørs nå hhv i UtvalgEnh() og varTilrettelegg


#' @title prepvar-funksjon. Lar denne stå som den er, men lager filtrerings og
#' tilretteleggingsfunksjoner ut fra vanlig struktur. SKAL FASES UT
#' @return Funksjonen ser ut tiil både å filtrere og preprosessere
#' @export
#'

prepVar <- function(data, var, var_kjonn, time1, time2, alder1, alder2,
                    type_op = "Begge", visning = "uten_tid") {

  data <- prep_var_na(data, var)

# FILTRERING ER FLYTTET TIL FUNKSJONEN "utvalgEnh()"


  # Filter by gender
  data <- data |>
    dplyr::filter(
      .data$Kjonn == dplyr::case_when(
        {{ var_kjonn }} == "kvinne" ~ "kvinne",
        {{ var_kjonn }} == "mann" ~ "mann",
        {{ var_kjonn }} != "kvinne" | {{ var_kjonn }} != "mann" ~ Kjonn
      )
    )

  # Filter by operation type

  data <- data |>
    dplyr::filter(
      dplyr::case_when(
        {{ type_op }} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
        {{ type_op }} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
        {{ type_op }} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)
      )
    )

  # Add filter on surgery date--------------------------------------------------

  data <- data |>
    dplyr::filter(
      dplyr::between(
        .data$SURGERY_DATE,
        as.Date({{ time1 }}),
        as.Date({{ time2 }})
      )
    )

  # Add filter on age-----------------------------------------------------------

  # Using column "Alder_num" in which alder is given as an integer

  data <- data |>
    dplyr::filter(
      dplyr::between(
        .data$Alder_num,
        {{ alder1 }},
        {{ alder2 }}
      )
    )


  gg_data <- data.frame(title = "")


  # Add good titles on each variable--------------------------------------------

  gg_data <- gg_data |>
    dplyr::mutate(
      title = dplyr::case_when(
        {{ var }} %in% c("BMI_kategori", "BMI") ~
          "Andel operasjoner fordelt på BMI-kategorier",

        # ALDER:
        {{ var }} %in% c("Alder", "Alder_num") ~
          "Andel operasjoner fordelt på aldersgrupper",

        # KURVE:
        {{ var }} %in% c("Kurve_preGr", "PRE_MAIN_CURVE") ~
          "Andel operasjoner fordelt på pre-operativ kurve",
        {{ var }} %in% c("Kurve_post", "POST_MAIN_CURVE") ~
          "Andel operasjoner fordelt på post-operativ kurve",
        {{ var }} %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw") ~
          "Andel operasjoner fordelt på prosentvis korreksjon i kurve",

        # LIGGETID
        {{ var }} %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE") ~
          "Andel operasjoner fordelt på liggetid etter operasjon",

        # KNIVTID
        {{ var }} %in% c("Knivtid", "kniv_tid") ~ "Andel operasjoner fordelt på knivtid",

        # BLODTAP:
        {{ var }} %in% c("Blodtap_100", "Blodtap_200", "PER_BLOOD_LOSS_VALUE") ~
          "Andel operasjoner fordelt på blodtap",

        # SRS22:total
        {{ var }} %in% c("SRS22_total", "SRS22_MAIN_SCORE") ~
          "Andel operasjoner fordelt på total SRS22 skår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE") ~
          "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 5 års oppfølging",

        # SRS22: funksjon
        {{ var }} %in% c("SRS22_funksjon", "SRS22_FUNCTION_SCORE") ~
          "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths") ~
          "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 5 års oppfølging",

        # SRS22: smerte
        {{ var }} %in% c("SRS22_smerte", "SRS22_PAIN_SCORE") ~
          "Andel operasjoner fordelt på SRS22-smertesskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_smerte_3mnd", "SRS22_PAIN_SCORE_patient3mths") ~
          "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_smerte_12mnd", "SRS22_PAIN_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_smerte_60mnd", "SRS22_PAIN_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 5 års oppfølging",


        # SRS22: selvbilde
        {{ var }} %in% c("SRS22_selvbilde", "SRS22_SELFIMAGE_SCORE") ~
          "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_selvbilde_3mnd", "SRS22_SELFIMAGE_SCORE_patient3mths") ~
          "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_selvbilde_12mnd", "SRS22_SELFIMAGE_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_selvbilde_60mnd", "SRS22_SELFIMAGE_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 5 års oppfølging",

        # SRS22: mental helse
        {{ var }} %in% c("SRS22_mhelse", "SRS22_MENTALHEALTH_SCORE") ~
          "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_mhelse_3mnd", "SRS22_MENTALHEALTH_SCORE_patient3mths") ~
          "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_mhelse_12mnd", "SRS22_MENTALHEALTH_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_mhelse_60mnd", "SRS22_MENTALHEALTH_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 5 års oppfølging",

        # SRS22: fornøyd
        {{ var }} %in% c("SRS22_fornoyd_3mnd", "SRS22_SATISFACTION_SCORE") ~
          "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_fornoyd_12mnd", "SRS22_SATISFACTION_SCORE_patient12mths") ~
          "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_fornoyd_60mnd", "SRS22_SATISFACTION_SCORE_patient60mths") ~
          "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 5 års oppfølging",

        # SRS22: spm 21 - hvor fornøyd?
        {{ var }} %in% c("SRS22_spm21_3mnd", "SRS22_21") ~
          "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm21_12mnd", "SRS22_21_patient12mths") ~
          "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm21_60mnd", "SRS22_21_patient60mths") ~
          "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 5 års oppfølging",

        # SRS22: spm 22 - på nytt?
        {{ var }} %in% c("SRS22_spm22_3mnd", "SRS22_22") ~
          "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm22_12mnd", "SRS22_22_patient12mths") ~
          "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm22_60mnd", "SRS22_22_patient60mths") ~
          "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 5 års oppfølging",

        # EQ5D

        # HELSETILSTAND
        {{ var }} %in% c("Helsetilstand", "HELSETILSTAND_SCALE") ~
          "Andel operasjoner fordelt på helsetilstandsskår (0-100) preoperativt",
        {{ var }} %in% c("Helsetilstand_3mnd", "HEALTH_CONDITION_SCALE") ~
          "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("Helsetilstand_12mnd", "HEALTH_CONDITION_SCALE_patient12mths") ~
          "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("Helsetilstand_60mnd", "HEALTH_CONDITION_SCALE_patient_60_mths") ~
          "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 5 års oppfølging",


        # KOMPLIKASJONER
        {{ var }} == "Komplikasjoner_3mnd" ~ "Andel komplikasjoner pr. operasjon(3-6 mnd)",
        {{ var }} == "Komplikasjoner_12mnd" ~ "Andel komplikasjoner pr. operasjon (12 mnd)",
        {{ var }} == "Komplikasjoner_60mnd" ~ "Andel komplikasjoner pr. operasjon (5 år)",
        {{ var }} == "Andel operasjoner" ~ "Andel operasjoner"
      ),

      # Add title for label in plot (specifically xlab in ggplot)---------------------

      xlab = dplyr::case_when(
        {{ var }} == "BMI_kategori" ~ "BMI-kategorier",
        {{ var }} == "BMI" ~ "BMI",

        # ALDER:
        {{ var }} == "Alder" ~ "Aldersgrupper",
        {{ var }} == "Alder_num" ~ "Alder",

        # KURVE:
        {{ var }} %in% c("Kurve_preGr", "PRE_MAIN_CURVE") ~
          "Pre-operativ kurve",
        {{ var }} %in% c("Kurve_post", "POST_MAIN_CURVE") ~
          "Post-operativ kurve",
        {{ var }} %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw") ~
          "Post-operativ prosent korreksjon",

        # LIGGETID
        {{ var }} %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE") ~
          "Liggetid etter operasjon, oppgitt i dager",

        # KNIVTID
        {{ var }} %in% c("Knivtid", "kniv_tid") ~ "Knivtid, oppgitt i minutter",

        # BLODTAP:
        {{ var }} == "Blodtap_100" ~ "Blodtap pr 100ml",
        {{ var }} == "Blodtap_200" ~ "Blodtap pr 200ml",
        {{ var }} == "PER_BLOOD_LOSS_VALUE" ~ "Blodtap i ml",

        # SRS22:total
        {{ var }} %in% c("SRS22_total", "SRS22_MAIN_SCORE") ~
          "Total SRS22 skår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE") ~
          "Total SRS22 skår (1-5) ved 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths") ~
          "Total SRS22 skår (1-5) ved 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths") ~
          "Total SRS22 skår (1-5) ved 5 års oppfølging",

        # SRS22: funksjon
        {{ var }} %in% c("SRS22_funksjon", "SRS22_FUNCTION_SCORE") ~
          "SRS22-funksjonsskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths") ~
          "SRS22-funksjonsskår (1-5), 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths") ~
          "SRS22-funksjonsskår (1-5), 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths") ~
          "SRS22-funksjonsskår (1-5), 5 års oppfølging",

        # SRS22: smerte
        {{ var }} %in% c("SRS22_smerte", "SRS22_PAIN_SCORE") ~
          "SRS22-smertesskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_smerte_3mnd", "SRS22_PAIN_SCORE_patient3mths") ~
          "SRS22-smertesskår (1-5), 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_smerte_12mnd", "SRS22_PAIN_SCORE_patient12mths") ~
          "SRS22-smertesskår (1-5), 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_smerte_60mnd", "SRS22_PAIN_SCORE_patient60mths") ~
          "SRS22-smertesskår (1-5), 5 års oppfølging",

        # SRS22: selvbilde
        {{ var }} %in% c("SRS22_selvbilde", "SRS22_SELFIMAGE_SCORE") ~
          "SRS22-selvbildesskår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_selvbilde_3mnd", "SRS22_SELFIMAGE_SCORE_patient3mths") ~
          "SRS22-selvbildesskår (1-5), 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_selvbilde_12mnd", "SRS22_SELFIMAGE_SCORE_patient12mths") ~
          "SRS22-selvbildesskår (1-5), 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_selvbilde_60mnd", "SRS22_SELFIMAGE_SCORE_patient60mths") ~
          "SRS22-selvbildesskår (1-5), 5 års oppfølging",

        # SRS22: mental helse
        {{ var }} %in% c("SRS22_mhelse", "SRS22_MENTALHEALTH_SCORE") ~
          "SRS22-mental-helse-skår (1-5) preoperativt",
        {{ var }} %in% c("SRS22_mhelse_3mnd", "SRS22_MENTALHEALTH_SCORE_patient3mths") ~
          "SRS22-mental-helse-skår (1-5), 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_mhelse_12mnd", "SRS22_MENTALHEALTH_SCORE_patient12mths") ~
          "SRS22-mental-helse-skår (1-5), 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_mhelse_60mnd", "SRS22_MENTALHEALTH_SCORE_patient60mths") ~
          "SRS22-mental-helse-skår (1-5), 5 års oppfølging",

        # SRS22: fornøyd
        {{ var }} %in% c("SRS22_fornoyd_3mnd", "SRS22_SATISFACTION_SCORE") ~
          "SRS22-fornøydhetsskår (1-5), 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_fornoyd_12mnd", "SRS22_SATISFACTION_SCORE_patient12mths") ~
          "SRS22-fornøydhetsskår (1-5), 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_fornoyd_60mnd", "SRS22_SATISFACTION_SCORE_patient60mths") ~
          "SRS22-fornøydhetsskår (1-5), 5 års oppfølging",


        # SRS22: spm 21 - hvor fornøyd?
        {{ var }} %in% c("SRS22_spm21_3mnd", "SRS22_21") ~
          "'Er du fornøyd med resultatet av behandlingen?', 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm21_12mnd", "SRS22_21_patient12mths") ~
          "'Er du fornøyd med resultatet av behandlingen?', 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm21_60mnd", "SRS22_21_patient60mths") ~
          "'Er du fornøyd med resultatet av behandlingen?', 5 år oppfølging",

        # SRS22: spm 22 - på nytt?
        {{ var }} %in% c("SRS22_spm22_3mnd", "SRS22_22") ~
          "'Ville du ønsket samme behandling på nytt?', 3-6 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm22_12mnd", "SRS22_22_patient12mths") ~
          "'Ville du ønsket samme behandling på nytt?', 12 måneders oppfølging",
        {{ var }} %in% c("SRS22_spm22_60mnd", "SRS22_22_patient60mths") ~
          "'Ville du ønsket samme behandling på nytt?', 5 års oppfølging",


        # EQ5D

        # HELSETILSTAND
        {{ var }} %in% c("Helsetilstand", "HELSETILSTAND_SCALE") ~
          "Helsetilstandsskår (0-100) preoperativt",
        {{ var }} %in% c("Helsetilstand_3mnd", "HEALTH_CONDITION_SCALE") ~
          "Helsetilstandsskår (0-100), 3-6 måneders oppfølging",
        {{ var }} %in% c("Helsetilstand_12mnd", "HEALTH_CONDITION_SCALE_patient12mths") ~
          "Helsetilstandsskår (0-100), 12 måneders oppfølging",
        {{ var }} %in% c("Helsetilstand_60mnd", "HEALTH_CONDITION_SCALE_patient_60_mths") ~
          "Helsetilstandsskår (0-100), 5 års oppfølging",

        # KOMPLIKASJONER
        {{ var }} == "Komplikasjoner_3mnd" ~ "Selvrapportert komplikasjon, 3-6 måneders oppfølging",
        {{ var }} == "Komplikasjoner_12mnd" ~ "Selvrapportert komplikasjon, 12 måneders oppfølging",
        {{ var }} == "Komplikasjoner_60mnd" ~ "Selvrapportert komplikasjon, 5 års oppfølging",
        {{ var }} == "Andel operasjoner" ~ "Andel operasjoner"
      )
    )


  #### Select and return the column of interest---------------------------------
  if (visning == "over_tid") {
    my_data <- data |>
      dplyr::select(
        c(
          "ShNavn",
          "CENTREID",
          dplyr::all_of({{ var }}),
          "Kjonn",
          "CURRENT_SURGERY",
          dplyr::all_of(
            dplyr::case_when(
              {{ var }} %in% skjema$tre_mnd_pas ~ "FILLING_DATE_patient3mths",
              {{ var }} %in% skjema$tre_mnd_lege ~ "FILLING_DATE_surgeon3mths",
              {{ var }} %in% skjema$tolv_mnd_pas ~ "FILLING_DATE_patient12mths",
              {{ var }} %in% skjema$tolv_mnd_lege ~ "FILLING_DATE_surgeon12mths",
              {{ var }} %in% skjema$seksti_mnd_pas ~ "FILLING_DATE_patient12mths",
              .default = "SURGERY_DATE"
            )
          ), .data$PasientID
        )
      )
  } else {
    my_data <- data |>
      dplyr::select(
        c("ShNavn", "CENTREID", dplyr::all_of({{ var }}), "Kjonn", "CURRENT_SURGERY")
      )
  }


  return(list(my_data, gg_data)) # returns a list (the list is unpacked in UI)
}

# nolint start
# Test of the function
## x <- prepVar(RegData, "Kurve_preGr", "mm", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", "over_tid")
# Inspect returned data frame (object 1 in list):
## rr <- data.frame(x[1])
## gg_data <- data.frame(x[2])
# nolint end

# Funksjon for å ta ut NA der det ikke er registrert oppfølging enda

#' @title Ta bort na
#'
#' @export

prep_var_na <- function(data, var) {

  oppflg <- data.frame(
    "tre" = c(
      "Helsetilstand_3mnd",
      "HEALTH_CONDITION_SCALE",
      "SRS22_spm22_3mnd",
      "SRS22_22",
      "SRS22_spm21_3mnd",
      "SRS22_21",
      "SRS22_total_3mnd",
      "SRS22_FULL_SCORE",
      "SRS22_funksjon_3mnd",
      "SRS22_FUNCTION_SCORE_patient3mths",
      "SRS22_smerte_3mnd",
      "SRS22_PAIN_SCORE_patient3mths",
      "SRS22_selvbilde_3mnd",
      "SRS22_SELFIMAGE_SCORE_patient3mths",
      "SRS22_mhelse_3mnd",
      "SRS22_MENTALHEALTH_SCORE_patient3mths",
      "SRS22_fornoyd_3mnd",
      "SRS22_SATISFACTION_SCORE",
      "Komplikasjoner_3mnd"
    ),
    "tolv" = c(
      "Helsetilstand_12mnd",
      "HEALTH_CONDITION_SCALE_patient12mths",
      "SRS22_spm22_12mnd",
      "SRS22_22_patient12mths",
      "SRS22_spm21_12mnd",
      "SRS22_21_patient12mths",
      "SRS22_total_12mnd",
      "SRS22_FULL_SCORE_patient12mths",
      "SRS22_funksjon_12mnd",
      "SRS22_FUNCTION_SCORE_patient12mths",
      "SRS22_smerte_12mnd",
      "SRS22_PAIN_SCORE_patient12mths",
      "SRS22_selvbilde_12mnd",
      "SRS22_SELFIMAGE_SCORE_patient12mths",
      "SRS22_mhelse_12mnd",
      "SRS22_MENTALHEALTH_SCORE_patient12mths",
      "SRS22_fornoyd_12mnd",
      "SRS22_SATISFACTION_SCORE_patient12mths",
      "Komplikasjoner_12mnd"
    ),
    "seksti" = c(
      "Helsetilstand_60mnd",
      "HEALTH_CONDITION_SCALE_patient_60_mths",
      "SRS22_spm22_60mnd",
      "SRS22_22_patient60mths",
      "SRS22_spm21_60mnd",
      "SRS22_21_patient60mths",
      "SRS22_total_60mnd",
      "SRS22_FULL_SCORE_patient60mths",
      "SRS22_funksjon_60mnd",
      "SRS22_FUNCTION_SCORE_patient60mths",
      "SRS22_smerte_60mnd",
      "SRS22_PAIN_SCORE_patient60mths",
      "SRS22_selvbilde_60mnd",
      "SRS22_SELFIMAGE_SCORE_patient60mths",
      "SRS22_mhelse_60mnd",
      "SRS22_MENTALHEALTH_SCORE_patient60mths",
      "SRS22_fornoyd_60mnd",
      "SRS22_SATISFACTION_SCORE_patient60mths",
      "Komplikasjoner_60mnd"
    )
  )

  if (var %in% oppflg$tre) {
    data <- data |>
      dplyr::filter(.data$FOLLOWUP == 3)
  }

  if (var %in% oppflg$tolv) {
    data <- data |>
      dplyr::filter(.data$FOLLOWUP_patient12mths == 12)
  }

  if (var %in% oppflg$seksti) {
    data <- data |>
      dplyr::filter(.data$FOLLOWUP_patient60mths == 60)
  }

  return(data)
}

# nolint start
# sjekk at det fungerer:
## r <- prep_var_na(RegData, "Helsetilstand_3mnd")
# nolint end
