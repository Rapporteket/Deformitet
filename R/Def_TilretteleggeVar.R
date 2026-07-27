#' Funksjon for å tilrettelegge variabler for beregning.
#'
#' Funksjonen gjør utvalg og tilrettelegger variabler (gitt ved valgtVar) til
#' videre bruk. Videre bruk kan eksempelvis være beregning av aggregerte
#' verdier.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den
#' valgte variabelen, samt ta høyde for avhengigheter med
#' andre variabler. Det er også her man angir aksetekster og titler for den
#' valgte variabelen.
#' NB: Mange variabler er tilrettelagt i preprosseseringa, arv fra Ingrid...
#'
#' @param valgtVar parameter som angir hvilke(n) variabel man ønsker å
#' tilrettelegge for videre beregning.
#' @param figurtype Hvilken figurtype det skal tilrettelegges variabler for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

varTilrettelegg <- function(RegData, valgtVar, figurtype = "ford") {
  # Funksjonen er kopiert fra Nakke

  "%i%" <- intersect

  #----------- Figurparametre ------------------------------
  retn <- "V" # Vertikal som standard. 'H' angis evt. for enkeltvariable
  flerevar <- 0
  grtxt <- "" # Spesifiseres for hver enkelt variabel
  xAkseTxt <- ""
  xlab <- "" # Benevning
  if (figurtype == "andelGrVar") {
    xAkseTxt <- "Andel operasjoner (%)"
  }
  ytxt1 <- ""
  varTxt <- ""
  sortAvtagende <- FALSE # Sortering av resultater. FALSE-laveste best
  tittel <- "Mangler tittel"
  deltittel <- ""
  variabler <- "Ingen"
  KIekstrem <- NULL
  RegData$Variabel <- 0

  #--------------- Definere variabler ------------------------------

  # ALDER:
  if (valgtVar == 'alder') { #fordeling
    tittel <-  "Andel operasjoner fordelt på aldersgrupper"

    grtxt <- c("<10", "10", "11", "12", "13", "14", "15", "16", "17",
               "18", "19", "20+")
    RegData$VariabelGr <-
      cut(RegData$Alder,
          breaks = c(-1, 10, 11, 12, 13, 14, 15, 16, 17,
                     18, 19, 20, 100),
          labels = grtxt,
          #include.lowest = TRUE,
          right = FALSE,
          ordered=TRUE)
    xlab <- 'år'
  }


  if (valgtVar == "BMI") {
    tittel <- "Andel operasjoner fordelt på BMI-kategorier"
    RegData$VariabelGr <- cut(RegData$BMI,
                                breaks = c(0, 16, 17, 18.5, 25, 30, 35, 40, 100),
                                labels = c("Alvorlig undervekt < 16",
                                           "Undervekt (16-17)",
                                           "Mild undervekt (17-18,5)",
                                           "Normal (18,5-25)",
                                           "Overvekt (25-30)",
                                           "Moderat fedme, klasse I (30-35)",
                                           "Fedme, klasse II (35-40)",
                                           "Fedme, klasse III (40+)"),
                                right = FALSE,
                                ordered=TRUE)
  }


   if (valgtVar == "reOp") { # AndelGrVar,AndelTid,
    # 1.	1 års reoperasjonsrate
    # Andel pasienter reoperert innen 1 år etter primæroperasjon.
    # Høy måloppnåelse: < 5%

    # Beregnes ved å se på differansen mellom CURRENT_SURGERY=1 og 2 for samme PasientID?
    #   # Det blir opprettet et nytt forløp ved reoperasjon, men kun for registering av skjema ved reoperasjon.
    # PROM data og kontroll intervaller følger fortsatt primæroperasjonen.
    # reoperert1år / alle pasienter

    tittel <- "Reoperert innen 1 år etter primæroperasjon"
    pasIDreop <- unique(RegData$PasientID[which(RegData$CURRENT_SURGERY == 2)])

    RegData_pas2op <- RegData[
      which(RegData$PasientID %in% pasIDreop),
      c("OpDato", "PasientID", "CURRENT_SURGERY", "MCEID", "PARENT")
    ]

    RegDataPas <- RegData_pas2op |>
      dplyr::group_by(PasientID) |>
      dplyr::summarise(
        PasientID = PasientID[1],
        Dager = sort(OpDato[CURRENT_SURGERY == 2])[1] - OpDato[CURRENT_SURGERY == 1]
      )
    RegData <- RegData[which(RegData$CURRENT_SURGERY == 1), ]
    RegData$Variabel[
      RegData$PasientID %in% RegDataPas$PasientID[RegDataPas$Dager < 365]
    ] <- 1

    subtxt <- "reoperasjon"
    TittelVar <- "Reoperert innen 1år"
    ytxt1 <- "reoperasjon"
  }


  if (valgtVar == "liggetidPostOp") { # fordeling, andelGrVar
    # Andel pasienter utskrevet innen (til og med) 6. postoperative dag
    # Høy måloppnåelse: >90%

    tittel <- "Utskrevet innen ei uke"

    RegData <- RegData[which(RegData$BED_DAYS_POSTOPERATIVE >= 0), ]
    # RegData$VariabelGr <- cut(RegData$BED_DAYS_POSTOPERATIVE, breaks=gr, include.lowest=TRUE, right=FALSE)
    # grtxt <- c(0:6, '7+')
    RegData$Variabel[RegData$BED_DAYS_POSTOPERATIVE <= 6] <- 1
    # gr <- c(0:7,100)
    xAkseTxt <- "Antall liggedøgn"
    ytxt1 <- "liggetid"
    sortAvtagende <- TRUE
  }

  if (valgtVar %in% c("fornoydBeh3mnd", "fornoydBeh2aar")) { # Andeler #AndelGrVar #AndelTid
    # 3.	Pasientfornøydhet etter 12mnd (2år)
    # Andel pasienter som på SRS-22 spørsmål nr 21 svarer "svært godt fornøyd"
    # (verdi 5) eller "ganske fornøyd" (verdi 4).
    # Høy måloppnåelse: >90%, # Moderat: 70-90%, # Lav: <70%
    # SRS22_21	Fornoyd2ar	21. Er du fornøyd med resultatet av behandlingen?	Ja

    RegData$VariabelGr <- switch(valgtVar,
      fornoydBeh3mnd = RegData$SRS22_21,
      fornoydBeh2aar = RegData$SRS22_21_patient12mths
    )

    grtxt <- c("Svært misfornøyd", "Litt misfornøyd", "Verken eller", "Ganske fornøyd", "Svært fornøyd", "Ikke svart")
    gr <- c(1:5, 9)
    RegData <- RegData[which(RegData$VariabelGr %in% gr), ]
    retn <- "H"

    if (figurtype == "fordeling") {
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = gr)
    }

    if (figurtype == "andeler") {
      RegData <- RegData[RegData$VariabelGr %in% 1:5, ]
      RegData$Variabel[RegData$VariabelGr %in% 5:4] <- 1
      varTxt <- "fornøyde"
    }
    tittel <- switch(valgtVar,
      fornoydBeh3mnd = "Svært/ganske fornøyd med beh. på sykehuset, 3 mnd etter",
      fornoydBeh2aar = "Svært/ganske fornøyd med beh. på sykehuset, 2 år etter"
    )
    sortAvtagende <- TRUE
  }


  #-------------- SAMMENSATTE variabler - finn
  # For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
  # (dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
  # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
  # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
  # som 0.
  # Vi sender tilbake alle variabler som indikatorvariable, dvs. med 0,1,NA

  if (valgtVar == "reopAarsak") {
    retn <- "H"
    flerevar <- 1
    tittel <- "Årsak til reoperasjon"
    grtxt <- c(
      "Blødning", "Koronal svikt",
      "Implantatbrekkasje", "Infeksjon",
      "Løsning av implantat", "Feilplassert implantat",
      "Nevr. skade/durarift", "Annen sykdom",
      "Smerte", "Sagittal svikt"
    )

    variabler <- c(
      "REOPERATION_BLEEDING", "REOPERATION_CORONAL",
      "REOPERATION_IMPLANT_BREAKAGE", "REOPERATION_INFECTION",
      "REOPERATION_LOOSE_UP_IMPLANT", "REOPERATION_MISPLACED_IMPLANT",
      "REOPERATION_NEUROLOGICAL_INJURY", "REOPERATION_OTHER_DISEASE",
      "REOPERATION_PAIN", "REOPERATION_SAGITTAL"
    )
    ind01 <- which(RegData[, variabler] != -1, arr.ind = T) # Alle ja/nei
    # ind1 <- which(RegData[ ,variabler] == 1, arr.ind=T) #Ja i alle variabler
    # RegData[ ,variabler] <- NA
    # RegData[ ,variabler][ind01] <- 0
    # RegData[ ,variabler][ind1] <- 1
    xAkseTxt <- "Andel opphold (%)"
  }


  # if (valgtVar == "Radiologi") {}
  # if (valgtVar == "Komorbiditet") {}
  # if (valgtVar == "KomplOpr") {}
  # if (valgtVar == "Kompl3mnd") {}
  # if (valgtVar == "OprIndikSmerter") {}
  # if (valgtVar == "OprIndikMyelopati") {}




  # Definerte variabler

        # KURVE:
        if (valgtVar  %in% c("Kurve_preGr", "PRE_MAIN_CURVE")){
          tittel <-  "Andel operasjoner fordelt på pre-operativ kurve"
        }
        if (valgtVar  %in% c("Kurve_post", "POST_MAIN_CURVE")) {
          tittel <-  "Andel operasjoner fordelt på post-operativ kurve"
        }
        if (valgtVar  %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw")) {
          tittel <-  "Andel operasjoner fordelt på prosentvis korreksjon i kurve"}

        # LIGGETID
        if (valgtVar  %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE")) {
          tittel <-  "Andel operasjoner fordelt på liggetid etter operasjon"}

        # KNIVTID
        if (valgtVar  %in% c("Knivtid", "kniv_tid")) {
          tittel <- "Andel operasjoner fordelt på knivtid"}

        # BLODTAP:
        if (valgtVar  %in% c("Blodtap_100", "Blodtap_200", "PER_BLOOD_LOSS_VALUE")) {
          tittel <-  "Andel operasjoner fordelt på blodtap"}

        # SRS22:total
        if (valgtVar %in% c("SRS22_total", "SRS22_MAIN_SCORE")) {
          tittel <-  "Andel operasjoner fordelt på total SRS22 skår (1-5) preoperativt"
          }
        if (valgtVar  %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE")) {
          tittel <-  "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 3-6 måneders oppfølging"}
        if (valgtVar  %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths")) {
          tittel <-  "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths")) {
          tittel <-  "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 5 års oppfølging"
        }

        # SRS22: funksjon
        if (valgtVar  %in% c("SRS22_funksjon", "SRS22_FUNCTION_SCORE")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) preoperativt"
        }
        if (valgtVar  %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 3-6 måneders oppfølging"}
        if (valgtVar  %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 5 års oppfølging"
        }

        # SRS22: smerte
        if (valgtVar  %in% c("SRS22_smerte", "SRS22_PAIN_SCORE")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-smertesskår (1-5) preoperativt"
        }
        if (valgtVar  %in% c("SRS22_smerte_3mnd", "SRS22_PAIN_SCORE_patient3mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_smerte_12mnd", "SRS22_PAIN_SCORE_patient12mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 12 måneders oppfølging"
        }
          if (valgtVar  %in% c("SRS22_smerte_60mnd", "SRS22_PAIN_SCORE_patient60mths")) {
          "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 5 års oppfølging"
        }


        # SRS22: selvbilde
        if (valgtVar  %in% c("SRS22_selvbilde", "SRS22_SELFIMAGE_SCORE")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) preoperativt"
        }
        if (valgtVar  %in% c("SRS22_selvbilde_3mnd", "SRS22_SELFIMAGE_SCORE_patient3mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_selvbilde_12mnd", "SRS22_SELFIMAGE_SCORE_patient12mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_selvbilde_60mnd", "SRS22_SELFIMAGE_SCORE_patient60mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 5 års oppfølging"
        }

        # SRS22: mental helse
        if (valgtVar  %in% c("SRS22_mhelse", "SRS22_MENTALHEALTH_SCORE")) {
          tittel <- "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) preoperativt"
        }
        if (valgtVar  %in% c("SRS22_mhelse_3mnd", "SRS22_MENTALHEALTH_SCORE_patient3mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_mhelse_12mnd", "SRS22_MENTALHEALTH_SCORE_patient12mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_mhelse_60mnd", "SRS22_MENTALHEALTH_SCORE_patient60mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 5 års oppfølging"
        }

        # SRS22: fornøyd
        if (valgtVar  %in% c("SRS22_fornoyd_3mnd", "SRS22_SATISFACTION_SCORE")) {
          tittel <- "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_fornoyd_12mnd", "SRS22_SATISFACTION_SCORE_patient12mths")) {
          tittel <- "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_fornoyd_60mnd", "SRS22_SATISFACTION_SCORE_patient60mths")) {
          tittel <-  "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 5 års oppfølging"
        }

        # SRS22: spm 21 - hvor fornøyd?
        if (valgtVar  %in% c("SRS22_spm21_3mnd", "SRS22_21")) {
          tittel <- "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_spm21_12mnd", "SRS22_21_patient12mths")) {
          tittel <-  "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_spm21_60mnd", "SRS22_21_patient60mths")) {
          tittel <- "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 5 års oppfølging"
        }

        # SRS22: spm 22 - på nytt?
        if (valgtVar  %in% c("SRS22_spm22_3mnd", "SRS22_22")) {
          tittel <-  "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_spm22_12mnd", "SRS22_22_patient12mths")) {
          tittel <- "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("SRS22_spm22_60mnd", "SRS22_22_patient60mths")) {
          "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 5 års oppfølging"
        }

        # EQ5D

        # HELSETILSTAND
        if (valgtVar  %in% c("Helsetilstand", "HELSETILSTAND_SCALE")) {
          tittel <-  "Andel operasjoner fordelt på helsetilstandsskår (0-100) preoperativt"
        }
        if (valgtVar  %in% c("Helsetilstand_3mnd", "HEALTH_CONDITION_SCALE")) {
          tittel <-  "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 3-6 måneders oppfølging"
        }
        if (valgtVar  %in% c("Helsetilstand_12mnd", "HEALTH_CONDITION_SCALE_patient12mths")) {
          tittel <- "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 12 måneders oppfølging"
        }
        if (valgtVar  %in% c("Helsetilstand_60mnd", "HEALTH_CONDITION_SCALE_patient_60_mths")) {
          tittel <- "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 5 års oppfølging"
        }


        # KOMPLIKASJONER
        if (valgtVar  == "Komplikasjoner_3mnd") {
          tittel <- "Andel komplikasjoner pr. operasjon(3-6 mnd)"
        }
        if (valgtVar  == "Komplikasjoner_12mnd") {
          tittel <- "Andel komplikasjoner pr. operasjon (12 mnd)"
        }
        if (valgtVar  == "Komplikasjoner_60mnd") {
          tittel <- "Andel komplikasjoner pr. operasjon (5 år)"
        }
        if (valgtVar  == "Andel operasjoner") {
          tittel <- "Andel operasjoner"
        }


      # Add title for label in plot (specifically xlab in ggplot)---------------------

      # xlab = dplyr::case_when(
      #   {{ var }} == "BMI_kategori" ~ "BMI-kategorier",
      #   {{ var }} == "BMI" ~ "BMI",
      #
      #   # ALDER:
      #   {{ var }} == "Alder" ~ "Aldersgrupper",
      #   {{ var }} == "Alder_num" ~ "Alder",
      #
      #   # KURVE:
      #   {{ var }} %in% c("Kurve_preGr", "PRE_MAIN_CURVE") ~
      #     "Pre-operativ kurve",
      #   {{ var }} %in% c("Kurve_post", "POST_MAIN_CURVE") ~
      #     "Post-operativ kurve",
      #   {{ var }} %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw") ~
      #     "Post-operativ prosent korreksjon",
      #
      #   # LIGGETID
      #   {{ var }} %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE") ~
      #     "Liggetid etter operasjon, oppgitt i dager",
      #
      #   # KNIVTID
      #   {{ var }} %in% c("Knivtid", "kniv_tid") ~ "Knivtid, oppgitt i minutter",
      #
      #   # BLODTAP:
      #   {{ var }} == "Blodtap_100" ~ "Blodtap pr 100ml",
      #   {{ var }} == "Blodtap_200" ~ "Blodtap pr 200ml",
      #   {{ var }} == "PER_BLOOD_LOSS_VALUE" ~ "Blodtap i ml",
      #
      #   # SRS22:total
      #   {{ var }} %in% c("SRS22_total", "SRS22_MAIN_SCORE") ~
      #     "Total SRS22 skår (1-5) preoperativt",
      #   {{ var }} %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE") ~
      #     "Total SRS22 skår (1-5) ved 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths") ~
      #     "Total SRS22 skår (1-5) ved 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths") ~
      #     "Total SRS22 skår (1-5) ved 5 års oppfølging",
      #
      #   # SRS22: funksjon
      #   {{ var }} %in% c("SRS22_funksjon", "SRS22_FUNCTION_SCORE") ~
      #     "SRS22-funksjonsskår (1-5) preoperativt",
      #   {{ var }} %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths") ~
      #     "SRS22-funksjonsskår (1-5), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths") ~
      #     "SRS22-funksjonsskår (1-5), 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths") ~
      #     "SRS22-funksjonsskår (1-5), 5 års oppfølging",
      #
      #   # SRS22: smerte
      #   {{ var }} %in% c("SRS22_smerte", "SRS22_PAIN_SCORE") ~
      #     "SRS22-smertesskår (1-5) preoperativt",
      #   {{ var }} %in% c("SRS22_smerte_3mnd", "SRS22_PAIN_SCORE_patient3mths") ~
      #     "SRS22-smertesskår (1-5), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_smerte_12mnd", "SRS22_PAIN_SCORE_patient12mths") ~
      #     "SRS22-smertesskår (1-5), 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_smerte_60mnd", "SRS22_PAIN_SCORE_patient60mths") ~
      #     "SRS22-smertesskår (1-5), 5 års oppfølging",
      #
      #   # SRS22: selvbilde
      #   {{ var }} %in% c("SRS22_selvbilde", "SRS22_SELFIMAGE_SCORE") ~
      #     "SRS22-selvbildesskår (1-5) preoperativt",
      #   {{ var }} %in% c("SRS22_selvbilde_3mnd", "SRS22_SELFIMAGE_SCORE_patient3mths") ~
      #     "SRS22-selvbildesskår (1-5), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_selvbilde_12mnd", "SRS22_SELFIMAGE_SCORE_patient12mths") ~
      #     "SRS22-selvbildesskår (1-5), 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_selvbilde_60mnd", "SRS22_SELFIMAGE_SCORE_patient60mths") ~
      #     "SRS22-selvbildesskår (1-5), 5 års oppfølging",
      #
      #   # SRS22: mental helse
      #   {{ var }} %in% c("SRS22_mhelse", "SRS22_MENTALHEALTH_SCORE") ~
      #     "SRS22-mental-helse-skår (1-5) preoperativt",
      #   {{ var }} %in% c("SRS22_mhelse_3mnd", "SRS22_MENTALHEALTH_SCORE_patient3mths") ~
      #     "SRS22-mental-helse-skår (1-5), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_mhelse_12mnd", "SRS22_MENTALHEALTH_SCORE_patient12mths") ~
      #     "SRS22-mental-helse-skår (1-5), 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_mhelse_60mnd", "SRS22_MENTALHEALTH_SCORE_patient60mths") ~
      #     "SRS22-mental-helse-skår (1-5), 5 års oppfølging",
      #
      #   # SRS22: fornøyd
      #   {{ var }} %in% c("SRS22_fornoyd_3mnd", "SRS22_SATISFACTION_SCORE") ~
      #     "SRS22-fornøydhetsskår (1-5), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_fornoyd_12mnd", "SRS22_SATISFACTION_SCORE_patient12mths") ~
      #     "SRS22-fornøydhetsskår (1-5), 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_fornoyd_60mnd", "SRS22_SATISFACTION_SCORE_patient60mths") ~
      #     "SRS22-fornøydhetsskår (1-5), 5 års oppfølging",
      #
      #
      #   # SRS22: spm 21 - hvor fornøyd?
      #   {{ var }} %in% c("SRS22_spm21_3mnd", "SRS22_21") ~
      #     "'Er du fornøyd med resultatet av behandlingen?', 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_spm21_12mnd", "SRS22_21_patient12mths") ~
      #     "'Er du fornøyd med resultatet av behandlingen?', 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_spm21_60mnd", "SRS22_21_patient60mths") ~
      #     "'Er du fornøyd med resultatet av behandlingen?', 5 år oppfølging",
      #
      #   # SRS22: spm 22 - på nytt?
      #   {{ var }} %in% c("SRS22_spm22_3mnd", "SRS22_22") ~
      #     "'Ville du ønsket samme behandling på nytt?', 3-6 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_spm22_12mnd", "SRS22_22_patient12mths") ~
      #     "'Ville du ønsket samme behandling på nytt?', 12 måneders oppfølging",
      #   {{ var }} %in% c("SRS22_spm22_60mnd", "SRS22_22_patient60mths") ~
      #     "'Ville du ønsket samme behandling på nytt?', 5 års oppfølging",
      #
      #
      #   # EQ5D
      #
      #   # HELSETILSTAND
      #   {{ var }} %in% c("Helsetilstand", "HELSETILSTAND_SCALE") ~
      #     "Helsetilstandsskår (0-100) preoperativt",
      #   {{ var }} %in% c("Helsetilstand_3mnd", "HEALTH_CONDITION_SCALE") ~
      #     "Helsetilstandsskår (0-100), 3-6 måneders oppfølging",
      #   {{ var }} %in% c("Helsetilstand_12mnd", "HEALTH_CONDITION_SCALE_patient12mths") ~
      #     "Helsetilstandsskår (0-100), 12 måneders oppfølging",
      #   {{ var }} %in% c("Helsetilstand_60mnd", "HEALTH_CONDITION_SCALE_patient_60_mths") ~
      #     "Helsetilstandsskår (0-100), 5 års oppfølging",
      #
      #   # KOMPLIKASJONER
      #   {{ var }} == "Komplikasjoner_3mnd" ~ "Selvrapportert komplikasjon, 3-6 måneders oppfølging",
      #   {{ var }} == "Komplikasjoner_12mnd" ~ "Selvrapportert komplikasjon, 12 måneders oppfølging",
      #   {{ var }} == "Komplikasjoner_60mnd" ~ "Selvrapportert komplikasjon, 5 års oppfølging",
      #   {{ var }} == "Andel operasjoner" ~ "Andel operasjoner"
      # )


gg_data <- data.frame(title = "",
                      xlab = "")

gg_data <- gg_data |>
  dplyr::mutate(
    title = tittel,
    xlab = xlab
  )

  UtData <- list(
    RegData = RegData, grtxt = grtxt, varTxt = varTxt, xlab = xAkseTxt,
    tittel = tittel, varTxt = varTxt, flerevar = flerevar, # KImaalGrenser=KImaalGrenser,
    variabler = variabler, sortAvtagende = sortAvtagende,
    retn = retn, ytxt1 = ytxt1, deltittel = deltittel, KIekstrem = KIekstrem
  )
  # RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
  return(invisible(UtData))

}
