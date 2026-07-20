#' Endre variabelnavn/kolonnenavn til selvvalgte navn
#' @param tabell datatabellnavn i databasen
#' @param tabType REGISTRATION_TYPE
#' @return tabell med selvvalgte variabelnavn spesifisert i friendlyvar
#' @export

# Funksjon mappingEgneNavn Skal flyttes hit.
mappingEgneNavnDum <- function(tabell, tabType) {
  indTabType <- which(.data$friendlyVarTab$REGISTRATION_TYPE %in% tabType)
  navn <- .data$friendlyVarTab$FIELD_NAME[indTabType]
  names(navn) <- .data$friendlyVarTab$USER_SUGGESTION[indTabType]
  tabell <- dplyr::rename(tabell, dplyr::all_of(navn))
}


# LEGG INN FJERNING AV VARIABLER SOM GJENTAS I FLERE TABELLER. f.EKS. ReshId (CENTREID)

#' Hent datatabell fra Deformitets database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
#'                   Kan ha følgende verdier:
#'                   mce
#'                   patient
#'                   patientform
#'                   patientfollowup
#'                   surgeonform
#'                   surgeonfollowup
#'
#' @param egneVarNavn 0 - Qreg-navn benyttes.
#'                    1 - selvvalgte navn fra Friendlyvar benyttes
#' mce har ingen selvvalgte navn
#' Egenvalgte navn omfatter REGISTRATION_TYPE:
#' PATIENT, PATIENTFOLLOWUP, PATIENTFOLLOWUP12, ,
#' SURGEONFORM, SURGEONFOLLOWUP SURGEONFOLLOWUP12
#' NB: Hvis egendefinerte navn velges for oppfølgingsskjema, flates skjemaet ut.
#'
#' @export

hentDataTabell <- function(tabellnavn = "surgeonform",
                           qVar = "*",
                           egneVarNavn = 0) {
  query <- paste0("SELECT ", qVar, " FROM ", tabellnavn)
  tabell <- rapbase::loadRegData(
    registryName = "data",
    query = query
  )


  if (tabellnavn %in% c("surgeonform", "patient_form")) {
    tabell <- tabell[tabell$STATUS == 1, ]
  }

  if (egneVarNavn == 1) {
    friendlyVarTab <- rapbase::loadRegData(
      registryName = "data",
      query = "SELECT * FROM friendlyvars"
    )
    indFeil <- which(friendlyVarTab$USER_SUGGESTION == "NEINNICHTS")
    if (length(indFeil) > 0) {
      friendlyVarTab$USER_SUGGESTION[indFeil] <- NA
    }

    friendlyVarTab <- friendlyVarTab[
      !is.na(friendlyVarTab$USER_SUGGESTION),
      c("FIELD_NAME", "VAR_ID", "TABLE_NAME", "USER_SUGGESTION", "REGISTRATION_TYPE")
    ]

    if (tabellnavn == "surgeonform") {
      tabell$KNIFE_TIME_CALCULATED <- 0
    }

    # Funksjon IKKE heldig at denne står inne i funksjon. Flytt..
    mappingEgneNavn <- function(tabell, tabType) {
      indTabType <- which(friendlyVarTab$REGISTRATION_TYPE %in% tabType)
      navn <- friendlyVarTab$FIELD_NAME[indTabType]
      names(navn) <- friendlyVarTab$USER_SUGGESTION[indTabType]
      tabell <- dplyr::rename(tabell, dplyr::any_of(navn))
    }

    if (tabellnavn %in% c("patientfollowup", "surgeonfollowup")) {
      tabell12 <- tabell |> dplyr::filter(.data$FOLLOWUP == 12)
      tabell12 <- mappingEgneNavn(
        tabell = tabell12,
        tabType = paste0(toupper(tabellnavn), "12")
      )
      tabell3 <- tabell |> dplyr::filter(.data$FOLLOWUP == 3)
      tabell3 <- mappingEgneNavn(
        tabell = tabell3,
        tabType = toupper(tabellnavn)
      )
      tabell <- merge(tabell3, tabell12, by = "MCEID", suffixes = c("3mnd", "12mnd"))
    } else {
      tabell <- mappingEgneNavn(
        tabell = tabell,
        tabType = toupper(tabellnavn)
      )
    }
  }

  return(tabell)
}


#' Hente og koble sammen "alle" tabeller
#' Ferdigstilte data
#'
#' @export
alleRegData <- function(egneVarNavn = 0) {
  stopifnot(egneVarNavn %in% 0:1)

  mce <- hentDataTabell("mce")
  # centre <- hentDataTabell("centre")
  patient <- hentDataTabell(
    "patient",
    egneVarNavn = egneVarNavn,
    qVar = "BIRTH_DATE, DECEASED, DECEASED_DATE, DISTRICTCODE, DISTRICTNAME,
    EDUCATION, GENDER, ID, MARITAL_STATUS, NORWEGIAN, REGISTERED_DATE,
    TSCREATED"
  ) # Resterende variabler er tomme eller krypterte

  patient_followup <- hentDataTabell("patientfollowup", egneVarNavn = egneVarNavn)
  patient_form <- hentDataTabell("patientform", egneVarNavn = egneVarNavn)
  surgeon_followup <- hentDataTabell("surgeonfollowup", egneVarNavn = egneVarNavn)
  surgeon_form <- hentDataTabell("surgeonform", egneVarNavn = egneVarNavn)
  # Sykehusnavn
  EnhetsNavn <- hentDataTabell(
    tabellnavn = "centreattribute",
    qVar = "ID, ATTRIBUTEVALUE as ShNavn"
  )

  if (egneVarNavn == 0) {
    RegData <- merge(mce, EnhetsNavn, by.x = "CENTREID", by.y = "ID", all.x = TRUE) |>
      merge(surgeon_form,
        by = "MCEID", suffixes = c("", "_surgeon")
      ) |>
      merge(patient_form,
        by = "MCEID", suffixes = c("", "_patient_form"), all.x = TRUE
      ) |>
      merge(patient,
        by.x = "PATIENT_ID", suffixes = c("", "_patient"),
        by.y = "ID"
      ) |>
      merge(patient_followup |> dplyr::filter(.data$FOLLOWUP == 3),
        suffixes = c("", "_patient3mths"), by = "MCEID", all.x = TRUE
      ) |>
      merge(patient_followup |> dplyr::filter(.data$FOLLOWUP == 12),
        suffixes = c("", "_patient12mths"), by = "MCEID", all.x = TRUE
      ) |>
      merge(patient_followup |> dplyr::filter(.data$FOLLOWUP == 60),
        suffixes = c("", "_patient60mths"), by = "MCEID", all.x = TRUE
      ) |>
      merge(surgeon_followup |> dplyr::filter(.data$FOLLOWUP == 3),
        suffixes = c("", "_surgeon3mths"), by = "MCEID", all.x = TRUE
      ) |>
      merge(surgeon_followup |> dplyr::filter(.data$FOLLOWUP == 12),
        suffixes = c("", "_surgeon12mths"), by = "MCEID", all.x = TRUE
      )
  }

  if (egneVarNavn == 1) {
    # NB: status-variabel har endret navn. Ta med filtrering på status før endrer navn
    RegData <- merge(mce, EnhetsNavn,
      by.x = "CENTREID", by.y = "ID",
      suffixes = c("", "Shus"), all.y = TRUE
    ) |>
      merge(patient,
        by.x = "PATIENT_ID", suffixes = c("", "_pasOppl"),
        by.y = "PasientID"
      ) |>
      merge(surgeon_form,
        by = "MCEID", suffixes = c("", "_lege")
      ) |>
      merge(patient_form,
        by = "MCEID", suffixes = c("", "_pasient"), all.x = TRUE
      ) |>
      merge(patient_followup,
        suffixes = c("", "_pasOppf"), by = "MCEID", all.x = TRUE
      ) |>
      merge(surgeon_followup,
        suffixes = c("", "_legeOppf"), by = "MCEID", all.x = TRUE
      )
  }
  return(RegData)
}
