#### ------------------------------- BOXPLOT -------------------------------####

# Først en funksjon som finner navn på kolonner. Brukeren velger variabel og
# denne funksjonen finner alle kolonnene som hører til denne variabelen over tid.

#' @title Finn variabler over tid
#'
#' @param var input fra UI - valg av variabel
#' @return en vektor med navn på kolonner som hører til valgt variabel
#'
#' @examples
#' \donttest{
#' try(finn_variabler("Funksjon"))
#' }
#'
#' @export

finn_variabler <- function(var) {
  variabler <- dplyr::case_when(
    {{ var }} == "SRS22 totalskår" ~ c(
      "SRS22_MAIN_SCORE",
      "SRS22_FULL_SCORE",
      "SRS22_FULL_SCORE_patient12mths",
      "SRS22_FULL_SCORE_patient60mths"
    ),
    {{ var }} == "Funksjon" ~ c(
      "SRS22_FUNCTION_SCORE",
      "SRS22_FUNCTION_SCORE_patient3mths",
      "SRS22_FUNCTION_SCORE_patient12mths",
      "SRS22_FUNCTION_SCORE_patient60mths"
    ),
    {{ var }} == "Selvbilde" ~ c(
      "SRS22_SELFIMAGE_SCORE",
      "SRS22_SELFIMAGE_SCORE_patient3mths",
      "SRS22_SELFIMAGE_SCORE_patient12mths",
      "SRS22_SELFIMAGE_SCORE_patient60mths"
    ),
    {{ var }} == "Mental helse" ~ c(
      "SRS22_MENTALHEALTH_SCORE",
      "SRS22_MENTALHEALTH_SCORE_patient3mths",
      "SRS22_MENTALHEALTH_SCORE_patient12mths",
      "SRS22_MENTALHEALTH_SCORE_patient60mths"
    ),
    {{ var }} == "Smerte" ~ c(
      "SRS22_PAIN_SCORE",
      "SRS22_PAIN_SCORE_patient3mths",
      "SRS22_PAIN_SCORE_patient12mths",
      "SRS22_PAIN_SCORE_patient60mths"
    ),
    {{ var }} == "Helsetilstand" ~ c(
      "HELSETILSTAND_SCALE",
      "HEALTH_CONDITION_SCALE",
      "HEALTH_CONDITION_SCALE_patient12mths",
      "HEALTH_CONDITION_SCALE_patient60mths"
    ),
    {{ var }} == "Tilfredshet" ~ c(
      "SRS22_SATISFACTION_SCORE",
      "SRS22_SATISFACTION_SCORE",
      "SRS22_SATISFACTION_SCORE_patient12mths",
      "SRS22_SATISFACTION_SCORE_patient60mths"
    )
  )
  variabler
}


#' @title Lag tabell til sammenligning
#' Denne funksjonen bruker funksjonen over til å lage et datasett/tabell med
#' alle kolonnene av interesse i et langt format. Det er kun variabelene som er
#' valgt, samt ShNavn som blir med i funksjonen.
#'
#' @param data datasett
#' @param var variabelen som skal brukes i finn_variabler()
#'
#' @return datasett/tabell i langt format
#'
#' @examples
#' \donttest{
#' try(lag_sam_tabell(RegData, "Funksjon"))
#' }
#'
#' @export

lag_sam_tabell <- function(data, var) {
  variables <- finn_variabler({{ var }})

  data_long <- data |>
    dplyr::select("ShNavn", dplyr::all_of(variables)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(variables), names_to = "Punkt", values_to = "Score")

  data_long
}


#' @title Lage nye navn på kolonne
#' Denne funksjonen gir nye navn til variablene i lag_sam_tabell slik at vi får
#' "før operasjon", "3mnd", "2 år" og "5 år".
#'
#' @param data datasett som skal endres
#' @return datasett med nye kolonnenavn
#'
#' @examples
#' \donttest{
#' try(nye_navn(data))
#' }
#'
#' @export

nye_navn <- function(data) {
  data <- data |>
    dplyr::mutate(Punkt = dplyr::recode_values(
      .data$Punkt,
      c(
        "SRS22_MAIN_SCORE",
        "SRS22_FUNCTION_SCORE",
        "SRS22_SELFIMAGE_SCORE",
        "SRS22_MENTALHEALTH_SCORE",
        "SRS22_PAIN_SCORE",
        "HELSETILSTAND_SCALE"
      ) ~ "Pre-operativt",
      c(
        "SRS22_FULL_SCORE",
        "SRS22_FUNCTION_SCORE_patient3mths",
        "SRS22_SELFIMAGE_SCORE_patient3mths",
        "SRS22_MENTALHEALTH_SCORE_patient3mths",
        "SRS22_PAIN_SCORE_patient3mths",
        "HEALTH_CONDITION_SCALE",
        "SRS22_SATISFACTION_SCORE"
      ) ~ "3 mnd",
      c(
        "SRS22_FULL_SCORE_patient12mths",
        "SRS22_FUNCTION_SCORE_patient12mths",
        "SRS22_SELFIMAGE_SCORE_patient12mths",
        "SRS22_MENTALHEALTH_SCORE_patient12mths",
        "SRS22_PAIN_SCORE_patient12mths",
        "HEALTH_CONDITION_SCALE_patient12mths",
        "SRS22_SATISFACTION_SCORE_patient12mths"
      ) ~ "12 mnd",
      c(
        "SRS22_FULL_SCORE_patient60mths",
        "SRS22_FUNCTION_SCORE_patient60mths",
        "SRS22_SELFIMAGE_SCORE_patient60mths",
        "SRS22_MENTALHEALTH_SCORE_patient60mths",
        "SRS22_PAIN_SCORE_patient60mths",
        "HEALTH_CONDITION_SCALE_patient60mths",
        "SRS22_SATISFACTION_SCORE_patient60mths"
      ) ~ "5 aar"
    ))
  data
}


# Videre vask av datasettet. Nå tas NA-er ut. Deretter grupper der det er
# 5 eller færre observasjoner.


#' Vaske datasettet
#'
#' @param data datasettet som skal vaskes
#' @param var variabel brukeren har valgt i UI-delen
#' @return et vasket datasett
#'
#' @export

vask_sam_tabell <- function(data, var) {
  data <- data |>
    dplyr::mutate(
      Punkt = forcats::as_factor(.data$Punkt),
      Punkt = dplyr::case_when(
        {{ var }} %in% c(
          "SRS22 totalskår", "Funksjon",
          "Selvbilde", "Mental helse",
          "Smerte", "Helsetilstand"
        ) ~
          forcats::fct_relevel(
            Punkt, "Pre-operativt",
            "3 mnd", "12 mnd", "5 aar"
          ),
        {{ var }} == "Tilfredshet" ~
          forcats::fct_relevel(
            Punkt, "3 mnd",
            "12 mnd", "5 aar"
          )
      )
    )

  data <- data |>
    dplyr::filter(!is.na(.data$Score))

  data <- data |>
    dplyr::group_by(.data$Punkt, .data$ShNavn) |>
    dplyr::add_count(.data$Punkt)

  data <- data |>
    dplyr::filter(.data$n > 5)
}

# Sjekk at funksjonen fungerer:
# nolint start
## f <- vask_sam_tabell(g, "Funksjon")
# nolint end


#' Funksjon som regner gjennomsnitt og konfidensintervall for variabler
#' @param data datasett som har vært gjennom vask_sam_tabell()
#' @return datasett med konfidensintervall for variabelen valgt i ui-delen
#' @export

finn_sam_konfidensint <- function(data) {
  konf_data <- data |>
    dplyr::group_by(.data$Punkt, .data$ShNavn) |>
    dplyr::mutate(
      gjennomsnitt = round(mean(.data$Score), 2),
      "konfidensintervall lav" = round(t.test(.data$Score)$conf.int[1], 2),
      "konfidensintervall hoey" = round(t.test(.data$Score)$conf.int[2], 2)
    ) |>
    dplyr::select(-c("Score", "n")) |>
    dplyr::distinct()

  return(konf_data)
}

# nolint start
# b <- finn_sam_konfidensint(f)
# nolint end


# Lag fine navn til plotting

#' Data for gg-plotting
#'
#' @param var Variabel som skal få finere navn
#' @return datasett med fine navn til ggplot
#' @export

ggdata_sam_plot <- function(var) {
  gg_data <- data.frame(forklaring = "")

  gg_data <- gg_data |>
    dplyr::mutate(forklaring = dplyr::case_when(
      {{ var }} == "SRS22 totalskår" ~
        "SRS22 totalskår (1: dårlig - 5: bra)",
      {{ var }} == "Funksjon" ~
        "SRS22 funksjon (1: dårlig - 5: bra)",
      {{ var }} == "Selvbilde" ~
        "SRS22 selvbilde (1: dårlig - 5: bra)",
      {{ var }} == "Mental helse" ~
        "SRS22 mental helse (1: dårlig - 5: bra)",
      {{ var }} == "Smerte" ~
        "SRS22 smerte (1: dårlig - 5: bra)",
      {{ var }} == "Helsetilstand" ~
        "Helsetilstand (0-100)",
      {{ var }} == "Tilfredshet" ~
        "SRS22 tilfredshet (1: dårlig - 5: bra)"
    ))
  gg_data
}

# Sjekk at funksjonen fungerer:
# nolint start
## h <- ggdata_sam_plot("Funksjon")
# nolint end


# Lag boxplot

#' Lag boxplot
#'
#' @param data datasett som skal plottes
#' @param ggdata datasett med navn til ggplot
#' @param input_data datasett med brukerens UI-valg
#' @return boxplot
#'
#' @export

boxplot_sam <- function(data, gg_data, input_data) {
  boxplot_sam <- ggplot2::ggplot()

  boxplot_sam <- boxplot_sam +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(x = .data$Punkt, y = .data$Score), fill = "#6CACE4") +
    ggplot2::ylab("Skår") +
    ggplot2::xlab("Utvikling over tid") +
    ggplot2::labs(
      title = gg_data$forklaring,
      caption = paste0(
        "**Valgte variabler:**", "\n", input_data[, 1], ", ", input_data[, 2], "\n",
        input_data[, 3], "-", input_data[, 4], "\n",
        input_data[, 5], "-", input_data[, 6]
      )
    ) +
    ggplot2::theme_light(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
      plot.title = ggplot2::element_text(
        size = 10,
        face = "bold"
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        face = "italic", color = "#87189D"
      )
    )

  return(boxplot_sam)
}

# Sjekk at funksjonen fungerer:
# nolint start
# Lag input_data:
# input_data <- tibble(stuff <- "Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")
## j <- boxplot_sam(f, h, input_data)
## j
# nolint end

#### ------------------------------- DENSITY -------------------------------####


# Forklaring av stegene som kreves for å lage density plot (se modulen)
# 1. finn_variabler()
# 2. lag_sam_tabell()
# 3. nye_navn()
# 4. vask_sam_tabell()
# 5. finn nøyaktig to variabler som skal sammenlignes
# 6. lag densityplot

# Finn nøyaktig to variabler som skal sammenlignes. Density plot-visninga tillater
# kun sammenligning av to variabler

#' Finn to variabler
#'
#' @param data datasett som skal brukes
#' @param valg_sam brukerens UI-valg av variabler som skal sammenlignes
#'
#' @return datasett med to variabler som skal sammenlignes
#'
#' @export

finn_sam_variabler <- function(data, valg_sam) {
  data <- data |>
    dplyr::filter(
      .data$Punkt == dplyr::case_when(
        {{ valg_sam }} == "Før operasjon - 3 mnd" ~ "Pre-operativt",
        {{ valg_sam }} == "Før operasjon - 12 mnd" ~ "Pre-operativt",
        {{ valg_sam }} == "Før operasjon - 5 år" ~ "Pre-operativt",
        {{ valg_sam }} == "3 mnd - 12 mnd" ~ "3 mnd",
        {{ valg_sam }} == "3 mnd - 5 år" ~ "3 mnd",
        {{ valg_sam }} == "12 mnd - 5 år" ~ "12 mnd"
      ) |
        .data$Punkt == dplyr::case_when(
          {{ valg_sam }} == "Før operasjon - 3 mnd" ~ "3 mnd",
          {{ valg_sam }} == "Før operasjon - 12 mnd" ~ "12 mnd",
          {{ valg_sam }} == "Før operasjon - 5 år" ~ "5 aar",
          {{ valg_sam }} == "3 mnd - 12 mnd" ~ "12 mnd",
          {{ valg_sam }} == "3 mnd - 5 år" ~ "5 aar",
          {{ valg_sam }} == "12 mnd - 5 år" ~ "5 aar"
        )
    )
}

# Sjekk at det fungerer:
# nolint start
## r <- finn_sam_variabler(f, "Før operasjon - 3 mnd")
# nolint end


# Lag density plot (tetthetsplot)

#' Density plot for sammenligning
#'
#' @param data datasett som skal plottes
#' @param gg_data datasett med fine navn til ggplot
#' @param input_data datasett med brukerens UI-valg
#'
#' @return density plot (tetthetsplot)
#'
#' @export

density_sam <- function(data, gg_data, input_data) {
  density_sam <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score, fill = .data$Punkt)) +
    ggplot2::geom_density(alpha = .3) +
    ggplot2::scale_fill_manual(values = c("#6FA287", "#6CACE4")) +
    ggplot2::xlab(gg_data$forklaring) +
    ggplot2::ylab("Tetthet") +
    ggplot2::labs(
      caption = paste0(
        "**Valgte variabler:**", "\n", input_data[, 1], ", ", input_data[, 2], "\n",
        input_data[, 3], "-", input_data[, 4], "\n",
        input_data[, 5], "-", input_data[, 6], "\n"
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend("")) +
    ggplot2::theme_light(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 10,
        face = "bold"
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        face = "italic", color = "#87189D"
      )
    )

  return(density_sam)
}

# Sjekk at funksjonen fungerer:
# nolint start
# Lag input_data:
# input_data <- tidyr::tibble(stuff <- "Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")
## p <-  density_sam(r, h, input_data)
## p
# nolint end
