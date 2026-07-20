## Funksjon for å telle forløp pr kvalitetsindikator
#' Kvalitetsindikatortelling
#'
#' Funksjon som teller tilfeller av visse vilkår til kvalitetsindikatorer
#'
#' @param data data som har vært gjennom prepVar()
#' @param kjoenn brukerens valg på kjønn
#' @param var brukerens valg av variabel
#' @param userRole brukerens brukerrolle
#' @param userUnitId brukuers reshId
#' @param map_data datasett som mapper reshId og sykehusnavn
#'
#' @examples
#' \donttest{
#' try(count_kvalind(RegData, "begge", "PRE_MAIN_CURVE", "SC", 111961, map_db_resh))
#' }
#'
#' @export

count_kvalind <- function(data, kjoenn, var, userRole, userUnitId, map_data) {
  # Legge til kolonne med telling av komplikasjoner der smerte er tatt ut
  data <- ny_komplikasjon3mnd_usmerte(data)


  # Telle forløp
  my_tiny_data <- data |>
    dplyr::group_by(.data$ShNavn, .data$Kjonn) |>
    dplyr::add_tally(name = "n") |> # antall pasienter per sykehus per kjønn
    dplyr::ungroup() |>
    dplyr::filter(dplyr::case_when(
      {{ var }} == "PRE_MAIN_CURVE" ~
        PRE_MAIN_CURVE > 70,
      {{ var }} == "Komplikasjoner_3mnd" ~
        komplikasjoner_uSmerte_3mnd == "ja",
      {{ var }} == "LiggetidGr" ~
        LiggetidGr == "> 7" | LiggetidGr == "7",
      {{ var }} == "SRS22_spm21_3mnd" ~
        SRS22_spm21_3mnd == "Ganske fornøyd" | SRS22_spm21_3mnd == "Svært godt fornøyd",
      {{ var }} == "CURRENT_SURGERY" ~
        CURRENT_SURGERY == 2,
      TRUE ~
        CURRENT_SURGERY == 1 | CURRENT_SURGERY == 2
    )) |>
    dplyr::group_by(.data$ShNavn, .data$Kjonn) |>
    dplyr::add_count(name = "antall_kval_syk_kjønn") |>
    dplyr::ungroup() |>
    dplyr::select("ShNavn", "Kjonn", "n", "antall_kval_syk_kjønn") |>
    dplyr::distinct()

  my_tiny_data_nasj <- data |>
    dplyr::group_by(.data$Kjonn) |>
    dplyr::add_tally(name = "n") |>
    dplyr::mutate(ShNavn = "Nasjonalt") |>
    dplyr::relocate(.data$ShNavn, .before = .data$Kjonn) |>
    dplyr::ungroup() |>
    dplyr::filter(dplyr::case_when(
      {{ var }} == "PRE_MAIN_CURVE" ~
        PRE_MAIN_CURVE > 70,
      {{ var }} == "Komplikasjoner_3mnd" ~
        komplikasjoner_uSmerte_3mnd == "ja",
      {{ var }} == "LiggetidGr" ~
        LiggetidGr == "> 7" | LiggetidGr == "7",
      {{ var }} == "SRS22_spm21_3mnd" ~
        SRS22_spm21_3mnd == "Ganske fornøyd" | SRS22_spm21_3mnd == "Svært godt fornøyd",
      {{ var }} == "CURRENT_SURGERY" ~
        CURRENT_SURGERY == 2,
      TRUE ~
        CURRENT_SURGERY == 1 | CURRENT_SURGERY == 2
    )) |>
    dplyr::group_by(.data$Kjonn) |>
    dplyr::add_count(name = "antall_kval_syk_kjønn") |>
    dplyr::ungroup() |>
    dplyr::select("ShNavn", "Kjonn", "n", "antall_kval_syk_kjønn") |>
    dplyr::distinct()

  my_tiny_data_total <- rbind(my_tiny_data_nasj, my_tiny_data)

  my_begge <- my_tiny_data_total |>
    dplyr::group_by(.data$ShNavn) |>
    dplyr::mutate(
      n = sum(.data$n),
      antall_kval_syk_kjønn = sum(.data$antall_kval_syk_kjønn)
    ) |>
    dplyr::select(-c("Kjonn")) |>
    dplyr::mutate(Kjonn = "begge") |>
    dplyr::relocate(.data$Kjonn, .before = .data$n) |>
    dplyr::distinct()

  data_total <- dplyr::full_join(my_tiny_data_total, my_begge)

  ######################### Regne ut andeler ###################################

  data_total <- data_total |>
    dplyr::mutate(
      andel_per_syk_kjønn =
        round(.data$antall_kval_syk_kjønn / .data$n * 100, 2)
    )


  ##### BRUKERVALG: #####

  if (kjoenn == "begge") {
    data_total <- data_total |>
      dplyr::filter(.data$Kjonn == "begge")
  } else {
    if (kjoenn == "kvinne") {
      data_total <- data_total |>
        dplyr::filter(.data$Kjonn == "kvinne")
    } else {
      if (kjoenn == "mann") {
        data_total <- data_total |>
          dplyr::filter(.data$Kjonn == "mann")
      }
    }
  }

  # Filtrer basert på brukertilhørighet:
  map_data <- map_data |>
    dplyr::rename(
      CENTREID = .data$UnitId,
      ShNavn = .data$orgname
    ) |>
    dplyr::add_row(
      CENTREID = "0",
      ShNavn = "Nasjonalt"
    )


  data_total <- dplyr::left_join(data_total, map_data)

  if (userRole != "SC") {
    data_total <- data_total |>
      dplyr::filter(.data$CENTREID == {{ userUnitId }} | .data$CENTREID == "0")
  }

  data_total <- data_total |>
    dplyr::select(-c("CENTREID"))


  return(data_total)
}

#' Funksjon som regner komplikasjoner
#'
#' Funksjonen regner antall komplikasjoner med unntak av smerte.
#'
#' @param data data som har vært gjennom utvalg_basic()
#'
#'
#' @export

ny_komplikasjon3mnd_usmerte <- function(data) {
  data <- data |>
    dplyr::mutate(
      komplikasjoner_uSmerte_3mnd =
        dplyr::if_else(
          .data$COMPLICATIONS_BLEEDING == 1 |
            .data$COMPLICATIONS_HEAD == 1 |
            .data$COMPLICATIONS_DVT == 1 |
            .data$COMPLICATIONS_UTI == 1 |
            .data$COMPLICATIONS_PNEUMONIA == 1 |
            .data$COMPLICATIONS_PE == 1 |
            .data$COMPLICATIONS_INFECTION_WOUND == 1 |
            .data$COMPLICATIONS_INFECTION_DEEP == 1 |
            .data$COMPLICATIONS_INFECTION_REOP == 1 |
            .data$COMPLICATIONS_NUMBNESS == 1 |
            .data$COMPLICATIONS_OTHER == 1, "ja", "nei"
        )
    ) |>
    dplyr::mutate(
      komplikasjoner_uSmerte_3mnd =
        dplyr::if_else(is.na(.data$komplikasjoner_uSmerte_3mnd), "nei",
          dplyr::if_else(.data$komplikasjoner_uSmerte_3mnd == "nei", "nei", "ja")
        )
    )

  return(data)
}


#' Funksjon som lager kvalitetsindikatorplot
#'
#' @param data data som har vært gjennom count_kvalind()
#' @param gg_data data som kommer fra perpVar()[2]
#' @param choice_kjonn brukervalg på visning på kjønn
#'
#' @examples
#' \donttest{
#' try(kval_plot(data, gg_data, data_var, "nei"))
#' }
#'
#' @export

kval_plot <- function(data, gg_data, data_var, choice_kjonn) {
  data <- data |>
    dplyr::mutate(ShNavn = forcats::fct_relevel(.data$ShNavn, "Nasjonalt", after = Inf))

  kval_plot <-
    ggplot2::ggplot(
      data = data, ggplot2::aes(
        x = .data$andel_per_syk_kjønn,
        y = .data$ShNavn,
        fill = .data$ShNavn
      )
    ) +
    ggplot2::annotate(
      "rect",
      xmin = gg_data$ymin, xmax = gg_data$ymax,
      ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "palegreen4"
    ) +
    ggplot2::geom_col(alpha = .7) +
    ggplot2::scale_x_continuous(breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +

    # TITLES

    ggplot2::xlab(paste("Andel pasienter (%)", gg_data$ylab)) +
    ggplot2::labs(
      title = gg_data$title,
      caption = paste0(
        "**Valgte variabler:**", "\n", data_var[1, ],
        ", ", "\n", "Kjonn: ", data_var[2, ], "\n",
        "Dato: ", data_var[3, ], "-", data_var[4, ], "\n",
        "Alder: ", data_var[5, ], "-", data_var[6, ]
      )
    ) +
    ggplot2::geom_label(
      ggplot2::aes(x = 0, label = paste(.data$antall_kval_syk_kjønn, "av", .data$n)),
      fill = "#BFCED6", color = "#003087", fontface = "italic",
      position = ggplot2::position_dodge(.9), hjust = -.01, size = 3,
      alpha = .8
    ) +
    ggplot2::facet_wrap(~Kjonn) +
    ggplot2::theme_bw(base_size = 16) + # light theme

    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        color = "#87189D", # add caption
        face = "italic"
      ),
      legend.position = dplyr::case_when(
        {{ choice_kjonn }} == "begge" ~ "none",
        TRUE ~ "right"
      ),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16)
    ) +
    ggplot2::scale_fill_manual(
      values = c("#6CACE4", "#6FA287", "#BFCED6", "#003087"),
      guide = ggplot2::guide_legend(reverse = TRUE)
    )


  return(kval_plot)
}


#' @title Function for getting explanations for kvalitetsindikatorer
#' @param kjonn_choice brukervalg på visning kjønn
#' @param var brukervalg på variabel
#'
#' @examples
#' \donttest{
#' try(explanation_kvalind("begge", "SRS22_spm22_3mnd"))
#' }
#'
#' @export


explanation_kvalind <- function(kjonn_choice, var) {
  data <- data.frame(header = "", text = "")


  data <- data |>
    dplyr::mutate(
      text = dplyr::case_when(
        {{ kjonn_choice }} == "begge" ~
          dplyr::recode_values(
            {{ var }},
            "SRS22_spm22_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel pasienter som svarte ",
                "'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ",
                "ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall pasienter som svarte 'definitivt ja' eller ",
                "'sannsynligvis ja' / antall opererte *100", "<br/>"
              ),
            "SRS22_spm21_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel pasienter som svarte ",
                "'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er ",
                "fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall pasienter som svarte 'svært godt fornøyd' eller ",
                "'ganske fornøyd' / antall opererte *100", "<br/>"
              ),
            "PRE_MAIN_CURVE" ~
              paste0(
                "Figuren viser fordelingen av andel pasienter som hadde ",
                "pre-operativ kurve over 70 grader. For hvert sykehus er det",
                " regnet ut slik:", "<br/>",
                "antall pasienter med preoperativ kurve > 70 grader / antall opererte *100", "<br/>"
              ),
            "Liggetid" ~
              paste0(
                "Figuren viser fordelingen av andel pasienter med ",
                "post-operativ liggetid på 7 eller flere dager. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall pasienter med post-operativ liggetid 7 dager eller mer / antall opererte *100", "<br/>"
              ),
            "Komplikasjoner_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel pasienter som rapporterte ",
                "komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall pasienter som rapporterer komplikasjoner (med unntak av smerte) ",
                "ved 3-6 mnds oppfølging / antall opererte *100", "<br/>"
              ),
            "CURRENT_SURGERY" ~
              paste0(
                "Figuren viser reoperasjonsrate. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall reoperasjoner / antall operasjoner *100", "<br/>"
              )
          ),
        {{ kjonn_choice }} == "mann" ~
          dplyr::recode_values(
            {{ var }},
            "SRS22_spm22_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel mannlige pasienter som svarte ",
                "'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket ",
                "samme operasjon på nytt ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall mannlige pasienter som svarte 'definitivt ja' eller ",
                "'sannsynligvis ja' / antall opererte menn *100", "<br/>"
              ),
            "SRS22_spm21_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel mannlige pasienter som svarte ",
                "'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er fornøyde ",
                "med resultatet av operasjonen ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall mannlige pasienter som svarte 'svært godt fornøyd' eller ",
                "'ganske fornøyd' / antall opererte menn *100", "<br/>"
              ),
            "PRE_MAIN_CURVE" ~
              paste0(
                "Figuren viser fordelingen av andel mannlige pasienter som hadde ",
                "pre-operativ kurve over 70 grader på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall mannlige pasienter med preoperativ kurve > 70 grader / antall opererte menn *100", "<br/>"
              ),
            "Liggetid" ~
              paste0(
                "Figuren viser fordelingen av andel mannlige pasienter med ",
                "post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall mannlige pasienter med post-operativ liggetid 7 dager eller ",
                "mer / antall opererte menn *100", "<br/>"
              ),
            "Komplikasjoner_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel mannlige pasienter som rapporterte ",
                "komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall mannlige pasienter som rapporterer komplikasjoner (med unntak av smerte) ",
                "ved 3-6 mnds oppfølging / antall opererte menn *100", "<br/>"
              ),
            "CURRENT_SURGERY" ~
              paste0(
                "Figuren viser reoperasjonsrate for menn på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall reopererte menn / antall opererte menn *100", "<br/>"
              )
          ),
        {{ kjonn_choice }} == "kvinne" ~
          dplyr::recode_values(
            {{ var }},
            "SRS22_spm22_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel kvinnelige pasienter som svarte ",
                "'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket ",
                "samme operasjon på nytt ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall kvinnelige pasienter som svarte 'definitivt ja' eller ",
                "'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>"
              ),
            "SRS22_spm21_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel kvinnelige pasienter som svarte ",
                "'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål ",
                "om de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall kvinnelige pasienter som svarte 'svært godt fornøyd' eller ",
                "'ganske fornøyd' / antall opererte kvinner * 100", "<br/>"
              ),
            "PRE_MAIN_CURVE" ~
              paste0(
                "Figuren viser fordelingen av andel kvinnelige pasienter som hadde ",
                "pre-operativ kurve over 70 grader på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall kvinnelige pasienter med preoperativ kurve > 70 grader / ",
                "antall opererte kvinner *100", "<br/>"
              ),
            "Liggetid" ~
              paste0(
                "Figuren viser fordelingen av andel kvinnelige pasienter med ",
                "post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall kvinnelige pasienter med post-operativ liggetid 7 dager eller mer / ",
                "antall opererte kvinner *100", "<br/>"
              ),
            "Komplikasjoner_3mnd" ~
              paste0(
                "Figuren viser fordelingen av andel kvinnelige pasienter som ",
                "rapporterte komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall kvinnelige pasienter som rapporterer komplikasjoner (med unntak av smerte) ",
                "ved 3-6 mnds oppfølging / antall opererte kvinner *100", "<br/>"
              ),
            "CURRENT_SURGERY" ~
              paste0(
                "Figuren viser reoperasjonsrate for kvinner på de ulike sykehusene. ",
                "For hvert sykehus er det regnet ut slik:", "<br/>",
                "antall reopererte kvinner / antall opererte kvinner *100", "<br/>"
              )
          ),
        {{ kjonn_choice }} == "nei" ~
          dplyr::recode_values(
            {{ var }},
            "SRS22_spm22_3mnd" ~
              paste0(
                "Figuren viser fordelinger av andel pasienter (av begge kjønn),
                      andel kvinnelige pasienter og andel mannlige pasienter som svarte
                      'definitivt ja' eller 'sannsynligvis ja' på spørsmål
                      om de ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging.
                      For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                "antall pasienter som svarte
                     'definitivt ja' og 'sannsynligvis ja' / antall opererte * 100", "<br/>",
                "antall kvinnelige pasienter som svarte
                     'definitivt ja' og 'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>",
                "antall mannlige pasienter som svarte
                     'definitivt ja' og 'sannsynligvis ja' / antall opererte menn * 100", "<br/>"
              ),
            "SRS22_spm21_3mnd" ~
              paste0(
                "Figuren viser fordelinger av andel pasienter (av begge kjønn),
                      andel kvinnelige pasienter og andel mannlige pasienter som svarte
                      'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om
                      de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging.
                      For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                "antall pasienter som svarte
                     'svært godt fornøyd' og 'ganske fornøyd' / antall opererte * 100", "<br/>",
                "antall kvinnelige pasienter som svarte
                     'svært godt fornøyd' og 'ganske fornøyd' / antall opererte kvinner * 100", "<br/>",
                "antall mannlige pasienter som svarte
                     'svært godt fornøyd' og 'ganske fornøyd' / antall opererte menn * 100", "<br/>"
              ),
            "PRE_MAIN_CURVE" ~
              paste0(
                "Figuren viser fordelinger av andel pasienter (av begge kjønn),
                      andel kvinnelige pasienter og andel mannlige pasienter som hadde
                      pre-operativ kurve over 70 grader på de ulike sykehusene.
                      For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                "antall pasienter med preoperativ kurve > 70 grader / antall opererte *100", "<br/>",
                "antall kvinnelige pasienter med preoperativ kurve
                     > 70 grader / antall opererte kvinner *100", "<br/>",
                "antall mannlige pasienter med preoperativ kurve
                     > 70 grader / antall opererte menn *100", "<br/>"
              ),
            "Liggetid" ~
              paste0(
                "Figuren viser fordelinger av andel pasienter (av begge kjønn),
                      andel kvinnelige pasienter og andel mannlige pasienter med
                      post-operativ liggetid på 7 eller flere dager på de ulike sykehusene.
                      For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                "antall pasienter med post-operativ liggetid 7 dager eller
                     mer / antall opererte *100", "<br/>",
                "antall kvinnelige pasienter med post-operativ liggetid 7 dager
                     eller mer / antall opererte kvinner *100", "<br/>",
                "antall mannlige pasienter med post-operativ liggetid 7 dager
                     eller mer / antall opererte menn *100", "<br/>"
              ),
            "Komplikasjoner_3mnd" ~
              paste0(
                "Figuren viser fordelinger av andel pasienter (av begge kjønn),
                      andel kvinnelige pasienter og andel mannlige pasienter som rapporterte
                      komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging.
                      Smerte som komplikasjon er tatt ut fra denne utregningen ettersom dette er
                      en komplikasjon som i høy grad er forventet etter operasjonen.
                      For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                "antall pasienter som rapporterer komplikasjoner (unntatt smerte) ved
                     3-6 mnds oppfølging / antall opererte *100", "<br/>",
                "antall kvinnelige pasienter som rapporterer komplikasjoner (unntatt smerte)
                     ved 3-6 mnds oppfølging / antall opererte kvinner *100", "<br/>",
                "antall mannlige pasienter som rapporterer komplikasjoner (unntatt smerte)
                     ved 3-6 mnds oppfølging / antall opererte menn *100", "<br/>"
              ),
            "CURRENT_SURGERY" ~
              paste0(
                "Figuren viser reoperasjonsrate for begge kjønn,
                      kvinnelige pasienter og mannlige pasienter.
                      For hvert sykehus er hver rate regnet ut slik:", "<br/>",
                "antall reoperasjoner / antall operasjoner *100", "<br/>",
                "antall reopererte kvinner / antall opererte kvinner *100", "<br/>",
                "antall reopererte menn / antall opererte menn *100", "<br/>"
              )
          )
      ),
      header = dplyr::recode_values(
        {{ var }},
        "SRS22_spm22_3mnd" ~ "SRS22 'Samme behandling på nytt?' 3-6 mnd:",
        "SRS22_spm21_3mnd" ~ "SRS22 'Fornøyd med behandlingen?' 3-6 mnd:",
        "PRE_MAIN_CURVE" ~ "Pre-operativ kurve:",
        "Liggetid" ~ "Liggetid:",
        "Komplikasjoner_3mnd" ~ "Komplikasjoner 3-6 mnd:",
        "CURRENT_SURGERY" ~ "Reoperasjonsrate:"
      )
    )

  return(data)
}
