#' @title Komplikasjonstyper
#' @export


# Making a function that returns a table of complications
# Returns a dataframe

kompl_data <- function(RegData, var, var_kjonn, time1, time2, alder1, alder2, type_op, map_data) {
  # Make data set smaller and more manageageble
  if (var == "Komplikasjonstype") {
    kompl <- RegData |>
      dplyr::mutate(
        Blødning =
          dplyr::recode_values(.data$COMPLICATIONS_BLEEDING, 1 ~ "blødning", 0 ~ "0"),
        UVI =
          dplyr::recode_values(.data$COMPLICATIONS_UTI, 1 ~ "uvi", 0 ~ "0"),
        Lunge =
          dplyr::recode_values(.data$COMPLICATIONS_PNEUMONIA, 1 ~ "lunge", 0 ~ "0"),
        DVT =
          dplyr::recode_values(.data$COMPLICATIONS_DVT, 1 ~ "DVT", 0 ~ "0"),
        Emboli =
          dplyr::recode_values(.data$COMPLICATIONS_PE, 1 ~ "emboli", 0 ~ "0"),
        Inf_over =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_WOUND, 1 ~ "infeks. overfladisk", 0 ~ "0"),
        Inf_dyp =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_DEEP, 1 ~ "infeks. dyp", 0 ~ "0"),
        Inf_reop =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_REOP, 1 ~ "infeks. reop", 0 ~ "0"),
        Lam =
          dplyr::recode_values(.data$COMPLICATIONS_NUMBNESS, 1 ~ "lam", 0 ~ "0"),
        Smerte =
          dplyr::recode_values(.data$COMPLICATIONS_PAIN, 1 ~ "smerte", 0 ~ "0"),
        Annet =
          dplyr::recode_values(.data$COMPLICATIONS_OTHER, 1 ~ "annet", 0 ~ "0")
      )
  }
  if (var == "Komplikasjonstype_12mnd") {
    kompl <- RegData |>
      dplyr::mutate(
        Blødning =
          dplyr::recode_values(.data$COMPLICATIONS_BLEEDING_patient12mths, 1 ~ "blødning", 0 ~ "0"),
        UVI =
          dplyr::recode_values(.data$COMPLICATIONS_UTI_patient12mths, 1 ~ "uvi", 0 ~ "0"),
        Lunge =
          dplyr::recode_values(.data$COMPLICATIONS_PNEUMONIA_patient12mths, 1 ~ "lunge", 0 ~ "0"),
        DVT =
          dplyr::recode_values(.data$COMPLICATIONS_DVT_patient12mths, 1 ~ "DVT", 0 ~ "0"),
        Emboli =
          dplyr::recode_values(.data$COMPLICATIONS_PE_patient12mths, 1 ~ "emboli", 0 ~ "0"),
        Inf_over =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_WOUND_patient12mths, 1 ~ "infeks. overfladisk", 0 ~ "0"),
        Inf_dyp =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_DEEP_patient12mths, 1 ~ "infeks. dyp", 0 ~ "0"),
        Inf_reop =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_REOP_patient12mths, 1 ~ "infeks. reop", 0 ~ "0"),
        Lam =
          dplyr::recode_values(.data$COMPLICATIONS_NUMBNESS_patient12mths, 1 ~ "lam", 0 ~ "0"),
        Smerte =
          dplyr::recode_values(.data$COMPLICATIONS_PAIN_patient12mths, 1 ~ "smerte", 0 ~ "0"),
        Annet =
          dplyr::recode_values(.data$COMPLICATIONS_OTHER_patient12mths, 1 ~ "annet", 0 ~ "0")
      )
  }

  if (var == "Komplikasjonstype_60mnd") {
    kompl <- RegData |>
      dplyr::mutate(
        Blødning =
          dplyr::recode_values(.data$COMPLICATIONS_BLEEDING_patient60mths, 1 ~ "blødning", 0 ~ "0"),
        UVI =
          dplyr::recode_values(.data$COMPLICATIONS_UTI_patient60mths, 1 ~ "uvi", 0 ~ "0"),
        Lunge =
          dplyr::recode_values(.data$COMPLICATIONS_PNEUMONIA_patient60mths, 1 ~ "lunge", 0 ~ "0"),
        DVT =
          dplyr::recode_values(.data$COMPLICATIONS_DVT_patient60mths, 1 ~ "DVT", 0 ~ "0"),
        Emboli =
          dplyr::recode_values(.data$COMPLICATIONS_PE_patient60mths, 1 ~ "emboli", 0 ~ "0"),
        Inf_over =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_WOUND_patient60mths, 1 ~ "infeks. overfladisk", 0 ~ "0"),
        Inf_dyp =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_DEEP_patient60mths, 1 ~ "infeks. dyp", 0 ~ "0"),
        Inf_reop =
          dplyr::recode_values(.data$COMPLICATIONS_INFECTION_REOP_patient60mths, 1 ~ "infeks. reop", 0 ~ "0"),
        Lam =
          dplyr::recode_values(.data$COMPLICATIONS_NUMBNESS_patient60mths, 1 ~ "lam", 0 ~ "0"),
        Smerte =
          dplyr::recode_values(.data$COMPLICATIONS_PAIN_patient12mths, 1 ~ "smerte", 0 ~ "0"),
        Annet =
          dplyr::recode_values(.data$COMPLICATIONS_OTHER_patient12mths, 1 ~ "annet", 0 ~ "0")
      )
  }


  # Filter to match user choices:

  ### by gender:

  kompl <- kompl |>
    dplyr::filter(.data$Kjonn == dplyr::case_when(
      {{ var_kjonn }} == "kvinne" ~ "kvinne",
      {{ var_kjonn }} == "mann" ~ "mann",
      {{ var_kjonn }} != "kvinne" | {{ var_kjonn }} != "mann" ~ Kjonn
    )) |>
    dplyr::mutate(Kjonn = dplyr::case_when(
      {{ var_kjonn }} == "kvinne" ~ "kvinne",
      {{ var_kjonn }} == "mann" ~ "mann",
      {{ var_kjonn }} != "kvinne" | {{ var_kjonn }} != "mann" ~ "begge"
    ))

  ### by operation type:

  kompl <- kompl |>
    dplyr::filter(dplyr::case_when(
      {{ type_op }} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
      {{ type_op }} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
      {{ type_op }} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)
    ))

  ### by surgery date:

  kompl <- kompl |>
    dplyr::filter(dplyr::between(
      .data$SURGERY_DATE,
      as.Date({{ time1 }}),
      as.Date({{ time2 }})
    ))

  ### by age:

  # Using column "Alder_num" in which alder is given as an integer

  kompl <- kompl |>
    dplyr::filter(dplyr::between(
      .data$Alder_num,
      {{ alder1 }},
      {{ alder2 }}
    ))

  kompl <- kompl |>
    dplyr::select(
      "PasientID", "ShNavn", "Kjonn", "CURRENT_SURGERY", "Blødning", "UVI", "Lunge", "DVT",
      "Emboli", "Inf_over", "Inf_dyp", "Inf_reop", "Lam", "Smerte", "Annet"
    )


  ################ TIDYING AND COUNTING ##########################################
  #------------------------------------------------------------------------------#

  # # pivot longer
  kompl <- kompl |>
    tidyr::pivot_longer(
      !c("PasientID", "ShNavn", "Kjonn", "CURRENT_SURGERY"),
      names_to = "type", values_to = "Komplikasjonstype"
    ) |>
    dplyr::select(-"type")

  # # remove "unknown" and nas
  kompl <- kompl |>
    dplyr::mutate(Komplikasjonstype = tidyr::replace_na(.data$Komplikasjonstype, "ukjent")) |>
    dplyr::filter(.data$Komplikasjonstype != "ukjent")

  # remove columns with no complications
  kompl <- kompl |>
    dplyr::filter(.data$Komplikasjonstype != "0")

  # # make data frames of tables
  kompl_df <- data.frame(table(kompl$ShNavn, kompl$Komplikasjonstype, kompl$Kjonn, kompl$CURRENT_SURGERY))

  # # rename columns
  kompl_df <- kompl_df |>
    dplyr::rename(
      ShNavn = .data$Var1,
      Komplikasjonstype = .data$Var2,
      Kjonn = .data$Var3,
      antall = .data$Freq,
      Operasjon = .data$Var4
    )

  # Add reshId based on hospital name
  kompl_df <- dplyr::left_join(kompl_df, map_data, dplyr::join_by(.data$ShNavn == .data$orgname))
}

# nolint start
# test
## g <- kompl_data(RegData, "Komplikasjonstype", "ee", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", map_db_resh)
# nolint end

#' @title Komplikasjonstyper - tabell
#' @export

###### MAKE TABLE WITH PERCENTAGES ########

# data 1 => prepvar-data (laget av prepVar()-funksjonen)
# data 2 => komplikasjonstypedata (laget av kompl_data()-funksjonen)

kompl_tbl <- function(data1, data2, var_kjonn, type_view, reshId) {
  data_based_on_ui_choices <- data1 |>
    dplyr::mutate(
      Kjonn = dplyr::case_when(
        {{ var_kjonn }} != "mann" |
          {{ var_kjonn }} != "kvinne" ~ "begge"
      )
    )

  data_based_on_ui_choices <- data_based_on_ui_choices |>
    dplyr::group_by(.data$ShNavn, .data$Kjonn) |>
    dplyr::tally()


  kompl_tbl <- dplyr::left_join(data_based_on_ui_choices, data2)

  kompl_tbl <- kompl_tbl |>
    dplyr::mutate(andel = round(.data$antall / .data$n * 100, 2))

  # Filtrering basert på "type_view":


  if (type_view == "hver enhet") {
    return(kompl_tbl)
  }

  if (type_view == "egen enhet") {
    kompl_tbl_hosp <- kompl_tbl |>
      dplyr::filter(.data$UnitId == {{ reshId }})

    return(kompl_tbl_hosp)
  }

  if (type_view == "hele landet, uten sammenligning") {
    kompl_tbl_all <- kompl_tbl |>
      dplyr::group_by(.data$Komplikasjonstype) |>
      dplyr::mutate(
        Antall = sum(.data$antall),
        n = sum(.data$n)
      ) |>
      dplyr::select("Komplikasjonstype", "Kjonn", "Antall", "n") |>
      dplyr::mutate(
        ShNavn = "Alle",
        andel = round(.data$Antall / .data$n * 100, 2)
      ) |>
      dplyr::distinct()

    return(kompl_tbl_all)
  } else {
    return(kompl_tbl)
  } # => hele landet med sammenligning
}

# nolint start
## test
## h <- kompl_tbl(rr, g, "fff", "hver enhet", "103240") #, "Komplikasjoner_3mnd", 111961, "begge", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", "hele landet, uten sammenligning")
# nolint end


#' @title Komplikasjonstyper - figur
#' @export

kompl_plot <- function(data, var, data_caption) {
  # Making labels
  if (var == "Komplikasjonstype") {
    xlab <- "Komplikasjonstype oppgitt ved 3-6 mndrs oppfølging"
  }

  if (var == "Komplikasjonstype_12mnd") {
    xlab <- "Komplikasjonstype oppgitt ved 12 mndrs oppfølging"
  }

  if (var == "Komplikasjonstype_60mnd") {
    xlab <- "Komplikasjonstype oppgitt ved 5 års oppfølging"
  }

  # Making plot

  kompl_plot <- ggplot2::ggplot()


  kompl_plot <- kompl_plot +
    ggplot2::geom_col(data = data, ggplot2::aes(x = .data$Komplikasjonstype, y = .data$andel), fill = "#6CACE4") +
    ggplot2::facet_wrap(~ShNavn) +


    ggplot2::theme_bw(base_size = 16) + # light theme

    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        color = "#87189D", # add caption
        face = "italic"
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 14)
    )


  # Change names of labels

  kompl_plot <- kompl_plot +

    ggplot2::xlab(xlab) +
    ggplot2::ylab("Andel (%)") +
    ggplot2::labs(
      caption = paste0(
        "**Valgte variabler:**", "\n",
        data_caption[1, ], ", ", data_caption[2, ], "\n",
        data_caption[3, ], "-", data_caption[4, ], "\n",
        data_caption[5, ], "-", data_caption[6, ]
      )
    ) +


    ggplot2::theme_bw(base_size = 16) + # light theme

    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        color = "#87189D", # add caption
        face = "italic"
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 14)
    )


  return(kompl_plot)
}

# nolint start
# test
# nolint end
