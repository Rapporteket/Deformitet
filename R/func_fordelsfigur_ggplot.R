#' @title Make gg_plot for fordelingsfirgurer og -tabeller
#'
#' @param data data som kommer ut fra lagTabell()
#' @param gg_data gg-data som kommer fra prepVar()
#' @param data_var data som lagrer brukerens valg
#' @param visning valg av visning
#'
#' @examples
#' \donttest{
#' try(lag_ggplot_fordeling(data, gg_data, data_var, "egen enhet"))
#' }
#' @export

#### LAG ANDELSPLOT ####


################################################################################
################## LAG PLOT ----------------------------------------------------

lag_ggplot_fordeling <- function(data, gg_data, data_var, visning) {
  tabell <- data.frame(data)

  tabell <- tabell |>
    dplyr::rename(var = colnames(tabell[2]))


  # Del data i to tabeller - alle vs. valgt enhet

  if (visning == "hele landet") {
    tabell1 <- tabell |>
      dplyr::filter(.data$ShNavn != "Alle")

    tabell2 <- tabell |>
      dplyr::filter(.data$ShNavn == "Alle")

    tabell1 <- tabell1 |>
      dplyr::mutate(ShNavn = paste(tabell1[, 1], "n:", tabell1[, 4]))

    tabell2 <- tabell2 |>
      dplyr::mutate(ShNavn = paste(tabell2[, 1], "n:", tabell2[, 4]))
  } else {
    tabell <- tabell |>
      dplyr::rename(var = colnames(tabell[2])) |>
      dplyr::mutate(ShNavn = paste(tabell[, 1], "n:", tabell[, 4]))
  }


  # Lage plot

  fig_plot <- ggplot2::ggplot()

  if (visning == "hele landet") { # => med sammenligning
    fig_plot <- fig_plot +
      ggplot2::geom_col(
        data = tabell2,
        ggplot2::aes(
          x = var, y = .data$Prosent,
          color = .data$ShNavn
        ), fill = "#6CACE4"
      ) +
      ggplot2::geom_point(
        data = tabell1,
        ggplot2::aes(
          x = var, y = .data$Prosent,
          color = .data$ShNavn
        ), shape = 23,
        fill = "#003087", size = 2.5
      ) +

      ggplot2::scale_color_manual(
        values = # adding chosen colors
          c("#6CACE4", "#6CACE4", "#6CACE4", "#6CACE4", "#6CACE4", "#003087", "#003087")
      )
  }

  if (visning != "hele landet") { # => hvert sykehus og hele landet uten sammenligning
    fig_plot <- fig_plot +
      ggplot2::geom_col(data = tabell, ggplot2::aes(x = var, y = .data$Prosent, fill = .data$ShNavn), alpha = .9) +
      ggplot2::facet_wrap(~ShNavn) +

      ggplot2::scale_fill_manual(
        values = # adding chosen colors
          c("#6CACE4", "#ADDFB3", "#87189D", "black")
      )
  }


  # Change names of labels
  fig_plot <- fig_plot +
    ggplot2::xlab(gg_data$xlab) +
    ggplot2::ylab("Andel") +
    ggplot2::labs(
      title = gg_data$title,
      caption = paste0(
        "**Valgte variabler:**", "\n", data_var[1, ], ", ", data_var[2, ], "\n",
        data_var[3, ], "-", data_var[4, ], "\n",
        data_var[5, ], "-", data_var[6, ]
      )
    ) +


    ggplot2::theme_bw(base_size = 16) + # light theme

    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        color = "#87189D", # add caption
        face = "italic"
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16)
    )

  return(fig_plot)
}
