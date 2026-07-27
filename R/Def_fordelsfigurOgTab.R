
#' Beregne antall og andeler til fordelingsfigur/tabell
#'
#' @param RegData Dataramme med preprossesserte datta
#' @param flerevar fordeling av en eller flere variabler (0 el 1)
#' @param variabler hvilke variabler som er med
#' @param ind indeks for hvilke registreringer som er med i $Hovedgruppa og
#' hvilke som er med i resten $Rest
#'
#' @return Beregnede andeler, samt info om grupper
#' @export
#'
lagFordelingstabell <- function(RegData, flerevar, variabler=NULL, ind){
  #RegData(inkl. VariabelGr)
  #Denne kan endres slik at Hoved og Rest
  #(eller hvilken som helst gruppe kjøres for seg...)
  #Evt at kjører for grupperingsvariabel som kan være enkeltsykehus,
  #to eller flere grupper

  medSml <- ifelse(sum(ind$Rest)==0, 0, 1)
  AggVerdier <- list(Hoved = 0, Rest = 0)
  N <- list(Hoved = 0, Rest = 0)
  AntHend <- list(Hoved = switch(as.character(flerevar),
                                 '0' = table(RegData$VariabelGr[ind$Hoved]),
                                 '1' = colSums(sapply(RegData[ind$Hoved ,variabler], as.numeric), na.rm = T)),
                  Rest = 0)
  N$Hoved <- switch(as.character(flerevar),
                    '0' = sum(AntHend$Hoved),
                    '1' = length(ind$Hoved))
  AggVerdier$Hoved <- 100*AntHend$Hoved/N$Hoved

  if (medSml==1) {
    AntHend$Rest <- switch(as.character(flerevar),
                           '0' = table(RegData$VariabelGr[ind$Rest]),
                           '1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm = T))
    N$Rest <- switch(as.character(flerevar),
                     '0' = sum(AntHend$Rest),	#length(ind$Rest)- Kan inneholde NA
                     '1' = length(ind$Rest))
    AggVerdier$Rest <- 100*AntHend$Rest/N$Rest
  }

  Nfig <- N
  #Nfig skal være forskjellig fra N når det er en sammensatt fordelingssfigur (variabler =1)
  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=Nfig,
                       Nvar=AntHend,
                       Ngr=Nfig,
                       #utvalgTxt=utvalgTxt,
                       #fargepalett=RyggUtvalg$fargepalett,
                       medSml=medSml)

  return(FigDataParam)
}




#' Title
#'
#' @param RegData preprosessert dataramme
#' @param outfile utfil for figur
#' @inheritParams varTilrettelegg
#' @inheritParams utvalgEnh
#'
#' @return Fordelingsfigur
#' @export
#'
DefFordeling  <- function(RegData, valgtVar='alder',
                          datoFra = '2023-01-01', datoTil = Sys.Date(),
                          aar = 0, minald = 0, maxald = 110, erMann = '',
                          op_type = 9,
                           outfile = '', reshID = 0, enhetsUtvalg = 0){

#Mottar preprosesserte data
# valgtVar <- 'alder'
# enhetsUtvalg <- 1
# reshID <- 102467
   VarSpes <- varTilrettelegg(RegData = RegData, valgtVar = valgtVar, figurtype = 'fordeling')

  Utvalg <- utvalgEnh(RegData = VarSpes$RegData, reshID = reshID,
                      # datoFra = datoFra, datoTil = datoTil,
                      #         minald = minald, maxald = maxald, erMann = erMann, aar = aar,
                              enhetsUtvalg = enhetsUtvalg)

  tabFordeling <- lagFordelingstabell(Utvalg$RegData, flerevar = VarSpes$flerevar,
                                      variabler = VarSpes$variable, ind = Utvalg$ind)
ind <- as.list(ind)
    rapFigurer::FigFordeling(
      AggVerdier = tabFordeling$AggVerdier,
      tittel=VarSpes$tittel,
      hovedgrTxt=Utvalg$hovedgrTxt, smltxt = Utvalg$smltxt,
      N=tabFordeling$N, Nfig=tabFordeling$Nvar,
      retn=VarSpes$retn,
      utvalgTxt=Utvalg$utvalgTxt,
      grtxt = levels(VarSpes$RegData$VariabelGr), medSml=tabFordeling$medSml,
      subtxt=VarSpes$subtxt, outfile="")
}












#' @title Make gg_plot for fordelingsfirgurer og -tabeller
#'
#' @param data data som kommer ut fra lagFordelingstabell()
#' @param gg_data gg-data som kommer fra prepVar()
#' @param data_var data som lagrer brukerens valg
#' @param visning valg av visning "Hele landet" eller ikke
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
      dplyr::filter(ShNavn != "Alle")

    tabell2 <- tabell |>
      dplyr::filter(ShNavn == "Alle")

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
          x = var, y = Prosent,
          color = ShNavn
        ), fill = "#6CACE4"
      ) +
      ggplot2::geom_point(
        data = tabell1,
        ggplot2::aes(
          x = var, y = Prosent,
          color = ShNavn
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
      ggplot2::geom_col(data = tabell, ggplot2::aes(x = var, y = Prosent, fill = ShNavn), alpha = .9) +
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
