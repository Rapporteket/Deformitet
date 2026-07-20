####### MODULE FOR KVALITETSINDIKATORER ########################################
#' @title kvalitetsindikator UI
#' @export

module_kvalind_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput( # First select
          inputId = ns("kval_var"),
          label = "Velg Kvalitetsindikator:",
          choices = c(
            "SRS22 'Hvor fornøyd er du med behandlingen?' 3-6 mnd" = "SRS22_spm21_3mnd",
            "Pre-operativ kurve" = "PRE_MAIN_CURVE",
            "Liggetid" = "Liggetid",
            "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd",
            "Reoperasjonsrate" = "CURRENT_SURGERY"
          ),
          selected = "SRS22_spm22_3mnd"
        ),
        shiny::radioButtons( # second select
          inputId = ns("kjonn_var"),
          label = "Dele på kjønn?",
          choices = c(
            "kvinne" = "kvinne",
            "mann" = "mann",
            "begge" = "begge",
            "se alle fordelinger" = "nei"
          ),
          selected = "begge"
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("kval_var"), "'] == 'CURRENT_SURGERY'"),
          shiny::radioButtons( # third select, not visible when looking at rate of re-operations
            inputId = ns("type_op"),
            label = "Type operasjon",
            choices = c("Begge"),
            selected = "Begge"
          )
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("kval_var"), "'] != 'CURRENT_SURGERY'"),
          shiny::radioButtons( # third select, not visible when looking at rate of re-operations
            inputId = ns("type_op"),
            label = "Type operasjon",
            choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
            selected = "Begge"
          )
        ),
        shiny::sliderInput( # fourth select
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(10, 20),
          dragRange = TRUE
        ),
        shiny::dateRangeInput( # fifth select
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = "2023-01-02",
          end = Sys.Date(), # "2024-09-02",
          min = "2023-01-01",
          max = Sys.Date(), # "2025-09-02",
          format = "dd-mm-yyyy",
          separator = " - "
        )
      ),
      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel(
            "Figur",
            shiny::plotOutput(outputId = ns("kval_plot")),
            shiny::downloadButton(ns("download_fig"), "Last ned figur")
          ),
          bslib::nav_panel(
            "Tabell",
            DT::DTOutput(outputId = ns("kval_table")),
            shiny::downloadButton(ns("download_tbl"), "Last ned tabell")
          )
        ),
        shiny::h4("Slik er kvalitetsindikatoren regnet ut"),
        shiny::strong(
          shiny::textOutput(
            outputId = ns("text_header")
          )
        ),
        shiny::htmlOutput(
          outputId = ns("text_body")
        )
      )
    )
  )
}


#' @export

module_kvalind_server <- function(id, data, userRole, userUnitId, map_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Make gg-data for plot
      gg_data <- data.frame(title = "") # Jeg skal legge alt dette inn i en funksjon

      gg_data_reactive <- shiny::reactive({
        gg_data <- gg_data |>
          dplyr::mutate(
            title = dplyr::recode_values(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ "Pasienter som har svart at de er fornøyd med behandlingen (3-6 mnd)",
              "PRE_MAIN_CURVE" ~ "Pasienter med pre-operativ kurve over 70 grader",
              "Liggetid" ~ "Pasienter med 7 dager eller lengre liggetid",
              "Komplikasjoner_3mnd" ~ "Pasienter som har rapportert komplikasjoner (unntatt smerte) etter 3-6 måneder",
              "CURRENT_SURGERY" ~ "Andel pasienter som reopereres (reoperasjonsrate)"
            ),
            ylab = dplyr::recode_values(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ "'Svært godt fornøyd' og 'ganske fornøyd' med behandlingen (3-6 mnd)",
              "PRE_MAIN_CURVE" ~ "Pre-operativ kurve over 70 grader",
              "Liggetid" ~ "Liggetid 7 dager eller lengre",
              "Komplikasjoner_3mnd" ~ "Rapportert komplikasjoner 3-6 mnd (unntatt smerte)",
              "CURRENT_SURGERY" ~ "Reoperasjonsrate"
            ),
            ymin = dplyr::recode_values(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ 70,
              "PRE_MAIN_CURVE" ~ 0,
              "Liggetid" ~ 0,
              "Komplikasjoner_3mnd" ~ 0,
              "CURRENT_SURGERY" ~ 0
            ),
            ymax = dplyr::recode_values(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ 100,
              "PRE_MAIN_CURVE" ~ 10,
              "Liggetid" ~ 10,
              "Komplikasjoner_3mnd" ~ 10,
              "CURRENT_SURGERY" ~ 5
            )
          )
      })


      # Store reactive choices in data set for caption in ggplot
      my_data_reactive <- shiny::reactive({
        data.frame(c(
          input$kval_var,
          if (input$kjonn_var == "nei") {
            "begge"
          } else {
            input$kjonn_var
          },
          format(input$date[1], "%d%m%y"),
          format(input$date[2], "%d%m%y"),
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
        ))
      })


      ###### COUNT KVALTITETSINDIKATORER #############################################

      # Basic utvalg
      df_reactive <- shiny::reactive({
        utvalg_basic(
          data,
          userUnitId(),
          input$kjonn_var,
          input$type_op,
          input$date[1],
          input$date[2],
          input$alder_var[1],
          input$alder_var[2],
          "ikke_filtrer_reshId"
        )
      })


      kval_df_reactive <- shiny::reactive({
        count_kvalind(
          df_reactive(),
          input$kjonn_var,
          input$kval_var,
          userRole(),
          userUnitId(),
          map_data
        )
      })

      ###### PLOT ####################################################################

      kval_plot_reactive <- shiny::reactive({
        kval_plot(
          kval_df_reactive(),
          gg_data_reactive(),
          my_data_reactive(),
          input$kjonn_var
        )
      })

      output$kval_plot <- shiny::renderPlot({
        kval_plot_reactive()
      })

      ##### TABLE ####################################################################

      output$kval_table <- DT::renderDT({
        DT::datatable(kval_df_reactive(),
          class = "white-space:nowrap compact",
          colnames = c(
            "ShNavn",
            "Kjonn",
            "Antall",
            "Antall - kvalitetsindikator",
            "Andel - kvalitetsindikator"
          )
        )
      })

      ##### KVALITETSINDIKATORER over tid ############################################


      ##### NEDLASTING ###############################################################
      output$download_fig <- shiny::downloadHandler(
        filename = function() {
          paste("Figur_", input$kval_var, "_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(kval_plot_reactive())
          dev.off()
        }
      )

      output$download_tbl <- shiny::downloadHandler(
        filename = function() {
          paste("Tabell_", input$kval_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(kval_df_reactive(), file)
        }
      )

      #### RENDER TEXT ##############################################################
      output$text_header <- shiny::renderText({
        data <- explanation_kvalind(input$kjonn_var, input$kval_var)
        data$header
      })

      output$text_body <- shiny::renderText({
        data <- explanation_kvalind(input$kjonn_var, input$kval_var)
        data$text
      })
    }
  )
}
