#' @title Module gjennomsnitt
#' @export

module_gjennomsnitt_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      # Inputs: select variables to plot
      shiny::sidebarPanel(
        width = 3,

        # Select variable for x-axis
        shiny::selectInput( # First select
          inputId = ns("x_var"),
          label = "Variabel:",
          choices = c(
            "Helsetilstand" = "Helsetilstand",
            "Helsetilstand 3-6 mnd" = "Helsetilstand_3mnd",
            "Helsetilstand 12 mnd" = "Helsetilstand_12mnd",
            "SRS22 'Samme behandling på nytt?' 3-6 mnd" = "SRS22_spm22_3mnd",
            "SRS22 'Samme behandling på nytt?' 12 mnd" = "SRS22_spm22_12mnd",
            "SRS22 'Fornøyd med resultatet?' 3-6 mnd" = "SRS22_spm21_3mnd",
            "SRS22 'Fornøyd med resultatet?' 12 mdn" = "SRS22_spm21_12mnd",
            "BMI-kategori" = "BMI_kategori",
            "Alder" = "Alder",
            "Pre-operativ kurve" = "Kurve_preGr",
            "Post-operativ kurve" = "Kurve_post",
            "Prosent korreksjon kurve" = "Diff_prosent_kurve",
            "Liggetid" = "Liggetid",
            "Knvitid" = "Knivtid",
            "Blodtap" = "Blodtap_100",
            "SRS22 totalscore preoperativt" = "SRS22_total",
            "SRS22 totalscore 3-6 mnd" = "SRS22_total_3mnd",
            "SRS22 totalscore 12 mnd" = "SRS22_total_12mnd",
            "SRS22 funksjon preoperativt" = "SRS22_funksjon",
            "SRS22 funksjon, 3-6 mnd" = "SRS22_funksjon_3mnd",
            "SRS22 funksjon, 12 mnd" = "SRS22_funksjon_12mnd",
            "SRS22 smerte preoperativt" = "SRS22_smerte",
            "SRS22 smerte, 3-6 mnd" = "SRS22_smerte_3mnd",
            "SRS22 smerte, 12 mnd" = "SRS22_smerte_12mnd",
            "SRS22 selvbilde preoperativt" = "SRS22_selvbilde",
            "SRS22 selvbilde, 3-6 mnd" = "SRS22_selvbilde_3mnd",
            "SRS22 selvbilde, 12 mnd" = "SRS22_selvbilde_12mnd",
            "SRS22 mental helse preoperativt" = "SRS22_mhelse",
            "SRS22 mental helse, 3-6 mnd" = "SRS22_mhelse_3mnd",
            "SRS22 mental helse, 12 mnd" = "SRS22_mhelse_12mnd",
            "SRS22 tilfredshet, 3-6 mnd" = "SRS22_fornoyd_3mnd",
            "SRS22 tilfredshet, 12 mnd" = "SRS22_fornoyd_12mnd"
            # Variabler som kan vurderes lagt til:
            # nolint start
            # "Helsetilstand 5 år" = "Helsetilstand_60mnd",
            # "SRS22 'Samme behandling på nytt?' 5 år" = "SRS22_spm22_60mnd",
            # "SRS22 tilfredshet, 5 år" = "SRS22_fornoyd_60mnd",
            # "SRS22 'Fornøyd med resultatet?' 5 år" = "SRS22_spm21_60mnd",
            # "SRS22 totalscore 5 år" = "SRS22_total_60mnd",
            # "SRS22 funksjon, 5 år" = "SRS22_funksjon_60mnd",
            # "SRS22 smerte, 5 år" = "SRS22_smerte_60mnd",
            # "SRS22 selvbilde, 5 år" = "SRS22_selvbilde_60mnd",
            # "SRS22 mental helse, 5 år" = "SRS22_mhelse_60mnd",
            # "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd",
            # "Komplikasjoner, 12 mnd" = "Komplikasjoner_12mnd",
            # "Komplikasjoner, 60 mnd" = "Komplikasjoner_60mnd",
            # "Komplikasjonstyper, 3-6 mnd" = "Komplikasjonstype",
            # "Komplikasjonstyper, 12 mnd" = "Komplikasjonstype_12mnd"
            # "Komplikasjonstyper, 60 mnd" = "Komplikasjonstype_60mnd"
            # nolint end
          ),
          selected = "BMI_kategori"
        ),
        shinyjs::hidden(shiny::uiOutput(outputId = ns("view_type"))), # second select

        shiny::radioButtons( # second select
          inputId = ns("tidsenhet"),
          label = "Tidsenhet",
          choices = c(
            "År" = "aar",
            "Kvartal" = "kvartal"
          ),
          selected = "kvartal"
        ),
        shiny::dateRangeInput( # third select
          inputId = ns("dato"),
          label = "Tidsintervall:",
          start = "2023-01-02",
          end = Sys.Date(), # "2025-09-02",
          min = "2023-01-01",
          max = Sys.Date(), # "2025-09-02",
          format = "dd-mm-yyyy",
          separator = " - "
        ),
        shiny::sliderInput( # fourth select
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(10, 20),
          dragRange = TRUE
        ),
        shiny::selectInput( # fifth select
          inputId = ns("kjonn_var"),
          label = "Utvalg basert på kjønn",
          choices = c("begge", "mann", "kvinne"),
          selected = "begge"
        ),
        shiny::radioButtons( # sixth select
          inputId = ns("type_op"),
          label = "Type operasjon",
          choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
          selected = "Primæroperasjon"
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = ns("tab"),
          shiny::tabPanel("Figur",
            value = "fig",
            shiny::textOutput(outputId = ns("my_text")),
            shiny::plotOutput(outputId = ns("my_plot"), height = "auto"),
            shiny::downloadButton(
              ns("download_gjennomsnittsfig"),
              "Last ned figur"
            )
          ),
          shiny::tabPanel("Tabell",
            value = "tab",
            shiny::tableOutput(outputId = ns("table")),
            shiny::downloadButton(
              ns("download_gjennomsnittstbl"),
              "Last ned tabell"
            )
          )
        )
      )
    )
  )
}


#' @title Server gjennomsnitt
#'
#' @export

module_gjennomsnitt_server <- function(id, userRole, userUnitId, data, map_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$view_type <- shiny::renderUI({
        ns <- session$ns
        if (userRole() == "SC") {
          shiny::radioButtons( # seventh select
            inputId = ns("visningstype"),
            label = "Vis rapport for:",
            choices = c(
              "Hele landet" = "hele landet",
              "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
              "Hver enhet" = "hver enhet",
              "Egen enhet" = "egen enhet"
            ),
            selected = "hele landet"
          )
        } else {
          shiny::radioButtons( # seventh select
            inputId = ns("visningstype"),
            label = "Vis rapport for:",
            choices = c(
              "Hele landet" = "hele landet",
              "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
              "Egen enhet" = "egen enhet"
            ),
            selected = "hele landet"
          )
        }
      })

      map_var_reactive <- shiny::reactive({
        mapping_navn(data, input$x_var)
      })

      prepvar_reactive <- shiny::reactive({
        prepVar(
          data,
          map_var_reactive(),
          input$kjonn_var,
          input$dato[1],
          input$dato[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op,
          "over_tid"
        )
      })

      # Make data frame where UI choices are stored

      my_data_reactive <- shiny::reactive({
        x <- format(input$dato, "%d/%m/%y")
        data.frame(c(
          input$x_var,
          input$kjonn_var,
          x[1],
          x[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
        ))
      })

      # prepVar() returns a list
      # Unpack part 1 of list: data

      data_reactive <- shiny::reactive({
        data <- data.frame(prepvar_reactive()[1])
      })

      # Unpack part 2 of list: gg-data

      gg_data_reactive <- shiny::reactive({
        data.frame(prepvar_reactive()[2])
      })


      ######## AGGREGATE DATA-------------------------------------------------------
      # Aggregate data in table format

      table_data <- shiny::reactive({
        table <- tabell_gjen_tid(
          data_reactive(),
          map_var_reactive(),
          map_data,
          input$tidsenhet,
          input$visningstype,
          userUnitId()
        )
        table <- table |>
          dplyr::rename("gjennomsnitt" = .data$gjen)
      })


      ########### DISPLAY DATA-------------------------------------------------------
      ### TABLE

      output$table <- shiny::renderTable({
        table_data()
      })

      ### FIGURE ###

      my_plot <- shiny::reactive({
        over_tid_plot(
          table_data(),
          input$visningstype,
          gg_data_reactive(),
          map_var_reactive(),
          input$tidsenhet,
          my_data_reactive()
        )
      })

      check <- shiny::reactive({
        sjekk_antall(
          data,
          table_data(),
          input$dato[1],
          input$dato[2],
          input$tidsenhet
        )
      })

      output$my_text <- shiny::renderText({
        if (check() == "Drop") {
          "For få verdier for visse variabler. Gjør nytt utvalg. Se tabell i neste fane."
        }
      })

      output$my_plot <- shiny::renderPlot(
        {
          if (check() == "Keep") {
            my_plot()
          }
        },
        width = 800,
        height = 600
      )

      ##### NEDLASTING ###############################################################
      output$download_gjennomsnittsfig <- shiny::downloadHandler(
        filename = function() {
          paste("Figur_", input$x_var, "_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(my_plot())
          dev.off()
        }
      )

      output$download_gjennomsnittstbl <- shiny::downloadHandler(
        filename = function() {
          paste("Tabell_", input$x_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(table(), file)
        }
      )
    }
  )
}
