#'@title Ui sammenligningsmodul
#'
#' @export

module_spc_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        selectInput( # First select
          inputId = ns("var"),
          label = "Velg variabel:",
          choices = c("Blodtap (ml)" = "PER_BLOOD_LOSS_VALUE",
                      "Pre-operativ kurve" = "PRE_MAIN_CURVE",
                      "Liggetid" = "BED_DAYS_TOTALT"),
          selected = "PER_BLOOD_LOSS_VALUE"),


        radioButtons(
          inputId = ns("period"),
          label = "Velg tidsramme:",
          choices = c("år" = "year",
                      "kvartal" = "quarter",
                      "måned" = "month",
                      "uke" = "week",
                      "dag" = "day"),
          selected = "year"),


        dateRangeInput( # second select
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = Sys.Date() - 150, #start dato wil være 150 dager før systemdato
          end = Sys.Date(), # end dato wil være dagens dato
          min = "2023-01-01",
          max = Sys.Date(),
          format = "mm/dd/yy",
          separator = " - "
        ),

        selectInput( # third select
          inputId = ns("unit"),
          label = "Enhet",
          choices = c("Haukeland" = 103240,
                      "Rikshospitalet" = 102467,
                      "St.Olav" = 111961,
                      "alle" = "alle"),
          selected = "Haukeland"
        )
      ),

      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("SPC-figur",
                           shiny::plotOutput(outputId = ns("spc_plot")),
                           shiny::downloadButton(ns("download_spc_plot"), "Last ned figur")),
          bslib::nav_panel("Tabell",
                           DT::DTOutput(outputId = ns("spc_table")))
        )
      )
    )
  )
}

#'@title Server sammenligningsmodul
#'
#'@export

module_spc_server <- function (id) {
  moduleServer(
    id,
    function(input, output, session){

      ### Read in data:
      RegData <- alleRegData()

      #### Clean and tidy data:

      RegData <- preprosData(RegData)

      # RegData <- readRDS("../dev/fake_data_deformitet.rds")
      #
      # RegData <- preprosData(RegData)


      ##### MAKE BASIC UTVALG ##################################################

      data_reactive <- reactive({
        x <- utvalg_basic(RegData,
                                      unit = input$unit,
                                      gender = "begge",
                                      type_op = "Begge",
                                      input$date[1],
                                      input$date[2],
                                      alder1 = 0,
                                      alder2 = 100,
                                      "filtrer_reshId")
      })

      #### MAKE SPC DATA FRAME (SORTED BY TIME PERIOD) #########################

      data_spc_reactive <- reactive({
        prepros_SPC(data_reactive(), input$var, input$period)
      })

      #### MAKE SPC PLOT #######################################################

      direction_reactive <- reactive({
        if (input$var == "PRE_MAIN_CURVE") {
          direction <- "increase"
        } else {
          direction <- "decrease"
        }
      })


      spc_plot_reactive <- reactive({
        spc_function(data_spc_reactive(),
                                 value,
                                 tidsperiode,
                                 direction_reactive(),
                                 input$var)
      })


      output$spc_plot <- renderPlot({
        spc_plot_reactive()
      })

      output$spc_table <- DT::renderDT({datatable(data_spc_reactive(),
                                                  extensions = 'Buttons',
                                                  options = list(
                                                    dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel','pdf')),
                                                  class = 'white-space:nowrap compact')
      })
    })
}
