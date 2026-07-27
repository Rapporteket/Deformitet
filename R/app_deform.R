# Resultattjeneste for Deformitet

#' Brukergrensesnitt (ui) til appen
#'
#' @return Brukergrensesnittet (ui) til appen
#' @export

ui_deform <- function() {
  shiny::tagList(
    shiny::navbarPage(
      ###### Graphics ######
      title = rapbase::regTitle("Rapporteket for deformitet"),
      windowTitle = "Rapporteket for deformitet",
      theme = rapbase::rapTheme(version = 5),
      id = "tabs",

      ###############################
      ######## TAB user info ########
      ###############################

      ##### Startside (info about Rapporteket and the registry) #####

      shiny::tabPanel(
        title = "Startside",
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput(
            "veiledning",
            inline = TRUE
          ),
          rapbase::navbarWidgetInput(
            "deformitetNavbarWidget",
            selectOrganization = TRUE
          )
        )
      ),


      ###########################################
      ##### TAB: Fordelingsfigur og -tabell #####
      ###########################################

      shiny::tabPanel(
        title = "Fordelingsfigur og -tabell",
        module_fordeling_ui("fordeling")
      ) #,


      #####################################
      ##### TAB: Kvalitetsindikatorer #####
      #####################################

      # shiny::tabPanel(
      #   title = "Kvalitetsindikatorer",
      #   module_kvalind_ui("kval1")
      # ),


      ###############################
      ##### TAB: Sammenligning  #####
      ###############################

      # shiny::tabPanel(
      #   title = "Sammenligning",
      #   module_sammenligning_ui("sam1")
      # ),


      ################################
      ##### TAB: Registreringer  #####
      ################################

      # shiny::tabPanel(
      #   title = "Registreringer",
      #   module_registreringer_ui("reg1")
      # ),


      ########################################
      ##### TAB: Nestlasting av datadump #####
      ########################################

      # shiny::tabPanel( # third tab
      #   title = "Registeradm",
      #   shiny::fluidPage(
      #     module_datadump_ui(
      #       id = "mod_datadump"
      #     )
      #   )
      # ),
    ) # navbarPage
  ) # tagList
}

#' Server logic for the deformitet app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

server_deform <- function(input, output, session) {
  ######### DATA TIDYING #########
  ### Read in data:
  raw_regdata <- alleRegData()

  #### Clean and tidy data:

  RegData <- preprosData(raw_regdata)

  ######## USER INFO ########

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  map_db_resh <- data.frame( # map_avdeling <-
    UnitId = unique(RegData$CENTREID),
    orgname = RegData$ShNavn[match(
      unique(RegData$CENTREID),
      RegData$CENTREID
    )]
  )

  user <- rapbase::navbarWidgetServer2(
    id = "deformitetNavbarWidget",
    orgName = "deformitet",
    caller = "deformitet",
    map_orgname = shiny::req(map_db_resh)
  )


  ##########################
  ##### TAB: Startside #####
  ##########################

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })


  ###########################################
  ##### TAB: Fordelingsfigur- og tabell #####
  ###########################################

  module_fordeling_server(
    "fordeling",
    regdata = RegData,
    raw_data = raw_regdata,
    userRole = user$role,
    userUnitId = user$org,
    map_data = map_db_resh
  )

  #####################################
  ##### TAB: Kvalitetsindikatorer #####
  #####################################

  # module_kvalind_server(
  #   "kval1",
  #   data = RegData,
  #   map_data = map_db_resh,
  #   userRole = user$role,
  #   userUnitId = user$org
  # )


  ##############################
  ##### TAB: Sammenligning #####
  ##############################

  # module_sammenligning_server(
  #   "sam1",
  #   data = RegData,
  #   userRole = user$role,
  #   userUnitId = user$org
  # )


  ################################
  ##### TAB: Registreringer ######
  ################################

  # module_registreringer_server(
  #   "reg1",
  #   data = RegData,
  #   userRole = user$role,
  #   userUnitId = user$org()
  # )


  #########################################
  ##### TAB: Nestlasting av datadump ######
  #########################################

  # module_datadump_server(
  #   "mod_datadump",
  #   data = RegData,
  #   userRole = user$role,
  #   userUnitId = user$org()
  # )

  ##################################
  ###### TAB: Exporting data #######
  ##################################

  shiny::observeEvent(
    shiny::req(user$role()),
    {
      if (user$role() != "SC") {
        shiny::removeTab("tabs", target = "Eksport")
      } else {
        shiny::insertTab(
          "tabs",
          shiny::tabPanel(
            title = "Eksport",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::exportUCInput("deformitetExport")
              ),
              shiny::mainPanel(
                rapbase::exportGuideUI("deformitetExportGuide")
              )
            )
          ),
          target = "Registeradm",
          position = "after"
        )
      }
    }
  )

  # Brukerkontroller

  rapbase::exportUCServer("deformitetExport", "deformitet")

  # Veiledning

  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")
}

#' Run the Deformitet Shiny Application
#'
#' @param browser Open app in browser window
#' @param logAsJson Log in json-format
#'
#' @return An object representeing the Deformitet app
#' @export

kjorDeformApp <- function(browser = FALSE, logAsJson = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  if (browser) {
    options(shiny.launch.browser = TRUE)
  }
  shiny::shinyApp(
    ui = ui_deform,
    server = server_deform
  )
}
