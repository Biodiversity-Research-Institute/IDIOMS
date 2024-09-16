#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# created: 24 Aug 2020
# A. Gilbert
# Biodiversity Research Institute
# 276 Canco Rd
# Portland, ME 04103

# modified:
# 0.41 - 01 Dec 2020: Slight mods to add conditional coloring, fix some issues
# 0.50 - 02 Dec 2020: Create more streamlined menu panel. Fixed issues with clear study area.
# Removed default study area and added button to create a default
# Reorganized the menuitems and added Motus towers and estimated coverage
# Added layers to map
# 0.51 - 07 Dec 2020: fixed issues with study design creation error and added BOEM lease and WPA areas as WFS
# 0.52 - 08 Dec 2020: Added frequency selection and antenna selection which changes antenna parameter
# 0.53 - 11 Jan 2021: Minor bug fixes - stn ID correction and pyschedelic colors
# 0.60 - 07 Feb 2022: Add 434 MHz detection patterns
#   25 May 2022: Modify 434 function to deal with horizon and free space path loss to limit actual detection due to FSPL
#   03 Jun 2022: Addition of Avoidance optimization and enhanced downloading, other enhancements. 
#   08 Jun 2022: Major enhancement to include simulated tracks and detection estimation of those tracks, R Markdown report
# 0.61 - 15 Jun 2022: Added shapefile outputs to the data export
# 0.62 - 16 June 2022: Changed coverage optimizer to better handle 434 data. Was putting towers clumped together to much. 
# 0.63 - 28 Jun 2022: Reduced grid size to 1km spacing as it was causing memory issues on shiny apps.io
# 0.64 - 29 Jun 2022: Minor changes to grid size to 750 from 1km to improve optimization and stay under memory issues and
#  changes to report to add selected station and color simulated tracks
# 0.65 - 29 Jun 2022: Fixed an error in study area creation by reducing distance for optimization, also fixed some Motus antenna
#  estimation patterns. 
#  30 Jun 2022: slight changes to code (reduced buffering) to reduce out of memory errors for avoidance optim.
# 0.66 - 04 Aug 2022: Bug fixes: all stations not always optimized and when loading 
#   shapefiles for the study area simulation is not possible.
# 0.66.1 - 08 Sep 2022: Motus data causing error with locations on the date line - polygons not closed. Fixed. 
# 0.66.2 - Super Narwhal - 28 Sep 2022: Fixed long tables not breaking over pages in report
# 0.67 - Orca - 04 Oct 2022: Updated report with map bug fix and added header/footer. Fixed issue with the simulation not running when 
#    default study area (or any created study area) is used. 
# 0.67.1 Crafty Orca - 16 Feb 23 - fixed some markdown bugs and problems with creating Motus antenna layer for display
# 0.68 Elegant Porpoise - 16 Feb 23 - Added the ability to max. coverage with known fixed antenna angles. 
# 0.68.1 - 06 Mar 23 - change to carry flight heights data through to outputs. 
# 0.68.2 - 02 Oct 23 - error when loading shapefiles for turbines with m and z attributes - remove those on load
# 0.68.3 - 16 Sep 24 - Very small possible floating point issues - round lat/longs; also some further changes from sp to sf
#  also remove maptools calls as its been discontinued

currentdir <- normalizePath(getwd(), winslash = "/")
source(file.path(currentdir, "helpers.R"))
source(file.path(currentdir, "generate_array_functions.R"))
source(file.path(currentdir, "radio_telemetry_functions.R"))
source(file.path(currentdir, "maxcovr_extractors.R"))
source(file.path(currentdir, "maxcovr_motustag.R"))
source(file.path(currentdir, "nearest_motus.R"))
source(file.path(currentdir, "MOTUS_scripts.R"))
source(file.path(currentdir, "detection_pattern_166.R"))
source(file.path(currentdir, "detection_pattern_434.R"))

library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

IDIOMS_version = "0.68.3 Fastidious Porpoise"
# options(shiny.trace = TRUE)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    titleWidth = 360,
    tags$li(
      class = "dropdown",
      actionLink("appvrsn", label = tags$b(paste("Informing the Design and Implementation of Offshore Motus Systems (IDIOMS): v", IDIOMS_version), style = "font-size: 14px")),
      style = "float: left"
    ),
    tags$li(
      class = "dropdown",
      a(id = "download_manual",
        icon('fa-solid fa-book', "fa-2x"),
        style = "padding-top: 10px; padding-bottom: 10px;padding-right: 6px; padding-left: 6px",
        target = '_blank',
        href = "IDIOMS_user_manual draft_20220628.pdf"),
      style = "float: left"
    ),
    tags$li(
      class = "dropdown",
      a(
        icon('github', "fa-2x"),
        href = 'https://github.com/Biodiversity-Research-Institute/IDIOMS',
        style = "padding-top: 10px; padding-bottom: 10px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_codeLink"
      ),
      style = "float: left"
    ),
    tags$li(
      class = "dropdown",
      a(
        icon('bug', "fa-2x"),
        href = 'https://github.com/Biodiversity-Research-Institute/IDIOMS/issues',
        #exclamation-circle
        style = "padding-top: 10px; padding-bottom: 10px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_issuesLink"
      ),
      style = "float: left"
    ),
    
    # tags$li(class = "dropdown", actionLink("bookmark_btt", label = NULL, icon("bookmark", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    # tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    # tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    
    
    tags$li(
      class = "dropdown",
      a(
        img(src = "BRI_color_logo_no_words.png", height = "40px"),
        href = 'https://www.briwildlife.org',
        style = "padding-top: 5px; padding-bottom: 5px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_BRILogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "USFWS.png", height = "40px"),
        href = 'https://www.fws.gov/',
        style = "padding-top: 5px; padding-bottom: 5px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_FWSLogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "URI.png", height = "40px"),
        href = 'https://www.uri.edu/',
        style = "padding-top: 5px; padding-bottom: 5px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_URILogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "NYSERDA.png", height = "40px"),
        href = 'https://www.NYSERDA.ny.gov',
        style = "padding-top: 5px; padding-bottom: 5px; padding-right: 6px; padding-left: 6px",
        target = '_blank',
        id = "lbl_NYSERDALogoLink"
      ),
      style = "float: right"
    )
  ),
  
  dashboardSidebar(
    collapsed = F,
    width=360,
    #style = "position: fixed; overflow: visible;",
    # width=240,
    sidebarMenu(
      id = "sidebar",
      tags$a(
        img(src = "IDIOMS_logo_final_2_small_onscreen.png", alt="Informing the Design and Implementation of Offshore Motus Systems", 
            width = "360px", class="header_img"),
        href = 'https://briwildlife.org/IDIOMS',
        style = "margin-bottom: 10px;",# padding-right:20px; margin-bottom: -10px; margin-top:-45px;",
        target = '_blank',
        id = "IDIOMS_LogoLink"
      ),
      h4("1) IDIOMS run details:", style = "padding-left: 10px; margin-bottom: 0px"),
      textInput(inputId = "project_name", label = "Project name: ", value = "", width = "90%", placeholder = "Project"),
      div(style = "margin-top: -20px;"),  #reduce space between elements
      textInput(inputId = "modeler", label = "Name of person running IDIOMS: ", value = "", width = "90%", placeholder = "Name"),
      hr(),
      
      conditionalPanel( 
        #show only when project names entered
        condition = ('input.project_name != "" & input.modeler != ""'),
        h4("2) Study area and array", style = "padding-left: 10px;"),
        sidebarMenu(menuItem(
          text = "Design a study",
          # tabName = "tab_StudyAreaPars",
          icon = icon("pencil"),
          # # --- number locations
          # menuItem(
          #   numericInput(
          #     width = "85%",
          #     inputId = "numInput_studyareaPars_nlocs",
          #     label = label.help(
          #       "Number of possible station locations",
          #       "lbl_studyareaPars_nlocs"
          #     ),
          #     value = NA, #startUpValues$studyareaPars$studyareaPars_nlocs,
          #     min = 1,
          #     step = 1
          #   ),
          #   bsTooltip(
          #     id = "lbl_studyareaPars_nlocs",
          #     title = paste0("Total number of station locations in the array."),
          #     options = list(container = "body"),
          #     placement = "right",
          #     trigger = "hover"
          #   )
          # ),
          
          # ---  Latitude
          numericInput(
            width = "85%",
            inputId = "numInput_studyareaPars_Latitude",
            label = "Latitude (deg) of center",
            value = NA, #startUpValues$studyareaPars$studyareaPars_Latitude,
            min = -90,
            max = 90,
            step = 0.01),
          bsTooltip(
            id = "numInput_studyareaPars_Latitude",
            title = "Latitude of the study area center in decimal degrees.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # ---  Longitude
          numericInput(
            width = "85%",
            inputId = "numInput_studyareaPars_Longitude",
            label = "Longitude (deg) of center",
            value = NA, #startUpValues$studyareaPars$studyareaPars_Longitude,
            min = -180,
            max = 180,
            step = 0.01),
          bsTooltip(
            id = "numInput_studyareaPars_Longitude",
            title = "Longitude of the study area center in decimal degrees.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # --- Width
          numericInput(
            width = "85%",
            inputId = "numInput_studyareaPars_width",
            label = "Width (km)",
            value = NA, #startUpValues$studyareaPars$studyareaPars_width,
            min = 1,
            step = 1),
          bsTooltip(
            id = "numInput_studyareaPars_width",
            title = paste0("The study area width in km"),
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # --- length
          numericInput(
            width = "85%",
            inputId = "numInput_studyareaPars_length",
            label = "Length (km)",
            value = NA, #startUpValues$studyareaPars$studyareaPars_length,
            min = 1,
            step = 1),
          bsTooltip(
            id = "numInput_studyareaPars_length",
            title = "The study area length in km",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # --- Station spacing (m)
          numericInput(
            width = "85%",
            inputId = "numInput_studyareaPars_spacing_m",
            label = "Station spacing (m)",
            value = NA, #startUpValues$studyareaPars$studyareaPars_spacing_m,
            min = 100,
            step = 100),
          bsTooltip(
            id = "numInput_studyareaPars_spacing_m",
            title = "Station spacing (m) in the array.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          br()
        ), style = "margin-top: -10px"),
        # sidebarMenu(menuItem(
        #   fluidRow(
        #     column(12, offset = 0.5,
        #            actionButton(inputId = "build_array", label = "Add the designed study"),
        #            tags$style(
        #              type = 'text/css',
        #              "#design_array { width:140px; float: right; margin-rigth: 10px; color: #fff; background-color:pink}")
        #     )
        #   ),
        #   br()
        #   ), style = "margin-top: -10px"),
        sidebarMenu(menuItem(
          text = "Upload a study",
          # tabName = "tab_StudyAreaPars",
          icon = icon("globe-americas"),
          # Input: Select a study area file
          # Use this instead of creating a study area using Create a wind farm
          fileInput(
            inputId = "filemap",
            label = "Upload a study area outline. Choose shapefile",
            multiple = TRUE,
            accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'),
            placeholder = "No study area uploaded"),
          # Input: Select an array file
          # Add possible antenna stations
          fileInput(
            inputId = "file_stn_locs",
            label = "Upload possible antenna station locations. Choose shapefile",
            multiple = TRUE,
            accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'),
            placeholder = "No station locations uploaded")
        ), style = "margin-top: -10px"),
        
        fluidRow(
          column(6, offset = 0,
                 actionButton(inputId = "default_array", label = "Add default study"),
                 tags$style(
                   type = 'text/css',
                   "#default_array { width:140px; float: right; margin-rigth: 10px; color: #fff; background-color:blue}")
          ),
          column(6, offset = 0, 
                 actionButton(inputId = "clear_array", label = "Remove study"),
                 tags$style(
                   type = 'text/css',
                   "#clear_array { width:140px; float: left; margin-left: 10px; color: #fff; background-color:gray}"),
                 )
          ),
      # ), #conditional panel
      
      # conditionalPanel( 
      #   condition = "!is.na(input.numInput_studyareaPars_spacing_m) & !is.na(input.numInput_studyareaPars_width) & !is.na(input.numInput_studyareaPars_length) & 
      #                !is.na(input.numInput_studyareaPars_Latitude) & !is.na(input.numInput_studyareaPars_Longitude)",
        
        hr(),
        
        h4("3) Input parameters", style = "padding-left: 10px;"),
        
        sidebarMenu(menuItem(
          text = "Receiving station parameters",
          icon = icon("broadcast-tower"),
          startExpanded = F,
          
          # --- Antenna number
          numericInput(
            width = "85%",
            inputId = "numInput_stnPars_antNum",
            label = "Number antennas per stn.",
            value = startUpValues$stnPars$antNum,
            min = 1),
          bsTooltip(
            id = "numInput_stnPars_antNum",
            title = "The number of receiving antennas per station",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"
          ),
          # ---- Antenna height
          numericInput(
            width = "85%",
            inputId = "numInput_stnPars_antHt",
            label =  "Antenna height (m)",
            value = startUpValues$stnPars$antHt,
            min = 1),
          bsTooltip(
            id = "numInput_stnPars_antHt",
            title = "Height of receiving stations in meters above MSL.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # ---- Antenna frequency
          selectInput(
            width = "85%",
            inputId = "selInput_stnPars_freq",
            label = "Receiver frequency",
            multiple = FALSE,
            choices = c("166 MHZ", "434 MHZ"),
            selected = "434 MHZ"),
          bsTooltip(
            id = "selInput_stnPars_freq",
            title = "Receiver frequency",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          # ---- Antenna type
          selectInput(
            width = "85%",
            inputId = "selInput_stnPars_anttype",
            label =  "Antenna type",
            multiple = FALSE,
            # choices = c("5-element-yagi-166MHZ", "7-element-yagi-166MHZ", "9-element-yagi-166MHZ", "omni-166MHZ",
            #             "10-element-circular-yagi-434MHZ", "omni-434MHZ"),
            choices = c("10-element-circular-yagi-434MHZ", "omni-434MHZ"),
            selected = "10-element-circular-yagi-434MHZ"),
          bsTooltip(
            id = "selInput_stnPars_anttype",
            title = "Antenna model",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          
          conditionalPanel(
            condition = "input.selInput_stnPars_freq == '166 MHZ'",
            # ---- Antenna lambda
            numericInput(
              width = "85%",
              inputId = "numInput_stnPars_antLambda",
              label = "Antenna lambda",
              value = startUpValues$stnPars$antLambda),
            bsTooltip(
              id = "numInput_stnPars_antLambda",
              title = "Antenna lambda",
              options = list(container = "body"),
              placement = "right",
              trigger = "hover"),
            # ---- Antenna D0
            numericInput(
              width = "85%",
              inputId = "numInput_stnPars_antD0",
              label = "Antenna D0",
              value = startUpValues$stnPars$antD0
            ),
            bsTooltip(
              id = "numInput_stnPars_antD0",
              title = "Antenna D0",
              options = list(container = "body"),
              placement = "right",
              trigger = "hover"),
            # ---- Antenna p0
            numericInput(
              width = "85%",
              inputId = "numInput_stnPars_antP0",
              label = "Antenna P0",
              value = startUpValues$stnPars$antP0),
            bsTooltip(
              id = "numInput_stnPars_antP0",
              title = "Antenna P0",
              options = list(container = "body"),
              placement = "right",
              trigger = "hover"),
            # ---- Receiver sensitivity
            numericInput(
              width = "85%",
              inputId = "numInput_stnPars_min_receiver_sensitivity",
              label =  "Receiver sensitivity (dbm)",
              value = startUpValues$stnPars$min_receiver_sensitivity),
            bsTooltip(
              id = "numInput_stnPars_min_receiver_sensitivity",
              title = "Receiver sensitivity (dbm)",
              options = list(container = "body"),
              placement = "right",
              trigger = "hover")
          ), #conditional panel 166
          br()
        ), style = "margin-top: -10px"),
        #---- Detection parameters
        sidebarMenu(menuItem(
          text = "Detection parameters",
          icon = icon("cog"),
          numericInput(
            width = "85%",
            inputId = "numInput_stnPars_NumStns",
            label = "Number of antenna stations",
            value = startUpValues$stnPars$stn_num, 
            min = 1),
          numericInput(
            width = "85%",
            inputId = "numInput_optimPars_MinFltHt",
            label = "Min. detection flight height",
            min = 1,
            step = 5,
            value = 25),
          numericInput(
            width = "85%",
            inputId = "numInput_optimPars_MaxFltHt",
            label = "Max. detection flight height (max 1500m)",
            min = 10,
            max = 1500,
            step = 5,
            value = 125),
          numericInput(
            width = "85%",
            inputId = "numInput_optimPars_FltHtInc",
            label = "Flight height detection increment",
            min = 5,
            step = 5,
            value = 25
          ),
          br()
        ), style = "margin-top: -10px; margin-bottom: -10px;"),
        #################Enter Receiving station input parameters specific to tag/antenna frequency type
        # menuItem(
        #   text = "Prop. max r-detect to buffer",
        #   startExpanded = T,
        #   numericInput(
        #     width = "85%",
        #     inputId = "numInput_optimPars_percmaxr",
        #     label = "",
        #     min = 0,
        #     max = 1,
        #     step = 0.01,
        #     value = 0.33
        #   )
        # ),
        
        hr(),
        h4("4) Detection array creation", style = "padding-left: 10px;"),
        
        # Input: Select the telemetry station generation type ----
        radioButtons(
          "op_type",
          "Optimization type:",
          c(
            "None (manual selection)" = "manual",
            "Coverage optimized" = "covg_optim",
            "Coverage optimized, fixed angle" = "covg_optim_fixed_angle",
            "Avoidance optimized" = "avoid_optim"#,
            # "Density optimized" = "density_optim"
          )
        ),
        
        conditionalPanel(
          condition = "input.op_type == 'avoid_optim'",
          menuItem(
            # ---- avoidance distance
            startExpanded = T,
            numericInput(
              width = "85%",
              inputId = "numInput_avoidance_dist",
              label =  "Avoidance distance in km",
              value = 10,
              min = 1),
            bsTooltip(
              id = "numInput_avoidance_dist",
              title = "Avoidance distance in km to try to detect within.",
              options = list(container = "body"),
              placement = "right",
              trigger = "hover")
          )
        ), #conditionalPanel
      
      conditionalPanel(
        condition = "input.op_type == 'covg_optim_fixed_angle'",
        menuItem(
          # ---- avoidance distance
          startExpanded = T,
          numericInput(
            width = "85%",
            inputId = "numInput_fixed_ant_angle",
            label =  "Fixed starting antenna angle",
            value = 0,
            min = 0, 
            max = 360,
            step = 1),
          bsTooltip(
            id = "numInput_fixed_ant_angle",
            title = "The angle of one of the antennas of the fixed array.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover")
        )
      ), #conditionalPanel
      
        # ),#conditionalPanel
        # menuItem(
        #   text = "Detection polygon color",
        #   startExpanded = T,
        sidebarMenu(
          selectInput(
            width = "50%",
            inputId = "selInput_optimPars_antcolors",
            label =  "Detection polygon color",
            multiple = FALSE,
            choices = c("red", "blue", "green","psychedelic"),
            selected = "blue"
          ), style = "margin-top: -10px"),
        hr(),
        
        fluidRow(
          column(3),
          column(3, offset = 0,
                 actionButton(inputId = "generate", label = "Generate array"),
                 tags$style(
                   type = 'text/css',
                   "#generate { margin-left: 15px; color: #fff; background-color:brown}"
                 )),
          column(3)
          
        ), 
        
        conditionalPanel(
          condition = "output.run_sucess",
          hr(),
          
          h4("5) Simulation parameters", style = "padding-left: 10px;"),
          
          # menuItem(
          # ---- simulation inputs
          startExpanded = T,
          selectInput(
            width = "50%",
            inputId = "cardinal_dir_sim",
            label =  "Simulation origin location",
            multiple = FALSE,
            choices = c("SW", "S", "SE", "E", "NE", "N", "NW", "W"),
            selected = "NE"
          ),
          bsTooltip(
            id = "cardinal_dir_sim",
            title = "The cardinal direction from which the simulated tracks originate relative to the study area.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          numericInput(
            width = "85%",
            inputId = "n_sim_birds",
            label = "Number of birds to simulate.",
            min = 5,
            max = 1000,
            step = 5,
            value = 25
          ),
          bsTooltip(
            id = "n_sim_birds",
            title = "Number of birds to simulate tracks for display and detection estiamtion.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          numericInput(
            width = "85%",
            inputId = "n_boot",
            label = "Number bootstrap resamples for detection est.",
            min = 100,
            step = 100,
            value = 500
          ),
          bsTooltip(
            id = "n_boot",
            title = "The number of birds to simulate tracks for display and detection calculation.",
            options = list(container = "body"),
            placement = "right",
            trigger = "hover"),
          br()
          # style = "margin-top: -10px" 
          # )#menunitems
        ), #conditionalPanel
      ), #conditionalPanel
      
      
      # 
      # br(),
      # 
      # actionButton(inputId = "seabird_tracks", label =  HTML("Simulate seabird <br/> tracks")),
      # tags$style(
      #   type = 'text/css',
      #   "#seabird_tracks { width:60%; margin-left: 15px; color: #fff; background-color:navy}"
      # ),
      # 
      # br(),
      # 
      # actionButton(inputId = "shorebird_tracks", label =  HTML("Simulate shorebird <br/> tracks")),
      # tags$style(
      #   type = 'text/css',
      #   "#shorebird_tracks { width:60%; margin-left: 15px; color: #fff; background-color:purple}"
      # ),
      # 
      # br(),
      # 
      # downloadButton("downloadData", HTML("Download <br/> antenna data")),
      # tags$style(
      #   type = 'text/css',
      #   "#downloadData { width:60%; margin-left: 15px; color: #fff; background-color:orange; visibility: hidden}"
      # ),
      
      
      tags$script(
        '
        $(document).ready(function () {
          navigator.geolocation.getCurrentPosition(onSuccess, onError);

          function onError (err) {
            Shiny.onInputChange("geolocation", false);
          }

          function onSuccess (position) {
            setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
            }, 1100)
          }
        });
                '
      )
    )
  ),
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "www/style.css")
    ),
    #set a backgroundimage
    # setBackgroundImage(src = "psychedelic_wall.jpg", shinydashboard = T),
    
    useShinyjs(),

    # shuny busy indicators
    # # shinybusy indicators initialize
    # # Use once in UI
    # add_loading_state(
    #   ".leaflet-pane",
    #   spinner = "hourglass",
    #   text = "Please wait...",
    #   svgColor = "steelblue"
    # ),
    
    # adds a indicator on slow load
    add_busy_spinner(spin = "intersecting-circles", 
                     color = "purple",
                     position = "top-left", 
                     margins = c("50%", "50%"), 
                     onstart = T),
     
    tabsetPanel(
      id = "tabsetpan",
      type = "tabs",
      selected = "study_area_panel",
      tabPanel(
        "Study area data",
        value = "study_area_panel",
        fluidRow(
          box(
            title = "Instructions",
            status = "primary",
            solidHeader = F,
            collapsible = T,
            width = 12,
            fluidRow(
              column(width = 9, htmlOutput("study_area_instructions")),
              column(
                width = 3,
                align = "center",
                img(src = "antenna_BIWF_434_cropped.jpg", width =
                      200)
              )
            )
          )
        ),
        fluidRow(column(6,
                        fluidRow(
                          box(
                            title = "Study area map",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            leafletOutput("studymap", width = "100%")
                          )
                        ),
                        fluidRow(
                          box(
                            title = "Selected stations",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            # DT::dataTableOutput("stations_selected"))
                            rHandsontableOutput("stations_selected")
                          )
                        )),
                 column(
                   6,
                   box(
                     title = "Station data",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     DT::dataTableOutput("stations")
                   )
                 ))
      ), #end tabpanel
      
      tabPanel(
        "Station coverage results",
        value = "coverage_panel",
        fluidRow(
          box(
            title = "Next steps:",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            #Put buttons all on a single line by wrapping in div blocks 
            # div(
            #   div(style="display: inline-block; width: 200px ;", uiOutput("sim_seabirds_btn")),
            #   div(style="display: inline-block; width: 200px ;", uiOutput("sim_shorebirds_btn")),
            #   div(style="display: inline-block; width: 200px ;", uiOutput("download_output_btn")),
            #   div(style="display: inline-block; width: 200px ;", uiOutput("gen_report_btn"))
            # )
            column(3,uiOutput("sim_seabirds_btn")), 
            column(3,uiOutput("sim_shorebirds_btn")), 
            column(3,uiOutput("download_output_btn")), 
            column(3,uiOutput("gen_report_btn")), 
            
          )
        ),
        fluidRow(
          box(
            title = "Receiver detection map",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("detectionmap", width = "100%")
          ),
          box(
            title = "Tag detection range plot by flight height",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("detection_range_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Station antenna angles",
            status = "success",
            solidHeader = TRUE,
            width = 3,
            dataTableOutput("angles")
          ),
          #Simulation detection results
          box(
            title = "Simulated bird track detection by flight height",
            status = "primary",
            solidHeader = TRUE,
            width = 5,
            dataTableOutput("sim_results")
          ),
          box(
            title = "Antenna station coverage",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            dataTableOutput("stn_detection_DT")
          ))
      ) #end tab panel: Detection results
      
      # tabPanel("Study design report", value = "study_design_report",
      #          fluidRow(
      #            box(
      #              title = "Report",
      #              status = "primary",
      #              solidHeader = F,
      #              collapsible = F,
      #              width = 12,
      #              fluidRow(column(
      #                width = 12, htmlOutput("study_design_report_txt")
      #              ))
      #            )
      #     )) #end tabpanel
    ) #end tabsetpanel
  )
)


verbose <- F

# Define server logic
server <- function(input, output, session) {
  
  #set reactive values of stn grid and study area inputs
  values <- reactiveValues()
  values$study_boundary <- NULL
  values$stn_grid <- NULL
  values$all_stns_df <- NULL
  values$ref_grid <- NULL
  values$clickIDs <- c()
  values$stations_selected <- NULL
  # values$run_sucess <- F
  values$seabird_sim <- 0
  values$shorebird_sim <- 0
  values$shorebird_lines <- data.frame()
  values$seabird_lines <- data.frame()
  
  #create hide panel output value to be able to hide the study design input when not needed
  output$hide_panel <- eventReactive(input$filemap, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  
  # output$avoid_tab <- eventReactive(input$op_type=="avoid_optim", TRUE, ignoreInit = TRUE)

  # output$study_design_report_txt <-
  #   renderText(
  #     'Here you will find the an assessment of the suitability of the station arrays to detect
  #                                              target species.'
  #   )
  
  #display instruction for various options
  observeEvent(input$op_type, {
    if (input$op_type == "manual") {
      output$study_area_instructions <-
        renderText(
          '1) Add project details and the name of the person running IDIOMS. <br>
            2) Upload a study area outline and possible antenna station locations as shapefiles or
            create a wind farm using parameters in the "Create a wind farm" parameters sidebar option. <br> 
            3) Select (deselect) locations for proposed Motus antenna arrays by either clicking
            station points on the map or the table. Stations will be added to the selected stations table. <br> 
            4) Modify default starting antenna angle of proposed (or actual) antenna angles by editing the
            "Starting antenna angle" column of the Selected Stations table. All other antennas will be assumed to be
            equally spaced with antenna spacing being determined by 360°/antenna number. <br>
            5) Modify receiving station parameters and detection parameters. <br>
            6) Click "Generate Array Button". <br>'
        )
      
    }
    
    if (input$op_type == "covg_optim") {
      output$study_area_instructions <-
        renderText(
          '1) Add project details and the name of the person running IDIOMS. <br>
            2) Upload a study area outline or create a wind farm using parameters in the "Create a wind farm" parameters sidebar option. <br> 
            3) Modify receiving station parameters and detection parameters especially setting the number of antennas arrayed at a station
            as well as the number of stations to optimize on (min. 2). <br> 
            4) Click "Generate Array" Button.'
        )
      #clear selected station data if any
      values$stations_selected <- values$stations_selected[c(),]
      dataTableProxy("stations") %>%
        selectRows(NULL)
      
    }
    
    if (input$op_type == "covg_optim_fixed_angle") {
      output$study_area_instructions <-
        renderText(
          '1) Add project details and the name of the person running IDIOMS. <br>
            2) Upload a study area outline or create a wind farm using parameters in the "Create a wind farm" parameters sidebar option. <br> 
            3) Modify receiving station parameters and detection parameters especially setting the number of antennas arrayed at a station
            as well as the number of stations to optimize on (min. 2). <br> 
            4) Set the fixed antenna angle for one of the antennas of the array. <br>
            5) Click "Generate Array" Button.'
        )
      #clear selected station data if any
      values$stations_selected <- values$stations_selected[c(),]
      dataTableProxy("stations") %>%
        selectRows(NULL)
      
    }
    
    if (input$op_type == "avoid_optim") {
      #clear selected station data if any
      output$study_area_instructions <- 
        renderText(
          '1) Add project details and the name of the person running IDIOMS. <br>
            2) Upload a study area outline or create a wind farm using parameters in the "Create a wind farm" parameters sidebar option. <br> 
            3) Modify receiving station parameters and detection parameters especially setting the number of antennas arrayed at a station
            as well as the number of stations to optimize on (min. 2). <br>  
            4) Enter the avoidance distance in km over which you want to optimize the array.
            5) Click "Generate Array" Button.')
      values$stations_selected <- values$stations_selected[c(),]
      dataTableProxy("stations") %>%
        selectRows(NULL)
    }
    
    if (input$op_type == "density_optim") {
      output$study_area_instructions <- renderText('1) TODO')
      #clear selected station data if any
      values$stations_selected <- values$stations_selected[c(),]
      dataTableProxy("stations") %>%
        selectRows(NULL)
    }
  })
  
  observe({
    #show antenna model choices relative to frequency
    
    if(input$selInput_stnPars_freq == "166 MHZ"){
      # 166MHZ selected
      updateSelectInput(session, "selInput_stnPars_anttype",
                        choices = c("5-element-yagi-166MHZ", "7-element-yagi-166MHZ", "9-element-yagi-166MHZ", "omni-166MHZ"))
    } else {
      # 434MHZ selected
      updateSelectInput(session, "selInput_stnPars_anttype",
                        choices = c("10-element-circular-yagi-434MHZ", "omni-434MHZ"))
    }
  })
  
  observe({
    #Set the dbi of the antenna
    if(input$selInput_stnPars_anttype == "5-element-yagi-166MHZ"){
      updateNumericInput(
        session,
        inputId = "numInput_stnPars_antD0",
        value = 9
      )
    }
    if(input$selInput_stnPars_anttype == "7-element-yagi-166MHZ"){
      updateNumericInput(
        session,
        inputId = "numInput_stnPars_antD0",
        value = 10 #TODO - this needs to be updated - not sure of this
      )
    }
    if(input$selInput_stnPars_anttype == "9-element-yagi-166MHZ"){
      updateNumericInput(
        session,
        inputId = "numInput_stnPars_antD0",
        value = 11.1
      )
    }
    if(input$selInput_stnPars_anttype == "10-element-circular-yagi-434MHZ"){
      updateNumericInput(
        session,
        inputId = "numInput_stnPars_antD0",
        value = 12
      )
    }
  })

  #create study area from center point and length/width in km
  study_area <- reactive({
    req(!is.na(input$numInput_studyareaPars_spacing_m), !is.na(input$numInput_studyareaPars_width), !is.na(input$numInput_studyareaPars_length), 
        !is.na(input$numInput_studyareaPars_Latitude), !is.na(input$numInput_studyareaPars_Longitude))
    #create rectangle from center point and length width in m
    x_half <- input$numInput_studyareaPars_width * 1000 / 2
    y_half <- input$numInput_studyareaPars_length * 1000 / 2
    #create rectangle from center point and length width in m
    center_pt <-
      st_sfc(st_point(
        c(
          input$numInput_studyareaPars_Longitude,
          input$numInput_studyareaPars_Latitude
        )
      ), crs = 4326) %>%
      st_transform(WebMerc)
    center_pt_coords <- st_coordinates(center_pt)
    center_x <- center_pt_coords[[1]]
    center_y <- center_pt_coords[[2]]
    ll <- (c(center_x - x_half, center_y - y_half))
    ul <- (c(center_x - x_half, center_y + y_half))
    ur <- (c(center_x + x_half, center_y + y_half))
    lr <- (c(center_x + x_half, center_y - y_half))
    pts <-
      st_sf(geometry = st_sfc(st_multipoint(rbind(ll, ul, ur, lr)), crs = 3857))
    poly <-
      st_as_sf(st_cast(pts, "POLYGON")) %>% st_transform(WGS84) #%>% as_Spatial()
    return(poly)
  })
  
  turbine_grid <- reactive({
    #create turbine grid in a reactive context
    req(!is.na(input$numInput_studyareaPars_spacing_m), !is.na(input$numInput_studyareaPars_width), !is.na(input$numInput_studyareaPars_length), 
        !is.na(input$numInput_studyareaPars_Latitude), !is.na(input$numInput_studyareaPars_Longitude))
    # req(!is.na(input$numInput_studyareaPars_spacing_m))
    ant_array <-
      create_grid(
        study = study_area(),
        resolution_m = input$numInput_studyareaPars_spacing_m
      )
    all_stns_WGS84 <- st_transform(ant_array, WGS84)
    return(all_stns_WGS84)
    
  })
  
  input_boundary <- reactive({
    # Load the input boundary shape files for display and processing
    # hide the create wind farm menu item
    req(input$filemap)
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- input$filemap
    
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i],
                  paste0(tempdirname, "/", shpdf$name[i]))
    }
    map <- spTransform(readOGR(paste(tempdirname,
                                     shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                     sep = "/")), WGS84)
    map
  })

  study_center <- reactive({
    geosphere::centroid(input_boundary())
  })

  input_stn_locs <- reactive({
    # Load station locations shapefile
    #hide the create wind farm menu item
    # display_create_wind <- -F
    
    req(input$file_stn_locs)
    
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- input$file_stn_locs
    
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i],
                  paste0(tempdirname, "/", shpdf$name[i]))
    }

    stn_locs <- spTransform(readOGR(paste(tempdirname,
                                          shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                          sep = "/")), WGS84)
    stn_locs$ID <- 1:nrow(stn_locs)
    stn_locs <-
      st_as_sf(stn_locs[, c("ID")])  #remove any attribute data
    
    #error when you have z or m attributes - drop
    stn_locs <- sf::st_zm(stn_locs)
    stn_locs
  })

  observeEvent(turbine_grid(), {
    values$stn_grid <- turbine_grid()
  })
  
  observeEvent(input_stn_locs(), {
    values$stn_grid <- input_stn_locs()
    # study_area() <- NULL  #causing to crash - need to remove study area though
  })
  
  # Create station grid spatial file for processing from user input
  observeEvent(values$stn_grid, {
    req(!is.null(values$stn_grid))
    #use input_stn_locs if added, otherwise use default study area
    stn_grid_WGS84 <- st_transform(values$stn_grid, WGS84)
    #creating as DF as the coordinates as X/Y need to be labeled as long, lat
    all_stns_df <-
      cbind(stn_grid_WGS84[, c("ID")], st_coordinates(stn_grid_WGS84)) %>%  st_set_geometry(NULL)
    colnames(all_stns_df) <- c("ID", "long", "lat")
    #add default starting antenna angle of 45 degrees
    first_angle <- (360 / input$numInput_stnPars_antNum) / 2
    all_stns_df$first_ant_angle <- first_angle
    # angle_list <- paste(antenna_angle_seq(input_angle = first_angle, num_antennas = input$numInput_stnPars_antNum), collapse=',')
    # all_stns_df$ant_angle_list <- angle_list
    values$all_stns_df <- all_stns_df
  })
  
  # If the study area changes set the boundary and reference grid for processing
  observeEvent(study_area(), {
    values$study_boundary <- study_area()
    study_boundary_webmerc <- st_transform(study_area(), 3857)
    #create grid to optimize against - try 500 m grid for
    #28 Jun 22 - grid size may be cause of exceeding memory (8gb), increase to 750 to see if this helps. 
    values$ref_grid <- create_grid(study = study_area(), resolution_m = 750)
    #Create the buffer around the study area boundary for avoidance optim
    # study_buff_webmerc <- spTransform(study_area(), WebMerc)

    # values$avoid_buffer <- gDifference(gBuffer(study_buff_webmerc, width=isolate(input$numInput_avoidance_dist)*1000, quadsegs=30), 
    #                                    study_buff_webmerc)
    values$avoid_buffer <- st_buffer(study_boundary_webmerc, dist=isolate(input$numInput_avoidance_dist)*1000) %>% 
      st_difference(study_boundary_webmerc)

    #create avoidance grid to optimize against - try 500 m grid for
    values$avoid_grid <- create_grid(study = values$avoid_buffer, resolution_m = 750)
    #get data.frame of cardinal points around study area to be able to simulate start from
    # study_boundary <- st_as_sf(values$study_boundary) %>% st_transform(3857)
    # study_boundary <- st_transform(values$study_boundary, 3857)
    
    #when study area or input boundary changes generate the simulation point coords 
    values$sim_pt_coords <- cardinal_start_coords(st_sf(study_boundary_webmerc), 20)
  })
  
  observeEvent(input_boundary(), {
    values$study_boundary <- isolate(input_boundary())
    study_boundary_webmerc <- st_transform(values$study_boundary, 3857)
    
    #create grid to optimize against - try  500 m grid for
    values$ref_grid <- create_grid(study = values$study_boundary, resolution_m = 750)
    #Create the buffer around the input_boundary for avoidance optim
    # input_bound_buff_webmerc <- spTransform(values$study_boundary, WebMerc)
    # 
    # values$avoid_buffer <- gDifference(gBuffer(input_bound_buff_webmerc, width=input$numInput_avoidance_dist*1000, quadsegs=30),
    #                                    input_bound_buff_webmerc)
    values$avoid_buffer <- st_buffer(study_boundary_webmerc, dist=isolate(input$numInput_avoidance_dist)*1000) %>% 
      st_difference(study_boundary_webmerc)
    
    #create avoidance grid to optimize against - try 500 m grid for
    values$avoid_grid <- create_grid(study = values$avoid_buffer, resolution_m = 750)
    # study_boundary <- st_as_sf(values$study_boundary) %>% st_transform(3857)
    #when study area or input boundary changes generate the simulation point coords 
    values$sim_pt_coords <- cardinal_start_coords(st_sf(study_boundary_webmerc), 20)
    
  })

  observeEvent(input$default_array, {
    #clear any existing array and load the default array
    values$study_boundary <- NULL
    values$stn_grid <- NULL
    values$ref_grid <- NULL
    values$avoid_buffer <- NULL
    values$avoid_grid <- NULL
    values$all_stns_df <- NULL
    values$stations_selected <- NULL
    # values$input_pars <- data.frame()
    
    # updateNumericInput(
    #   session,
    #   inputId = "numInput_studyareaPars_nlocs",
    #   value = startUpValues$studyareaPars$studyareaPars_nlocs
    # )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_spacing_m",
      value = startUpValues$studyareaPars$studyareaPars_spacing_m
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_Latitude",
      value = startUpValues$studyareaPars$studyareaPars_Latitude
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_Longitude",
      value = startUpValues$studyareaPars$studyareaPars_Longitude
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_width",
      value = startUpValues$studyareaPars$studyareaPars_width
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_length",
      value = startUpValues$studyareaPars$studyareaPars_length
    )
  })
  
  #clear out study area and array
  observeEvent(input$clear_array, {
    values$study_boundary <- NULL
    values$stn_grid <- NULL
    values$ref_grid <- NULL
    values$avoid_buffer <- NULL
    values$avoid_grid <- NULL
    values$all_stns_df <- NULL
    values$stations_selected <- NULL
    # values$input_pars <- data.frame()
    
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_nlocs",
      value = NA
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_spacing_m",
      value = NA
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_Latitude",
      value = NA
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_Longitude",
      value = NA
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_width",
      value = NA
    )
    updateNumericInput(
      session,
      inputId = "numInput_studyareaPars_length",
      value = NA
    )
    
    leafletProxy("studymap") %>% 
      clearGroup(group = "selected_stns")

  })
  
  #geolocation to get proper timezone
  # values$local_tz <- ""  #set GMT as default
  observeEvent(input$lat, {
    values$local_tz <-
      lutz::tz_lookup_coords(as.numeric(input$lat), as.numeric(input$long), warn = F)
  })
  
  output$local_tz <- renderText({
    values$local_tz
  })
  
  # observeEvent(input$generate, {
  #   if (is.null(values$study_boundary)){
  #     showNotification(
  #       "Please add as study area!",
  #       duration = 6,
  #       type = "error"
  #     )
  #     return(NULL)
  #   }
  #   
  #   req(values$study_boundary)
  #   flag = 0
  #   if (nrow(values$stations_selected) == 0 & input$op_type == "manual") {
  #     showNotification(
  #       "Please select at least one station location.",
  #       duration = 10,
  #       type = "error"
  #     )
  #     flag = 1
  #   }
  #   if (input$numInput_stnPars_NumStns < 2 &
  #       (input$op_type == "covg_optim" | input$op_type == "avoid_optim")) {
  #     showNotification(
  #       "Please enter a value of at least 2 for 'Number of antenna stations' to run the optimization routine.",
  #       duration = 10,
  #       type = "error"
  #     )
  #     flag = 1
  #   }
  #   #if no flag go to detection panel on generate
  #   if(flag==0) updateTabItems(session, "tabsetpan",
  #                    selected = "coverage_panel")
  # 
  # }) #observeEvent
  
  # generate_error_modal <- function(){
  #   if (is.null(study_area())) {
  #     showNotification(
  #       "Please add a study area before continuing.",
  #       duration = 10,
  #       type = "error"
  #     )
  #     flag = 1
  #   }
  # 
  #   #if no flag go to detection panel on generate
  #   if(flag==0) {updateTabItems(session, "tabsetpan",
  #                              selected = "coverage_panel")
  #   }
  #   return(flag)
  # }
  
  
  run_times <- reactiveValues()
  select_stns_by_hgt <- NULL
  select_stns_by_hgt <- eventReactive(input$generate, {
    # values$run_sucess <- F
    #data checks before proceeding
    #need study area data before proceeding
    if (is.null(values$study_boundary)){
      showNotification(
        "Please add a study area!",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$numInput_optimPars_MaxFltHt > 1500){
      showNotification(
        "Please choose a maximum flight height value less than or equal to 1500 m.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$numInput_optimPars_MinFltHt < 0){
      showNotification(
        "Please choose a minimum flight height value greater than 0 m.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$numInput_optimPars_MaxFltHt < input$numInput_optimPars_MinFltHt){
      showNotification(
        "Please choose a maximum flight height value greater than the minimum flight height value!",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    # #data validation before proceeding
    # validate(
    #   #need study area data before proceeding
    #   need(!is.null(values$study_boundary), 'Please add a study area!'),
    #   #check to make sure max flight height >= min fligth height
    #   need(input$numInput_optimPars_MaxFltHt >= input$numInput_optimPars_MinFltHt, 
    #        'Please choose a maximum flight height value greater than the minimum flighe height value')
    # )
    
    # flag = 0
    if (nrow(values$stations_selected) == 0 & input$op_type == "manual") {
      showNotification(
        "Please select at least one station location.",
        duration = 10,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$numInput_stnPars_NumStns < 2 &
        (input$op_type == "covg_optim" | input$op_type == "covg_optim_fixed_angle" | input$op_type == "avoid_optim")) {
      showNotification(
        "Please enter a value of at least 2 for 'Number of antenna stations' to run the optimization routine.",
        duration = 10,
        type = "error"
      )
      return(NULL)
    }
    
    updateTabItems(session, "tabsetpan",
                   selected = "coverage_panel")
    
    # Create a Progress object
    num_calcs <-
      length(
        seq(
          input$numInput_optimPars_MinFltHt,
          input$numInput_optimPars_MaxFltHt,
          input$numInput_optimPars_FltHtInc
        )
      )
    #clear memory just in case
    gc()
    
    withProgress(message = "Calculation in progress", detail = 'This may take a while...', value = 0, min = 0,  max = 1, {
      run_times$start <- Sys.time()
      #make download button visible now since data will be calculated
      runjs("document.getElementById('downloadData').style.visibility = 'visible';")
      
      # #use input_stn_locs if added, otherwise use default study area
      # #need to get lat/longs so transform to WGS84
      # stn_grid_WGS84 <- st_transform(values$stn_grid, WGS84)
      # #creating as DF as the coordinates as X/Y need to be labeled as long, lat
      # all_stns_df <- cbind(stn_grid_WGS84[,c("ID")], st_coordinates(stn_grid_WGS84)) %>%  st_set_geometry(NULL)
      # colnames(all_stns_df) <- c("ID", "long", "lat")

      study_area_sf <-
        st_as_sf(values$study_boundary) %>% st_transform(WebMerc)
      #get reference grid for optimization routine
      ref_grid_WGS84 <- st_transform(values$ref_grid, WGS84)
      
      #need to get lat/longs so transform to WGS84
      ref_grid_df <-
        cbind(ref_grid_WGS84[, c("ID")], st_coordinates(ref_grid_WGS84)) %>%  st_set_geometry(NULL)
      colnames(ref_grid_df) <- c("ID", "long", "lat")
      
      # Manual station selection estimation of coverage when user enters stations manually with manual antenna angles
      if (input$op_type == "manual") {
        values$stations_selected$ant_angle_list <-
          isolate(lapply(values$stations_selected$first_ant_angle, function(first_angle)
            antenna_angle_seq(
              input_angle = first_angle,
              num_antennas = input$numInput_stnPars_antNum
            )))
        #spatial points for stations for further processing
        selected_stns_sp <-
          SpatialPointsDataFrame(
            cbind(
              values$stations_selected$long,
              values$stations_selected$lat
            ),
            data = values$stations_selected,
            proj4string = WGS84
          )
        #Pass station locations to coverage estimation routine
        out_df <-
          manual_coverage(
            selected_stns_sp = selected_stns_sp,
            study_area_sf = study_area_sf,
            min_ht = input$numInput_optimPars_MinFltHt,
            max_ht = input$numInput_optimPars_MaxFltHt,
            interval_m = input$numInput_optimPars_FltHtInc,
            stn_HT = input$numInput_stnPars_antHt,
            tag_freq = input$selInput_stnPars_freq,
            ant_type = input$selInput_stnPars_anttype,
            xi_min_dbm = input$numInput_stnPars_min_receiver_sensitivity,
            num_ant = input$numInput_stnPars_antNum,
            lambda = input$numInput_stnPars_antLambda, 
            D0 = input$numInput_stnPars_antD0, 
            p0 = input$numInput_stnPars_antP0,
            # updateProgress = updateProgress,
            local_tz = values$local_tz
          )
      }
      
      # Enter the number of antennas desired and the function will determine the coverage optimized location and angles of these
      # depending on whether 166 or 434 MHZ
      if (input$op_type == "covg_optim") {
        out_df <-
          optimize_study_area_covg(
            study_area_sf = study_area_sf,
            min_ht = input$numInput_optimPars_MinFltHt,
            max_ht = input$numInput_optimPars_MaxFltHt,
            interval_m = input$numInput_optimPars_FltHtInc,
            stn_HT = input$numInput_stnPars_antHt,
            xi_min_dbm = input$numInput_stnPars_min_receiver_sensitivity,
            all_stns_df = values$all_stns_df,
            grid_df = ref_grid_df,
            grid_sf = values$ref_grid,
            tag_freq = input$selInput_stnPars_freq,
            ant_type = input$selInput_stnPars_anttype,
            n_stations = input$numInput_stnPars_NumStns,
            num_ant = input$numInput_stnPars_antNum,
            lambda = input$numInput_stnPars_antLambda, 
            D0 = input$numInput_stnPars_antD0, 
            p0 = input$numInput_stnPars_antP0,
            # updateProgress = updateProgress,
            local_tz = values$local_tz,
            # max_rbuff_prop = input$numInput_optimPars_percmaxr,
            max_rbuff_prop = 0.75,
            optim_type="coverage"
          )
      }
      
      # Enter the number of antennas desired and the function will determine the coverage optimized location and angles of these
      # depending on whether 166 or 434 MHZ
      if (input$op_type == "covg_optim_fixed_angle") {
        out_df <-
          optimize_study_area_covg_fixed_angles(
            study_area_sf = study_area_sf,
            min_ht = input$numInput_optimPars_MinFltHt,
            max_ht = input$numInput_optimPars_MaxFltHt,
            interval_m = input$numInput_optimPars_FltHtInc,
            stn_HT = input$numInput_stnPars_antHt,
            xi_min_dbm = input$numInput_stnPars_min_receiver_sensitivity,
            all_stns_df = values$all_stns_df,
            grid_df = ref_grid_df,
            grid_sf = values$ref_grid,
            tag_freq = input$selInput_stnPars_freq,
            ant_type = input$selInput_stnPars_anttype,
            ant_starting_angle = input$numInput_fixed_ant_angle,
            n_stations = input$numInput_stnPars_NumStns,
            num_ant = input$numInput_stnPars_antNum,
            lambda = input$numInput_stnPars_antLambda, 
            D0 = input$numInput_stnPars_antD0, 
            p0 = input$numInput_stnPars_antP0,
            # updateProgress = updateProgress,
            local_tz = values$local_tz,
            # max_rbuff_prop = input$numInput_optimPars_percmaxr,
            max_rbuff_prop = 0.75,
            optim_type="coverage"
          )
      }
      
      # Enter the number of antennas desired and the function will determine the avoidance optimized location and angles of these
      # depending on whether 166 or 434 MHZ
      # Ref grid should be located in buffer wherease the station grid will be in the wind farm area still
      if (input$op_type == "avoid_optim") {
        
        avoid_area_sf <-
          st_as_sf(values$avoid_buffer) %>% st_transform(WebMerc)
        
        #get reference grid for optimization routine
        avoid_grid_WGS84 <- st_transform(values$avoid_grid, WGS84)
        
        #need to get lat/longs so transform to WGS84
        avoid_grid_df <-
          cbind(avoid_grid_WGS84[, c("ID")], st_coordinates(avoid_grid_WGS84)) %>%  st_set_geometry(NULL)
        colnames(avoid_grid_df) <- c("ID", "long", "lat")
        
        out_df <-
          optimize_study_area_covg(
            study_area_sf = avoid_area_sf,
            min_ht = input$numInput_optimPars_MinFltHt,
            max_ht = input$numInput_optimPars_MaxFltHt,
            interval_m = input$numInput_optimPars_FltHtInc,
            stn_HT = input$numInput_stnPars_antHt,
            xi_min_dbm = input$numInput_stnPars_min_receiver_sensitivity,
            all_stns_df = values$all_stns_df,
            grid_df = avoid_grid_df,
            grid_sf = values$avoid_grid,
            tag_freq = input$selInput_stnPars_freq,
            ant_type = input$selInput_stnPars_anttype,
            n_stations = input$numInput_stnPars_NumStns,
            num_ant = input$numInput_stnPars_antNum,
            lambda = input$numInput_stnPars_antLambda, 
            D0 = input$numInput_stnPars_antD0, 
            p0 = input$numInput_stnPars_antP0,
            # updateProgress = updateProgress,
            local_tz = values$local_tz,
            # max_rbuff_prop = input$numInput_optimPars_percmaxr,
            max_rbuff_prop = 0.75,# changed from 0.33 to 0.5
            optim_type="avoid"
          )
      }
      
      #set exceed min covg threshold flag
      out_df$min_covg_flag <- 0
      out_df[which(out_df$study_area_covered > startUpValues$thresholdsPars$min_coverage), "min_covg_flag"] <- 1
      # save(out_df, file="data/select_stns_by_hgt.RData")

      run_times$end <- Sys.time()
      # values$run_sucess <<- T
      Sys.sleep(3) #sleep to give progress modal chance to show final and then close
      
      
      # }
      # else return(NULL)
    }) #withProgress
    return(out_df)
    # } else {
    #   #if (generate_error_modal() flag
    #   return(null)
    #   stop()
    # }
  })
  
  #save off the input pars for reporting
  
  input_pars <- eventReactive(select_stns_by_hgt() ,{
    df <- data.frame(min_ht = input$numInput_optimPars_MinFltHt,
                     max_ht = input$numInput_optimPars_MaxFltHt,
                     interval_m = input$numInput_optimPars_FltHtInc,
                     stn_HT = input$numInput_stnPars_antHt,
                     xi_min_dbm = input$numInput_stnPars_min_receiver_sensitivity,
                     tag_freq = input$selInput_stnPars_freq,
                     ant_type = input$selInput_stnPars_anttype,
                     n_stations = input$numInput_stnPars_NumStns,
                     num_ant = input$numInput_stnPars_antNum,
                     lambda = input$numInput_stnPars_antLambda, 
                     D0 = input$numInput_stnPars_antD0, 
                     p0 = input$numInput_stnPars_antP0)
    # save(df, file="data/input_pars.RData")
    
    return(df)
    })

  
  # Create dataframe for output of selected stations and angles from optimizers as well as inputs for storing
  observeEvent(select_stns_by_hgt() ,{

    num_stns <- input$numInput_stnPars_NumStns
    num_flt_hts <- 
      length(seq(input$numInput_optimPars_MinFltHt, input$numInput_optimPars_MaxFltHt,
          input$numInput_optimPars_FltHtInc))
    num_ant <- input$numInput_stnPars_antNum
    
    selected_stn_data <- rep(select_stns_by_hgt()$flt_ht, times = 1, each = num_stns * num_ant)
    
    #slightly different data storage depending on station selection method
    if (input$op_type == "manual" | input$op_type == "covg_optim_fixed_angle") {
      selected_stns_rep <- rbindlist(isolate(select_stns_by_hgt()$angle_df))
      selected_stns_rep <- selected_stns_rep %>% 
        dplyr::rename(station_ID = station)
      stns_loc <- isolate(values$stations_selected)
      stns_loc$ID <- as.character(stns_loc$ID)
      selected_stns_rep <- left_join(selected_stns_rep, stns_loc[,c("ID", "lat", "long")], by=c("station_ID"="ID"), copy = T)

      #save the output table for later downloading and display
      values$selected_stn_data_angles <- st_sf(selected_stns_rep, crs=3857)
      
    } else {
      selected_stns <- rbindlist(select_stns_by_hgt()$stns) %>% 
        dplyr::rename(station_ID = ID) %>% 
        dplyr::select(-c(first_ant_angle))

      selected_stns_rep <- as.data.frame(sapply(selected_stns, function(x) rep(x, times = 1, each = num_ant)))
      selected_stns_rep <- cbind(flt_ht = selected_stn_data, selected_stns_rep)
      
      stn_angles <- rbindlist(select_stns_by_hgt()$angle_df) %>% 
        select(-c(station))

      #save the output table for later downloading and display
      values$selected_stn_data_angles <- st_sf(cbind(selected_stns_rep, stn_angles), crs=3857)
    }
    
    values$select_stns_covg_by_hgt <- select_stns_by_hgt()[, c("flt_ht",
                                                               "max_r",
                                                               "study_area_covered",
                                                               "antenna_coverage_overlap")]
  })
  
  
  # Range plot for detection distances and coverage proportions 
  output$detection_range_plot <- renderPlot({
    coeff <- max(select_stns_by_hgt()$max_r)
    ggplot(select_stns_by_hgt(), aes(x = flt_ht, y = max_r)) +
      geom_point(aes(col = "red")) +
      geom_line(aes(col = "red")) +
      geom_point(aes(
        x = flt_ht,
        y = study_area_covered * coeff,
        col =
          "blue"
      )) +
      geom_line(aes(
        x = flt_ht,
        y = study_area_covered * coeff,
        col =
          "blue"
      )) +
      xlab("Flight height (m)") +
      ylab("Max est. detection range (m)") +
      scale_y_continuous(
        # Add a second axis and specify its features
        sec.axis = sec_axis(~ . / coeff, name = "Study area coverage prop."),
        limits = c(0, NA)
      ) +
      # ylim(0,NA) +
      scale_color_manual(
        values = c("red", "blue"),
        labels = c("study area covered", "max detection dist.")
      ) +
      theme(legend.position = c(.8, .2),
            legend.title = element_blank())
    
  })
  
  #optimization table creation and interaction
  # Must put selection parameter in the datatable creation or is ignored
  # output$stn_detection_DT <- DT::renderDataTable(datatable(select_stns_by_hgt()[,c("flt_ht", "max_r", "study_area_covered", "ref_area_covered", "buff_dist","antenna_coverage_overlap")],
  #                                                      colnames=c("Flight height (m)", "Max detection range (m)", "Study area covg.", "Ref area covg.", "Buff dist (m)","Antenna coverage overlap"),
  #                                                      selection = list(mode = "single", selected = 1)) %>% formatRound(columns = c(2, 5),digits = 0) %>%
  #                                                      formatRound(columns = c(3,4,6),digits = 2), server = T)
  

  # Insert leaflet map showing study area
  output$studymap <- renderLeaflet({
    studymap <- leaflet(options = leafletOptions(preferCanvas = T, tolerance = 100)) %>%  #add tolerance to improve click
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
      addEsriFeatureLayer(
        #add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/0",
        weight = 1, fill=FALSE, color = "#808080", #gray
        layerId = "BOEM_wind_leases", 
        group = "BOEM wind leases") %>% 
      addEsriFeatureLayer(
        #add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/2",
        weight = 1, fill=FALSE, color = "#C0C0C0", #silver
        layerId = "BOEM_wpa", 
        group = "BOEM wind planning areas") %>% 
      # addEsriDynamicMapLayer(
      #   #add MDAT bird abundance layers
      #   url = "https://mgelmaps.env.duke.edu/mdat/rest/services/MDAT/Avian_Abundance/MapServer",
      #   layerId = "MDAT_avian", 
      #   group = "MDAT avian occurrence", 
      #   options = dynamicMapLayerOptions(layers = c("1", "2"))) %>% 
      setView(lat = 40.4, lng = -72.9, zoom = 6) %>% 
      addPolygons(
        data = MOTUS_receiver_antennas_active_sf,
        color = "green",
        fillOpacity = 0,
        weight = 1,
        group = "Motus coverage"
      ) %>%
      addCircleMarkers(
        data = MOTUS_receiver_antennas_active_sf,
        lat = ~ latitude,
        lng = ~ longitude,
        popup =
          paste0(
            "Receiver ID: ",
            MOTUS_receiver_antennas_active_sf$receiverID,
            "<br/>Deployment name: ",
            MOTUS_receiver_antennas_active_sf$deploymentName,
            "<br/>Deployment started: ",
            MOTUS_receiver_antennas_active_sf$dtStart,
            "<br/>Project: ",
            MOTUS_receiver_antennas_active_sf$projectName,
            "<br/>Antenna type: ",
            MOTUS_receiver_antennas_active_sf$antennaType,
            "<br/>Antenna type: ",
            MOTUS_receiver_antennas_active_sf$elevation,
            "<br/>Frequency: ",
            MOTUS_receiver_antennas_active_sf$frequency,
            "<br/>Magnetic declination: ",
            round(MOTUS_receiver_antennas_active_sf$magdecl, 2),
            "<br/>Current status: ",
            MOTUS_receiver_antennas_active_sf$deploymentStatus
          ),
        group = "Active Motus receivers",
        color = "orange",
        radius = 2,
        fill = FALSE,
        weight = 1
      ) %>%
      # addCircleMarkers(
      #   data = MOTUS_receiver_antennas_inactive,
      #   lat = ~ latitude,
      #   lng = ~ longitude,
      #   group = "Inactive MOTUS receivers",
      #   color = "yellow",
      #   radius = 2,
      #   fill = FALSE,
      #   weight = 1
      # ) %>%

      # Layers control
    addLayersControl(
      overlayGroups = c("Study boundary", "Station locations", "Active Motus receivers", "Motus coverage", 
                        "BOEM wind leases", "BOEM wind planning areas", "MDAT avian occurrence"),
      position = "topright", #"topleft",
      options = layersControlOptions(collapsed = TRUE)
    )
  })

  # Insert detection map showing study area
  output$detectionmap <- renderLeaflet({
    detectionmap <- leaflet(options = leafletOptions(preferCanvas = T, tolerance = 1)) %>%
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>% 
      addEsriFeatureLayer(
        #add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/0",
        weight = 1, fill=FALSE, color = "#808080", #gray
        layerId = "BOEM_wind_leases", 
        group = "BOEM wind leases") %>% 
      addEsriFeatureLayer(
        # add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/2",
        weight = 1, fill=FALSE, color = "#C0C0C0", #silver
        layerId = "BOEM_wpa", 
        group = "BOEM wind planning areas") %>% 
      setView(lat = 40.4, lng = -72.9, zoom = 6) %>% 
      # MOTUS antenna coverage polys
      addPolygons(
        data = MOTUS_receiver_antennas_active_sf,
        color = "green",
        fillOpacity = 0,
        weight = 1,
        group = "Motus coverage"
      ) %>%
      # MOTUS antenna data
      addCircleMarkers(
        data = MOTUS_receiver_antennas_active_sf,
        lat = ~ latitude,
        lng = ~ longitude,
        popup =
          paste0(
            "Receiver ID: ",
            MOTUS_receiver_antennas_active_sf$receiverID,
            "<br/>Deployment name: ",
            MOTUS_receiver_antennas_active_sf$deploymentName,
            "<br/>Deployment started: ",
            MOTUS_receiver_antennas_active_sf$dtStart,
            "<br/>Project: ",
            MOTUS_receiver_antennas_active_sf$projectName,
            "<br/>Antenna type: ",
            MOTUS_receiver_antennas_active_sf$antennaType,
            "<br/>Antenna type: ",
            MOTUS_receiver_antennas_active_sf$elevation,
            "<br/>Frequency: ",
            MOTUS_receiver_antennas_active_sf$frequency,
            "<br/>Magnetic declination: ",
            round(MOTUS_receiver_antennas_active_sf$magdecl, 2),
            "<br/>Current status: ",
            MOTUS_receiver_antennas_active_sf$deploymentStatus
          ),
        group = "Active Motus receivers",
        color = "orange",
        radius = 3,
        fill = FALSE,
        weight = 1
      ) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Study boundary", "Station locations", "Active Motus receivers", "Motus coverage", "BOEM wind leases", "BOEM wind planning areas", 
                          "Simulated seabirds", "Simulated shorebirds"),
        position = "topright", #"topleft",
        options = layersControlOptions(collapsed = TRUE)
    )
  })
  
  # study map updates
  observe({
    # need to account for clearing study area - map fails when we don't account for null spatial objects
    study_map_add <- leafletProxy("studymap") %>%
      clearGroup(group = "Study boundary") %>%
      clearGroup(group = "Station locations")

        # add the study boundary and zoom to if available
    if (!is.null(values$study_boundary)) {
      # get bounds of study area to fit to map window
      # bounds <- bbox(values$study_boundary)
      bounds <- st_bbox(values$study_boundary)
      study_map_add <-
        addPolygons(
          map = study_map_add,
          data = values$study_boundary,
          color = "red",
          fillOpacity = 0,
          weight = 2,
          group = "Study boundary"
        ) %>%
        flyToBounds(
          # lng1 = bounds["x", "max"] + 0.05,
          # lat1 = bounds["y", "max"] + 0.05,
          # lng2 = bounds["x", "min"] - 0.05,
          # lat2 = bounds["y", "min"] - 0.05
          lng1 = bounds["xmax"] + 0.05,
          lat1 = bounds["ymax"] + 0.05,
          lng2 = bounds["xmin"] - 0.05,
          lat2 = bounds["ymin"] - 0.05
        )
    } 
    
    if (!is.null(values$stn_grid)) {
      study_map_add <-
        addMarkers(
          map = study_map_add,
          data = values$stn_grid,
          icon = turbine_icon,
          group = "Station locations",
          layerId = values$stn_grid$ID,
          label = values$stn_grid$ID, 
          labelOptions = (c(noHide = T))
        )
    }
  })
  
  #detection map updates
  observe({
    # Because map on other tab, need to add reactivity element to make leafletproxy work properly
    # see: https://github.com/rstudio/leaflet/issues/590
    # per schloerke
    # so to overcome this: "The observe is completing with no map rendered before, and nothing is 
    # triggering the observe once the map is generated after the fact.   
    # This is expected behavior for leafletProxy as it only works on a map that has already been rendered.
    # This could be addressed by inserting a reactive of what tab you're currently viewing."
    req(input$tabsetpan == "coverage_panel") # Only display if tab is 'Map Tab'
    if (!is.null(values$study_boundary)){
      # bounds <- bbox(values$study_boundary)
      bounds <- st_bbox(values$study_boundary)
      detection_map_add <- leafletProxy("detectionmap") %>% 
        clearGroup(group = "Study boundary") %>%
        clearGroup(group = "Station locations") %>% 
        flyToBounds(
          # lng1 = bounds["x", "max"] + 0.05,
          # lat1 = bounds["y", "max"] + 0.05,
          # lng2 = bounds["x", "min"] - 0.05,
          # lat2 = bounds["y", "min"] - 0.05
          lng1 = bounds["xmax"] + 0.05,
          lat1 = bounds["ymax"] + 0.05,
          lng2 = bounds["xmin"] - 0.05,
          lat2 = bounds["ymin"] - 0.05
        )
      
      detection_map_add <-
        #study boundary update add
        addPolygons(
          map = detection_map_add,
          data = values$study_boundary,
          color = "red",
          fillOpacity = 0,
          weight = 2,
          group = "Study boundary"
        ) 
      
      #stn grid marker update add
      detection_map_add <-
        addMarkers(
          map = detection_map_add,
          data = values$stn_grid,
          icon = turbine_icon,
          group = "Station locations",
          layerId = values$stn_grid$ID,
          label = values$stn_grid$ID, 
          labelOptions = (c(noHide = T))
        )
    } else{
      detection_map_add <- leafletProxy("detectionmap") %>% 
        clearGroup(group = "Study boundary") %>%
        clearGroup(group = "Station locations")
      }
  })
  
  values$boot_mean_detection_CI <- data.frame()
  
  #flag for hiding/showing simulation inputs
  
  output$run_sucess <- reactive({select_stns_by_hgt()})
  outputOptions(output, "run_sucess", suspendWhenHidden = F)
  
  # study map updates
  observeEvent(input$sim_seabird_tracks, {
    #Prep simulated tracks for rendering on maps by selecting subset and orienting in proper direction to study area
    #affine shift simulated tracks to center of study area
    # boundary_center <- st_as_sf(study_area()) %>% st_transform(3857) %>% st_centroid()

    if (input$n_boot < 100){
      showNotification(
        "Please choose bootstrap sample of at least 100.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$n_sim_birds < 5 | input$n_sim_birds > 1000){
      showNotification(
        "Please enter a number greater than 4 no more than 1000 for the simulated number of birds.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    start_sim_loc <- values$sim_pt_coords[which(values$sim_pt_coords$dir==input$cardinal_dir_sim), ]

    #shift and transform (The Leaflet package expects all point, line, and shape data to be specified in 
    # latitude and longitude using WGS 84 (a.k.a. EPSG:4326). By default, when displaying this data it projects everything to EPSG:3857 and expects that any map tiles are also displayed in EPSG:3857.)
    seabird_lines_geom <- st_geometry(seabird_lines_sf)

    seabird_lines_shifted <- seabird_lines_geom + st_coordinates(start_sim_loc)

    seabird_lines_shifted <- st_sf(st_set_geometry(seabird_lines_sf, seabird_lines_shifted), crs = 3857)

    #calculate bootstrap mean detection rate for simulated bird tracks with antenna pattern
    #Loop through each flight height to generate detection at flight height
    for (hgt in unique(sort(values$selected_stn_data_angles$flt_ht))){
      antennas_at_ht <- values$selected_stn_data_angles[which(values$selected_stn_data_angles$flt_ht==hgt), ]
      values$boot_mean_detection_CI <- rbind(values$boot_mean_detection_CI, cbind(taxa="seabirds", boot_mean_detection_CI(flt_ht=hgt, antenna_pattern = antennas_at_ht, 
                                                              simulated_birds = seabird_lines_shifted, n_birds = input$n_sim_birds, R = input$n_boot)))
    }

        #Prep single simulated tracks for rendering on maps by selecting subset and orienting in proper direction to study area
    seabird_lines_rand_shifted <- st_as_sf(seabird_lines_shifted[sample(1:nrow(seabird_lines_shifted), size = input$n_sim_birds, replace = T), ]) %>% 
      st_transform(4326)
    values$seabird_lines <- seabird_lines_rand_shifted
    values$seabird_sim = 1
    
    # need to account for clearing study area - map fails when we don't account for null spatial objects
    study_map_add <- leafletProxy("detectionmap") %>%
        addPolylines(data = seabird_lines_rand_shifted$geometry,
                     color = "purple",
                     group = "Simulated seabirds")
  }, ignoreInit = TRUE)

  
  # study map updates
  observeEvent(input$sim_shorebird_tracks, {
    #Prep simulated tracks for rendering on maps by selecting subset and orienting in proper direction to study area
    #affine shift simulated tracks to center of study area
    # boundary_center <- st_as_sf(study_area()) %>% st_transform(3857) %>% st_centroid()

    if (input$n_boot < 100){
      showNotification(
        "Please choose bootstrap sample of at least 100.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    if (input$n_sim_birds < 5 | input$n_sim_birds > 1000){
      showNotification(
        "Please enter a number greater than 4 no more than 1000 for the simulated number of birds.",
        duration = 6,
        type = "error"
      )
      return(NULL)
    }
    
    start_sim_loc <- values$sim_pt_coords[which(values$sim_pt_coords$dir==input$cardinal_dir_sim), ]
    
    #shift and transform (The Leaflet package expects all point, line, and shape data to be specified in 
    # latitude and longitude using WGS 84 (a.k.a. EPSG:4326). By default, when displaying this data it projects everything to EPSG:3857 and expects that any map tiles are also displayed in EPSG:3857.)
    shorebird_lines_geom <- st_geometry(shorebird_lines_sf)
    
    shorebird_lines_shifted <- shorebird_lines_geom + st_coordinates(start_sim_loc)
    
    shorebird_lines_shifted <- st_sf(st_set_geometry(shorebird_lines_sf, shorebird_lines_shifted), crs = 3857)
    
    #calculate bootstrap mean detection rate for simulated bird tracks with antenna pattern
    #Loop through each flight height to generate detection at flight height
    for (hgt in unique(sort(values$selected_stn_data_angles$flt_ht))){
      antennas_at_ht <- values$selected_stn_data_angles[which(values$selected_stn_data_angles$flt_ht==hgt), ]
      values$boot_mean_detection_CI <- rbind(values$boot_mean_detection_CI, cbind(taxa="shorebirds", boot_mean_detection_CI(flt_ht=hgt, antenna_pattern = antennas_at_ht, 
                                                                                   simulated_birds = shorebird_lines_shifted, n_birds = input$n_sim_birds, R = input$n_boot)))
    
      }
    
    #Prep single simulated tracks for rendering on maps by selecting subset and orienting in proper direction to study area
    shorebird_lines_rand_shifted <- st_as_sf(shorebird_lines_shifted[sample(1:nrow(shorebird_lines_shifted), size = input$n_sim_birds, replace = T), ]) %>% 
      st_transform(4326)
    
    values$shorebird_lines <- shorebird_lines_rand_shifted
    values$shorebird_sim = 1
    
    # need to account for clearing study area - map fails when we don't account for null spatial objects
    study_map_add <- leafletProxy("detectionmap") %>%
      addPolylines(data = shorebird_lines_rand_shifted$geometry,
                   color = "yellow",
                   group = "Simulated shorebirds"
      )
  }, ignoreInit = TRUE)
  
  output$sim_results <- 
    DT::renderDataTable(values$boot_mean_detection_CI,
                        colnames = c(
                          "Species group",
                          "Flight height (m)",
                          "Mean prop. detected",
                          "Lower 95% CI prop. detected",
                          "Upper 95% CI prop. detected"),
                        options = list(
                          paging = FALSE,
                          scrollY = "500px",
                          searching = FALSE
                        ),
                        rownames= FALSE
    )

  #Station data table
  output$stations <- DT::renderDataTable({
    req(values$all_stns_df)
    DT::datatable(
      values$all_stns_df[, c("ID", "lat", "long")],
      selection = "multiple",
      colnames = c("ID", "Latitude", "Longitude"),
      options = list(
        paging = FALSE,
        scrollY = "500px",
        searching = FALSE)) %>% 
      formatRound(c("lat", "long"), 5)
  }, server = TRUE)
  
  #selection stations table - editable to be able to specify starting antenna array angle
  # output$stations_selected <- DT::renderDataTable({
  #   DT::datatable(values$stations_selected, selection = "none", editable = T) %>% 
  #     formatRound(c("lat", "long"), 5) %>% formatRound(c("ant_angle_start"), 1)
  # }, server = TRUE)
  
  #use Handson table type which supports numeric validation on the fly so can check
  #also data editing and entry
  output$stations_selected <- renderRHandsontable({
    req(!is.null(values$stations_selected))
    rhandsontable(
      values$stations_selected[, c("ID", "lat", "long", "first_ant_angle")], #, "ant_angle_list")],
      readOnly = F,
      colHeaders = c("ID", "Latitude", "Longitude", "First antenna angle") #, "Antenn angle list")
    ) %>%
      hot_validate_numeric(
        cols = "First antenna angle",
        min = 0,
        # max = (360 / input$numInput_stnPars_antNum)
        # allow any starting angle and then the rest in increments of 360 / input$numInput_stnPars_antNum to be calculated from it
        # This allows more natural inputting of the first angle a user aligns to
        max = 360
        
      ) %>%
      hot_col("Longitude", format = "0.00000") %>%
      hot_col("Latitude", format = "0.00000") %>%
      hot_col("First antenna angle", format = "0.0")
  })
  

  #when table changed, need to reflect changes back using hot_to_r function
  observeEvent(
    input$stations_selected,{
    values$stations_selected <- hot_to_r(input$stations_selected)}
  )
  
  
  output$stn_detection_DT <-
    DT::renderDataTable(
      datatable(
        select_stns_by_hgt()[, c(
          "flt_ht",
          "max_r",
          "study_area_covered",
          "antenna_coverage_overlap", 
          "min_covg_flag")],
        colnames = c(
          "Flight height (m)",
          "Max detection range (m)",
          "Study area covg.",
          "Antenna coverage overlap",
          "Min. covg. flag"
        ),
        selection = list(mode = "single", selected = 1),
        options = list(
          paging = FALSE,
          scrollY = "500px",
          searching = FALSE
        ),
        rownames= FALSE
      ) %>%
        formatRound(columns = c(2), digits = 0) %>%
        formatRound(columns = c(3, 4), digits = 2) %>%
        formatStyle(
          "min_covg_flag",
          target = 'row',
          backgroundColor = styleEqual(c(0, 1), values = c('red', 'green'))
        )
      ,
      server = T
    )
  
  
  
  #add click event on row to have the optimized antenna beam pattern be displayed on map and data about the antennas shown
  observe({
    row_id <- input$stn_detection_DT_rows_selected
    
    if (!is.null(row_id)) {
      poly_data <- as.data.frame(select_stns_by_hgt()$angle_df[[row_id]])
      poly_data$geometry <- st_transform(poly_data$geometry, WGS84)
      #need to create as sf object (or sp object to use color palette and data)
      poly_data <- st_sf(poly_data)
      
      # Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
      #   Evaluation error: Found 3 features with invalid spherical geometry.
      # [1] Loop 0 is not valid: Edge 1998 is degenerate (duplicate vertex)
      #https://github.com/r-spatial/sf/issues/1762
      # Resolved by adding sf_use_sd
      sf_use_s2(FALSE)

      #calculate for display the centroids of the turbines
      poly_center_sf <- poly_data %>% 
        dplyr::group_by(station) %>% 
        dplyr::summarize(geometry = st_union(geometry)) %>%
        st_sf() %>% 
        st_centroid()
      
      if (input$selInput_optimPars_antcolors=="psychedelic"){
        pal = colorFactor(colorRamp(c('yellow','red','green', 'magenta','purple', 'cyan')), poly_data$theta) #station for whole stn color
      } else {
        pal = colorFactor(colorRamp(c(input$selInput_optimPars_antcolors)), poly_data$theta)}
      leafletProxy("detectionmap") %>%
        clearGroup(c("antenna_beam")) %>%
        clearGroup(c("selected_stn")) %>%
        addPolygons(
          data = poly_data,
          color = ~pal(theta), #station for whole stn color
          fill = ~pal(theta), #station for whole stn color
          fillOpacity = 0.25,
          weight = 2,
          group = "antenna_beam"
        ) %>%
        addMarkers(
          data = poly_center_sf,
          icon = turbine_icon_red,
          group = "selected_stn")

      output$angles <-
        DT::renderDataTable(datatable(
          select_stns_by_hgt()$angle_df[[row_id]][, c("station", "theta")],
          colnames = c("Station #", "Antenna angle"),
          options = list(
            paging = FALSE,
            scrollY = "500px",
            searching = FALSE
          ),
          rownames= FALSE
        ),
        server = F)
    }
  })
  
  
  # select table rows for setting as antenna stations - flag on map
  observe({
    if (input$op_type == "manual") {
      values$stations_selected <-
        values$all_stns_df[input$stations_rows_selected,]
    }
  })
  
  observeEvent(values$stations_selected, {
    leafletProxy('studymap') %>%
      clearGroup(c("selected_stns")) %>%
      addMarkers(
        data = values$stations_selected,
        lng =  ~ long,
        lat =  ~ lat,
        layerId =  ~ ID,
        group = "selected_stns",
        options = list(
          searching = FALSE)
      )
  })
  
  observeEvent(input$studymap_marker_click, {
    if (input$studymap_marker_click$group == "Station locations") {
      if (input$op_type == "manual") {
        previously_selected_IDs <- values$stations_selected$ID
        
        clickID <- input$studymap_marker_click[["id"]]
        if (clickID %in% previously_selected_IDs) {
          #remove from selected rows
          values$stations_selected <-
            values$stations_selected[which(values$stations_selected$ID %in% setdiff(previously_selected_IDs, clickID)), ]
        } else {
          #add to selected data
          #TODO - need to keep any changed antenna angles when adding a new location!
          values$stations_selected <- rbind(values$stations_selected, values$all_stns_df[which(values$all_stns_df$ID == clickID), ])
          # values$stations_selected <-
          #   values$all_stns_df[which(values$all_stns_df$ID %in% c(previously_selected_IDs, clickID)), ]
        }
        
        # #remove from list of selected IDs if already selected, else add
        
        # isolate(if(is.null(values$clickIDs)){
        #   values$clickIDs <- c(clickID)
        # } else {
        #   if (clickID %in% values$clickIDs){
        #     values$clickIDs <- setdiff(values$clickIDs, clickID)
        #     } else values$clickIDs <- c(values$clickIDs, clickID)}
        # )
        # #
        # selected_map_stns <- values$all_stns_df[values$all_stns_df$ID %in% values$clickIDs, ]
        # selected_map_stns <- setdiff(values$clickIDs, values$stations_selected$ID)
        
        # leafletProxy('studymap')%>%
        #   clearGroup(c("selected_map_stns")) %>%
        #   addMarkers(data=selected_map_stns, lng=~long, lat=~lat,
        #              layerId=~ID, group="selected_map_stns")
        
        dataTableProxy("stations") %>%
          selectRows(which(values$all_stns_df$ID %in% values$stations_selected$ID))
      }
    }
  })
  
  
  # download handler for raw results download
  output$downloadData <-
    downloadHandler(
      filename = paste0('IDIOMS_output_', strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.zip'),
      content = function(fname) {
        IDIOMS_selected_stn_data_angles <- isolate(values$selected_stn_data_angles)
        #remove geometry and store to store as csv data
        IDIOMS_selected_stn_data_angles_df <- IDIOMS_selected_stn_data_angles %>% data.frame() %>% select(-c(geometry))
        IDIOMS_input_pars <- input_pars()
        IDIOMS_study_boundary <- st_as_sf(values$study_boundary, crs=4326) %>% st_transform(3857)
        tmpdir = tempdir()
        fnames4zip1 <- list()

        save(IDIOMS_input_pars, file =  file.path(tmpdir, "IDIOMS_input_pars.RData"))
        save(IDIOMS_study_boundary, file = file.path(tmpdir, "IDIOMS_study_boundary.RData"))
        #create shapefiles of output as well for easy mapping
        sf::write_sf(IDIOMS_study_boundary, dsn = file.path(tmpdir, "IDIOMS_study_boundary.shp"))
        save(IDIOMS_selected_stn_data_angles, file =  file.path(tmpdir, "IDIOMS_selected_stn_data_angles.RData"))
        sf::write_sf(IDIOMS_selected_stn_data_angles, dsn =  file.path(tmpdir, "IDIOMS_selected_stn_data_angles.shp"))
        write.csv(IDIOMS_selected_stn_data_angles_df, file =  file.path(tmpdir, "IDIOMS_selected_stn_data_angles.csv"), row.names = F)
        write.csv(values$select_stns_covg_by_hgt, file =  file.path(tmpdir, "IDIOMS_select_stns_covg_by_hgt.csv"), row.names = F)

        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_input_pars.RData"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_study_boundary.RData"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_study_boundary.shp"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_study_boundary.shx"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_study_boundary.dbf"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_study_boundary.prj"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.csv"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.RData"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.shp"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.shx"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.dbf"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.prj"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_selected_stn_data_angles.csv"))
        fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_select_stns_covg_by_hgt.csv"))
        
        if(values$seabird_sim + values$shorebird_sim >0){
          write.csv(values$boot_mean_detection_CI, file =  file.path(tmpdir, "IDIOMS_simulated_bird_track_detections.csv"), row.names = F)
          fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, "IDIOMS_simulated_bird_track_detections.csv"))
        }
        
        # utils::zip(zipfile=fname, files=unlist(c(fnames4zip1, fnames4zip2, fnames4zip3, fnames4zip4, fnames4zip5)), flags = "-r9Xj")
        utils::zip(zipfile=fname, files=unlist(c(fnames4zip1)), flags = "-r9Xj")
        
      },
      contentType = "application/zip"
    )
  
  # download handler for report using R Markdown
  output$downloadReport <- downloadHandler(
    # for PDF output, change this to "report.pdf"
    filename = paste0("IDIOMS_report_", strftime(isolate(run_times$end), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      # copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      input_data <- input_pars()
      study_boundary <-  st_as_sf(values$study_boundary, crs=4326)
      simulated_tracks <- data.frame()
      if(values$seabird_sim + values$shorebird_sim > 0){
        simulated_tracks <- values$boot_mean_detection_CI
      }
      
      tempReport <- file.path(tempdir(), "report_IDIOMS.Rmd")
      img1 <- file.path(tempdir(), "IDIOMS_logo_final_2_large_print.png")
      img2 <- file.path(tempdir(), "BRI_color_logo_no_words.png")
      img3 <- file.path(tempdir(), "USFWS.png")
      img4 <- file.path(tempdir(), "NYSERDA.png")
      
      file.copy("data/report_IDIOMS.Rmd", tempReport, overwrite = TRUE)
      # need to copy images to temp dir otherwise can't be found 
      # see: (https://stackoverflow.com/questions/35800883/using-image-in-r-markdown-report-downloaded-from-shiny-app?rq=1)
      file.copy("www/IDIOMS_logo_final_2_large_print.png", img1, overwrite = TRUE)
      file.copy("www/BRI_color_logo_no_words.png", img2, overwrite = TRUE)
      file.copy("www/USFWS.png", img3, overwrite = TRUE)
      file.copy("www/NYSERDA.png", img4, overwrite = TRUE)
      
      # set up parameters to pass to Rmd document
      params <- list(IDIOMS_version = IDIOMS_version, project = input$project_name, modeler = input$modeler, run_start_time = run_times$start, 
                     run_end_time = run_times$end, input_pars = input_data, study_boundary = study_boundary, study_covg = values$select_stns_covg_by_hgt,
                     output_df = isolate(values$selected_stn_data_angles), option = input$op_type, simulated_tracks = simulated_tracks, 
                     seabird_sims = values$seabird_lines, shorebird_sims = values$shorebird_lines, 
                     MOTUS_receivers_active = MOTUS_receiver_antennas_active_sf_merc)
      # save(params, file = "data/params.Rdata")
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # Below works for HTML files, not pdf
      # rmarkdown::render(tempReport, output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv())
      # )
      # can't render to PDF - error with latexpdf. Found this solution: 
      # https://stackoverflow.com/questions/66056764/knitr-cannot-find-pdflatex-when-creating-pdf-from-shiny-app
      # "ou should NOT specify the output_file argument in render() Instead you need to rename the file AFTER rendering."
      
      out <- rmarkdown::render(tempReport,
                               params = params,
                               envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
    # contentType = "application/pdf"
  )
  
  ################UI buttons############
  
  # when results are available, render button for simulating seabird tracks
  observeEvent({req(!is.null(select_stns_by_hgt()))}, {
    output$sim_seabirds_btn <- renderUI({
      actionButton("sim_seabird_tracks", HTML("Simulate seabird <br/> tracks"), 
                   style = "margin: 0px; background-color: #566573; color: white; font-weight: bold;")
    })
  })
  
  # when results are available, render button for simulating shorebirds tracks
  observeEvent({req(!is.null(select_stns_by_hgt()))}, {
    output$sim_shorebirds_btn <- renderUI({
      actionButton('sim_shorebird_tracks', HTML("Simulate shorebird <br/> tracks"), 
                     style = "margin: 0px; background-color: #566573; color: white; font-weight: bold;")
    })
  })
  
    # if results are available, render button for downloading data
  observeEvent({req(!is.null(select_stns_by_hgt()))}, {
    output$download_output_btn <- renderUI({
      downloadButton("downloadData", HTML("Download model <br/> results"), 
                     style = "margin: 0px; background-color: #566573; color: white; font-weight: bold;")
    })
  })
  
  # if results are available, render button for downloading data
  observeEvent({req(!is.null(select_stns_by_hgt()))}, {
    output$gen_report_btn <- renderUI({
      downloadButton("downloadReport", HTML("Download results <br/> report"), 
                     style = "margin: 0px; background-color: #566573; color: white; font-weight: bold;")
      # style = "margin-left: 0px; margin-top: 0px; background-color: maroon; color: white; font-weight: bold;")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
