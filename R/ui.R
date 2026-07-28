suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(feather)
  library(DT)
  library(rbokeh)
  library(shinydashboard)
  #library(googleAnalyticsR) # Some errors in dependencies if I include this, but also not needed yet
})

# Define UI for application that draws a histogram
ui <- function(request) {   # Note that I might need to remove "function(request)" for Google Analytics to work.  Revisit later if this breaks anything.
  dashboardPage(
    
    dashboardHeader(
      title = tags$a(
        href = "https://alleninstitute.org",
        target = "_blank",
        tags$img(src = "allen_institute_logo.svg",
                 alt = "Allen Institute",
                 height = "30")
      ),
      titleWidth = 300,
      
      tags$li(
        class = "dropdown ace-app-title",
        tags$span("Annotation Comparison Explorer (ACE)")
      ),
      
      tags$li(
        class = "dropdown ace-nav-link",
        tags$a(
          href = "https://brain-map.org/consortia/sea-ad",
          target = "_blank",
          icon("brain", lib = "font-awesome"), " SEA-AD.org"
        )
      ),
      
      tags$li(
        class = "dropdown ace-nav-link",
        tags$a(
          href = "https://brain-map.org",
          target = "_blank",
          icon("globe"), " Brain Map"
        )
      )
    ),
    
    dashboardSidebar(title = tags$img(src='ACE_logo.png', width = '225', style= 'display: block; margin-left: auto; margin-right: auto;'), #), #disable = TRUE
                     
                     width = 300,
                     br(),
                     actionButton(
                       "open_video_btn",
                       HTML("New to ACE?<br>Watch this overview!"),
                       icon = icon("video"),
                       style = "font-size: 125%; padding: 5px 5px;
                                width: 225px; /* Give it a specific width so margin: auto can work */
                                display: block; /* Make it a block element */
                                margin: 0 auto; /* Center the block element horizontally */
                                text-align: center;
                                white-space: normal;",
                       onclick = paste0("window.open('https://alleninstitute.github.io/ACE/ACE_overview.mp4', '_blank');")
                     ),
                     h3("What is ACE?"),
                     p("Annotation Comparison Explorer (ACE) is a versitile application for comparison of two or more annotations such as (i) cell type assignments (e.g., from different mapping/clustering algorithms), (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics). Several example annotation tables are included, or you can point it at your own files."),
                     p(tags$i("Click the three lines next to the title above to minimize this sidebar.")),
                     p(tags$i("Note: ACE must be reloaded if left idle for 10 minutes.")),
                     
                     h3("Get started"),
                     p("We provide multiple entry points into ACE, from short use case videos to an extensive user guide."),
                     fluidRow(
                       column(5,
                              actionButton(inputId = "tutorial",
                                           icon = icon("file-video", lib = "font-awesome"),
                                           a(" WEBINAR",
                                             style="color: #000000; border-color: #2e6da4",
                                             target = "_blank",
                                             href="https://www.youtube.com/watch?v=csxRkTgP50k")
                              )
                       ),
                       column(5,
                              actionButton(inputId = "usecase",
                                           icon = icon("circle-play", lib = "font-awesome"),
                                           a("USE CASES",
                                             style="color: #000000; border-color: #2e6da4",
                                             target = "_blank",
                                             href="https://alleninstitute.github.io/HMBA_BasalGanglia_ACE/")
                              )
                       )
                     ),
                     fluidRow(
                       column(5,
                              actionButton(inputId = "manuscript",
                                           icon = icon("book", lib = "font-awesome"),
                                           a("PREPRINT",
                                             style="color: #000000; border-color: #2e6da4",
                                             target = "_blank",
                                             href="https://doi.org/10.1101/2025.02.11.637559")
                              )
                       ),
                       column(5,
                              actionButton(inputId = "guide",
                                           icon = icon("hand-spock", lib = "font-awesome"),
                                           a("USER GUIDE",
                                             style="color: #000000; border-color: #2e6da4",
                                             target = "_blank",
                                             href="https://github.com/AllenInstitute/ACE/blob/main/ACE_User_Guide.pdf")
                              )
                       )
                     ),
                     h3("Related tools"),
                     p("The Allen Institute provides additional tools for assigning cell type names to user data (MapMyCells) and visualizing single cell and spatial -omics data across the mammalian brain (ABC Atlas)."),
                     actionButton(inputId = "mapmycellsWithAce",
                                  icon = icon(
                                    name = NULL,
                                    style = "
                                      background: url('mapmycells-icon-black.png');
                                      background-size: contain;
                                      background-position: center;
                                      background-repeat: no-repeat;
                                      height: 20px;
                                      width: 20px;
                                      display: inline-block;
                                    "
                                  ),
                                  a("Using ACE with MapMyCells",
                                    style="color: #000000; border-color: #2e6da4",
                                    target = "_blank",
                                    href="https://portal.brain-map.org/atlases-and-data/bkp/mapmycells/step-by-step-guide")
                     ),
                     actionButton(inputId = "abcAtlas",
                                  icon = icon(
                                    name = NULL,
                                    style = "
                                      background: url('abc-atlas-icon.png');
                                      background-size: contain;
                                      background-position: center;
                                      background-repeat: no-repeat;
                                      height: 20px;
                                      width: 20px;
                                      display: inline-block;
                                    "
                                  ),
                                  a("Visualizing data with ABC Atlas",
                                    style="color: #000000; border-color: #2e6da4",
                                    target = "_blank",
                                    href="https://portal.brain-map.org/atlases-and-data/bkp/abc-atlas")
                     ),
                     h3("Contribute"),
                     p("If you would like to contribute to this app, please reach out via email or on GitHub."),
                     actionButton(inputId = "email1",
                                  icon = icon("envelope", lib = "font-awesome"),
                                  a("PROVIDE FEEDBACK",
                                    style="color: #000000; border-color: #2e6da4",
                                    href="mailto:jeremym@alleninstitute.org?body=''&subject='Annotation Comparison' app comments")
                     ),
                     actionButton(inputId = "GitHub",
                                  icon = icon("code", lib = "font-awesome"),
                                  a("ACCESS SOURCE CODE",
                                    style="color: #000000; border-color: #2e6da4",
                                    target = "_blank",
                                    href="https://github.com/AllenInstitute/ACE/")
                     ),
                     h3("Acknowledgements"),
                     p("App developed by Jeremy Miller with support from Aaron Oster and Bosiljka Tasic, using some original code developed by Lucas Graybuck. Included annotation tables created by Jeremy Miller, Kyle Travaglini, Tain Luquez, Rachel Hostetler, and Vilas Menon. Logo credit: Lauren Alfiler.")
    ),
    
    dashboardBody(
      
      shinyjs::useShinyjs(),
      
      tags$head(
        tags$title("ACE"),
        tags$link(rel = "icon", type = "image/x-icon", href = "ACE.ico"),
        tags$script(HTML("document.title = 'ACE';")),
        
        tags$style(HTML("
          /* ==========================================================
             HEADER: NAVBAR BASICS
             ========================================================== */
          .skin-blue .main-header .logo,
          .skin-blue .main-header .navbar {
            background-color: #252525 !important;
          }

          .skin-blue .main-header .logo:hover,
          .skin-blue .main-header .navbar .sidebar-toggle:hover {
            background-color: rgba(255,255,255,0.15) !important;
          }

          .main-header .logo {
            display: flex !important;
            align-items: center;
            justify-content: center;
            height: 60px !important;
            padding: 0 18px !important;
          }

          .main-header .navbar,
          .main-header .sidebar-toggle {
            min-height: 60px !important;
            height: 60px !important;
          }

          .main-header .sidebar-toggle {
            display: flex !important;
            align-items: center;
            padding: 0 18px !important;
            color: #ffffff !important;
          }

          .main-header .navbar .nav > li > a {
            color: #ffffff !important;
            padding-top: 20px !important;
            padding-bottom: 20px !important;
            line-height: 20px !important;
          }

          /* Make the FULL header the positioning context, not just the navbar */
          .main-header {
            position: relative;
          }

          .main-header .navbar {
            position: static;
            overflow: visible;
          }

          /* ==========================================================
             HEADER: CENTERED TITLE (scrolls with header)
             ========================================================== */
          .ace-app-title {
            position: absolute !important;
            top: 0;
            left: 50%;
            transform: translateX(-50%);
            height: 60px;
            line-height: 60px;
            z-index: 900;
            margin: 0;
            padding: 0;
            color: #ffffff;
            font-size: 24px;
            font-weight: 700;
            white-space: nowrap;
            pointer-events: none;
            max-width: 60vw;
            overflow: hidden;
            text-overflow: ellipsis;
          }

          .ace-app-title > span {
            display: block;
          }

          /* ==========================================================
             HEADER: RIGHT-SIDE NAV LINKS
             ========================================================== */
          .ace-nav-link > a {
            font-size: 14px;
            border-radius: 3px;
          }

          .ace-nav-link > a:hover {
            background-color: rgba(255,255,255,0.15) !important;
          }

          @media (max-width: 900px) {
            .ace-app-title {
              font-size: 16px;
              max-width: calc(100vw - 200px);
            }
            .ace-nav-link {
              display: none !important;
            }
          }

          /* ==========================================================
             LAYOUT SPACING
             ========================================================== */
          .content-wrapper,
          .right-side {
            padding-top: 0px;
          }

          .main-sidebar,
          .left-side {
            padding-top: 70px !important;
          }

          /* ==========================================================
             SIDEBAR
             ========================================================== */
          .main-sidebar {
            background-color: #0E3D5A !important;
          }

          /* ==========================================================
             BOX HEADERS
             ========================================================== */
          .box.box-solid.box-primary > .box-header,
          .box.box-solid.box-primary > .box-header .box-title,
          .box.box-solid.box-primary > .box-header a {
            background-color: #000000 !important;
            color: #ffffff !important;
          }

          .box.box-solid.box-primary {
            border-color: #000000 !important;
          }
        ")),
        
        includeHTML("google-analytics.html"),  # Tag for general Google Analytics!
        tags$script('var dimension = [0, 0];
                      $(document).on("shiny:connected", function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                      });
                      $(window).resize(function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                      });
                      ')
      ),
      
      #useShinyjs(),  # shinyjs not currently used
      
      fluidRow(width = 12,
               
               box(title = "Upload or select data set",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
                     column(11, offset=0.5,
                            p("Upload your own table(s) using the buttons -OR- select a category and a comparison table from the boxes below. After files are selected, please WAIT for the annotation table to load, which could take up to a minute after which the controls will become responsive. Once a data set is chosen, this pane can be minimized with the '-' in the upper right if desired.")
                     ),
                     column(4,
                            uiOutput("select_category")
                     ),
                     column(4,
                            uiOutput("select_textbox")
                     )
                   ),
                   fluidRow(
                     column(3,
                            fileInput("database_upload", "UPLOAD")
                     ),
                     column(7,
                            uiOutput("database_textbox")
                     ),
                     column(2,
                            uiOutput("checkInput")
                     )
                   ),
                   fluidRow(
                     column(3,
                            fileInput("metadata_upload", "UPLOAD")
                     ),
                     column(7,
                            uiOutput("metadata_textbox")
                     ),
                     column(2,
                            uiOutput("metadata_checkInput")
                     )
                   ),
                   fluidRow(
                     column(11,
                            uiOutput("dataset_description")
                     )
                   ),
               ),
               
               box(title = "Filter cells in dataset",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
                     column(3,
                            uiOutput("filter_selection")
                     ),
                     conditionalPanel(
                       condition = "output.sf_active == true",
                       column(7,
                              strong("Filter for:"),
                              br(),
                              uiOutput("filter_panel")
                       )
                     ),
                     conditionalPanel(
                       condition = "output.sf_active == true",
                       column(2,
                              uiOutput("filter_invert")
                       )
                     )
                   ),
                   fluidRow(
                     column(9,
                            uiOutput("summary_text")
                     ),
                     conditionalPanel(
                       condition = "output.sf_active == true",
                       column(3,
                              p("Filters misbehaving?"),
                              actionButton(
                                "refresh_btn",
                                "Reset App",
                                icon = icon("redo"),
                                onclick = "window.location.reload(true);",
                                style = "background-color: #f8d7da; color: #721c24; border-color: #f5c6cb;"
                              ),
                       )
                     )
                   )
               ),
               
               box(title = "Visualizations and statistics",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE, color = "green",
                   tabsetPanel(id = "visualizations",
                               tabPanel("Intro",
                                        fluidRow(
                                          column(12,
                                                 suppressWarnings({  # Gives a warning about tags$iframe, which is the WRONG way to do it.
                                                   includeHTML("www/vis_info_panel.html")
                                                 })
                                          )
                                        )
                               ),
                               tabPanel("Compare pairs of annotations",
                                        fluidRow(
                                          column(2,
                                                 uiOutput("annocomp_x_selection")
                                          ),
                                          column(2,
                                                 uiOutput("annocomp_y_selection")
                                          ),
                                          column(2,
                                                 uiOutput("annocomp_color_selection")
                                          ),
                                          column(2,
                                                 uiOutput("annocomp_denom_selection")
                                          ),
                                          column(1,
                                                 uiOutput("annocomp_reorderY_selection")
                                          ),
                                          column(1,
                                                 uiOutput("annocomp_width_textbox")
                                          ),
                                          column(1,
                                                 uiOutput("annocomp_height_textbox")
                                          ),
                                        ),
                                        fluidRow(
                                          column(12,
                                                 uiOutput("annocomp_plot_ui")
                                          )
                                        ),
                                        fluidRow(
                                          column(7,
                                                 strong("Plot download Options"),
                                                 fluidRow(
                                                   column(4,textInput("annocomp_dlw","Width (in)",8)),
                                                   column(4,textInput("annocomp_dlh","Height (in)",4)),
                                                   column(4,textInput("annocomp_dlf","Font (pt)",6))
                                                 ),
                                                 fluidRow(
                                                   column(4,uiOutput("annocomp_downloadButton")),
                                                   column(4,downloadButton("data_csv","Download Plot Data")),
                                                   column(4,actionButton("do_stats", "Compute statistics"))
                                                 ),
                                          ),
                                          column(5,conditionalPanel(condition = "output.showStatistics == true",
                                                                    div(style = "font-size: 1.5em;", textOutput("stats_result")))
                                          ),
                                        )
                               ),
                               tabPanel("Link 2+ annotations (river plots)",
                                        fluidRow(
                                          column(11,
                                                 uiOutput("river_group_selection")
                                          )
                                        ),
                                        p("NOTE: do not resize your browser window while a river plot is showing or ACE may freeze. Instead, try downloading plots with different sizes below."),
                                        fluidRow(
                                          column(12,
                                                 uiOutput("river_plot_ui"),
                                                 plotOutput("river_widthfinder",width = "100%", height = "1px")
                                          )
                                        ),
                                        fluidRow(
                                          column(4,
                                                 strong("Download Options"),
                                                 fluidRow(
                                                   column(4,textInput("dlw","Width (in)",12)),
                                                   column(4,textInput("dlh","Height (in)",8)),
                                                   column(4,textInput("dlf","Font (pt)",10))),
                                                 downloadButton('downloadRiverPDF',"Download PDF"))
                                        )
                               ),
                               tabPanel("Explore individual annotations",
                                        fluidRow(
                                          column(2,
                                                 uiOutput("explorer_group_selection")
                                          ),
                                          column(2,
                                                 uiOutput("explorer_annotation_selection")
                                          ),
                                          column(3,
                                                 uiOutput("explorer_comparison_selection")
                                          ),
                                          column(2,
                                                 uiOutput("explorer_plot_type_selection")
                                          ),
                                          column(1,
                                                 uiOutput("explorer_maxtypes_textbox")
                                          ),
                                          column(2,
                                                 uiOutput("explorer_height_textbox")
                                          )
                                        ),
                                        fluidRow(
                                          column(12,
                                                 dataTableOutput("explorer_table")
                                          ),
                                        ),
                                        fluidRow(
                                          column(8,
                                                 downloadButton("download_explorer_table", "Download Table Data", style = "width: 175px; margin-left: 15px;")
                                          ),
                                          column(4,
                                                 p("For provided tables, directionality is defined in the relevant published manuscript.")
                                          ),
                                        ),
                                        fluidRow(
                                          width=12,
                                          br(),
                                        ),
                                        fluidRow(
                                          column(12,
                                                 uiOutput("explorer_box_ui")
                                          )
                                        ),
                                        fluidRow(
                                          column(12,
                                                 dataTableOutput("selected_cluster_table")
                                          )
                                        )
                               ),
                               tabPanel("Compare numeric annotations",
                                        fluidRow(
                                          column(4,
                                                 uiOutput("scatter_x_selection"),
                                                 uiOutput("scatter_y_selection"),
                                                 uiOutput("radio_show_filtered_data"),
                                                 textInput("scatter_pt_size","Point Size",6),
                                                 uiOutput("radio_scatter_color_type"),
                                                 conditionalPanel(condition = "input.scatter_color_type == 'Categoric Annotation'",
                                                                  uiOutput("selectize_scatter_plot_color")
                                                 ),
                                                 conditionalPanel(condition = "input.scatter_color_type == 'Numeric Annotations'",
                                                                  fluidRow(column(12,
                                                                                  HTML("Numeric annotation 1 (Red)"),
                                                                                  uiOutput("scatter_color_gene_red_textbox"),
                                                                                  HTML("Numeric annotation 2 (Green)"),
                                                                                  uiOutput("scatter_color_gene_green_textbox"),
                                                                                  HTML("Numeric annotation 3 (Blue)"),
                                                                                  uiOutput("scatter_color_gene_blue_textbox")
                                                                  )
                                                                  ),
                                                                  fluidRow(
                                                                    column(12,
                                                                           uiOutput("selectize_scatter_plot_scaling")
                                                                    )
                                                                  )
                                                 ),
                                                 uiOutput("scatter_plot_hover_selectize"),
                                                 textOutput("scatter_plot_hover_warning"),
                                                 actionButton("scatter_plot_go","GO!",
                                                              style="color: #fff; background-color: #EC008C; border-color: #BE1E2D; font-weight: bold;"),
                                                 p(" ^ Click 'GO!' after making updates to refresh display."),
                                                 fluidRow(
                                                   column(5,downloadButton("scatterplot_data_csv","Download Plot Data")),
                                                   column(7,actionButton("do_corr", "Compute correlation"))
                                                 ),
                                                 br(),
                                                 conditionalPanel(condition = "output.showCorrelation == true",
                                                                  div(style = "font-size: 1.5em;", textOutput("corr_result")))
                                          ),
                                          column(8,
                                                 uiOutput("scatter_plot_ui"),
                                                 plotOutput("scatter_widthfinder",width = "100%",height = "10px")
                                          )
                                        )
                               )
                   )
               )
      ),
      
      fluidRow(width = 12, br(), br()),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    )
  )
}