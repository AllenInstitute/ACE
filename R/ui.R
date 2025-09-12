suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(feather)
  library(DT)
  library(rbokeh)
  library(shinydashboard)
  #library(googleAnalyticsR)  # Some errors in dependencies if I include this, but also not needed yet
})

# Define UI for application that draws a histogram
ui <- function(request) {   # Note that I might need to remove "function(request)" for Google Analytics to work.  Revisit later if this breaks anything.
  dashboardPage(
    
    dashboardHeader(title = "Annotation Comparison Explorer (ACE)", titleWidth = 400),
    
    dashboardSidebar(title = tags$img(src='ACE_logo.png', width = '225', style= 'display: block; margin-left: auto; margin-right: auto;'), #), #disable = TRUE
    
      width = 300,
                  
      h3("What is ACE?"),
      p("Annotation Comparison Explorer (ACE) is a versitile application for comparison of two or more annotations such as (i) cell type assignments (e.g., from different mapping/clustering algorithms), (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics). Several example annotation tables are included, or you can point it at your own files."),
      p(tags$i("Click the three lines next to the title above to minimize this sidebar.")),
      p(tags$i("Note: ACE must be reloaded if left idle for 10 minutes.")),
      #br(),
      # h4("Bookmarking"),
      # p("To get the current state of the app, push 'Get state' and copy text. To set the state, paste text into the 'Set state' box."),
      # actionButton(inputId = "bookmark_url", label="Bookmark (BROKEN)",
      #              icon = icon("link", lib = "glyphicon")
      # ),
      # verbatimTextOutput("show_url"),
      # uiOutput("state_textbox"),
      # h4("More help"),
      
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
               ),
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
                     href="mailto:jeremym@alleninstitute.org?
                                  body=''
                                  &subject='Annotation Comparison' app comments")
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
      tags$head(includeHTML("google-analytics.html"),  # Tag for general Google Analytics!
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
                          ')),
      
      #useShinyjs(),  # shinyjs not currently used
      
      fluidRow(width = 12,
               
               box(title = "Upload or select data set",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
                     column(11, offset=0.5,
                     p("Upload your own table(s) using the buttons -OR- select a category and a comparison table from the boxes below. After files are selected, please WAIT for the annotation table to load, which could take up to a minute after which the controls will become responsive. Once a data set is chosen, this pane can be minimized with the '-' in the upper right if desired.")
                     ),
                     column(4,  # I can't get bookmarking to work with categories, but I can get it CLOSE with the nested list approach. If I eventually do get it working, then revisit the nested list approach, but for now, I like the categories better.
                            uiOutput("select_category")
                     ),
                     column(4,
                            uiOutput("select_textbox")
                     ),
                     #column(1,
                     #        bookmarkButton(label="Bookmark (BROKEN)")  # This button does nothing when clicked but SHOULD pop open the URL to copy
                     # )
                     # NEW - start
                     #column(2,#4  # Maybe remove this since it's broken
                    #        actionButton(inputId = "bookmark_url", label="Bookmark (BROKEN)",
                    #                     icon = icon("link", lib = "glyphicon")
                    #        ),
                    #        verbatimTextOutput("show_url")
                    # ),
                     # NEW - END
                   ),
                   fluidRow(
                     column(3,
                            fileInput("database_upload", "UPLOAD or")
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
                            fileInput("metadata_upload", "UPLOAD or")
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
                     column(12,
                            uiOutput("summary_text")
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
                                       suppressWarnings({ # Gives a warning about tags$iframe, which is the WRONG way to do it.
                                         includeHTML("www/vis_info_panel.html") 
                                       })
                                )
                              )
                     ),
                     tabPanel("Compare pairs of annotations",
                              # box(title = "Annotation Comparisons",
                              #     solidHeader = TRUE, status = "primary", width = 12,
                              #     collapsible = TRUE, collapsed = TRUE,
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
                                #column(1,
                                #       uiOutput("annocomp_select_mode_selection")
                                #),
                                column(1,
                                       uiOutput("annocomp_width_textbox")
                                ),
                                column(1,
                                       uiOutput("annocomp_height_textbox")
                                ),
                                #conditionalPanel(  # Add this back for a "Go" button
                                #  condition = "button_press_needed() == false",
                                #  column(1,
                                #       br(),
                                #       actionButton("anno_go","GO!",
                                #                    style="color: #fff; background-color: #EC008C; border-color: #BE1E2D; font-weight: bold;")
                                #  )
                                #)
                              ),
                              fluidRow(
                                column(12,
                                       uiOutput("annocomp_plot_ui")
                                )
                              ),
                              fluidRow(
                                column(6,
                                       strong("Plot download Options"),
                                       fluidRow(
                                         column(4,textInput("annocomp_dlw","Width (in)",8)),
                                         column(4,textInput("annocomp_dlh","Height (in)",4)),
                                         column(4,textInput("annocomp_dlf","Font (pt)",6))
                                       ),
                                       fluidRow(
                                         column(5,uiOutput("annocomp_downloadButton")),
                                         column(5,downloadButton("data_csv","Download Plot Data"))
                                       ),
                                ),
                                
                              )
                     ),
                     tabPanel("Link 2+ annotations (river plots)",
                              fluidRow(
                                column(11,
                                       uiOutput("river_group_selection")
                                )#,  # Uncomment this comma and the next four lines to return the "Go" button for river plots
                                #column(1,
                                #       br(),
                                #       actionButton("river_go","GO!",
                                #                    style="color: #fff; background-color: #EC008C; border-color: #BE1E2D; font-weight: bold;")
                                #)
                              ),
                              fluidRow(
                                column(12,
                                       uiOutput("river_plot_ui"),
                                       plotOutput("river_widthfinder",width = "100%",
                                                  height = "10px")
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
                              #fluidRow(  # This message isn't working properly, so I've removed it.  It also shouldn't be needed anymore
                              #  column(12,
                              #         helpText("If you see this message, enter a space in the 'Location of metadata (e.g., cluster) information (optional; csv file)' box above, or provide a metadata table.")
                              #  )
                              #),
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
                                downloadButton("download_explorer_table", 
                                               "Download Table Data",
                                               style = "width: 175px; margin-left: 15px;")
                              ),
                              fluidRow(
                                width=12, br(), 
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
                     tabPanel("Compare numeric annotations", #id="scatterplot",  # NOTE: This should be a conditional panel that only appears when at least 2 numeric variables are present.  I have a workaround in server.r for now.
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
                                                                        HTML("<strong>Numeric annotation 1 <span style='color: red;'>(Red)</span></strong>"),
                                                                        uiOutput("scatter_color_gene_red_textbox"),
                                                                        HTML("<strong>Numeric annotation 2 <span style='color: green;'>(Green)</span></strong>"),
                                                                        uiOutput("scatter_color_gene_green_textbox"),
                                                                        HTML("<strong>Numeric annotation 3 <span style='color: blue;'>(Blue)</span></strong>"),
                                                                        uiOutput("scatter_color_gene_blue_textbox")
                                                                )
                                                        ),
                                                        fluidRow(
                                                          column(12, uiOutput("selectize_scatter_plot_scaling") )
                                                        )
                                       ),
                                       uiOutput("scatter_plot_hover_selectize"),
                                       textOutput("scatter_plot_hover_warning"),
                                       actionButton("scatter_plot_go","GO!",
                                                    style="color: #fff; background-color: #EC008C; border-color: #BE1E2D; font-weight: bold;"),
                                       p("  ^ Click 'GO!' after making updates to refresh display."),
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

      fluidRow(width = 12, br(), br())
      
    )
  )
  
}