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
ui <- #function(request) {   # Note that I needed to remove "function(request)" for Google Analytics to work.  Revisit later if this breaks anything.
  dashboardPage(
    
    dashboardHeader(title = "Annotation Comparison Explorer (ACE)", titleWidth = 400),
    
    dashboardSidebar(title = tags$img(src='ACE_logo.png', width = '225', style= 'display: block; margin-left: auto; margin-right: auto;'), #), #disable = TRUE
    
      width = 250,
                  
      h3("What is ACE?"),
      p("Annotation Comparison Explorer (ACE) is a versitile application for comparison of two or more annotations such as (i) cell type assignments (e.g., from different mapping/clustering algorithms), (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics). Several example annotation tables are included, or you can point it at your own files.  Read the user guide linked below for more details and send a message if you have any comments."),
      br(),
      # h4("Bookmarking"),
      # p("To get the current state of the app, push 'Get state' and copy text. To set the state, paste text into the 'Set state' box."),
      # actionButton(inputId = "bookmark_url", label="Bookmark (BROKEN)",
      #              icon = icon("link", lib = "glyphicon")
      # ),
      # verbatimTextOutput("show_url"),
      # uiOutput("state_textbox"),
      # h4("More help"),

      actionButton(inputId = "guide", 
                   icon = icon("hand-spock", lib = "font-awesome"), 
                   a("USER GUIDE",
                     style="color: #000000; border-color: #2e6da4",
                     target = "_blank", 
                     href="https://github.com/AllenInstitute/ACE/blob/main/ACE_User_Guide.pdf")
      ),
      actionButton(inputId = "tutorial", 
                   icon = icon("file-video", lib = "font-awesome"), 
                   a("TUTORIAL (coming soon)",
                     style="color: #000000; border-color: #2e6da4",
                     target = "_blank", 
                     href="https://www.youtube.com/user/AllenInstitute")
      ),
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
      br(),
      h4("Click the three lines next to the title above to minimize this sidebar."),
      br(),
      p("----------------"),
      br(),
      h4("Acknowledgements"),
      p("App developed by Jeremy Miller with support from Aaron Oster and Bosiljka Tasic, using some original code developed by Lucas Graybuck. Included annotation tables created by Jeremy Miller, Kyle Travaglini, Tain Luquez, Rachel Hostetler, and Vilas Menon. Logo credit: Lauren Alfiler."),
      br(),
      p("If you would like to contribute to this app, please reach out via email or GitHub using the links above."),
      br()
    ),
    
    dashboardBody(
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
      
      fluidRow(width = 12,
               
               box(title = "Select data set",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
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
                     column(2,#4  # Maybe remove this since it's broken
                            actionButton(inputId = "bookmark_url", label="Bookmark (BROKEN)",
                                         icon = icon("link", lib = "glyphicon")
                            ),
                            verbatimTextOutput("show_url")
                     ),
                     # NEW - END
                   ),
                   fluidRow(
                     column(8,
                            uiOutput("database_textbox")
                     ),
                     column(2,
                            fileInput("database_upload", "or UPLOAD")
                     ),
                     column(2,
                            uiOutput("checkInput")
                     )
                   ),
                   fluidRow(
                     column(8,
                            uiOutput("metadata_textbox")
                     ),
                     column(2,
                            fileInput("metadata_upload", "or UPLOAD")
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
                   tabsetPanel(
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
                                column(4,
                                       strong("Download Options"),
                                       fluidRow(
                                         column(4,textInput("annocomp_dlw","Width (in)",8)),
                                         column(4,textInput("annocomp_dlh","Height (in)",4)),
                                         column(4,textInput("annocomp_dlf","Font (pt)",6))),
                                       uiOutput("annocomp_downloadButton"))
                              )
                     ),
                     tabPanel("Link 2+ annotations (river plots)",
                              fluidRow(
                                column(11,
                                       uiOutput("river_group_selection")
                                ),
                                column(1,
                                       br(),
                                       actionButton("river_go","GO!",
                                                    style="color: #fff; background-color: #EC008C; border-color: #BE1E2D; font-weight: bold;")
                                )
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
                     tabPanel("Compare numeric annotations",  # NOTE: This should be a conditional panel that only appears when at least 2 numeric variables are present.  I have a workaround in server.r for now.
                              fluidRow(
                                column(4,
                                       uiOutput("scatter_x_selection"),
                                       uiOutput("scatter_y_selection"),
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
                                       p("  ^ Click 'Go!' after making updates to refresh display.")
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
  
#}