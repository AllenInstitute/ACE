suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(feather)
  library(DT)
  library(rbokeh)
  library(shinydashboard)
})

# Define UI for application that draws a histogram
ui <- function(request) {
  dashboardPage(
    
    dashboardHeader(title = "Annotation_Comparison"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      tags$head(tags$script('var dimension = [0, 0];
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
               
               box(title = "Data Selection",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
                     column(7,
                            uiOutput("database_textbox")
                     ),
                     column(1,
                            uiOutput("checkInput")
                     ),
                     column(2,
                            actionButton(inputId = "email1", 
                                         icon = icon("envelope", lib = "font-awesome"), 
                                         a("Comments/bugs?", 
                                           href="mailto:jeremym@alleninstitute.org?
                                           body=''
                                           &subject='Annotation Comparison' app comments"))
                     ),
                     column(2,
                            bookmarkButton()
                     )
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("filter_selection")
                     ),
                     conditionalPanel(
                       condition = "output.sf_active == true",
                       column(9,
                              strong("Filter for:"),
                              br(),
                              uiOutput("filter_panel")
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
                     tabPanel("River Plot",
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
                     tabPanel("Annotation Comparisons",
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
                                column(2,
                                       uiOutput("annocomp_select_mode_selection")
                                ),
                                column(1,
                                       uiOutput("annocomp_width_textbox")
                                ),
                                column(1,
                                       uiOutput("annocomp_height_textbox")
                                )
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
                     tabPanel("Pairwise Comparisons",
                              fluidRow(
                                column(2,
                                       uiOutput("paircomp_x_selection")
                                ),
                                column(2,
                                       uiOutput("paircomp_y_selection")
                                ),
                                column(2,
                                       uiOutput("paircomp_threshold_selection")
                                ),
                                column(1,
                                       uiOutput("paircomp_width_textbox")
                                ),
                                column(1,
                                       uiOutput("paircomp_height_textbox")
                                )
                              ),
                              fluidRow(
                                column(6,
                                       uiOutput("paircomp_jaccard_ui")
                                ),
                                column(6,
                                       uiOutput("paircomp_heatmap_ui")
                                )
                              ),
                              fluidRow(
                                column(6,
                                       strong("Download Options (Jaccard plot)"),
                                       fluidRow(
                                         column(4,textInput("paircomp_dlw","Width (in)",8)),
                                         column(4,textInput("paircomp_dlh","Height (in)",8)),
                                         column(4,textInput("paircomp_dlf","Font (pt)",10))),
                                       uiOutput("paircomp_jaccard_downloadButton")
                                ),
                                column(6,
                                       strong("Download Options (Heatmap plot)"),
                                       fluidRow(
                                         column(4,textInput("paircomp_dlw","Width (in)",8)),
                                         column(4,textInput("paircomp_dlh","Height (in)",8)),
                                         column(4,textInput("paircomp_dlf","Font (pt)",10))),
                                       uiOutput("paircomp_heatmap_downloadButton"))
                              )
                     ),
                     tabPanel("Annotation Explorer",
                              fluidRow(
                                column(3,
                                       uiOutput("explorer_group_selection")
                                ),
                                column(3,
                                       uiOutput("explorer_annotation_selection")
                                ),
                                column(3,
                                       uiOutput("explorer_comparison_selection")
                                )
                              ),
                              fluidRow(
                                column(12,
                                       #uiOutput("explorer_box_ui")
                                       dataTableOutput("explorer_table")
                                )
                              )
                     )
                   )
               ),
               box(title = "Annotation Summaries",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = TRUE,
                   fluidRow(
                     column(12,uiOutput("summary_group_selection"))
                   ),
                   fluidRow(
                     column(12,dataTableOutput("summary_table"))
                   ),
                   fluidRow(
                     column(12,downloadButton("summary_csv","Download Summary as CSV"))
                   )
               ),
               box(title = "Browse Selection",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = TRUE,
                   fluidRow(
                     column(2,
                            uiOutput("browse_show_ids_checkbox")
                     ),
                     column(2,
                            uiOutput("browse_show_colors_checkbox")
                     ),
                     column(2,
                            uiOutput("browse_truncate_long_checkbox")
                     )
                   ),
                   fluidRow(
                     column(12,
                            div(style = 'overflow-x: scroll',
                                dataTableOutput("browse_table")
                            )
                     )
                   ),
                   downloadButton("browse_csv","Download Selection as CSV")
               )
               
               
      ),
      fluidRow(width = 12,
               br(),br(),br(),br(),br(),br(),br())
      
    )
  )
  
}