library(shiny)
library(dplyr)
library(feather)
library(DT)
library(rbokeh)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- function(request) {
  dashboardPage(
    
    dashboardHeader(title = "Annotation_Comparison"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      fluidRow(width = 12,
               
               box(title = "Data Selection",
                   solidHeader = TRUE, status = "primary", width = 12,
                   collapsible = TRUE, collapsed = FALSE,
                   fluidRow(
                     column(11,
                            uiOutput("database_textbox")
                     ),
                     column(1,
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