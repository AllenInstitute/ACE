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
    
    title = 'SEA-AD annotation comparison',
    ##
    dashboardHeader(title = div(h3("SEA-AD", style="margin: 0;"),h4("Annotation comparison", style="margin: 0;"))),
    
    ##
    dashboardSidebar(disable = TRUE),
    
    # dashboardSidebar(
    #   verticalLayout(
    #     div(),
    #     h6("Please select an option and click open table to begin.", 
    #        style='justify-self: center; padding-left: 10px; padding-right: 10px; padding-bottom: 0px;margin-bottom: 0px; text-align: center;'),
    #     ## Let the user filter tables to just one species
    #     selectInput("Level", "Taxonomy Level", choices=c("All","Class","Subclass","Supertype"), selected="Class", multiple=FALSE),
    #     actionButton("openTable", 
    #                  "Open beta coefficent table",
    #                  style = "color: #fff; background-color: #27ae60; border-color: #fff; padding: 10px 20px 10px px; margin: 5px 5px 5px 20px; "),
    #   ),
    #   br(),
    #   htmlOutput("text1"),
    #   tags$head(tags$style("#text1{color: white;
    #                              font-size: 12px;
    #                              text-align: center;
    #                              }"
    #   )
    #   ),
    #   div(
    #     actionButton("showInfo","Show/Hide Info", style = "margin: 0px;"),
    #     style="display: flex; align-content: center; justify-content: center; flex-wrap: wrap; padding-top: 10%"
    #   )
    # ),
    
    ##
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
                     column(2,
                            uiOutput("checkInput")
                     ),
                     column(2,
                            actionButton(inputId = "email1", 
                                         icon = icon("envelope", lib = "font-awesome"), 
                                         a("Comments/bugs?", 
                                           href="mailto:jeremym@alleninstitute.org?
                                           body=''
                                           &subject='Annotation Comparison' app comments"))
                     )
                   ),
                   fluidRow(
                     column(7,
                            uiOutput("metadata_textbox")
                     ),
                     column(2,
                            uiOutput("metadata_checkInput")
                     ),
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
                     tabPanel("Pairwise Comparisons",
                              fluidRow(
                                column(2,
                                       uiOutput("paircomp_x_selection")
                                ),
                                column(2,
                                       uiOutput("paircomp_y_selection")
                                ),
                                column(2,
                                       uiOutput("reorderY_selection")
                                ),
                                column(2,
                                       uiOutput("paircomp_width_textbox")
                                ),
                                column(2,
                                       uiOutput("paircomp_height_textbox")
                                )
                              ),
                              fluidRow(
                                column(12,
                                       uiOutput("paircomp_jaccard_ui")
                                ),
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
                            )
                     ),
                     tabPanel("Annotation Explorer",
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
                     )
                   )
               )
      ),
      fluidRow(width = 12, br(),br())
      
    )
  )
  
}