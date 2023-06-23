suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  #library(DT)
  library(feather)
  library(ggplot2)
  #library(ggbeeswarm)
  #library(purrr)
  #library(rhdf5)
  library(rbokeh)
  #library(scrattch.io)
  #library(scrattch.vis)
  library(shiny)
  library(UpSetR)
  #library(ggplotify)
})
options(stringsAsFactors = F)

source("annocomp_functions.R")
source("bookmark_functions.R")
source("river_functions.R")
source("pairwise_functions.R")

enableBookmarking(store = "server")

guess_type <- function(x) {
  if(try(sum(is.na(as.numeric(x))) > 0,silent = T)) {
    "cat"
  } else {
    "num"
  }
}

default_vals <- list(db = "//allen/programs/celltypes/workgroups/humancelltypes/JeremyM/github/annotation_comparison/example",
                     sf = "")

server <- function(input, output, session) {

  ##########################################
  ## Bookmarking and State Initialization ##
  ##########################################
  write("Reading bookmarks and setting state.", stderr())
  
  # When the bookmark button is clicked, store the input values as a string
  onBookmark(function(state) {
    
    # build_storage_string() is in bookmark_functions.R
    state$values$vals <- build_storage_string(input, keep_empty = FALSE)
    
  })
  
  # When the page is restored from a bookmark, read the stored values as a reactive list
  restored_vals <- reactiveValues(vals = list())
  
  onRestore(function(state) {
    
    store <- state$values$vals
    
    if(!is.null(store)) {
      
      # parse_storage_string() is in bookmark_functions.R
      restored_vals$vals <- parse_storage_string(store)
      
    }
    
  })
  
  init <- reactiveValues(vals = list())
  
  # Build initial values list
  # These are used to set the state of the input values for UI elements
  
  # First from default_vals,
  # then restored_vals,
  # then from URL parsing
  
  observe({
    
    # default values
    # defined in the default_vals list before the server() call, above.
    vals <- default_vals
    
    # restored values
    # defined by the stored input value string, parsed in onRestore(), above.
    # These supercede the defaults
    restored <- restored_vals$vals
    
    # Substitute default values for
    if(length(restored) > 0) {
      for(val in names(restored)) {
        vals[[val]] <- restored[[val]]
        
      }
      
    }
    
    # URL values
    # defined in the URL
    # These supercede both defaults and restored values
    if(length(session$clientData$url_search) > 0) {
      
      query <- as.list(parseQueryString(session$clientData$url_search))
      
      for(val in names(query)) {
        vals[[val]] <- query[[val]]
      }
    }
    
    init$vals <- vals
    
  })
  
  # Direct link based on input parsing
  # This can be used to provide a direct URL to users that can be bookmarked.
  output$url <- renderUI({
    req(input)
    url <- build_url(session, input)
    a("Direct Link", href = url)
    
  })
  

  #########################
  ## General UI Elements ##
  #########################
  
  # Database selection textbox.
  # Users provide the network path to the dataset
  # This is in the server.R section so that the default value can be
  #   set using the init$vals reactive values based on defaults, 
  #   bookmarks, and URL parsing
  #
  # output$database_textbox - Textbox UI Object
  #
  # input$db - character object
  #
  output$database_textbox <- renderUI({
    req(init$vals)
    
    id <- "db"
    label <- "Feather Directory"
    
    # If a stored db exists, pull the value from init$vals
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  
  ##################################
  ## Loading tables from input$db ##
  ##################################
  
  # Check the path provided by input$db
  # returns a corrected path
  #
  # note: rv_ prefix stands for reactive value
  #
  # rv_path() - length 1 character vector
  #
  rv_path <- reactive({
    req(input$db)
    write("Checking and setting input$db.", stderr())
    
    input$db
  })
  
  # Read the annotations table from the dataset
  #
  # rv_anno() - a data.frame
  #
  rv_anno <- reactive({
    req(rv_path())
    write("Reading anno.", stderr())
    
    # If a .csv file, use read.csv
    if(grepl(".csv$",rv_path())) {
      
      if(file.exists(rv_path())) {
        withProgress({
          setProgress(value = 0.2,
                      message = "Loading Annotations")
          
          anno <- fread(rv_path())
          
          anno <- as.data.frame(anno)
          
          # Check if first column is unique IDs, and if not, create a new column
          if (length(anno[,1])!=length(unique(anno[,1]))){
            anno <- data.frame(sample_id=paste0("i",1:dim(anno)[1]),anno)
          }

          # Rename sample ids as sample_id
          names(anno)[1] <- "sample_id"
          
          anno <- auto_annotate(anno)
          
          setProgress(value = 1,
                      message = "Annotations Loaded")
          
          return(anno)
          
        })
      } else {
        write(paste(rv_path(),"does note exist."), stderr())
      }
    # If not a csv file, we expect a directory with an anno.feather file
    } else {
      # use read_feather() from feather
      anno_file <- paste0(rv_path(),"/anno.feather")
      
      if(dir.exists(rv_path()) & file.exists(anno_file)) {
        withProgress({
          setProgress(value = 0.2,
                      message = "Loading Annotations")
          anno <- read_feather(anno_file)
          
          anno <- as.data.frame(anno)
          
          # Rename sample_name to sample_id for compatibility
          # with code written for .feather files
          colnames(anno)[colnames(anno)=="sample_name"] <- "sample_id"

          # Move sample_id to the first column
          anno <- anno %>%
            select("sample_id", everything())
          
          setProgress(value = 1,
                      message = "Annotations Loaded")
          
        })
        return(anno)
        
      } else {
        write(paste(rv_path(),"does note exist."), stderr())
      }
    } 
    
  }) # end rv_anno()
  
  # Build the annotation descriptions table
  # 
  # rv_desc() - a data.frame
  #
  rv_desc <- reactive({
    req(rv_path())
    req(rv_anno())
    write("Building desc.", stderr())
    
    anno <- rv_anno()
    names <- colnames(anno)[grepl("_label$",colnames(anno))]
    names <- substr(names,1,nchar(names)-6)
    desc_table <- data.frame(base=names,name=names)
    
    suppressWarnings({
      desc_table <- desc_table %>%
        rowwise() %>%
        mutate(type = guess_type(anno[[paste0(base,"_label")]]))
    })
    
    return(desc_table)
 
  }) # end of rv_desc()
  
  #############################
  ## Filtering and Filter UI ##
  #############################
  
  # Build a set of filter options from 
  # the rv_desc() data.frame
  #
  # filter_options() - a named character vector
  #
  filter_options <- reactive({
    req(rv_desc())
    
    desc <- rv_desc()
    
    anno_opts <- desc$base
    names(anno_opts) <- desc$name
    
    anno_opts
    
  })
  
  # Build a set of categorical filter options from
  # the rv_desc() data.frame
  #
  # cat_options() - a named character vector
  #
  cat_options <- reactive({
    req(rv_desc())
    
    desc <- rv_desc()
    desc <- desc[desc$type == "cat",]
    anno_opts <- desc$base
    names(anno_opts) <- desc$name
    
    anno_opts
    
  })
  
  # Filter selection dropdown menu
  # 
  # output$filter_selection - UI object
  #
  # input$sf - character object corresponding to rv_desc()$base value
  #   sf stands for selected filter
  #
  output$filter_selection <- renderUI({
    req(filter_options)
    
    filter_opts <- filter_options()
    
    id <- "sf"
    label <- "Choose Filter Set"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "")
    
    if(grepl(",", initial)) {
      initial_split <- unlist(strsplit(initial,","))
      initial <- filter_opts[match(initial_split, filter_opts)]
    }
    
    selectizeInput(inputId = id,
                   label = label,
                   filter_opts,
                   initial,
                   multiple = T,
                   width = "100%")
  })
  
  # Reactive value to show/hide the filter panel
  #   useful before any filters are selected.
  #
  # output$sf_active - a logical object
  #
  output$sf_active <- reactive({
    length(input$sf) > 0
  })
  
  # pass sf_active even when hidden
  outputOptions(output,
                "sf_active",
                suspendWhenHidden = FALSE)
  
  ######
  # Building the filter panels
  ######
  
  # Initialize filters$current to store values selected
  # using the filter UI panels.
  #
  filters <- reactiveValues(current = list())

  # function for assembling filter UI panels based on desc
  #
  # This doesn't actually apply the filters or create filter values
  #   instead, this generates the UI elements that return selection
  #   values.
  #
  build_filter_panel_list <- function(desc, 
                                      sf, 
                                      all_anno) {
    
    desc <- desc %>%
      filter(base %in% sf)
    
    anno <- all_anno
    
    n_desc <- nrow(desc)
    
    tabList <- list()
    
    if(n_desc > 0) {
      for(i in 1:n_desc) {
        # base_id and base_label used to retrieve
        # values from rv_anno()
        filter_id <- paste0(desc$base[i],"_id")
        filter_label <- paste0(desc$base[i],"_label")
        
        # base_filter and desc$name used to build UI elements
        filter_inputid <- paste0(desc$base[i],"_filter")
        filter_name <- desc$name[i]
        
        if(desc$type[i] == "cat") {
          # If this is a categorical annotation, use selectizeInput to generate
          # a multiple-selection dropdown menu
          
          # Select unique id and label values from the annotations table
          # for the categorical annotation.
          filter_anno <- anno %>% 
            select(one_of(filter_id,filter_label)) %>%
            unique() %>%
            arrange(filter_id)
          names(filter_anno) <- c("id","label")
          
          # Make a named numeric for the options
          filter_opts <- filter_anno$id
          names(filter_opts) <- filter_anno$label
          
          # look for values that are already selected.
          # This is necessary for recovering filter settings after
          # removing a filter.
          #
          # The isolate() prevents an infinite loop from occurring
          #
          filter_init <- isolate(filters$current[[filter_inputid]])
          
          # Add a tabPanel to the tabList for this annotation
          # using the choices (filter_opts) generated above
          tabList[[i]] <- tabPanel(filter_name,
                                   selectizeInput(inputId = filter_inputid,
                                                  label = filter_name,
                                                  choices = filter_opts,
                                                  selected = filter_init,
                                                  multiple = TRUE,
                                                  width = "100%"))
        } else if(desc$type[i] == "num") {
          # If this is a numeric annotation, use sliderInput to allow
          # selection of a range of numeric values.
          
          # Generate a range based on the numeric values
          values <- as.numeric(anno[[filter_label]])
          values <- data.frame(val = values[!is.na(values)])
          
          filter_range <- range(values$val,
                                na.rm = TRUE)
          
          # look for values that are already selected.
          # This is necessary for recovering filter settings after
          # removing a filter.
          #
          # The isolate() prevents an infinite loop from occurring
          #
          filter_init <- isolate(filters$current[[filter_inputid]])
          
          # Generate a histogram to show distribution of the numeric values
          # hist_plot_name <- paste0(desc$base[i], "_hist_plot")
          # print(hist_plot_name)
          # 
          # output[[hist_plot_name]] <- renderPlot({
          #   
          #   ggplot(values) + 
          #     geom_histogram(aes(x = val),
          #                    bins = 100) +
          #     scale_x_continuous("",expand = c(0,0)) +
          #     scale_y_continuous("",expand = c(0,0)) +
          #     theme_classic() +
          #     theme(axis.title = element_blank(),
          #           axis.ticks.length = unit(0, "pt"),
          #           plot.margin=grid::unit(c(0,0,0,0),"cm"))
          #   
          # })
          
          # Add a tabPanel to the tabList for this annotation
          # using the filter_range, above.
          tabList[[i]] <- tabPanel(filter_name,
                                   # plotOutput(hist_plot_name,
                                   #            width = "100%",
                                   #            height = 80),
                                   sliderInput(inputId = filter_inputid,
                                               label = filter_name,
                                               min = filter_range[1],
                                               max = filter_range[2],
                                               value = filter_init))
        }
      }
    }
    
    return(tabList)
    
  } # end of build_filter_panel_list()
  
  # build the filter panel UI
  # 
  # output$filter_panel - UI tabsetPanel object
  #
  output$filter_panel <- renderUI({
    req(rv_desc())
    req(input$sf)
    req(rv_anno())
    write("Building filter panel.", stderr())
    
    withProgress({
      tabList <- build_filter_panel_list(desc = rv_desc(), 
                                         sf   = input$sf, 
                                         all_anno = rv_anno())
      do.call(tabsetPanel, tabList)
    })
    
  })
  
  # Update filters in filters$current based on input, 
  # initialize if there's no input,
  # and remove if not present after initialization
  observe({
    req(rv_anno())
    req(rv_desc())
    req(init$vals)
    req(filters$current)
    req(input$sf)
    
    desc <- rv_desc()
    anno <- rv_anno()

    for(i in 1:nrow(desc)) {
      filter_inputid = paste0(desc$base[i], "_filter")
      
      if(!desc$base[i] %in% input$sf) {
        
        if(desc$type[i] == "cat") {
          filters$current[[filter_inputid]] <- ""
        } else if(desc$type[i] == "num") {
          filter_label <- paste0(desc$base[i],"_label")
          filter_values <- as.numeric(anno[[filter_label]])
          filter_values <- filter_values[!is.na(filter_values)]
          filter_range <- range(filter_values)
          filters$current[[filter_inputid]] <- filter_range
        }
        
      } else {
        if(!is.null(input[[filter_inputid]])) {
          # update filters when input changes
          
          if(!identical(filters$current[[filter_inputid]],
                        input[[filter_inputid]])) {
            filters$current[[filter_inputid]] <- input[[filter_inputid]]
          }
          
        } else if(!isTruthy(input[[filter_inputid]])) {
          if(desc$type[i] == "cat") {
            if(isTruthy(init$vals[[filter_inputid]])) {
              filters$current[[filter_inputid]] <- as.character(init$vals[[filter_inputid]])
            } else {
              filters$current[[filter_inputid]] <- ""
            }
            
          } else if(desc$type[i] == "num") {
            filter_label <- paste0(desc$base[i],"_label")
            filter_values <- as.numeric(anno[[filter_label]])
            filter_values <- filter_values[!is.na(filter_values)]
            filter_range <- range(filter_values)
            filters$current[[filter_inputid]] <- filter_range
          }
        }
      }
      
    }
    
  })
  
  # Filter the annotations table
  # This is the core observer for all anno filtering
  #
  # rv_filtered() - a data.frame
  #
  rv_filtered <- reactive({
    req(rv_desc())
    req(rv_anno())
    req(filters$current)
    write("Filtering annotations", stderr())
    
    withProgress({
      setProgress(value = 0.2,
                  message = "Applying Filters")
      
      
      desc <- rv_desc()
      filtered <- rv_anno()
      
      filter_inputids <- names(filters$current)
      
      if(length(filter_inputids) > 0) {
        
        for(i in 1:length(filter_inputids)) {
          
          filter_base <- sub("_filter","",filter_inputids[i])
          filter_id <- paste0(filter_base,"_id")
          filter_label <- paste0(filter_base,"_label")
          filter_values <- filters$current[[filter_inputids[i]]]
          filter_type <- desc$type[desc$base == filter_base]
          
          if(filter_values[1] != "" & filter_base %in% input$sf) {
            if(filter_type == "cat") {
              filter_text <- paste0(filter_id," %in% c(",paste(filter_values,collapse=","),")")
              
              filtered <- filtered %>%
                filter_(filter_text)
            } else if(filter_type == "num") {
              filter_text_low <- paste0("as.numeric(",filter_label,") >= ",filter_values[1])
              filter_text_high <- paste0("as.numeric(",filter_label,") <= ",filter_values[2])
              
              filtered <- filtered %>%
                filter_(filter_text_low) %>%
                filter_(filter_text_high)
            }
            
          }
          
        }
        
      }
      
      setProgress(1, message = "Filters Applied")
      
      filtered
    })
  })
  
  
  # Generate text to display the current filter settings on the page
  output$summary_text <- renderUI({
    req(rv_anno())
    req(rv_desc())
    req(rv_filtered())
    write("Building filter text.", stderr())
    
    anno <- rv_anno()
    desc <- rv_desc()
    filtered <- rv_filtered()
    
    counts <- paste0(nrow(filtered)," of ",nrow(anno)," samples selected.")
    
    filters <- names(input)[grepl("_filter",names(input))]

    if(length(filters) > 0) {
      
      for(i in 1:length(filters)) {
        
        filter_base <- sub("_filter","",filters[i])
        filter_id <- paste0(filter_base,"_id")
        
        filter_label <- paste0(filter_base,"_label")
        filter_values <- input[[filters[i]]]
        
        filter_type <- desc$type[desc$base == filter_base]
        filter_name <- desc$name[desc$base == filter_base]
        
        if(filter_base %in% input$sf) {
          if(filter_type == "cat") {
            
            anno_groups <- anno %>%
              select(one_of(filter_id,filter_label)) %>%
              unique() %>%
              filter_(paste0(filter_id," %in% c(",paste(filter_values, collapse = ","),")"))
            filter_groups <- anno_groups[[filter_label]]
            
            if(length(filter_values) > 0) {
              filter_text <- paste0(filter_name,": ",paste(filter_groups,collapse=", "))
            } else {
              filter_text <- ""
            }
            
          } else if(filter_type == "num") {
            anno_vals <- as.numeric(anno[[filter_label]])
            anno_vals <- anno_vals[!is.na(anno_vals)]
            anno_range <- range(anno_vals)
            print(anno_range)
            print(filter_values)
            
            if(filter_values[1] != anno_range[1] | filter_values[2] != anno_range[2]) {
              if(filter_values[1] > anno_range[1]) {
                filter_low <- paste0(" >= ", filter_values[1])
              } else {
                filter_low <- ""
              }
              if(filter_values[2] < anno_range[2]) {
                filter_high <- paste0(" <= ", filter_values[2])
              } else {
                filter_high <- ""
              }
              filter_and <- ifelse(filter_low != "" & filter_high != "", " and ","")
              
              filter_text <- paste0(filter_name, filter_low, filter_and, filter_high)
              
            } else {
              filter_text <- ""
            }
          }
        } else {
          filter_text <- ""
        }
        
        
        
        if(filter_text != "") {
          if(exists("applied_filters")) {
            applied_filters <- paste0(applied_filters,"<br/>",filter_text)
          } else {
            applied_filters <- filter_text
          }
        }
        
      }
    } 
    
    if(!exists("applied_filters")) { applied_filters <- "No filters applied." }
    
    HTML(paste0(applied_filters,"<br/>",counts))
    
  })
  
  ####################
  ## River Plot Box ##
  ####################
  
  # UI for riverplot annotation selection
  output$river_group_selection <- renderUI({
    req(cat_options)
    anno_opts <- cat_options()
    
    id <- "river_groups"
    label <- "Node Groups"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "")
    
    if(grepl(",", initial)) {
      initial_split <- unlist(strsplit(initial,","))
      initial <- anno_opts[match(initial_split, anno_opts)]
    }
    
    selectizeInput(inputId = id,
                   label = label,
                   anno_opts,
                   initial,
                   multiple = T,
                   width = "100%")
  })
  
  # Width finding hack, as bokeh doesn't detect available space.
  output$river_widthfinder <- renderPlot({
    p()
  })
  
  river_anno <- eventReactive(input$river_go, {
    rv_filtered()
  })
  
  river_groups <- eventReactive(input$river_go, {
    input$river_groups
  })
  
  
  # Call dendrogram plot rendering funciton
  output$river_plot <- renderRbokeh({
    req(river_anno())
    req(river_groups())
    
    available_width <- input$dimension[1]#as.numeric(session$clientData$output_dendro_widthfinder_width)
    print(available_width)

    anno <- river_anno()
    river_groups <- river_groups()
        
    suppressWarnings(build_river_plot_bokeh(anno = anno,
                                            group_by = river_groups,
                                            node_labels = "all",
                                            width = available_width)
    )
    
  })
  
  output$downloadRiverPDF <- downloadHandler(
    filename = "river.pdf",
    content = function(file) {
      req(river_anno())
      req(river_groups())
      
      
      anno <- river_anno()
      river_groups <- river_groups()
      
      plot_river<-build_river_plot(anno = anno,
                                   group_by = river_groups)
      ggsave(file, 
             plot = plot_river,
             #device = device, 
             width = as.numeric(input$dlw), 
             height = as.numeric(input$dlh),
             useDingbats = FALSE)
    }
  )
  
  
  # UI for dendrogram plot output
  output$river_plot_ui <- renderUI({
    req(rv_filtered())
    if(nrow(rv_filtered()) > 0) {
      rbokehOutput("river_plot", height = 800)
    } else {
      p("No cells meet the current filtering criteria.")
    }
    
  })
  
  
  
  ################################
  ## Annotation Comparisons Box ##
  ################################
  
  ## UI Elements
  output$annocomp_x_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base
    names(options) <- desc$name
    
    id <- "annocomp_x"
    label <- "X-axis annotation"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$annocomp_y_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base
    names(options) <- desc$name
    
    id <- "annocomp_y"
    label <- "Y-axis annotation"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[2])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$annocomp_color_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    req(input$annocomp_x)
    req(input$annocomp_y)
    
    desc <- rv_desc()
    num_anno <- desc$base[desc$type == "num"]
    
    if(input$annocomp_x %in% num_anno | input$annocomp_y %in% num_anno) {
      options <- c("none",desc$base)
      names(options) <- c("None",desc$name)
    } else {
      options <- c("none",input$annocomp_x, input$annocomp_y)
      names(options) <- c("None",
                          desc$name[desc$base == input$annocomp_x],
                          desc$name[desc$base == input$annocomp_y])
    }
  
    id <- "annocomp_color"
    label <- "Color points by"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "none")
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
    
    
  })
  
  output$annocomp_denom_selection <- renderUI({
    req(rv_desc())
    req(input$annocomp_x)
    req(input$annocomp_y)
    
    
    
    desc <- rv_desc()
    cat_anno <- desc$base[desc$type == "cat"]
    
    if(input$annocomp_x %in% cat_anno & input$annocomp_y %in% cat_anno) {
      
      denom_options <- c("none",input$annocomp_x, input$annocomp_y)
      names(denom_options) <- c("None",desc$name[desc$base == input$annocomp_x],
                                desc$name[desc$base == input$annocomp_y])
      id <- "annocomp_denom"
      label <- "As fraction of"
      
      if(isTruthy(input$annocomp_denom)) {
        if(input$annocomp_denom %in% denom_options) {
          initial <- input$annocomp_denom
        } else {
          initial <- ifelse(length(init$vals[[id]]) > 0,
                            init$vals[[id]],
                            denom_options[1])
        }
      } else {
        initial <- ifelse(length(init$vals[[id]]) > 0,
                          init$vals[[id]],
                          denom_options[1])
      }
      
      
      
      selectizeInput(inputId = id, 
                     label = strong(label), 
                     choices = denom_options, 
                     selected = initial,
                     multiple = FALSE)
      
    }
  })
  
  output$annocomp_select_mode_selection <- renderUI({
    req(init$vals)
    
    options <- c("Filter" = "filter",
                 "Highlight" = "highlight")
    
    id <- "annocomp_select_mode"
    label <- "Selection Mode"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "filter")
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
    
    
  })
  
  output$annocomp_height_textbox <- renderUI({
    req(init$vals)
    
    id <- "annocomp_height"
    label <- "Plot Height"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "500px")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  output$annocomp_width_textbox <- renderUI({
    req(init$vals)
    
    id <- "annocomp_width"
    label <- "Plot Width"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "100%")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  # Calculate stats for medians and whiskers if one dimension is numeric
  # Then build the plot.
  annocomp_plot <- reactive({
    req(rv_anno())
    req(rv_filtered())
    req(rv_desc())
    req(input$annocomp_x)
    req(input$annocomp_y)
    req(input$annocomp_color)
    req(input$annocomp_select_mode)
    
    stats <- build_annocomp_stats(anno = rv_filtered(),
                                  desc = rv_desc(),
                                  x_group = input$annocomp_x, 
                                  y_group = input$annocomp_y)
    
    build_annocomp_plot(anno = rv_anno(), 
                        filtered = rv_filtered(), 
                        desc = rv_desc(), 
                        stats = stats, 
                        x_group = input$annocomp_x, 
                        y_group = input$annocomp_y, 
                        c_group = input$annocomp_color, 
                        denom = input$annocomp_denom,
                        filter_mode = input$annocomp_select_mode)
  })
  
  output$annocomp_plot <- renderPlot({
    annocomp_plot()
  })
  
  output$annocomp_plot_ui <- renderUI({
    plotOutput("annocomp_plot", height = input$annocomp_height, width = input$annocomp_width)
  })
  
  
  # heatmap download objects
  output$annocomp_downloadButton <- renderUI({
    req(annocomp_plot())
    downloadButton('annocomp_downloadPlot')
  })
  
  
  output$annocomp_downloadPlot <- downloadHandler(
    
    filename = "distillery_annotation_plot.pdf",
    content = function(file) {
      
      plot <- annocomp_plot() + theme(text = element_text(size = as.numeric(input$annocomp_dlf)))
      
      out_h <- as.numeric(input$annocomp_dlh)
      out_w <- as.numeric(input$annocomp_dlw)
      
      ggsave(file, 
             plot = plot,
             width = out_w, 
             height = out_h)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############################
  ## Pairwise Comparisons Box ##
  ##############################
  
  ## UI Elements
  output$paircomp_x_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base[desc$type == "cat"]
    names(options) <- desc$name[desc$type == "cat"]
    
    id <- "paircomp_x"
    label <- "X-axis annotation"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$paircomp_y_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base[desc$type == "cat"]
    names(options) <- desc$name[desc$type == "cat"]
    
    id <- "paircomp_y"
    label <- "Y-axis annotation"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[2])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  
  output$paircomp_threshold_selection <- renderUI({
    req(init$vals)
    
    id <- "paircomp_threshold"
    label <- "Heatmap Threshold"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "0.2")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  output$paircomp_height_textbox <- renderUI({
    req(init$vals)
    
    id <- "paircomp_height"
    label <- "Plot Height"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "500px")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  output$paircomp_width_textbox <- renderUI({
    req(init$vals)
    
    id <- "paircomp_width"
    label <- "Plot Width"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "100%")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  # Calculate and then builds Jaccard comparison plot
  paircomp_jaccard_plot <- reactive({
    req(rv_filtered())
    req(input$paircomp_x)
    req(input$paircomp_y)
    
    # Get input values
    build_compare_jaccard_plot(anno = rv_filtered(), 
                               x_group = input$paircomp_x, 
                               y_group = input$paircomp_y)
  })
  
  output$paircomp_jaccard_plot <- renderPlot({
    paircomp_jaccard_plot()
  })
  
  output$paircomp_jaccard_ui <- renderUI({
    plotOutput("paircomp_jaccard_plot", height = input$paircomp_height, width = input$paircomp_width)
  })
  
  
  # download objects
  output$paircomp_jaccard_downloadButton <- renderUI({
    req(paircomp_jaccard_plot())
    downloadButton('paircomp_jaccard_downloadPlot')
  })
  
  
  output$paircomp_jaccard_downloadPlot <- downloadHandler(
    
    filename = "paircomp_jaccard_plot.pdf",
    content = function(file) {
      
      plot <- paircomp_jaccard_plot() + theme(text = element_text(size = as.numeric(input$paircomp_dlf)))
      
      out_h <- as.numeric(input$paircomp_dlh)
      out_w <- as.numeric(input$paircomp_dlw)
      
      ggsave(file, 
             plot = plot,
             width = out_w, 
             height = out_h)
    }
  )
  
  
  # Calculate and then builds heatmap comparison plot
  paircomp_heatmap_plot <- reactive({
    req(rv_filtered())
    req(input$paircomp_x)
    req(input$paircomp_y)
    req(input$paircomp_threshold)
    
    # Get input values
    build_compare_heatmap_plot(anno = rv_filtered(), 
                               x_group = input$paircomp_x, 
                               y_group = input$paircomp_y,
                               threshold = as.numeric(input$paircomp_threshold))
  })
  
  output$paircomp_heatmap_plot <- renderPlot({
    paircomp_heatmap_plot()
  })
  
  output$paircomp_heatmap_ui <- renderUI({
    plotOutput("paircomp_heatmap_plot", height = input$paircomp_height, width = input$paircomp_width)
  })
  
  
  # download objects
  output$paircomp_heatmap_downloadButton <- renderUI({
    req(paircomp_heatmap_plot())
    downloadButton('paircomp_heatmap_downloadPlot')
  })
  
  
  output$paircomp_heatmap_downloadPlot <- downloadHandler(
    
    filename = "paircomp_heatmap_plot.pdf",
    content = function(file) {
      
      plot <- paircomp_heatmap_plot() + theme(text = element_text(size = as.numeric(input$paircomp_dlf)))
      
      out_h <- as.numeric(input$paircomp_dlh)
      out_w <- as.numeric(input$paircomp_dlw)
      
      ggsave(file, 
             plot = plot,
             width = out_w, 
             height = out_h)
    }
  ) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########################
  ## Browse Selection Box ##
  ##########################
  
  # Show IDs checkbox
  output$browse_show_ids_checkbox <- renderUI({
    req(init$vals)
    
    id <- "browse_show_ids"
    label <- "Show ID Columns"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      FALSE)
    
    checkboxInput(inputId = id,
                  label = label,
                  initial)
    
  })
  # Show Colors checkbox
  output$browse_show_colors_checkbox <- renderUI({
    req(init$vals)
    
    id <- "browse_show_colors"
    label <- "Show Color Columns"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      FALSE)
    
    checkboxInput(inputId = id,
                  label = label,
                  initial)
    
  })
  # Truncate Long Values checkbox
  output$browse_truncate_long_checkbox <- renderUI({
    req(init$vals)
    
    id <- "browse_truncate_long"
    label <- "Truncate long values"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      TRUE)
    
    checkboxInput(inputId = id,
                  label = label,
                  initial)
    
  })
  
  # datatable showing all of the rv_filtered values
  output$browse_table <- renderDataTable({
    
    show_table <- rv_filtered() %>% group_annotations()
    
    if(!input$browse_show_ids) {
      keep_cols <- names(show_table)[!grepl("_id",names(show_table))]
      keep_cols <- c("sample_id",keep_cols)
      show_table <- show_table %>%
        select(one_of(keep_cols))
    }
    
    if(!input$browse_show_colors) {
      keep_cols <- names(show_table)[!grepl("_color",names(show_table))]
      show_table <- show_table %>%
        select(one_of(keep_cols))
    }
    
    if(input$browse_truncate_long) {
      show_table <- as.data.frame(lapply(show_table, function(x) {
        if(is.character(x)) {
          lens <- nchar(x, allowNA = TRUE)
          too_long <- lens > 50
          missing <- is.na(x)
          x[too_long & !missing] <- paste(substr(x[too_long & !missing],1,50),"...")
          x
        } else {
          x
        }
      }))
    }
    
    datatable(show_table)
    
  })
  
  # Download handler for Browse Selection tab.
  output$browse_csv <- downloadHandler(
    filename = function() {"distillery_selection.csv"},
    content = function(file) {
      out_table <- rv_filtered()
      write.csv(out_table, file, quote = T, row.names = F)
    }
  )
  
  ##############################
  ## Annotation Summaries Box ##
  ##############################

  # UI for group selection in the Annotation Summaries tab
  output$summary_group_selection <- renderUI({
    req(cat_options)
    anno_opts <- cat_options()
    
    id <- "summary_groups"
    label <- "Summary Grouping"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "")
    
    if(grepl(",", initial)) {
      initial_split <- unlist(strsplit(initial,","))
      initial <- anno_opts[match(initial_split, anno_opts)]
    }
    
    selectizeInput(inputId = id,
                   label = label,
                   anno_opts,
                   initial,
                   multiple = T,
                   width = "100%")
  })
  
  # Summary functions based on selected grouping
  summarize_filtered <- reactive({
    req(input$summary_groups)
    req(rv_filtered())
    req(rv_desc())
    
    desc <- rv_desc()
    
    if(!identical(input$summary_groups,"")) {
      summary_labels <- paste0(input$summary_groups, "_label")
      summary_names <- desc$name[match(input$summary_groups, desc$base)]
      
      data <- rv_filtered()
      
      data <- data %>%
        group_by(.dots = summary_labels) %>%
        summarise(n = n())
      
      names(data) <- c(summary_names,"n")
      
      data
    } else {
      
      data <- data.frame(selected_cells = nrow(rv_filtered))
      
      data
    }
    
  })
  
  # datatable for displaying the summarize_filtered() table
  output$summary_table <- renderDataTable({
    req(summarize_filtered())
    show_table <- summarize_filtered()
    datatable(show_table)
    
  })
  
  # Download handler for the summarize_filtered() table
  output$summary_csv <- downloadHandler(
    filename = function() {"distillery_summary.csv"},
    content = function(file) {
      out_table <- summarize_filtered()
      write.csv(out_table, file, quote = T, row.names = F)
    }
  )

  # Batch dendrograms
  # Legends for annotation comparisons
  
}
