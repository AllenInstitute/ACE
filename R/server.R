suppressPackageStartupMessages({
  library(dplyr)
  library(DT)
  library(feather)
  library(ggplot2)
  library(ggbeeswarm)
  library(rbokeh)
  library(scrattch.io)
  library(shiny)
  library(UpSetR)
  library(anndata)
  library(vroom)
})
options(stringsAsFactors = F)

source("initialization.R")
source("annocomp_functions.R")
source("river_functions.R")
source("pairwise_functions.R")
source("multistudy_functions.R")
source("bookmark_functions.R")    # this shouldn't be required at all, as it is a workaround for regular bookmarking (both are currently broken)
source("scatterplot_functions.R") 
source("auto_annotate.R")  # This can be removed when scrattch.io is updated with new version of auto_annotate

enableBookmarking("url")  # It was "server", but it doesn't seem to work either way

guess_type <- function(x) {
  if(try(sum(is.na(as.numeric(x))) > 0,silent = T)) {
    "cat"
  } else {
    "num"
  }
}

default_vals <- list(db = "Enter a file path or URL here, or choose from dropdown above.",
                     sf = "Enter a file path or URL here, or choose from dropdown above.",
                     metadata = "Enter a file path or URL here, or choose from dropdown above."
)


######################################################
## Default table information is in initialization.R ##
######################################################


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
  # then dropdown_vals,
  # then from URL parsing

  observe({
    
    # default values
    # defined in the default_vals list before the server() call, above.
    vals <- default_vals
    
    # restored values
    # defined by the stored input value string, parsed in onRestore(), above.
    # These supercede the defaults
    restored <- restored_vals$vals
    
    #write(paste0("RESTORED LENGTH: ",length(restored)),stderr())
    
    # Substitute default values for initialized values
    if(length(restored) > 0) {
      for(val in names(restored)) {
        vals[[val]] <- restored[[val]]
        #write(paste(val, restored[[val]], collapse=": "),stderr())
        
      }
      
    }
    
    # URL values
    # defined in the URL
    # These supercede both defaults and restored values
    if(length(session$clientData$url_search) > 0) {
      
      #write("RESTORED URL OBSERVED",stderr())
      #write(paste0("URL VALUE: ",session$clientData$url_search),stderr())
      
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
  
  
  output$show_url <- renderText("")
  observeEvent(input$bookmark_url, {
    req(input)
    url <- build_url(session, input)
    output$show_url <- renderText(url)
  })
  
  ### THIS DOESN'T WORK
  # ### Manual state restoration
  # output$state_textbox <- renderUI({
  #   id <- "restore_state"
  #   label <- "Set state"
  #   
  #   textInput(inputId = id, 
  #             label = strong(label), 
  #             value = "", 
  #             width = "100%")
  #   
  # })
  # rv_path <- observeEvent(input$restore_state,{
  #   write("Checking and setting $restore_state.", stderr())
  #   write(input$restore_state,stderr())
  #   vals <- parse_storage_string(input$restore_state)
  #   
  #   for (nm in names(vals)){
  #     write(nm,stderr())
  #     write(vals[[nm]],stderr())
  #     init[[nm]] <- vals[[nm]]
  #   }
  # })

  
  updateSelectInput(session, inputId = "select_category", label = "Choose your data type:", choices = names(table_name)) # "Enter your own location"
  
  observeEvent(input$select_category, {
    # Choose a value from the default table, if selected
    # This is updated to be a list of lists
    if(length(input$select_category)>0) category = input$select_category
    updateSelectInput(session, inputId = "select_textbox", label = "Choose an existing data set comparison:", choices = table_name[[category]])
    
  })
  
  
  #updateSelectInput(session, inputId = "select_textbox", label = "Select comparison table:", choices = table_names)
  
  #########################
  ## General UI Elements ##
  #########################
  
  # Database selection textbox and dropdown.
  # Users provide the network path to the dataset
  # This is in the server.R section so that the default value can be
  #   set using the init$vals reactive values based on defaults, 
  #   bookmarks, a drop-down menu, and URL parsing
  #
  # output$database_textbox - Textbox UI Object
  #
  # input$db - character object
  # 
  
  # Doesn't work with bookmarking
   output$select_category <- renderUI({
     req(init$vals)
     
     id <- "select_category"
     #write("SELECT CATEGORY",stderr())
   
     initial <- NULL
     if(!is.null(init$vals[[id]]))
       initial <- init$vals[[id]]
     #write(initial,stderr())
   
     selectInput("select_category", "choose a category", choices = names(table_name), selected=initial)
     
   })
  output$select_textbox <- renderUI({
    req(init$vals)
    
    id <- "select_textbox"
    #write("SELECT TABLE",stderr())
    
    initial <- NULL
    if(!is.null(init$vals[[id]]))
      initial <- init$vals[[id]]
    
    #write(initial,stderr())
    selectizeInput("select_textbox", "select a table", choices = table_name, selected=initial)
    
  })
  

  output$database_textbox <- renderUI({
    req(init$vals)
    
    id      <- "db"
    label   <- "Location of file with information about each data point (e.g., cell) for comparison"
    initial <- input$Not_on_list
    
    # For Bookmarking... does not work
    # If a stored db exists, pull the value from init$vals
    #if(length(init$vals[[id]]) > 0){
    #  initial <- init$vals[[id]]
      #init$vals <- init$vals[colnames(init$vals)!=id]
    #} else { # Either pull from select_textbox or leave blank
    if (length(input$select_textbox)>0)
      if (!is.element(input$select_textbox,c("Select comparison table...",'Enter your own location'))) {
        initial = table_info[table_info$table_name==input$select_textbox,"table_loc"]
      }
   # }
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  
  output$metadata_textbox <- renderUI({
    req(init$vals)
    
    id      <- "metadata"
    label   <- "(Optional) Location of csv file with information about each annotation (e.g., cell type)"
    initial <- input$Not_on_list
    
    # For Bookmarking... does not work
    # If a stored db exists, pull the value from init$vals
    #if(length(init$vals[[id]]) > 0){
    #  write("DB EXISTS",stderr())
    #  initial <- init$vals[[id]]
    #  write(initial,stderr())
    #  init$vals <- init$vals[colnames(init$vals)!=id]
    #} else { # Either pull from select_textbox or leave blank
    if (length(input$select_textbox)>0)
      if (!is.element(input$select_textbox,c("Select comparison table...",'Enter your own location'))) {
        initial = table_info[table_info$table_name==input$select_textbox,"metadata_loc"]
      }
    #}
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  
  output$dataset_description <- renderUI({
    req(init$vals)
    
    text_desc = "README: Select a category and a comparison table from the boxes above -OR- to compare your own annotation data, choose 'Enter your own location' from the 'Select annotation category' and enter the locations of relevant files in the two boxes above. After files are selected, please WAIT for the annotation table to load. This could take up to a minute, but will likely be much faster. Once loaded, the controls above and below will become responsive.Once a data set is chosen, this pane can be minimized with the '-' in the upper right. The '+' can then be pressed to re-open in order to select a new data set or bookmark the current state of the app."
    
    if (length(input$select_textbox)>0){
    
      # If a stored db exists, pull the value from init$vals
      if(length(init$vals[["select_textbox"]]) > 0){
        text_desc <- init$vals[["select_textbox"]]
      } else {
        
        if (input$select_textbox == 'Enter your own location') {
          text_desc = "User-provided data and (optionally) metadata files. This option allows you to compare multiple annotations for your own cells. It does NOT compare your data against any pre-created annotation tables."
        } else if (input$select_textbox == 'Select comparison table...') {
          # Do nothing... text_desc should remain as initialized above
        } else {
          text_desc = table_info[table_info$table_name==input$select_textbox,"description"]
        }
      }
    }
    
    div(style = "font-size:14px;", strong("Dataset description"),br(),text_desc)
    
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
  
  
  # Check the metadata path provided by input$db
  # returns a corrected path
  #
  # rv_path_metadata() - length 1 character vector
  #
  rv_path_metadata <- reactive({
    req(input$metadata)
    write("Checking and setting input$metadata.", stderr())
    
    input$metadata
  })
  
  
  # Read the CELL annotations table from the dataset
  #
  # rv_anno() - a data.frame
  #
  rv_anno <- reactive({
    req(rv_path())
    write("Reading anno.", stderr())
    
    req(rv_anno_metadata())
    write("... reading cell type information for reordering factors", stderr())
    metadata <- rv_anno_metadata()
    
    # If a .csv file, use vroom.  These can be gzipped
    if(grepl(".csv$",rv_path())|grepl(".gz$",rv_path())) {
      
      withProgress({
        setProgress(value = 0.2,
                    message = "Reading csv file, if it exists.")
        
        fn <- rv_path()
        if(substr(fn,1,4)=="http") {
          if(grepl(".gz$",rv_path())){
            download.file(fn,"tmp.csv.gz",method="auto")
            fn = "tmp.csv.gz"
          } else {
            download.file(fn,"tmp.csv",method="auto")
            fn = "tmp.csv"
          }
        }
        
        anno <- try(vroom(fn))
      })
      
      if(class(anno)[1]!="try-error") {
        withProgress({
          setProgress(value = 0.2,
                      message = "Loading Annotations")

          anno <- as.data.frame(anno)
          
          # Check if first column is unique IDs, and if not, create a new column
          if (length(anno[,1])!=length(unique(anno[,1]))){
            anno <- data.frame(sample_id=paste0("i",1:dim(anno)[1]),anno)
          }
          
          names(anno)[1] <- "sample_id" # Rename sample ids as sample_id
          # Add labels and colors, if needed
          anno <- factorize_annotations(anno,metadata)  # NEW
          anno <- auto_annotate(anno)
          names(anno)[1] <- "sample_id" # Rename sample ids as sample_id again
          
          setProgress(value = 1,
                      message = "Annotations Loaded")
          
          return(anno)
          
        })
      } else {
        write(paste(rv_path(),"does not exist."), stderr())
        return(NULL)
      }
      # If not a csv file, check for h5ad file with annotations in obs (e.g., in scrattch-mapping format)
    } else if(grepl(".h5ad$",rv_path())) {
      
      if(file.exists(rv_path())) {
        withProgress({
          setProgress(value = 0.2,
                      message = "Loading Annotations")
          h5ad <- read_h5ad(rv_path(), backed="r")
          
          anno <- as.data.frame(h5ad$obs)
          
          # Move sample_id to the first column, if it exists
          anno <- anno %>%
            select("sample_id", everything())
          
          # Check if first column is unique IDs, and if not, create a new column
          if (length(anno[,1])!=length(unique(anno[,1]))){
            colnames(anno)[colnames(anno)=="cluster_id"] = "cluster_id2"
            anno <- data.frame(sample_id=paste0("i",1:dim(anno)[1]),anno)
          }
          
          names(anno)[1] <- "sample_id" # Rename sample ids as sample_id
          # Add labels and colors, if needed
          anno <- factorize_annotations(anno,metadata)  # NEW
          anno <- auto_annotate(anno)
          names(anno)[1] <- "sample_id" # Rename sample ids as sample_id again
          
          # Convert factors to characters
          for (cn in colnames(anno)[grepl("_label",colnames(anno))])
            if(is.factor(anno[,cn])) {
              anno[,paste0(substr(cn,1,nchar(cn)-6),"_id")] <- as.numeric(anno[,cn])
              anno[,cn] <- as.character(anno[,cn])
            }
          
          setProgress(value = 1,
                      message = "Annotations Loaded")
          
        })
        return(anno)
        
      } else {
        write(paste(rv_path(),"does not exist."), stderr())
        return(NULL)
      }
      # If not a csv or h5ad file, we expect a directory with an anno.feather file
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
        write(paste(rv_path(),"does not exist."), stderr())
        return(NULL)
      }
    } 
    
  }) # end rv_anno()
  
  
  # Check for valid input
  output$checkInput <- renderUI({
    req(rv_anno)
    if(is.null(rv_anno())){
      p("ENTER VALID CELL ANNOTATION FILE.")
    } else {
      p(" ")
    }
  })
  
  
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
  
  
  
  # Read the CLUSTER annotations table from the dataset (if present)
  #
  # rv_anno_metadata() - a data.frame
  #
  rv_anno_metadata <- reactive({
    req(rv_path_metadata())
    write("Reading cluster anno.", stderr())
    
    # If a .csv file, use vroom; can be gzipped
    if(grepl(".csv$",rv_path_metadata())|grepl(".gz$",rv_path_metadata())) {
      
      anno <- vroom(rv_path_metadata())
      if(class(anno)[1]!="try-error"){ 
        anno <- as.data.frame(anno)
        return(anno)
      } else {
        write(paste(rv_path_metadata(),"does not exist."), stderr())
        return(data.frame(cell_type=c("none","none2"),direction=c("none","none"))) # Provide a generic data frame
      }
    }
    return(data.frame(cell_type=c("none","none2"),direction=c("none","none"))) # Provide a generic data frame
    
  }) # end rv_anno_metadata()
  
  # Check for valid input
  output$metadata_checkInput <- renderUI({
    req(rv_anno_metadata)
    if(is.null(rv_anno_metadata())){
      p("Cluster information file not available.")
    } else {
      p(" ")
    }
  })
  
  
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
  
  # build the filter invert checkbox
  # 
  # output$invert <- which filter values should be inverted
  # invert_annos  <- THIS IS THE VARIABLE TO USE for which filter values shoudl be inverted
  #
  invert_annos <- reactiveVal(character())
  observeEvent(input$invert,{
    invert_annos(input$invert)
  })
  
  output$filter_invert <- renderUI({
    req(rv_desc())
    req(input$sf)
    req(invert_annos)
    
    desc      <- rv_desc()
    usefilter <- intersect(input$sf,desc$base[desc$type=="cat"])
    
    if(length(usefilter)>0){
      id      <- "invert"
      label   <- "Invert?"
      initial <- intersect(usefilter,invert_annos())
      if(length(initial)==0) initial = NULL
    
      checkboxGroupInput(id, label, usefilter, selected = initial)
    }
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
    req(invert_annos)
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
              # Take the opposite if invert filter set
              if (filter_base %in% invert_annos()){ 
                filter_text <- paste0("!(",filter_text,")")
              } 
              
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
    req(invert_annos)
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
              filter_text <- paste0(filter_name,": ",paste(filter_groups,collapse=", "),
                                    ifelse(filter_base %in% invert_annos()," EXCLUDED"," INCLUDED"))
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
  
  
  # Call dendrogram plot rendering function
  output$river_plot <- renderRbokeh({
    req(river_anno())
    req(river_groups())
    
    available_width <- input$dimension[1]#as.numeric(session$clientData$output_dendro_widthfinder_width)
    #print(available_width)

    anno <- river_anno()
    river_groups <- river_groups()
        
    # New code for reordering riverplots to match first in the chain
    anno <- reorder_anno_for_river_plot(anno,river_groups)
    
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
      options <- c("Jaccard","none",input$annocomp_x, input$annocomp_y)
      names(options) <- c("Jaccard","None",
                          desc$name[desc$base == input$annocomp_x],
                          desc$name[desc$base == input$annocomp_y])
    }
  
    id <- "annocomp_color"
    label <- "Color points by"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "none")
    if(length(options)>=1) if(is.element("Jaccard",options))
      initial = "Jaccard"
    
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
    req(input$annocomp_color)
    
    desc <- rv_desc()
    cat_anno <- desc$base[desc$type == "cat"]
    
    if((input$annocomp_x %in% cat_anno & input$annocomp_y %in% cat_anno) & (input$annocomp_color!="Jaccard")) {
      
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
  
  
  output$annocomp_reorderY_selection <- renderUI({
    
    req(rv_desc())
    req(input$annocomp_x)
    req(input$annocomp_y)
    
    desc <- rv_desc()
    cat_anno <- desc$base[desc$type == "cat"]
    
    if(input$annocomp_x %in% cat_anno & input$annocomp_y %in% cat_anno) {
      
      id <- "anno_reorderY"
      label <- "Reorder query?"
      initial <- TRUE
      selectInput(id, label, c("Yes" = TRUE,"No"= FALSE), multiple = FALSE)
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
  
#  annocomp_plot <- eventReactive(input$anno_go, {   #NOTE: to make plots automatic again, replace with " <- reactive { " and delete button on UI page.
   annocomp_plot <- reactive ({   #NOTE: to make plots require a button again, replace with " <- eventReactive(input$anno_go,  { " and uncomment button on UI page.
    
    req(rv_anno())
    req(rv_filtered())
    req(rv_desc())
    req(input$annocomp_x)
    req(input$annocomp_y)
    req(input$annocomp_color)
    #req(input$annocomp_select_mode)
    
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
                        reorderY = input$anno_reorderY,
                        filter_mode = "filter")#input$annocomp_select_mode)
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
  ## Annotation Explorer Box ##
  ##############################
  
  ## UI Elements
  output$explorer_group_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base[desc$type == "cat"]
    names(options) <- desc$name[desc$type == "cat"]
    
    id <- "explorer_group"
    label <- "Annotation group"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$explorer_annotation_selection <- renderUI({
    req(init$vals)
    req(rv_filtered())
    req(input$explorer_group)
    
    # Get values for inputted annotation
    x_group <- input$explorer_group
    anno    <- rv_filtered()
    x       <- anno[,paste0(x_group,"_id")]
    x       <- anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)]
    options <- setNames(x,x)
    
    id <- "explorer_annotation"
    label <- "Annotation value"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  
  output$explorer_comparison_selection <- renderUI({
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- desc$base[desc$type == "cat"]
    names(options) <- desc$name[desc$type == "cat"]
    
    id <- "explorer_comparison"
    label <- "Comparison groups"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[2])
    
    selectizeInput(inputId = id, 
                   label = strong(label), 
                   choices = options, 
                   selected = initial,
                   multiple = TRUE)
  })
  
  output$explorer_plot_type_selection <- renderUI({
    id      <- "explorer_plot_type"
    label   <- "Show plots?"
    initial <- TRUE
    
    selectInput(inputId=id, 
                label=label, 
                choices=c("No"= FALSE,"Yes" = TRUE),
                selected=initial)
  })
  
  output$explorer_height_textbox <- renderUI({
    req(init$vals)
    
    id <- "explorer_height"
    label <- "Plot Height"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "300px")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  output$explorer_maxtypes_textbox <- renderUI({
    req(init$vals)
    
    id <- "explorer_maxtypes"
    label <- "Max to plot"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "10")
    
    textInput(inputId = id, 
              label = strong(label), 
              value = initial, 
              width = "100%")
    
  })
  
  
  
  ## Table with top annotation comparisons
  
  output$explorer_table <- DT::renderDataTable({
    
    req(input$explorer_annotation)
    req(input$explorer_group)
    req(input$explorer_comparison)
    req(rv_filtered())
    req(rv_anno_metadata())
    
    # display top 5 rows at a time
    options(DT.options = list(pageLength = 5, selection=list(mode="single", target="cell")))
    
    # Collect relevant inputs
    anno = as.data.frame(rv_filtered())
    anno = anno[anno[,paste0(input$explorer_group,"_label")]==input$explorer_annotation,]
    anno = as.data.frame(anno)
    cats = input$explorer_comparison # explorer comparison categories, short name
    
    # set up the data frame for max length
    rows = 0
    for (cat in cats) rows = max(rows,length(unique(anno[,paste0(cat,"_label")])))
    df   = data.frame(matrix(NA,nrow = rows, ncol = 2*length(cats)))
    cn   = NULL # Column names for data frame
    for (cat in cats) cn <- c(cn,cat,paste0(cat,"_percent"))
    colnames(df) = cn
    
    # Build the data frame
    for (cat in cats){
      value <- -sort(-table(as.character(anno[,paste0(cat,"_label")])))
      value <- round(1000*value/sum(value))/10
      df[1:length(value),cat] = names(value)
      df[1:length(value),paste0(cat,"_percent")] = value
      if(length(value)<rows){
        df[(length(value)+1):rows,cat] = ""
        df[(length(value)+1):rows,paste0(cat,"_percent")] = 0
      }
    }
    
    # Add a direction column, if available
    # --- This is typically only used in disease studies
    # First check in the anno
    for (cat in cats) if(is.element(paste0(cat,"_direction"),colnames(anno))){
      df[,paste0(cat,"_direction")] <- anno[,paste0(cat,"_direction")][match(df[,cat],anno[,paste0(cat,"_label")])]
      df[,paste0(cat,"_direction")][is.na(df[,paste0(cat,"_direction")])] = "none"
    }
    # Next check in the metadata table, if available
    metadata <- rv_anno_metadata()
    if(!is.null(metadata)) if(is.element("direction",colnames(metadata)))
      for (cat in cats) for (i in 1:dim(df)[1]){
        cluster  <- as.character(df[i,cat])
        whichRow <- which(metadata==cluster, arr.ind = TRUE)
        if(dim(whichRow)[1]>0){
          direction <- as.character(metadata[as.character(whichRow[1,1]),"direction"])
          if(!is.element(direction,c("none", "up","down"))) direction = "none"  # REMOVE THIS HARDCODED LINE
          df[i,paste0(cat,"_direction")] = direction
        }
      }
    # Finally, set all values to "none" if no data exists NOTE: THIS BREAKS THE COLOR CODE, I need to figure out why!
    #for (cat in cats) if(!is.element(paste0(cat,"_direction"),colnames(anno))){
    #  df[,paste0(cat,"_direction")] = rep("none",dim(df)[1])
    #}
    
    # set conditions and return the beautiful table
    return(format_datatable(df,cats))
  })
  
  
  # Plots with top annotation comparisons, if desired
  explorer_box_plot <- reactive({
    req(input$explorer_annotation)
    req(input$explorer_group)
    req(input$explorer_comparison)
    req(input$explorer_plot_type)
    req(input$explorer_maxtypes)
    req(rv_filtered())
    
    if (input$explorer_plot_type){
      # Collect relevant inputs
      anno = as.data.frame(rv_filtered())
      anno = anno[anno[,paste0(input$explorer_group,"_label")]==input$explorer_annotation,]
      anno = as.data.frame(anno)
      cats = input$explorer_comparison # explorer comparison categories, short name
      
      # set up the data frame for max length
      rows = 0
      for (cat in cats) rows = max(rows,length(unique(anno[,paste0(cat,"_label")])))
      df   = data.frame(matrix(NA,nrow = rows, ncol = 2*length(cats)))
      cn   = NULL # Column names for data frame
      for (cat in cats) cn <- c(cn,cat,paste0(cat,"_percent"))
      colnames(df) = cn
      
      # Build the data frame
      for (cat in cats){
        value <- -sort(-table(as.character(anno[,paste0(cat,"_label")])))
        value <- round(1000*value/sum(value))/10
        df[1:length(value),cat] = names(value)
        df[1:length(value),paste0(cat,"_percent")] = value
        if(length(value)<rows){
          df[(length(value)+1):rows,cat] = ""
          df[(length(value)+1):rows,paste0(cat,"_percent")] = 0
        }
      }
      
      # Add a direction column, if available
      # --- This is typically only used in disease studies
      # First check in the anno
      for (cat in cats) if(is.element(paste0(cat,"_direction"),colnames(anno))){
        df[,paste0(cat,"_direction")] <- anno[,paste0(cat,"_direction")][match(df[,cat],anno[,paste0(cat,"_label")])]
        df[,paste0(cat,"_direction")][is.na(df[,paste0(cat,"_direction")])] = "none"
      }
      # Next check in the metadata table, if available
      metadata <- rv_anno_metadata()
      if(!is.null(metadata)) if(is.element("direction",colnames(metadata)))
        for (cat in cats) for (i in 1:dim(df)[1]){
          cluster  <- as.character(df[i,cat])
          whichRow <- which(metadata==cluster, arr.ind = TRUE)
          if(dim(whichRow)[1]>0){
            direction <- as.character(metadata[as.character(whichRow[1,1]),"direction"])
            if(!is.element(direction,c("none", "up","down"))) direction = "none"  # REMOVE THIS HARDCODED LINE
            df[i,paste0(cat,"_direction")] = direction
          }
        }
      # Finally, set all values to "none" if no data exists  # NOTE: THIS BREAKS THE COLOR CODE, I need to figure out why!
      #for (cat in cats) if(!is.element(paste0(cat,"_direction"),colnames(anno))){
      #  df[,paste0(cat,"_direction")] = rep("none",dim(df)[1])
      #}
      
      labeled_barplot_summary(df,cats,maxTypes = as.numeric(input$explorer_maxtypes))
    } else {     # If no plot requested, return void
      ggplot() + theme_void()
    } 
    
  })
  
  output$explorer_box_plot <- renderPlot({
    explorer_box_plot()
  })
  
  output$explorer_box_ui <- renderUI({
    if (input$explorer_plot_type){
      height = input$explorer_height
    } else {
      height = "10px"
    }
    plotOutput("explorer_box_plot", height = height, width = "100%")
  })
  
  
  output$selected_cluster_table <- DT::renderDataTable({
    req(input$explorer_table_cell_clicked)
    req(rv_anno_metadata())
    
    metadata <- rv_anno_metadata()
    cluster  <- input$explorer_table_cell_clicked$value
    
    return(cluster_datatable(cluster,metadata))
  })
  

  
  
  
  
  ###################################################
  ###################################################  
  ###################################################
  ###################################################  
  ###################################################
  ###################################################  
  ###################################################
  ###################################################
  ###################################################
  ###################################################  
  ###################################################
  ###################################################  
  ###################################################
  ###################################################  
  ###################################################
  ###################################################
  
  
  
  
  
  
  
  
  
  
  
  ################################
  ## 2D numeric scatterplot box ##
  ################################
  
  rv_plot_scatter <- reactiveValues()
  
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
  
  # Build a set of categorical filter options from
  # the rv_desc() data.frame
  #
  # num_options() - a named character vector
  #
  num_options <- reactive({
    req(rv_desc())
    
    desc <- rv_desc()
    desc <- desc[desc$type == "num",]
    anno_opts <- desc$base
    names(anno_opts) <- desc$name
    
    anno_opts
    
  })
  
  observe({
    req(rv_anno())
    
    rv_plot_scatter$num_dims <- length(num_options())
    
  })
  
  ## UI Elements
  output$scatter_x_selection <- renderUI({
    req(init$vals)

    options <- num_options()
    
    id <- "scatter_x"
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
  
  output$scatter_y_selection <- renderUI({
    req(init$vals)
    
    options <- num_options()
    
    id <- "scatter_y"
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
  
  
  # UI for choosing to use annotations or expression for colors
  output$radio_scatter_color_type <- renderUI({
    req(init$vals)
    
    id <- "scatter_color_type"
    label <- "Color points using"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "Categoric Annotation")
    
    radioButtons(
      inputId = id,
      label = strong(label),
      choices = c("Categoric Annotation","Numeric Annotations"),
      selected = "Categoric Annotation",
      inline = TRUE
    )
  })
  
  #Selection UI for scatter colors based on Categoric Annotations
  output$selectize_scatter_plot_color <- renderUI({
    req(init$vals)
    
    cat_options <- cat_options()
    
    id <- "scatter_plot_color"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      cat_options[1])
    
    selectizeInput(inputId = id, 
                   label = NULL, 
                   choices = cat_options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  #Selection UI for scatter colors based on Numeric Annotations
  output$scatter_color_gene_red_textbox <- renderUI({
    
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- c("",desc$base)
    names(options) <- c("(none)",desc$name)
    options <- options[c(TRUE,desc$type == "num")]
    
    id <- "scatter_color_gene_red"
    label <- NULL
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = label, 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$scatter_color_gene_green_textbox <- renderUI({
    
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- c("",desc$base)
    names(options) <- c("(none)",desc$name)
    options <- options[c(TRUE,desc$type == "num")]
    
    id <- "scatter_color_gene_green"
    label <- NULL
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = label, 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  output$scatter_color_gene_blue_textbox <- renderUI({
    
    req(init$vals)
    req(rv_desc())
    
    desc <- rv_desc()
    options <- c("",desc$base)
    names(options) <- c("(none)",desc$name)
    options <- options[c(TRUE,desc$type == "num")]
    
    id <- "scatter_color_gene_blue"
    label <- NULL
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      options[1])
    
    selectizeInput(inputId = id, 
                   label = label, 
                   choices = options, 
                   selected = initial,
                   multiple = FALSE)
  })
  
  # UI for scatter plot scaling selection
  output$selectize_scatter_plot_scaling <- renderUI({
    opts <- c("Log 10" = "log10",
              "Linear" = "none")
    
    id <- "scatter_plot_scaling"
    label <- "Scaling"
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      "none")
    
    selectizeInput(inputId = id,
                   label = label,
                   opts,
                   initial,
                   multiple = F,
                   width = "100%")
  })
  
  
  # Selection UI for scatter hover
  output$scatter_plot_hover_selectize <- renderUI({
    
    id <- "scatter_plot_hover"
    label <- "Hover Annotations"
    
    cat_options <- cat_options()
    
    initial <- ifelse(length(init$vals[[id]]) > 0,
                      init$vals[[id]],
                      cat_options[1])
    
    if(grepl(",", initial)) {
      initial_split <- unlist(strsplit(initial,","))
      initial <- cat_options[match(initial_split, cat_options)]
    }
    
    selectizeInput(inputId = id,
                   label = label,
                   cat_options,
                   initial,
                   multiple = T,
                   width = "100%")
  })
  
  
  output$scatter_plot_hover_warning <- renderText({
    req(rv_filtered()) 
    if(nrow(rv_filtered()) > 65530) {
      "Too many points to render all hovers. Some points may not have tooltips."
    } else {
      ""
    }
  })
  
  
  # Width finding hack, as bokeh doesn't detect available space.
  output$scatter_widthfinder <- renderPlot({
    p()
  })
  
  scatter_fg_bg <- eventReactive(input$scatter_plot_go, {
    req(rv_filtered())
    req(rv_desc())
    req(input$scatter_x)
    req(input$scatter_y)

    select_anno <- rv_filtered()
    names(select_anno)[names(select_anno) == "sample_id"] <- "sample_name"
    
    if(input$scatter_color_type == "Categoric Annotation") {
      red_num   <- green_num <- blue_num <- ""
      color_by  <- input$scatter_plot_color
    } else {
      red_num   <- input$scatter_color_gene_red
      green_num <- input$scatter_color_gene_green
      blue_num  <- input$scatter_color_gene_blue
      color_by  <- ".numeric"
    }
    
    build_scatter_fg_bg_points(select_anno = select_anno,
                            color_by = color_by,
                            x_group = input$scatter_x, 
                            y_group = input$scatter_y, 
                            desc = rv_desc(),
                            red_num = red_num,
                            green_num = green_num,
                            blue_num = blue_num)
    
  })
  
  
  # Build scatter plot using rbokeh
  scatter_bokeh_plot <- reactive({
    req(scatter_fg_bg())
    req(rv_anno())
    req(rv_desc())

    available_width <- as.numeric(session$clientData$output_scatter_widthfinder_width)
    
    if(available_width > 800) { available_width <- 800 }
    
    anno <- rv_anno()
    names(anno)[names(anno) == "sample_id"] <- "sample_name"
    
    build_scatter_bokeh(foreground_points = scatter_fg_bg()$foreground_points,
                     anno = anno,
                     desc = rv_desc(),
                     hovers = input$scatter_plot_hover,
                     width = available_width,
                     webgl = TRUE,
                     pointSize = input$scatter_pt_size,
                     xlab = input$scatter_x,
                     ylab = input$scatter_y)
  })
  
  
  output$scatter_bokeh_plot <- renderRbokeh({
    
    # Call plot rendering function 
    scatter_bokeh_plot()
  })
  
  
  # Plot UI for scatter bokeh plot
  output$scatter_plot_ui <- renderUI({
    req(rv_desc())
    desc     <- rv_desc()
    num_dims <- sum(desc$type == "num")
    if(num_dims>1) {
      
      available_width <- as.numeric(session$clientData$output_scatter_widthfinder_width)
      
      if(available_width > 800) { available_width <- 800 }
      
      rbokehOutput("scatter_bokeh_plot", width = available_width, height = available_width)
      
    } else {
      
      # Ideally this should be printed with a figure, as in the commented text below, but it doesn't work for some reason

      print("     2+ numeric values must be in annotation table to compare.",stderr())
      
    }
  })
  
  
  
  # scatter save button 
  output$scatter_save_svg <- downloadHandler(
    
    available_width <- as.numeric(session$clientData$output_scatter_widthfinder_width),
    
    
    filename = function() {
      paste("scatter_plot-", Sys.Date(), ".svg", sep="")
    },
    content = function(file) {
      withProgress(message = "Writing SVG file. Conversion from HTML widget may take 30-40 seconds...", {
        
        # Save widget as HTML file 
        htmlwidgets::saveWidget(widget=scatter_bokeh_plot(), file="./png_to_svg_conversion/simplePlot.html")
        
        # This converts HTML to PDF 
        # jsGraphic2Pdf("simplePlot.html", c(500, 500, 10, 10), highRes = TRUE)
        
        # This only converts HTML to PNG
        jsGraphic2Png("./png_to_svg_conversion/simplePlot.html", outFile = "./png_to_svg_conversion/JSgraphic.png", c(900, 900, 10, 10), slow = TRUE, highRes = TRUE)
        # jsGraphic2Png("simplePlot.html", outFile = "JSgraphic.png", c(900, 900, 10, 10), highRes = TRUE)
        
        # Move said file to a chmod 777 directory 
        # system("mv JSgraphic.png /local1/connect_tmp/")
        
        # Roundabout way of generating SVG from a pre-saved image, in this case a PNG 
        # This conversion is done through the "magick" package
        my_image <- image_read("./png_to_svg_conversion/JSgraphic.png")
        
        # Might not need to convert at all? 
        # my_svg <- image_convert(my_image, format="svg")
        
        # Save the pre-made SVG 
        image_write(my_image, path = file, format = "svg")
        
      })
      
    }
  )
  
  
  
  

   
}





