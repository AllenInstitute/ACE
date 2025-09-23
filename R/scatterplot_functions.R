color_sum <- function(col1, col2) {
  rgbmat1 <- col2rgb(col1)/255
  rgbmat2 <- col2rgb(col2)/255
  # print(rgbmat1)
  # print(rgbmat2)
  mix <- rgbmat1 + rgbmat2
  
  mix[mix > 1] <- 1
  
  rgb(mix[1],mix[2],mix[3])
}

build_scatter_fg_bg_points <- function(anno = NULL,
                                       select_anno, 
                                       color_by = ".numeric",
                                       x_group = "", 
                                       y_group = "", 
                                       desc = NULL,
                                       red_num = "",
                                       green_num = "",
                                       blue_num = "") {
  
  common_samples  <- select_anno$sample_name
  cat_annotations <- desc$name[desc$type == "cat"]
  num_annotations <- desc$name[desc$type == "num"]
  
  x_id <- paste0(x_group,"_id")
  x_label <- paste0(x_group,"_label")
  x_name <- desc$name[desc$base == x_group]
  
  y_id <- paste0(y_group,"_id")
  y_label <- paste0(y_group,"_label")
  y_name <- desc$name[desc$base == y_group]
  
  if(is.null(anno)){
    background_samples <- character(0)
    background_points  <- NULL
  } else {
    background_samples <- setdiff(anno$sample_name,common_samples)
    anno <- anno[is.element(anno$sample_name,background_samples),]
  }

  if(color_by == ".numeric") {
    
    foreground_points   <- select_anno[,paste0(num_annotations,"_label")]
    colnames(foreground_points) <- substr(colnames(foreground_points),1,nchar(colnames(foreground_points))-6)
    foreground_points$x <- select_anno[[x_label]]
    foreground_points$y <- select_anno[[y_label]]
    
    if(length(background_samples)>1){
      background_points   <- anno[,paste0(num_annotations,"_label")]
      colnames(background_points) <- substr(colnames(background_points),1,nchar(colnames(background_points))-6)
      background_points$x <- anno[[x_label]]
      background_points$y <- anno[[y_label]]
      
      background_points <- background_points %>%
        mutate(red = NA,
               green = NA,
               blue = NA) %>%
        mutate(color = "#808080")
    }
    
    if(red_num != "") {
      red_data <- foreground_points[[red_num]]
      
      if(sum(red_data) > 0) {
        red_colors <- values_to_colors(red_data, colorset = c("black","red"))
      } else {
        red_colors <- rep("#000000",nrow(foreground_points))
      }
      
    } else{
      red_colors <- rep("#000000",nrow(foreground_points))
    }
    
    if(green_num != "") {
      green_data <- foreground_points[[green_num]]
      
      if(sum(green_data) > 0) {
        green_colors <- values_to_colors(green_data, colorset = c("black","green"))
      } else {
        green_colors <- rep("#000000",nrow(foreground_points))
      }
      
    } else{
      green_colors <- rep("#000000",nrow(foreground_points))
    }
    
    if(blue_num != "") {
      blue_data <- foreground_points[[blue_num]]
      
      if(sum(blue_data) > 0) {
        blue_colors <- values_to_colors(blue_data, colorset = c("black","blue"))
      } else {
        blue_colors <- rep("#000000",nrow(foreground_points))
      }
      
    } else{
      blue_colors <- rep("#000000",nrow(foreground_points))
    }
    
    rgb <- rep("#000000", nrow(select_anno))
    
    for(i in 1:nrow(select_anno)) {
      rg <- color_sum(red_colors[i], green_colors[i])
      rgb[i] <- color_sum(rg, blue_colors[i])
    }
    
    foreground_points$red   <- red_colors
    foreground_points$green <- green_colors
    foreground_points$blue  <- blue_colors
    foreground_points$color <- rgb

  } else {
    
    foreground_points   <- select_anno[,paste0(cat_annotations,"_label")]
    colnames(foreground_points) <- substr(colnames(foreground_points),1,nchar(colnames(foreground_points))-6)
    foreground_points$x <- as.numeric(select_anno[[x_label]])
    foreground_points$y <- as.numeric(select_anno[[y_label]])
    foreground_points$color <- as.character(select_anno[,paste0(color_by,"_color")])
    
    if(length(background_samples)>1){
      background_points   <- anno[,paste0(cat_annotations,"_label")]
      colnames(background_points) <- substr(colnames(background_points),1,nchar(colnames(background_points))-6)
      background_points$x <- as.numeric(anno[[x_label]])
      background_points$y <- as.numeric(anno[[y_label]])
      
      background_points <- background_points %>%
        mutate(color = "#808080")
    }

  }

  foreground_points$sample_name <- select_anno$sample_name
  
  if(length(background_samples)>1){
    background_points$sample_name <- anno[is.element(anno$sample_name,background_samples),"sample_name"]
  } else {
    background_points = NULL
  }
  
  list(foreground_points = foreground_points, 
       background_points = background_points)
  
}


build_scatter_bokeh <- function(foreground_points = NULL,
                                background_points = NULL,
                                anno,
                                desc,
                                hovers = NULL,
                                width = 1000,
                                webgl = FALSE,
                                pointSize = 6,
                                xlab = "",
                                ylab = "") {
  
  library(dplyr)
  library(rbokeh)
  foreground_points$pointSize = pointSize
  foreground_points$pointSize = as.numeric(foreground_points$pointSize)
  
  if(length(background_points)>1){
    write("TEST",stderr())
    write(dim(foreground_points),stderr())
    write(dim(background_points),stderr())
    write(colnames(foreground_points),stderr())
    write(colnames(background_points),stderr())
    background_points$pointSize = 2
    background_points$pointSize = as.numeric(background_points$pointSize)
  }
  
  write(xlab,stderr())
  write(ylab,stderr())

  if(!is.null(hovers)) {
    hover_columns <- paste0(hovers,"_label")
    
    hover_anno <- anno %>%
      select(one_of(c("sample_name",hover_columns)))
    
    hover_set <- c("sample_name",hover_columns)
    
    if(!is.null(desc)) {
      names(hover_set) <- c("sample_name",desc$name[match(hovers,desc$base)])
    }
    names(hover_set)[1] <- "Sample Name"
    #print(names(hover_set))
  }
  
  b <- figure(width = width, height = width,
              xlab = xlab, ylab = ylab,
              #xaxes = FALSE, yaxes = FALSE, # new
              xgrid = FALSE, ygrid = FALSE, # new
              tools = c("pan", "wheel_zoom", "box_zoom", "reset", "save", "help"),
              webgl = webgl)
  
  if(is.null(foreground_points)){
    text <- bokeh.models.Label(x = 0.5, y = 0.5, text = "There are not enough numeric variables in this annotation table to compare two numeric annotations.", text_align = "center", text_baseline = "middle")
    b.add_layout(text)
    return(b)
  }
  
  if(is.null(hovers)) {
    
    if(!is.null(background_points) > 0) {
      b <- b %>%
        ly_points(data = background_points,
                  x = x, y = y,
                  color = color,
                  size = pointSize,
                  legend = FALSE) 
    }
    
    b <- b %>%
      ly_points(data = foreground_points,
                x = x, 
                y = y,
                color = color,
                size = pointSize,
                fill_alpha = 1,
                legend = FALSE) 
    
  } else {
    
    if(!is.null(background_points) > 0) {
      b <- b %>%
        ly_points(data = background_points,
                  x = x, y = y,
                  color = color,
                  size = pointSize,
                  legend = FALSE) 
    }
    
    foreground_points <- foreground_points %>%
      left_join(hover_anno, by = "sample_name")
    
    # Workaround for rbokeh limits on number of hovers
    # Can't have more than 65536 without causing problems
    # Truncate to 65530 just to be safe.
    
    if(nrow(foreground_points) > 65530) {
      hover_sample <- sample(1:nrow(foreground_points),
                             65530)
      non_hover_points <- foreground_points[-hover_sample,]
      foreground_points <- foreground_points[hover_sample,]
      
      b <- b %>% 
        ly_points(data = non_hover_points,
                  x = x, 
                  y = y,
                  color = color,
                  size = pointSize,
                  fill_alpha = 1,
                  legend = FALSE) 
    }
    
    b <- b %>%
      ly_points(data = foreground_points,
                x = x, 
                y = y,
                color = color,
                size = pointSize,
                fill_alpha = 1,
                legend = FALSE, # to remove legend, which sometimes appears
                hover = as.list(hover_set)) 
    
  } 
  
  b
  
}