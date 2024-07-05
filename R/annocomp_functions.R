## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE, roundall = F) {
  require(dplyr)
  # This does the summary. For each group return a vector with
  # N, mean, and sd
  
  names(data)[names(data) == measurevar] <- "measurevar"
  
  datac <- data %>%
    select(one_of(groupvars,"measurevar")) %>%
    filter(ifelse(na.rm == T, !is.na(measurevar), T)) %>%
    mutate(measurevar = as.numeric(measurevar)) %>%
    group_by_(c(groupvars)) %>%
    summarise(N = n(),
              median = median(measurevar),
              mean = mean(measurevar),
              max = max(measurevar),
              sd = ifelse(N == 1, 0, sd(measurevar)),
              q25 = as.numeric(quantile(measurevar, 0.25)),
              q75 = as.numeric(quantile(measurevar, 0.75))) %>%
    mutate(se = sd/sqrt(N))
  #%>%
  #  mutate(ci =  se * qt(conf.interval/2 + 0.5, N-1))
  
  
  if(roundall) {
    roundcols <- c("median","mean","max","sd","q25","q75","se","ci")
    datac[roundcols] <- round(datac[roundcols],3)
  }
  
  # datac <- datac %>%
  #   mutate(xpos = 1:n())
  
  return(datac)
}

sci.label <- function(vec) {
  labels <- character()
  for(i in 1:length(vec)) {
    x <- round(vec[i],0)
    first <- round(x/(10^(nchar(x)-1)),1)
    if(first %% 1 == 0) {
      first <- paste0(first,".0")
    }
    label <- first
    #    label <- paste0(first,"E",nchar(x)-1)
    labels <- c(labels,label)
  }
  return(labels)
}


build_annocomp_stats <- function(anno, desc, x_group, y_group) {
  
  cat_annotations <- desc$base[desc$type == "cat"]
  num_annotations <- desc$base[desc$type == "num"]
  
  x_type <- ifelse(x_group %in% cat_annotations,"cat","num")
  y_type <- ifelse(y_group %in% cat_annotations,"cat","num")
  
  if(x_type == "cat" & y_type == "num") {
    
    data_col <- paste0(y_group,"_label")
    group_col <- paste0(x_group,"_id")
    do_stats <- T
    
  } else if(x_type == "num" & y_type == "cat") {
    
    data_col <- paste0(x_group,"_label")
    group_col <- paste0(y_group,"_id")
    do_stats <- T
    
  } else {
    
    do_stats <- F
    
  }
  
  if(do_stats == T) {
    
    stats <- summarySE(anno, measurevar = data_col, groupvars = group_col) %>%
      mutate(median_lab = ifelse(median < 1,round(median,3),median)) %>%
      mutate(median_lab = ifelse(median >= 1 & median < 10, round(median,2),median_lab)) %>%
      mutate(median_lab = ifelse(median > 10,round(median,1),median_lab))
    
    
  } else {
    stats <- NULL
  }
  
  return(stats)
  
}

build_annocomp_plot <- function(anno, 
                                filtered, 
                                desc, 
                                stats, 
                                x_group, 
                                y_group, 
                                c_group, 
                                denom, 
                                reorderY,
                                filter_mode = "filter") {
  
  cat_annotations <- desc$base[desc$type == "cat"]
  num_annotations <- desc$base[desc$type == "num"]
  
  x_id <- paste0(x_group,"_id")
  x_label <- paste0(x_group,"_label")
  x_type <- ifelse(x_group %in% cat_annotations,"cat","num")
  x_name <- desc$name[desc$base == x_group]
  
  y_id <- paste0(y_group,"_id")
  y_label <- paste0(y_group,"_label")
  y_type <- ifelse(y_group %in% cat_annotations,"cat","num")
  y_name <- desc$name[desc$base == y_group]
  
  # Redirect to 2d scatterplot panel if data is numeric
  if(x_type == "num" & y_type == "num"){
    p = ggplot() + theme_void() + ggtitle("     Pairs of numeric values must be visualized in the 'Compare numeric annotations' tab.")
    return(p)
  }
  
  if(c_group=="Jaccard"){
    # These functions are located in "pairwise_functions.R" currently
    p <- build_compare_jaccard_plot(anno = filtered, 
                                    x_group = x_group, 
                                    y_group = y_group,
                                    reorderY = reorderY,
                                    maxInputs = 10000000)  # Can fix later to avoid hard-coding
    
  } else {
  
  point_color <- paste0(c_group,"_color")
  
  # Reorder the y-axis ids IF reorderY=TRUE
  if (reorderY){
    x <- filtered[,paste0(x_group,"_id")]
    x <- factor(filtered[,paste0(x_group,"_label")], levels = filtered[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
    y <- filtered[,paste0(y_group,"_id")]
    y <- factor(filtered[,paste0(y_group,"_label")], levels = filtered[,paste0(y_group,"_label")][match(sort(unique(y), decreasing = TRUE),y)])
    names(x) <- names(y) <- filtered$sample_id
    
    common.cells <- intersect(names(x), names(y))
    y     <- y[common.cells]
    x     <- x[common.cells]
    tb    <- table(x, y)
    tmp   <- t(tb)
    tmp   <- tmp/rowSums(tmp)
    ord   <- order(apply(tmp,1,which.max)*10,rowMeans(t(apply(tmp,1,cumsum))))
    y     <- factor(filtered[,paste0(y_group,"_label")], levels = colnames(tb)[ord])
    filtered[,paste0(y_group,"_id")] = as.numeric(y)
  }
  
  # Do the rest of the stuff
  
  if(filter_mode == "filter") {
    plot_anno <- filtered
  } else {
    plot_anno <- anno
  }
  
  if(!is.null(stats)) {
    plot_anno <- plot_anno %>% 
      left_join(stats)
  }
  
  # Determine positions on axes
  # X-axis
  if(x_type == "cat") {
    
    x_order <- data.frame(id = unique(plot_anno[[x_id]])) %>%
      arrange(id) %>%
      mutate(xpos = 1:n())
    
    names(x_order)[1] <- x_id
    
    plot_anno <- plot_anno %>% 
      left_join(x_order)
    
  } else if(x_type == "num") {
    
    x_order <- data.frame(sample_id = plot_anno$sample_id,
                          xpos = as.numeric(plot_anno[[x_label]]))
    
    plot_anno <- plot_anno %>%
      left_join(x_order) %>%
      filter(complete.cases(plot_anno))
    
  }
  
  # Y-axis
  if(y_type == "cat") {
    

      y_order <- data.frame(id = unique(plot_anno[[y_id]])) %>%
        arrange(id) %>%
        mutate(ypos = n():1)

    names(y_order)[1] <- y_id
    plot_anno <- plot_anno %>% 
      left_join(y_order)
    
  } else if(y_type == "num") {
    
    y_order <- data.frame(sample_id = plot_anno$sample_id,
                          ypos = as.numeric(plot_anno[[y_label]]))
    
    plot_anno <- plot_anno %>% 
      left_join(y_order)
    # %>%
    #   filter(complete.cases(plot_anno))
    
    
  }
  
  # If x is numeric, set y positions based on ypos_anno
  if(x_type == "num" & y_type == "cat") {
    
    ypos_anno <- plot_anno %>%
      select(one_of(y_id,"ypos")) %>%
      unique()
    
    stats <- ypos_anno %>%
      left_join(stats)
    
  }
  
  # If y is numeric, set x positions based on xpos_anno
  if(x_type == "cat" & y_type == "num") {
    
    xpos_anno <- plot_anno %>%
      select(one_of(x_id,"xpos")) %>%
      unique()
    
    stats <- xpos_anno %>%
      left_join(stats)
    
  }
  
  if(c_group == "none") {
    
    plot_anno <- plot_anno %>% 
      mutate(point_color = "skyblue")
    
  } else {
    
    plot_anno <- plot_anno %>% 
      mutate(point_color = plot_anno[[point_color]])
    
  }
  
  if(filter_mode == "highlight") {
    plot_anno$point_color[!plot_anno$sample_id %in% filtered$sample_id] <- "#808080"
  }
  
  if(x_type == "cat") {
    
    x_labels <- plot_anno %>%
      select(one_of("xpos",x_label)) %>%
      unique() %>%
      arrange(xpos) %>%
      rename("x_label" = x_label)
    
  }
  
  if(y_type == "cat") {
    
    y_labels <- plot_anno %>%
      select(one_of("ypos",y_label)) %>%
      unique() %>%
      arrange(ypos) %>%
      rename("y_label" = y_label)
    
  }
  
  p <- ggplot() +
    scale_color_identity() +
    theme_bw(14) +
    theme(panel.border = element_blank()) +
    theme(line=element_line(size=0.2))
  
  if(x_type == "cat" & y_type == "num") {
    
    p <- p +
      geom_quasirandom(data = plot_anno,
                       aes(x = xpos, y = ypos, color = point_color),
                       groupOnX= TRUE,
                       position = position_jitter(width = .3,height = 0)) +
      geom_errorbar(data = stats,
                    aes(x = xpos, 
                        ymin = q25, 
                        ymax = q75), 
                    width = 0.5, 
                    size =  0.5) +
      geom_segment(data = stats,
                   aes(x = xpos - 0.1, xend = xpos + 0.1, y = median, yend = median),
                   size = 1, color = "red") +
      scale_x_continuous(x_name, breaks = x_labels$xpos, labels = x_labels$x_label, expand = c(0,0)) +
      scale_y_continuous(y_name) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
    
  } else if(x_type == "num" & y_type == "cat") {
    
    p <- p +
      geom_quasirandom(data = plot_anno,
                       aes(x = xpos, 
                           y = ypos, 
                           color = point_color),
                       groupOnX = FALSE,
                       position = position_jitter(height = 0.3, 
                                                  width = 0)) +
      geom_errorbarh(data = stats,
                     aes(x = 0,
                         y = ypos, 
                         xmin = q25, 
                         xmax = q75), 
                     height = 0.5, 
                     size =  0.5) +
      geom_segment(data = stats,
                   aes(y = ypos - 0.1, 
                       yend = ypos + 0.1, 
                       x = median, 
                       xend = median),
                   size = 1, 
                   color = "red") +
      scale_x_continuous(x_name) +
      scale_y_continuous(y_name, breaks = y_labels$ypos, labels = y_labels$y_label)
    
  } else if(x_type == "num" & y_type == "num") {
    
    p <- p +
      geom_point(data = plot_anno,
                 aes(x = xpos, y = ypos, color = point_color))+
      scale_x_continuous(x_name) +
      scale_y_continuous(y_name)
    
  } else if(x_type == "cat" & y_type == "cat") {
    
    plot_anno <- plot_anno %>%
      group_by(xpos,ypos,point_color) %>%
      summarise(n = n()) %>%
      ungroup()
    
    if(isTruthy(denom)) {
      if(denom == x_group) {
        # As fraction of all X categories
        
        denom_vals <- plot_anno %>%
          group_by(xpos) %>%
          summarise(denom = sum(n))
        
        plot_anno <- plot_anno %>%
          left_join(denom_vals) %>%
          mutate(frac = n/denom)
        
        p <- p +
          geom_point(data = plot_anno,
                     aes(x = xpos, y = ypos, size = frac, color = point_color))
        
      } else if(denom == y_group) {
        # As fraction of all Y categories
        denom_vals <- plot_anno %>%
          group_by(ypos) %>%
          summarise(denom = sum(n))
        
        plot_anno <- plot_anno %>%
          left_join(denom_vals) %>%
          mutate(frac = n/denom)
        
        p <- p +
          geom_point(data = plot_anno,
                     aes(x = xpos, y = ypos, size = frac, color = point_color))
      } else {
        
        p <- p +
          geom_point(data = plot_anno,
                     aes(x = xpos, y = ypos, size = n, color = point_color))
        
      }
      
    } else {
      p <- p +
        geom_point(data = plot_anno,
                   aes(x = xpos, y = ypos, size = n, color = point_color))
    }
    
    p <- p +
      scale_size_area() +
      scale_x_continuous(x_name, breaks = x_labels$xpos, labels = x_labels$x_label) +
      scale_y_continuous(y_name, breaks = y_labels$ypos, labels = y_labels$y_label) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
    }
  }
  
  return(p)
  
}
