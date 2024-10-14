# New code for reordering riverplots to match first in the chain
reorder_anno_for_river_plot <- function(anno,river_groups){
  if(length(river_groups)<2)
    return(anno)
  
  for (i in 2:length(river_groups)){
    
    x_group <- river_groups[1]
    y_group <- river_groups[i]
    x <- anno[,paste0(x_group,"_id")]
    x <- factor(anno[,paste0(x_group,"_label")], levels = anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
    y <- anno[,paste0(y_group,"_id")]
    y <- factor(anno[,paste0(y_group,"_label")], levels = anno[,paste0(y_group,"_label")][match(sort(unique(y), decreasing = TRUE),y)])
    names(x) <- names(y) <- anno$sample_id
    common.cells <- intersect(names(x), names(y))
    y   <- y[common.cells]
    x   <- x[common.cells]
    tb  <- table(x, y)
    tmp <- t(tb)
    tmp <- tmp/rowSums(tmp)
    ord <- order(apply(tmp,1,which.max)*10,rowMeans(t(apply(tmp,1,cumsum))))
    y   <- factor(y,levels = colnames(tb)[ord])
    anno[,paste0(y_group,"_label")] <- y
    anno[,paste0(y_group,"_id")]    <- as.numeric(y)
  }
  return(anno)
}


######################################

# library(ggplot2)
# library(dplyr)
# library(feather)

# Error function
# used to generate sigmoidal curve
erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}

# generates x-y coordinates for a sigmoidal line from x,y to xend,yend with
# the given number of steps.
# additional arguments define what curve to use
# and how far in the x-direction to use it
sigline <- function(x = 0, xend = 1, 
                    y = 0, yend = 1, 
                    steps = 50,
                    sigfun = "erf",
                    sigx = 1.5) {
  
  xsteps <- seq(-sigx, sigx, length.out = steps)
  
  if(sigfun == "erf") {
    ysteps <- erf(xsteps)
  }
  
  xsteps <- (xsteps + sigx)/(2*sigx)
  ysteps <- (ysteps + max(ysteps))/(2*max(ysteps))
  
  xscaled <- xsteps*(xend - x) + x
  yscaled <- ysteps*(yend - y) + y
  
  data.frame(x = c(x, xscaled, xend),
             y = c(y, yscaled, yend))
  
}

# expands a sigline into a ribbon by adding a height
# can expand using original line as the top, bottom, or mid-point 
# of the ribbon
sigribbon <- function(sigline, height, from = "top") {
  library(dplyr)
  
  if(from == "top") {
    ribbon <- sigline %>%
      mutate(ymin = y - height)
  } else if(from == "bot") {
    ribbon <- sigline %>%
      rename(y = ymin) %>%
      mutate(y = ymin + height)
  } else if(from == "mid") {
    ribbon <- sigline %>%
      mutate(y = y + height/2,
             ymin = y - height)
  }
  
  ribbon
  
}

make_group_nodes <- function(anno,
                             group_by,
                             xpos = NULL,
                             rev_y = FALSE) {
  
  library(dplyr)
  
  nodes <- data.frame(id = numeric(),
                      name = character(),
                      color = character(),
                      n = numeric(),
                      group = character(),
                      xpos = numeric())
  
  for(i in 1:length(group_by)) {
    base <- group_by[i]
    anno_id <- paste0(base,"_id")
    anno_label <- paste0(base,"_label")
    anno_color <- paste0(base,"_color")
    
    grouping <- c(anno_id,anno_label,anno_color)
    
    if(rev_y) {
      group_nodes <- anno %>%
        select(one_of(grouping))
      
      names(group_nodes) <- c("id","name","color")
      
      group_nodes <- group_nodes %>%
        group_by(id, name, color) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        arrange(-id) %>%
        mutate(group = base) %>%
        ungroup()
    } else {
      group_nodes <- anno %>%
        select(one_of(grouping))
      
      names(group_nodes) <- c("id","name","color")
      
      group_nodes <- group_nodes  %>%
        group_by(id, name, color) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        arrange(id) %>%
        mutate(group = base) %>%
        ungroup()
    }
    
    
    if(is.null(xpos)) {
      group_nodes <- mutate(group_nodes, xpos = i)
    } else {
      group_nodes <- mutate(group_nodes, xpos = xpos[i])
    }
    
    names(group_nodes) <- c("id","name","color","n","group","xpos")
    
    nodes <- rbind(nodes, group_nodes)
  }
  
  nodes
}

make_plot_nodes <- function(group_nodes,
                            # % of height to distribute for padding between nodes
                            pad = 0.1,
                            # plot space for total width
                            width = 0.1) {
  
  library(dplyr)
  
  total_n <- sum(group_nodes$n)/length(unique(group_nodes$group))
  total_pad <- total_n * pad
  
  group_rects <- group_nodes %>%
    group_by(group) %>%
    mutate(xmin = xpos - width/2,
           xmax = xpos + width/2,
           n_groups = n(),
           group_pad = ifelse(n_groups > 1, 
                              total_pad/(n() - 1), 
                              total_pad),
           n_cum = cumsum(n),
           ymin = ifelse(n_groups > 1,
                         lag(n_cum, default = 0) + (1:n() - 1)*group_pad,
                         group_pad / 2),
           ymax = ifelse(n_groups > 1,
                         n_cum + (0:(n()-1))*group_pad,
                         group_pad /2 + n))
  
  group_rects
  
}

make_group_links <- function(anno,
                             group_by,
                             plot_nodes) {
  
  library(dplyr)
  
  pairs <- list()
  
  for(i in 2:length(group_by)) {
    pair <- group_by[(i-1):i]
    pairs <- c(pairs, list(pair))
  }
  
  for(pair in pairs) {
    base <- pair
    anno_id <- paste0(base,"_id")
    anno_label <- paste0(base,"_label")
    anno_color <- paste0(base,"_color")
    
    group1_nodes <- plot_nodes %>%
      filter(group == base[1]) %>%
      select(group, id, xmax, ymin, n)
    names(group1_nodes) <- c("group1",anno_id[1],"x","group1_min","group1_n")
    
    group2_nodes <- plot_nodes %>%
      filter(group == base[2]) %>%
      select(group, id, xmin, ymin, n)
    names(group2_nodes) <- c("group2",anno_id[2],"xend","group2_min","group2_n")
    
    grouping <- c(anno_id, anno_label, anno_color)
    
    group_links <- anno %>%
      select(one_of(grouping)) %>%
      group_by_(.dots = grouping) %>%
      summarise(n = n()) %>%
      arrange_(.dots = anno_id) %>%
      mutate(group1 = base[1],
             group2 = base[2]) %>%
      ungroup() %>%
      left_join(group1_nodes) %>%
      left_join(group2_nodes) %>%
      group_by_(.dots = anno_id[1]) %>%
      arrange_(.dots = anno_id[2]) %>%
      mutate(y = group1_min + cumsum(n)) %>%
      ungroup() %>%
      group_by_(.dots = anno_id[2]) %>%
      arrange_(.dots = anno_id[1]) %>%
      mutate(yend = group2_min + cumsum(n)) %>%
      ungroup()
    
    names(group_links) <- c("group1_id","group2_id",
                            "group1_label","group2_label",
                            "group1_color","group2_color",
                            "n","group1","group2",
                            "x","group1_min","group1_n",
                            "xend","group2_min","group2_n",
                            "y","yend")
    
    group_links <- group_links %>%
      rowwise() %>%
      mutate(link_id = paste0(group1_label,"_",group1_id,"_to_",
                              group2_label,"_",group2_id)) %>%
      ungroup()
    
    if(exists("all_links")) {
      all_links <- rbind(all_links, group_links)
    } else {
      all_links <- group_links
    }
    
  }
  
  all_links <- all_links %>%
    mutate(group1_perc = round(n/group1_n*100,2),
           group2_perc = round(n/group2_n*100,2))
  
  all_links
  
}

make_plot_links <- function(group_links,
                            fill = NULL) {
  
  library(dplyr)
  
  for(i in 1:nrow(group_links)) {
    
    link_line <- sigline(x = group_links$x[i], xend = group_links$xend[i],
                         y = group_links$y[i], yend = group_links$yend[i])
    
    link_ribbon <- sigribbon(link_line, h = group_links$n[i])
    
    if(is.null(fill)) {
      link_ribbon <- mutate(link_ribbon, fill = "#A7A9AC")
    } else if(fill == group_links$group1[i]) {
      link_ribbon <- mutate(link_ribbon, fill = group_links$group1_color[i])
    } else if(fill == group_links$group2[i]) {
      link_ribbon <- mutate(link_ribbon, fill = group_links$group2_color[i])
    } else {
      link_ribbon <- mutate(link_ribbon, fill = "#A7A9AC")
    }
    
    link_ribbon <- mutate(link_ribbon, link_id = i)
    
    if(exists("all_ribbons")) {
      all_ribbons <- rbind(all_ribbons, link_ribbon)
    } else {
      all_ribbons <- link_ribbon
    }
  }
  
  all_ribbons
  
}

build_river_plot <- function(anno, group_by, pad = 0.1, fill_group = NULL) {
  
  library(ggplot2)
  
  group_nodes <- make_group_nodes(anno, group_by)
  plot_nodes <- make_plot_nodes(group_nodes, pad = pad)
  
  group_links <- make_group_links(anno, group_by, plot_nodes)
  plot_links <- make_plot_links(group_links, fill = fill_group)
 
  p <- ggplot() +
    geom_rect(data = plot_nodes,
              aes(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax,
                  fill = color),
              color = "#A7A9AC") +
    geom_ribbon(data = plot_links,
                aes(x = x, ymax = y,
                    ymin = ymin,
                    group = link_id,
                    fill = fill),
                color = "#A7A9AC",
                alpha = 0.4) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void()+
    geom_text(data = plot_nodes,
            aes(x = (xmin+xmax)/2,
            y = (ymax + ymin)/2,
            label = name)) 
    
  p
  
}

build_river_plot_bokeh <- function(anno, 
                                   group_by, 
                                   pad = 0.2, 
                                   fill_group = NULL,
                                   node_labels = c("none","all","outer"),
                                   node_n = FALSE,
                                   height = 750,
                                   width = 1000) {
  library(rbokeh)
  
  group_nodes <- make_group_nodes(anno, group_by, rev_y = FALSE)
  plot_nodes <- make_plot_nodes(group_nodes, pad = pad) %>%
    ungroup()
  
  #must be sorted by link_id for polygon hover to work
  group_links <- make_group_links(anno, group_by, plot_nodes) %>%
    arrange(link_id)
  
  poly_links <- data.frame(x = numeric, y = numeric, link_id = character())
  for(i in 1:nrow(group_links)) {
    plot_links <- make_plot_links(group_links[i,], fill = fill_group)
    
    poly_link <- data.frame(x = c(rev(plot_links$x),plot_links$x),
                            y = c(rev(plot_links$ymin),plot_links$y),
                            link_id = rep(i, 2),
                            fill = rep(plot_links$fill,2))
    
    poly_links <- rbind(poly_links, poly_link)
    
  }
  
  node_ymax <- max(plot_nodes$ymax)
  
  plot_nodes$ymin <- node_ymax - plot_nodes$ymin
  plot_nodes$ymax <- node_ymax - plot_nodes$ymax
  poly_links$y <- node_ymax - poly_links$y
  
  if(node_n) {
    plot_nodes$name <- paste0(plot_nodes$name," (",plot_nodes$n,")")
  }
  
  # hover behavior is strange - it doesn't follow grouping,
  # so these have to be added in sequence
  poly_links$group1_label <- paste0(group_links$group1_label, " (", group_links$group1_perc, "%)")
  poly_links$group2_label <- paste0(group_links$group2_label, " (", group_links$group2_perc, "%)")
  poly_links$n <- group_links$n
  
  b <- figure(height = height, width = width) %>%
    ly_rect(data = plot_nodes,
            xleft = xmin, xright = xmax,
            ybottom = ymax, ytop = ymin,
            color = color,
            hover = list("Group" = name,
                         "N Cells" = n),
            fill_alpha = 1)
  
  if(is.null(fill_group)) {
    b <- b%>%
      ly_polygons(data = poly_links,
                  xs = x, ys = y,
                  group = link_id,
                  hover = list("Group 1" = group1_label,
                               "Group 2" = group2_label,
                               "N Cells" = n),
                  color = "#A7A9AC")
  } else {
    b <- b%>%
      ly_polygons(data = poly_links,
                  xs = x, ys = y,
                  group = link_id,
                  hover = list("Group 1" = group1_label,
                               "Group 2" = group2_label,
                               "N Cells" = n),
                  color = fill)
  }
  
  if(node_labels == "all") {
    b <- b %>%
      ly_text(data = plot_nodes,
              x = (xmax + xmin)/2,
              y = (ymax + ymin)/2,
              text = name,
              baseline = "middle",
              align = "center")
  } else if(node_labels == "outer") {
    xpad <- max(plot_nodes$xmax) * 0.01
    
    left_nodes <- plot_nodes %>%
      filter(xmin == min(xmin))
    right_nodes <- plot_nodes %>%
      filter(xmax == max(xmax))
    b <- b %>%
      ly_text(data = left_nodes,
              x = xmin - xpad,
              y = (ymax + ymin)/2,
              text = name,
              baseline = "middle",
              align = "right") %>%
      ly_text(data = right_nodes,
              x = xmax + xpad,
              y = (ymax + ymin)/2,
              text = name,
              baseline = "middle",
              align = "left")
  }
  
  b
  
}