build_compare_jaccard_plot <- function (anno, x_group, y_group, reorderY, maxInputs = 50){
  
  x <- anno[,paste0(x_group,"_id")]
  y <- anno[,paste0(y_group,"_id")]
  numInputs <- length(unique(x))+length(unique(y))
  print(paste("maxInputs:",maxInputs),stderr())
  print(paste("numInputs:",numInputs),stderr())
  if (numInputs>maxInputs){
    print("BLANK PLOT RETURNING",stderr())
    return(ggplot() + theme_void() + theme(plot.title = element_text(hjust = 0.5)) +
             ggtitle("If you are comparing two categorical values: There are too many unique X+Y values to plot.\nIf you are expecting a numeric plot, please be patient; for large data sets plots can take up to a minute to load.\nIn either case, consider filtering data or increasing window size."))
  }
  
  x <- factor(anno[,paste0(x_group,"_label")], levels = anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
  y <- factor(anno[,paste0(y_group,"_label")], levels = anno[,paste0(y_group,"_label")][match(sort(unique(y), decreasing = TRUE),y)])
  names(x) <- names(y) <- anno$sample_id
  
  if(min(length(unique(x)),length(unique(y)))==1){
    return(ggplot() + theme_void() + ggtitle("     Visualization require multiple values for X and Y."))
  } else {
    return(compare_plot(x, y, reorderY, x_group, y_group))
  }
}


compare_plot <- function (x, y, reorderY=TRUE, x_group="x", y_group="y") 
{
  
  common.cells <- intersect(names(x), names(y))
  y  <- y[common.cells]
  x  <- x[common.cells]
  tb <- table(x, y)
  if(reorderY){
    tmp   <- t(tb)
    tmp   <- tmp/rowSums(tmp)
    ord   <- order(-apply(tmp,1,which.max)*10,rowMeans(t(apply(tmp,1,cumsum))))
    y     <- setNames(factor(y,levels = colnames(tb)[ord]),names(y))
    tb    <- table(x, y)
  }
  
  tb.df <- as.data.frame(tb)
  tb.df <- tb.df[tb.df$Freq > 0, ]
  select.cells <- names(x)
  tb.df$jaccard <- 0
  for (i in 1:nrow(tb.df)) {
    n_ol <- length(union(common.cells[x == as.character(tb.df[i, 1])], 
                         common.cells[y == as.character(tb.df[i, 2])]))
    tb.df$jaccard[i] <- tb.df$Freq[i]/n_ol
  }
  colnames(tb.df) <- c("x", "y", "Freq", "jaccard")
  
  g <- ggplot2::ggplot(tb.df, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_point(aes(size = sqrt(Freq), color = jaccard)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.3, hjust = 1, angle = 90, size = 10),
                   axis.text.y = ggplot2::element_text(vjust = 0.3, hjust = 1, size = 10)) + 
    ggplot2::scale_color_gradient(low = "yellow", high = "darkblue") + 
    ggplot2::scale_size(
      name = "Number\nof data\npoints",      # Sets the new legend title
      range = c(0, 5),
      labels = function(x) round(signif(x^2,2), 0) # Reverses the sqrt() for the labels
    ) +
    ggplot2::labs(
      color = "Jaccard\nSimilarity",
      x = x_group,
      y = y_group
    )
  g
}



return_plot_data <- function (x, y, desc, reorderY, x_group, y_group,
                              other_columns=NULL, anno=NULL)  # This indicates which other columns to be appended; only used for scatterplot data returns
{
  ## Subset to only include cells in both input vectors
  common.cells <- intersect(names(x), names(y))
  y <- y[common.cells]
  x <- x[common.cells]
  
  ## Determine the type of incoming data
  cat_annotations <- desc$base[desc$type == "cat"]
  num_annotations <- desc$base[desc$type == "num"]
  x_type <- ifelse(x_group %in% cat_annotations,"cat","num")
  y_type <- ifelse(y_group %in% cat_annotations,"cat","num")
  
  ## Return confusion matrix if both a categorical
  if((x_type=="cat")&(y_type=="cat")){
    
    tb <- table(x, y)
    if(reorderY){
      tmp   <- t(tb)
      tmp   <- tmp/rowSums(tmp)
      ord   <- order(-apply(tmp,1,which.max)*10,rowMeans(t(apply(tmp,1,cumsum))))
      y     <- setNames(factor(y,levels = colnames(tb)[ord]),names(y))
      tb    <- table(x, y)
    }
    tb <- tb[,dim(tb)[2]:1] # reverse order of columns
    
    tb <- t(tb) # swap rows and columns for download
    return(tb)
  }
  
  ## Otherwise:
  ## -- Return Nx2 data matrix if both are numeric.  This can also be used for scatterplot_function data returns.
  ## -- If exactly one is value is numeric, return a Nx2 data frame including a vector of values and the corresponding categorical for each value
  data <- data.frame(sample_id=common.cells,x=x,y=y)
  other_columns <- unique(intersect(setdiff(other_columns,c(x_group,y_group)),desc$base))
  colnames(data) <- c("sample_id",x_group,y_group)
  
  if(length(other_columns)>0){
    anno_out <- anno[common.cells,paste0(other_columns,"_label")]
    colnames(anno_out) <- other_columns
    data <- cbind(data,anno_out)
  }
  
  rownames(data) <- paste0("ZZZ",rownames(data))
  return(data)
  
}