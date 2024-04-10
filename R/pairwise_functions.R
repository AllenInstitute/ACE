build_compare_jaccard_plot <- function (anno, x_group, y_group, reorderY, maxInputs = 50){
  x <- anno[,paste0(x_group,"_id")]
  x <- factor(anno[,paste0(x_group,"_label")], levels = anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
  y <- anno[,paste0(y_group,"_id")]
  y <- factor(anno[,paste0(y_group,"_label")], levels = anno[,paste0(y_group,"_label")][match(sort(unique(y)),y)])
  names(x) <- names(y) <- anno$sample_id
  numInputs <- length(levels(x))+length(levels(y))
  
  if(min(length(unique(x)),length(unique(y)))==1){
    ggplot() + theme_void() + ggtitle("     Visualization require multiple values for X and Y.")
  } else if (numInputs>maxInputs){
    ggplot() + theme_void() + ggtitle("     Too many unique X+Y values to plot. Filter data or increase window size.")
  } else {
    compare_plot(x, y, reorderY)
  }
}


compare_plot <- function (x, y, reorderY=TRUE) 
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
    ggplot2::scale_size(range = c(0, 5))
  g
}