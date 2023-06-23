build_compare_jaccard_plot <- function (anno, x_group, y_group){
  x <- anno[,paste0(x_group,"_id")]
  x <- factor(anno[,paste0(x_group,"_label")], levels = anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
  y <- anno[,paste0(y_group,"_id")]
  y <- factor(anno[,paste0(y_group,"_label")], levels = anno[,paste0(y_group,"_label")][match(sort(unique(y)),y)])
  names(x) <- names(y) <- anno$sample_id
  
  if(min(length(unique(x)),length(unique(y)))==1){
    ggplot() + theme_void() + ggtitle("Visualization require multiple values for X and Y")
  } else{
    compare_plot(x,y)
  }
}


compare_plot <- function (x, y) 
{
  common.cells <- intersect(names(x), names(y))
  tb <- table(x[common.cells], y[common.cells])
  tb.df <- as.data.frame(tb)
  tb.df <- tb.df[tb.df$Freq > 0, ]
  select.cells <- names(x)
  tb.df$jaccard <- 0
  for (i in 1:nrow(tb.df)) {
    n_ol <- length(union(common.cells[x[common.cells] == 
                                        as.character(tb.df[i, 1])], common.cells[y[common.cells] == 
                                                                                   as.character(tb.df[i, 2])]))
    tb.df$jaccard[i] <- tb.df$Freq[i]/n_ol
  }
  colnames(tb.df) <- c("x", "y", "Freq", "jaccard")
  g <- ggplot2::ggplot(tb.df, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_point(aes(size = sqrt(Freq), color = jaccard)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.1, 
                                                       hjust = 0.2, angle = 90, size = 7), axis.text.y = ggplot2::element_text(size = 6)) + 
    ggplot2::scale_color_gradient(low = "yellow", high = "darkblue") + 
    ggplot2::scale_size(range = c(0, 3))
  g
}




build_compare_heatmap_plot <- function(anno, x_group, y_group, threshold){
  x <- anno[,paste0(x_group,"_id")]
  x <- factor(anno[,paste0(x_group,"_label")], levels = anno[,paste0(x_group,"_label")][match(sort(unique(x)),x)])
  y <- anno[,paste0(y_group,"_id")]
  y <- factor(anno[,paste0(y_group,"_label")], levels = anno[,paste0(y_group,"_label")][match(sort(unique(y)),y)])
  names(x) <- names(y) <- anno$sample_id
  compare_heatmap(x,y,threshold)
}


compare_heatmap <- function(x, 
                            y,
                            threshold=0.2,
                            cexLab=NULL,
                            Rowv=NA, Colv=NA, ylab=NULL, xlab=NULL, main=NULL, 
                            margins = c(6,6), scale="none",trace="none", dendrogram="none"){
  tab <- pmin((t(table(x,y))/apply(table(x,y),1,max)),threshold)
  g   <- ggplot(as.data.frame(as.table(tab)),aes(x,y,fill=Freq)) + geom_tile() + 
         ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.1, 
                                                         hjust = 0.2, angle = 90, size = 7), axis.text.y = ggplot2::element_text(size = 6)) + 
         ggplot2::scale_color_gradient(low = "yellow", high = "darkblue")
  mn  <- paste("Score:",signif(sum(tab)-threshold*(sum(tab==max(tab))),3),"(low=good)")
  g+ggtitle(mn)
}




