## Plot a labeled barplot based on the table information in the annotation explorer
# NOTE: This is currently hard-coded for changes with [Alzheimer's] disease. In theory this could be made more robust by allowing the direction colors and direction levels to be input as variables into the function.  This would not be too challenging.
#

labeled_barplot_summary <- function(df, cats, maxTypes = 10, minPercent = 2){
  Data <- NULL
  maxTypes <- min(maxTypes,dim(df)[1])
  for (cat in cats){
    dataTmp <- data.frame(DataSet=cat, CellType=df[1:maxTypes,cat], Rank = letters[maxTypes:1],
                          PercentOfCellsInReferenceCluster = df[1:maxTypes,paste0(cat,"_percent")],
                          Direction = letters[maxTypes:1])
    if(sum(grepl(paste0(cat,"_direction"),colnames(df)))>0){
      dataTmp$Direction <- df[1:maxTypes,paste0(cat,"_direction")]
      dataTmp$Direction[dataTmp$Direction=="up"] = "Up with disease"
      dataTmp$Direction[dataTmp$Direction=="none"] = "No change"
      dataTmp$Direction[dataTmp$Direction=="unchanged"] = "No change"
      dataTmp$Direction[dataTmp$Direction=="down"] = "Down with disease"
      dataTmp$Direction[dataTmp$Direction=="not_assessed"] = "Not assessed"
    } else {
      dataTmp$Direction = "Not provided"
    }
    Data <- rbind(Data,dataTmp)
  }
  Data <- Data[Data$PercentOfCellsInReferenceCluster>=minPercent,]
  
  Data$DataSet = factor(Data$DataSet, levels=cats)
  Data$Direction= factor(Data$Direction, levels=c("Up with disease","No change","Down with disease","Not provided","Not assessed"))
  colors = c("pink", "white", "lightblue","grey","lightgrey")
  colorsUse = colors[is.element(levels(Data$Direction),Data$Direction)]

  g <- ggplot(Data, aes(x=DataSet, y=PercentOfCellsInReferenceCluster, fill = Direction, label = CellType)) +
    geom_bar(stat = "identity") +
    geom_col(color = 'black') +
    geom_text(size = 4, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = colorsUse)
  g
}



# Return a formatted data table showing bar plots sized by percentages and colored by changes with disease
format_datatable <- function (df, cats, range = c(0.1,100), minPercent = 2, pageLength = 5,
                              dir_values = c("none", "up","down"), dir_colors = c('white', 'pink','lightblue')) {
  
  if(sum(grepl("_direction",colnames(df)))>0) for (cat in cats){
    df[df[paste0(cat,"_percent")]<minPercent,paste0(cat,"_direction")] = "none"
  }
  
  toHide  <- c(0,which(!is.element(colnames(df),cats)))
  datatab <- datatable(df, options = list(dom = 't',pageLength = pageLength,
                                          columnDefs = list(list(visible=FALSE, targets=toHide))),
                       selection = list(mode="single", target="cell"))
  
  for (cat in cats){
    datatab <- datatab %>% formatStyle(cat, paste0(cat,"_percent"),
                                       background = styleColorBar(range, 'lightgray'),
                                       backgroundSize = '98% 58%',
                                       backgroundRepeat = 'no-repeat',
                                       backgroundPosition = 'right')
  }
  if(sum(grepl("_direction",colnames(df)))>0) for (cat in cats){
    datatab <- datatab %>% formatStyle(cat, paste0(cat,"_direction"),
                                       backgroundColor = styleEqual(dir_values, dir_colors))
  }
  return(datatab)
}


# Return a formatted data table showing cluster information for selected cluster
cluster_datatable <- function(cluster,metadata){
  
  if(is.null(metadata)){
    df  <- data.frame(Annotation = c("cluster","description"), Value = c(cluster,"**No cluster annotations available**"))
  } else {
    whichRow <- which(metadata==cluster, arr.ind = TRUE)
    if(dim(whichRow)[1]==0){
      df  <- data.frame(Annotation = c("cluster","description"), Value = c(cluster,"**No cluster annotations available**"))
    } else {
      row <- as.numeric(whichRow[1,1])
      df  <- data.frame(Annotation=colnames(metadata),Value=as.character(metadata[row,]))
    }
  }
  datatab <- datatable(df, options = list(dom = 't',pageLength = 50))
  return(datatab)
}
