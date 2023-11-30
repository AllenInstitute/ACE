## Plot a labeled barplot based on the table information in the annotation explorer

labeled_barplot_summary <- function(df, cats, maxTypes = 5, minPercent = 2){
  Data <- NULL
  maxTypes <- min(maxTypes,dim(df)[1])
  for (cat in cats){
    dataTmp <- data.frame(DataSet=cat, CellType=df[1:maxTypes,cat], Rank = letters[maxTypes:1],
                          PercentOfCellsInReferenceCluster = df[1:maxTypes,paste0(cat,"_percent")],
                          Directon = letters[maxTypes:1])
    
    if(sum(grepl("_direction",colnames(df)))>0){
      dataTmp$Direction <- df[1:maxTypes,paste0(cat,"_direction")]
      dataTmp$Direction[dataTmp$Direction=="up"] = "1. Up with AD"
      dataTmp$Direction[dataTmp$Direction=="none"] = "2. No change"
      dataTmp$Direction[dataTmp$Direction=="down"] = "3. Down with AD"
    }
    Data <- rbind(Data,dataTmp)
  }
  Data <- Data[Data$PercentOfCellsInReferenceCluster>=minPercent,]
  
  g <- ggplot(Data, aes(x=DataSet, y=PercentOfCellsInReferenceCluster, fill = Direction, label = CellType)) +
    geom_bar(stat = "identity") +
    geom_col(color = 'black') +
    geom_text(size = 4, position = position_stack(vjust = 0.5)) 
  g
}



# Return a formatted data table showing bar plots sized by percentages and colored by changes with AD
format_datatable <- function (df, cats, range = c(0.1,100), minPercent = 2) {
  
  if(sum(grepl("_direction",colnames(df)))>0) for (cat in cats){
    df[df[paste0(cat,"_percent")]<minPercent,paste0(cat,"_direction")] = "none"
  }
  
  toHide  <- which(!is.element(colnames(df),cats))
  datatab <- datatable(df, options = list(dom = 't',pageLength = 50,
                       columnDefs = list(list(visible=FALSE, targets=toHide))))
  
  for (cat in cats){
    datatab <- datatab %>% formatStyle(cat, paste0(cat,"_percent"),
                                       background = styleColorBar(range, 'lightgray'),
                                       backgroundSize = '98% 58%',
                                       backgroundRepeat = 'no-repeat',
                                       backgroundPosition = 'right')
  }
  if(sum(grepl("_direction",colnames(df)))>0) for (cat in cats){
    datatab <- datatab %>% formatStyle(cat, paste0(cat,"_direction"),
                                       backgroundColor = styleEqual(c("none", "up","down"), c('white', 'pink','lightblue')))
  }
  return(datatab)
}
  


