## Plot a labeled barplot based on the table information in the annotation explorer

labeled_barplot_summary <- function(df, cats, maxTypes = 10, minPercent = 2){
  Data <- NULL
  maxTypes <- min(maxTypes,dim(df)[1])
  for (cat in cats){
    dataTmp <- data.frame(DataSet=cat, CellType=df[1:maxTypes,cat], Rank = letters[maxTypes:1],
                          PercentOfCellsInReferenceCluster = df[1:maxTypes,paste0(cat,"_percent")],
                          Direction = letters[maxTypes:1])
    
    if(sum(grepl(paste0(cat,"_direction"),colnames(df)))>0){
      dataTmp$Direction <- df[1:maxTypes,paste0(cat,"_direction")]
      dataTmp$Direction[dataTmp$Direction=="up"] = "1. Up with AD"
      dataTmp$Direction[dataTmp$Direction=="none"] = "2. No change"
      dataTmp$Direction[dataTmp$Direction=="down"] = "3. Down with AD"
    } else {
      dataTmp$Direction = "4. Not provided"
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
format_datatable <- function (df, cats, range = c(0.1,100), minPercent = 2, 
                              dir_values = c("none", "up","down"), dir_colors = c('white', 'pink','lightblue')) {
  
  if(sum(grepl("_direction",colnames(df)))>0) for (cat in cats){
    df[df[paste0(cat,"_percent")]<minPercent,paste0(cat,"_direction")] = "none"
  }
  
  toHide  <- c(0,which(!is.element(colnames(df),cats)))
  datatab <- datatable(df, options = list(dom = 't',pageLength = 10,
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
  datatab <- datatable(df, options = list(dom = 't',pageLength = 10))
  return(datatab)
}





##########################

# Need an updated auto_annotate function to exclude "_direction" columns
auto_annotate <- function (anno, scale_num = "predicted", na_val_num = 0, colorset_num = c("darkblue", 
                                                                                           "white", "red"), sort_label_cat = TRUE, na_val_cat = "ZZ_Missing", 
                           colorset_cat = "varibow", color_order_cat = "sort") 
{
  anno_out <- anno
  if (!is.element("sample_name", colnames(anno_out))) {
    colnames(anno_out) <- gsub("sample_id", "sample_name", 
                               colnames(anno_out))
  }
  cn <- colnames(anno_out)
  convertColumns <- cn[(!grepl("_label", cn)) & (!grepl("_id", 
                                                        cn)) & (!grepl("_color", cn)) & (!grepl("_direction", cn))]  # UPDATE TO OMIT DIRECTION COLUMNS
  convertColumns <- setdiff(convertColumns, "sample_name")
  convertColumns <- setdiff(convertColumns, gsub("_label", 
                                                 "", cn[grepl("_label", cn)]))
  for (cc in convertColumns) {
    value <- anno_out[, cc]
    if (sum(!is.na(value)) == 0) 
      value = rep("N/A", length(value))
    if (is.numeric(value)) {
      if (length(table(value)) == 1) 
        value = jitter(value, 1e-06)
      val2 <- value[!is.na(value)]
      if (is.element(scale_num, c("linear", "log10", "log2", 
                                  "zscore"))) {
        anno_out <- annotate_num(df = anno_out, col = cc, 
                                 scale = scale_num, na_val = na_val_num, colorset = colorset_num)
      }
      else {
        scalePred <- ifelse(min(val2) < 0, "linear", 
                            "log10")
        if ((max(val2 + 1)/min(val2 + 1)) < 100) {
          scalePred <- "linear"
        }
        if (mean((val2 - min(val2))/diff(range(val2))) < 
            0.01) {
          scalePred <- "log10"
        }
        anno_out <- annotate_num(df = anno_out, col = cc, 
                                 scale = scalePred, na_val = na_val_num, colorset = colorset_num)
      }
    }
    else {
      if (is.factor(value)) {
        anno_out <- annotate_factor(df = anno_out, col = cc, 
                                    base = cc, na_val = na_val_cat, colorset = colorset_cat, 
                                    color_order = color_order_cat)
      }
      else {
        anno_out <- annotate_cat(df = anno_out, col = cc, 
                                 base = cc, na_val = na_val_cat, colorset = colorset_cat, 
                                 color_order = color_order_cat, sort_label = sort_label_cat)
      }
    }
  }
  anno_out <- group_annotations(anno_out)
  anno_out
}

