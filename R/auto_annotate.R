# This function converts each column with existing cell type information in the metadata file into factor 
#    ordered first by the ordering in the metadata file, and then second alphabetically.
# == NOTE: this function must be run AFTER auto_annotate or it will not work properly
refactorize_annotations <- function(anno, metadata){
  # Do nothing if cell type names are not provided
  if(sum(colnames(metadata)=="cell_type")==0)
    return(anno)
  
  # If provided, look for cell type names and order annotations accordingly by default.
  cns <- colnames(anno)
  cns <- gsub("_label","",cns[grepl("_label",cns)])
  for (cn in cns){
    intersecting_cell_types <- intersect(metadata$cell_type,as.character(anno[,paste0(cn,"_label")]))
    if(length(intersecting_cell_types)>0){
      new_levels <- c(intersecting_cell_types,setdiff(as.character(anno[,paste0(cn,"_label")]),metadata$cell_type))
      anno[,paste0(cn,"_id")]  <- as.numeric(factor(anno[,paste0(cn,"_label")],levels=new_levels))
    }
    
    ## If a column is called "color" in the metadata file, then look for categorical variable colors in the "cell_type" column
    
    if((sum(colnames(metadata)=="cell_type")==1)&(length(intersecting_cell_types)>0)){
      colors <- anno[,paste0(cn,"_color")][match(new_levels,anno[,paste0(cn,"_label")])]
      colors[match(intersecting_cell_types,new_levels)] <- metadata$color[match(intersecting_cell_types,metadata$cell_type)]
      anno[,paste0(cn,"_color")] <- colors[match(anno[,paste0(cn,"_label")],new_levels)]
      new_levels <- c(intersecting_cell_types,setdiff(as.character(anno[,paste0(cn,"_label")]),metadata$cell_type))
      anno[,paste0(cn,"_id")]  <- as.numeric(factor(anno[,paste0(cn,"_label")],levels=new_levels))
    }
    
  }
  
  return(anno)
}


#################################################################################
# UPDATED FUNCTION WITH BUG FIX FOR LARGE CSV FILES AND TO OMIT DIRECTION COLUMNS

auto_annotate <- function (anno, scale_num = "predicted", na_val_num = 0, colorset_num = c("darkblue", 
                                                                                           "white", "red"), sort_label_cat = TRUE, na_val_cat = "ZZ_Missing", 
                           colorset_cat = "varibow", color_order_cat = "sort") 
{
  # Define and properly format a sample name
  anno_out <- as.data.frame(anno)
  cn <- colnames(anno_out)
  if (!is.element("sample_name", cn)) {
    colnames(anno_out) <- gsub("sample_id", "sample_name", cn)
  }
  if (!is.element("sample_name", colnames(anno_out))) {
    anno_out <- cbind(anno_out, paste0("sn_",1:dim(anno_out)[1]))
    colnames(anno_out) <- c(cn,"sample_name")
  }
  anno_out <- anno_out[,c("sample_name",setdiff(colnames(anno_out),"sample_name"))]
  
  # Annotate any columns missing annotations
  cn <- colnames(anno_out)
  convertColumns <- cn[(!grepl("_label", cn)) & (!grepl("_id", 
                                                        cn)) & (!grepl("_color", cn)) & (!grepl("_direction", cn))]  # UPDATE TO OMIT DIRECTION COLUMNS
  convertColumns <- setdiff(convertColumns, "sample_name")
  convertColumns <- setdiff(convertColumns, gsub("_label", 
                                                 "", cn[grepl("_label", cn)]))
  
  # Return input annotation file if there is nothing to convert
  if(length(convertColumns)==0){
    anno_out  <- group_annotations(anno_out)
    return(anno_out)
  }
  
  anno_list <- list()
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
        out <- annotate_num(df = anno_out[,c("sample_name",cc)], col = cc, 
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
        out <- annotate_num(df = anno_out[,c("sample_name",cc)], col = cc, 
                            scale = scalePred, na_val = na_val_num, colorset = colorset_num)
      }
    }
    else {
      if (is.factor(value)) {
        out <- annotate_factor(df = anno_out[,c("sample_name",cc)], col = cc, 
                               base = cc, na_val = na_val_cat, colorset = colorset_cat, 
                               color_order = color_order_cat)
      }
      else {
        out <- annotate_cat(df = anno_out[,c("sample_name",cc)], col = cc, 
                            base = cc, na_val = na_val_cat, colorset = colorset_cat, 
                            color_order = color_order_cat, sort_label = sort_label_cat)
      }
    }
    anno_list[[cc]] <- out[,colnames(out)!="sample_name"]
  }
  
  # Format the annotations as an appropriate data frame
  for(cc in convertColumns)
    anno_list[[cc]] <- anno_list[[cc]][,colnames(anno_list[[cc]])!="sample_name"]
  anno_out2 <- bind_cols(anno_list)
  anno_out  <- cbind(anno_out[,c(1,which(!is.element(colnames(anno_out),convertColumns)))],anno_out2)
  anno_out  <- group_annotations(anno_out[,c(1,3:dim(anno_out)[2])])
  anno_out
}
