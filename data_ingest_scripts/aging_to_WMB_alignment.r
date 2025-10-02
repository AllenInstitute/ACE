## This script downloads files from the mouse aging study and whole mouse brain (WMB) taxonomies, and joins them together into cell annotation tables (since the taxonomy matches the one used in WMB, we can reuse that cell annotation table).  These come from the publications (Jin et al 2025; https://doi.org/10.1038/s41586-024-08350-8) and (Yao et al 2023; https://doi.org/10.1038/s41586-023-06812-z), respectively.  We also note that the WMB annotations are already included in the files from the aging mouse study, so no additional files related to WMB study is needed.


##############################################################
## Aging mouse brain data download and ingest

# Now download the aging mouse brain "cell_metadata.csv", "cell_cluster_annotations.csv" from the ABC Atlas Access website (https://alleninstitute.github.io/abc_atlas_access/descriptions/Zeng_Aging_Mouse_10Xv3.html; and specifically from this bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/Zeng-Aging-Mouse-10Xv3/20250131/)
# We also need the cluster information table "cl.df_CCN202307220.xlsx" from here: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-taxonomy/20231215/.  Note that this is used both for the cell information table and also for the cell type information table (likewise for the M1 version above).  For convenience we save the "cluster_annotation" tab as "cluster.annotation_WMB.csv"
# Finally, we create a cluster annotation pivot table to deal with minor formating differenes in cluster names (e.g., to add leading 0s).  This file called "cluster_mapping_pivot.csv.gz" is saved in the script folder.


##############################################################
## Read in the annotation files 

library(data.table)

# Aging data
aging_meta <- fread("cell_metadata.csv") # Includes wmb cluster alias
aging_anno <- fread("cell_cluster_annotations.csv")
aging_cluster_pivot <- fread("aging_mouse_cluster_mapping_pivot.csv.gz")  # Minor edits to add leading 0's to the cluster_name

# WMB data (this is also available on the website)
wmb_cluster_info <- fread("cluster.annotation_WMB.csv")  # A csv version of the file "cl.df_CCN202307220.xlsx" which includes cluster information

# Subset aging_dataset to columns of interest and merge with WMB clusters of interest
meta_kp <- c("cluster_alias","wmb_cluster_alias","region_of_interest_label","donor_age_category","donor_sex","umi_count","gene_count","x","y")
pivot_kp <- c("cluster_name","cluster_color","cluster_order","supertype_name","subclass_name","class_name")
anno_kp  <- c("neurotransmitter_combined_label","cluster_age_bias")
wmb_kp <- c("cluster_id_label","supertype_id_label","subclass_id_label","class_id_label")

aging_merge <- cbind(
  as.data.frame(aging_cluster_pivot)[match(aging_meta$cluster_alias,aging_cluster_pivot$cluster_alias),pivot_kp],
  as.data.frame(aging_meta)[,meta_kp],
  as.data.frame(aging_anno)[,anno_kp],
  as.data.frame(wmb_cluster_info)[match(aging_meta$wmb_cluster_alias,wmb_cluster_info$cl),wmb_kp])

colnames(aging_merge) <- c("AGING_cluster_label","AGING_cluster_color","AGING_cluster_id",
      "AGING_supertype","AGING_subclass","AGING_class","AGING_cluster_alias",
      "WMB_cluster_alias","region_of_interest","donor_age","donor_sex",
	  "umi_count","gene_count","AGING_UMAP_X","AGING_UMAP_Y",
	  "Neurotransmitter","cluster_age_bias",
	  "WMB_cluster","WMB_supertype","WMB_subclass","WMB_class")

aging_merge <- aging_merge[,setdiff(colnames(aging_merge),c("AGING_cluster_alias","WMB_cluster_alias"))]
aging_merge[is.na(aging_merge)] = "Not in WMB study"	  

# Write out complete data set
fwrite(aging_merge,"aging_mouse_brain_complete.csv.gz")


## Now for ACE, subsample to remove 75% of the cells randomly, and remove cells from WMB clusters that show up in 2 or fewer cells per cluster after subsampling
set.seed(42)
samp <- sample(1:dim(aging_merge)[1],round(dim(aging_merge)[1]/4))
aging_merge_sub <- aging_merge[samp,]
omit <- names(table(aging_merge_sub$WMB_cluster))[table(aging_merge_sub$WMB_cluster)<=2]
aging_merge_sub <- aging_merge_sub[!is.element(aging_merge_sub$WMB_cluster,omit),]

# Write out subsampled data set
fwrite(aging_merge_sub,"aging_mouse_brain_subsample.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# This table called "WMB_cluster_annotations.csv.gz" is built almost entirely from the "cl.df_CCN202307220.xlsx" file discussed above.  It's creation is described in other data ingest scripts.

