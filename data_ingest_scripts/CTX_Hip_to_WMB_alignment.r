## This script downloads files from the mouse study of hippocampus + neocortex and whole mouse brain (WMB) taxonomies, and joins them together into cell annotation and cell type annotation tables.  These come from the publications (Yao et al 2021; https://doi.org/10.1016/j.cell.2021.04.021) and (Yao et al 2023; https://doi.org/10.1038/s41586-023-06812-z), respectively.


##############################################################
## Cortex+hippocampus data download and ingest

# This information starts from supplemental materials in Yao et al 2021 availabe on NeMO here: 
# 1. https://data.nemoarchive.org/biccn/grant/u19_zeng/zeng/transcriptome/scell/10x_v2/mouse/processed/YaoHippo2020/
#  - CTX_Hip_anno_10x.csv.tar = The annotation information 
# This files are unzipped in the current directory prior to running the script 

library(data.table)
source("https://raw.githubusercontent.com/AllenInstitute/mfishtools/977022e8faf0e79a512b3536ff07375ff7ffc33e/R/markerGenesAndMapping.r") # for "subsampleCells" function
options(stringsAsCharacter=TRUE)

## Read in annotations (#1 above)
anno_all <- fread("CTX_Hip_anno_10x.csv")
anno_all <- as.data.frame(anno_all)

## Subset to the relevant columns
kpCl <- c("exp_component_name","cluster_label","supertype_label","subclass_label","neighborhood_label","class_label",
          "sex_label","region_label","gene.counts","donor_label")
anno <- anno_all[,kpCl]
anno$donor_label <- paste0("M:",as.character(anno$donor_label)) # Mouse ID needs to be a character, not a number

## Rename some of the columns
cn <- colnames(anno)
cn <- gsub("_label","",cn)
cn[2:6] <- paste0("CtxHip_",cn[2:6])
colnames(anno) <- cn


##############################################################
## Whole mouse brain data download and ingest

# Now download the WMB "cell_metadata.csv" from the ABC Atlas Access website (https://alleninstitute.github.io/abc_atlas_access/descriptions/WMB_dataset.html; and specifically from this bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-10X/20241115/)
# We also need the cluster information table "cl.df_CCN202307220.xlsx" from here: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-taxonomy/20231215/.  Note that this is used both for the cell information table and also for the cell type information table (likewise for the M1 version above).  For convenience we save the "cluster_annotation" tab as "cluster.annotation_WMB.csv"

wmb_anno <- fread("cell_metadata.csv")
wmb_anno <- as.data.frame(wmb_anno)
wmb_clInfo <- read.csv("cluster.annotation_WMB.csv")  # We will join this later after matching cells across studies.


##############################################################
## Join the two data sets

## Find a common cell name
ctxHip_name <- gsub("-","_",anno$exp_component_name)
wmb_name    <- paste0(substr(wmb_anno$cell_label,1,16),"_",wmb_anno$library_label)
kp_name     <- intersect(ctxHip_name,wmb_name)

## Subset the data set
rownames(anno) <- ctxHip_name
rownames(wmb_anno) <- wmb_name
ctxHip_anno <- anno[kp_name,]
wmb_anno    <- wmb_anno[kp_name,]

## Join on cell type information for whole mouse brain
wmb_clInfo <- wmb_clInfo[match(wmb_anno$cluster_alias,wmb_clInfo$cl),]
rownames(wmb_clInfo) <- rownames(wmb_anno)
wmb_anno <- cbind(wmb_clInfo,wmb_anno)


##############################################################
# Now merge relevant subsets of the files 

## Remove the cell ID column from CTX/HIP study (already chose columns)
ctxHip_cn   <- colnames(ctxHip_anno)[colnames(ctxHip_anno)!="exp_component_name"]
ctxHip_anno <- ctxHip_anno[,ctxHip_cn]

## Decide on WMB columns to include and rename
wmb_cn <- c("cluster_id_label","supertype_id_label","subclass_id_label","class_id_label","neighborhood",
            "x","y")
wmb_anno <- wmb_anno[,wmb_cn]
wmb_cn <- c("WMB_cluster","WMB_supertype","WMB_subclass","WMB_class","WMB_neighborhood",
            "WMB_UMAP_1","WMB_UMAP_2")
colnames(wmb_anno) <- wmb_cn

## Merge table and reorder columns
anno <- cbind(ctxHip_anno,wmb_anno)
rownames(anno) <- NULL

cn_anno <- c("CtxHip_cluster","CtxHip_supertype","CtxHip_subclass","CtxHip_neighborhood","CtxHip_class",
             "WMB_cluster","WMB_supertype","WMB_subclass","WMB_class","WMB_neighborhood",
             "region","sex","gene.counts","donor","WMB_UMAP_1", "WMB_UMAP_2")
anno <- anno[,cn_anno]

## Omit off target cells (e.g., <100 cells in a neighborhood)
kpNb <- names(table(wmb_anno$WMB_neighborhood))[table(wmb_anno$WMB_neighborhood)>100]
kpNb <- is.element(wmb_anno$WMB_neighborhood,kpNb)
anno <- anno[kpNb,] # Remove 5 cells from off-target neighborhoods

dim(anno)
#[1] 1122569     17


##############################################################
# Downsample ~10-fold and output to csv

# Include at least 25 cells per cluster
kpCluster <- subsampleCells(anno$CtxHip_cluster,25)
set.seed(42)
kpCells   <- is.element(1:length(kpCluster),sample(1:length(kpCluster),100000))

anno_subset <- anno[kpCluster|kpCells,]
colnames(anno_subset)[colnames(anno_subset)=="region"] = "Dissection"

# Add a . at the end of CtxHip labels to disambiguate
anno_subset$CtxHip_supertype    <- paste0(as.character(anno_subset$CtxHip_supertype),".")
anno_subset$CtxHip_subclass     <- paste0(as.character(anno_subset$CtxHip_subclass),".")
anno_subset$CtxHip_neighborhood <- paste0(as.character(anno_subset$CtxHip_neighborhood),".")

## Write the annotation table
fwrite(anno_subset,"CtxHip_WMB_translation.csv.gz",row.names=FALSE)

###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# This table called "WMB_cluster_annotations.csv.gz" is built almost entirely from the "cl.df_CCN202307220.xlsx" file downloaded above.  We create the following columns:
# label = The [XX]_id_label value (where [XX] corresponds to relevant tab)
# parent = The [XX+1]_label value, where [XX+1] is one step up in the hierarchy
# category = The relevant level of the hierachy (also the tab label)
# markers_global = The "cluster.markers.combo" column, which includes global marker genes using some computational approach
# markers_local = The "cluster.markers.combo (within [XX+1])" column, which includes global marker genes using some computational approach
# broad_anatomy = The "CCF_broad.freq" column (corrsponding to which fraction of cells come from which higher-level dissections).  I don't know whether this is from the 10X or MERFISH data initially. 
# local_anatomy = The "CCF_acronym.freq" column, which is more precise anatomic structures
# direction = "none" for all cases (not strictly necessary, as ACE could infer this value)

# The first 6896 rows include these values from the "cluster_annotation", "supertype_annotation", "subclass_annotation", and "class_annotation" tabs of the above file.

# Finally, we add 392 rows as a flattened and subset version of Table S3 from Yao et al 2021 above, showing the ordering and some relevant information about the CTX+HIP cell types, as well as another ~150 rows included for the order and color of other taxonomy levels in the Ctx+HIP taxonomy (and are also taken from the same supplemental table).
