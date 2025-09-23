## This script downloads files from the mouse motor cortex (MOp or M1) and whole mouse brain (WMB) taxonomies, and joins them together into cell annotation and cell type annotation tables.  These come from the publications (Yao et al 2021; https://doi.org/10.1038/s41586-021-03500-8) and (Yao et al 2023; https://doi.org/10.1038/s41586-023-06812-z), respectively.


##############################################################
## Motor cortex data download and ingest

# This information is taken from data link-outs from the Cell Type Knowledge Explorer, here: https://knowledge.brain-map.org/celltypes/CCN202002013
# Specifically, we download "sample_metadata.csv" and "cluster.membership.csv" from the following two folder and add _v2 and _v3 suffix to disambiguate cells from 10x version 2 and cells from 10x version 3, which are stored separately and where are also the only ones included in the whole mouse brain study. We also download "cluster.annotation.csv" from either folder (it's the same file in both).
# https://data.nemoarchive.org/biccn/grant/u19_zeng/zeng/transcriptome/scell/10x_v2/mouse/processed/analysis/10X_cells_v2_AIBS/
# https://data.nemoarchive.org/biccn/grant/u19_zeng/zeng/transcriptome/scell/10x_v3/mouse/processed/analysis/10X_cells_v3_AIBS/

options(stringsAsCharacter=TRUE)
m1_si_v2 <- read.csv("sample_metadata_v2.csv",row.names=1)
m1_si_v3 <- read.csv("sample_metadata_v3.csv",row.names=1)
m1_ci_v2 <- read.csv("cluster.membership_v2.csv",row.names=1)
m1_ci_v3 <- read.csv("cluster.membership_v3.csv",row.names=1)
m1_clInfo<- read.csv("cluster.annotation.csv")

m1_ci <- rbind(m1_ci_v2,m1_ci_v3)
m1_si <- rbind(m1_si_v2,m1_si_v3)
m1_si <- m1_si[rownames(m1_ci),]
m1_anno <- cbind(m1_si,m1_ci)
m1_clInfo <- m1_clInfo[match(m1_anno$x,m1_clInfo$cluster_id),]
rownames(m1_clInfo) <- rownames(m1_anno)
m1_anno <- cbind(m1_clInfo,m1_anno)


##############################################################
## Whole mouse brain data download and ingest

# Now download the WMB "cell_metadata.csv" from the ABC Atlas Access website (https://alleninstitute.github.io/abc_atlas_access/descriptions/WMB_dataset.html; and specifically from this bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-10X/20241115/)
# We also need the cluster information table "cl.df_CCN202307220.xlsx" from here: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-taxonomy/20231215/.  Note that this is used both for the cell information table and also for the cell type information table (likewise for the M1 version above).  For convenience we save the "cluster_annotation" tab as "cluster.annotation_WMB.csv"

library(data.table)
wmb_anno <- fread("cell_metadata.csv")
wmb_anno <- as.data.frame(wmb_anno)
wmb_clInfo <- read.csv("cluster.annotation_WMB.csv")  # We will join this later after matching cells across studies.


##############################################################
## Join the two data sets

## Find a common cell name
m1_name  <- paste0(substr(rownames(m1_anno),1,16),"_",m1_anno$library_id)
wmb_name <- paste0(substr(wmb_anno$cell_label,1,16),"_",wmb_anno$library_label)
kp_name  <- intersect(m1_name,wmb_name)

## Subset the data set
rownames(m1_anno)  <- m1_name
rownames(wmb_anno) <- wmb_name
m1_anno <- m1_anno[kp_name,]
wmb_anno <- wmb_anno[kp_name,]

## Join on cell type information for whole mouse brain
wmb_clInfo <- wmb_clInfo[match(wmb_anno$cluster_alias,wmb_clInfo$cl),]
rownames(wmb_clInfo) <- rownames(wmb_anno)
wmb_anno <- cbind(wmb_clInfo,wmb_anno)


##############################################################
# Now merge relevant subsets of the files and output to csv

## Decide on M1 columns to include and rename
m1_cn <- c("cluster_label","cluster_color","cluster_id", "subclass_label","class_label",
           "method", "Gender", "umi.counts", "gene.counts", "doublet.score")
m1_anno <- m1_anno[,m1_cn]
m1_cn <- c("MOp_cluster_label","MOp_cluster_color","MOp_cluster_id", "MOp_subclass","MOp_class",
           "method", "sex", "UMI_counts", "gene_counts", "doublet_score")
colnames(m1_anno) <- m1_cn

## Decide on WMB columns to include and rename
wmb_cn <- c("cluster_id_label","supertype_id_label","subclass_id_label","class_id_label","neighborhood",
            "x","y")
wmb_anno <- wmb_anno[,wmb_cn]
wmb_cn <- c("WMB_cluster","WMB_supertype","WMB_subclass","WMB_class","WMB_neighborhood",
            "WMB_UMAP_1","WMB_UMAP_2")
colnames(wmb_anno) <- wmb_cn

## Merge table and reorder columns
anno <- cbind(m1_anno,wmb_anno)
rownames(anno) <- NULL

cn_anno <- c("MOp_cluster_label","MOp_cluster_color","MOp_cluster_id", "MOp_subclass","MOp_class",
             "WMB_cluster","WMB_supertype","WMB_subclass","WMB_class","WMB_neighborhood",
             "sex","method","UMI_counts","gene_counts","doublet_score","WMB_UMAP_1", "WMB_UMAP_2")
anno <- anno[,cn_anno]

## Omit off target or low quality cells
anno <- anno[nchar(wmb_anno$WMB_neighborhood)<20,] # Remove 5 cells from off-target neighborhoods
anno <- anno[!is.element(anno$MOp_subclass,c("doublet","Low Quality")),] # Remove low quality and doublets (~4000 total out of ~160,000)

dim(anno)
#[1] 160410     17

## Write the annotation table
fwrite(anno,"M1_to_WMB_cell_info.csv.gz",row.names=FALSE)


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

# We also add rows for M1 which are only used for ordering subclasses and classes (MOp clusters have "id" and "color" columns included).  Note that none of the subclass and class names overlap with WMB, so no need to disambiguate)

out <- c(unique(anno$MOp_subclass[order(anno$MOp_cluster_id)]),unique(anno$MOp_class[order(anno$MOp_cluster_id)]))
write(out,"MOp_order.txt")
