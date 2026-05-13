## This script downloads files from the whole mouse brain (WMB) taxonomy and the updated consensus taxonomy include data from the Macosco lab, and joins them together into cell annotation and cell type annotation tables.  These come from the publications (Yao et al 2023; https://doi.org/10.1038/s41586-023-06812-z) and (Langlieb et al 2023; https://doi.org/10.1038/s41586-023-06818-7), respectively.


##############################################################
## ORIGINAL whole mouse brain data download and ingest

# Now download the WMB "cell_metadata.csv" from the ABC Atlas Access website (https://alleninstitute.github.io/abc_atlas_access/descriptions/WMB_dataset.html; and specifically from this bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-10X/20241115/)
# We also need the cluster information table "cl.df_CCN202307220.xlsx" from here: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/WMB-taxonomy/20231215/.  Note that this is used both for the cell information table and also for the cell type information table (likewise for the M1 version above).  For convenience we save the "cluster_annotation" tab as "cluster.annotation_WMB.csv"

library(data.table)

# Faster subsampleCells function that should have identical results
subsampleCells_fast <- function(clusters,
                                subSamp = 25,
                                seed = 5) {

  clust <- as.character(clusters)
  uclust <- unique(clust)

  if (length(subSamp) == 1)
    subSamp <- rep(subSamp, length(uclust))

  if (is.null(names(subSamp)))
    names(subSamp) <- uclust

  # Precompute indices once
  idx_list <- split(seq_along(clust), clust)

  kpSamp <- logical(length(clust))

  for (cli in uclust) {
    val <- subSamp[cli]

    if (!is.na(val)[1]) {
      set.seed(seed)
      seed <- seed + 1

      kp <- idx_list[[cli]]
      n <- min(length(kp), val)

      kpSamp[kp[sample.int(length(kp), n)]] <- TRUE
    }
  }

  kpSamp
}


wmb_anno <- fread("cell_metadata.csv")
wmb_anno <- as.data.frame(wmb_anno)
wmb_clInfo <- read.csv("cluster.annotation_WMB.csv")  # We will join this later after matching cells across studies.


##############################################################
## Read in consensus annotations for the same cells

cluster                       <- fread("cluster.csv",data.table=FALSE) 
cell_2d_embedding_coordinates <- fread("cell_2d_embedding_coordinates.csv",data.table=FALSE) 
cell_to_cluster_membership    <- fread("cell_to_cluster_membership.csv",data.table=FALSE) 
cluster_annotation_term       <- fread("cluster_annotation_term.csv",data.table=FALSE) 

## Create the cell annotation file
cell_extended = data.frame(cell_to_cluster_membership, label=cluster$label[match(cell_to_cluster_membership$cluster_alias,cluster$cluster_alias)])


# Add the taxonomy
cell_extended$cluster = cluster_annotation_term[match(cell_extended$label,cluster_annotation_term$label),"name"]
cell_extended$supertype = cluster_annotation_term[grepl("LEVEL_3",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$cluster,cluster_annotation_term$name[grepl("LEVEL_3",cluster_annotation_term$parent_term_set_label)])]
cell_extended$subclass = cluster_annotation_term[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$supertype,cluster_annotation_term$name[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label)])]
cell_extended$class = cluster_annotation_term[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$subclass,cluster_annotation_term$name[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label)])]
cell_extended$neighborhood = cluster_annotation_term[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$class,cluster_annotation_term$name[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label)])]
cons_anno <- cell_extended

##############################################################
## Join the two data sets

## Find a common cell name
cons_name <- cons_anno$cell_label
wmb_name  <- wmb_anno$cell_label
kp_name   <- intersect(cons_name,wmb_name)

## Subset the data set
rownames(cons_anno)  <- cons_name
rownames(wmb_anno) <- wmb_name
cons_anno <- cons_anno[kp_name,]
wmb_anno  <- wmb_anno[kp_name,]

## Join on cell type information for whole mouse brain
wmb_clInfo <- wmb_clInfo[match(wmb_anno$cluster_alias,wmb_clInfo$cl),]
rownames(wmb_clInfo) <- rownames(wmb_anno)
wmb_anno <- cbind(wmb_clInfo,wmb_anno)


##############################################################
# Now merge/rename relevant subsets of the files

## Decide on consensus columns to include and rename
cons_cn <- c("cell_label","cluster","supertype","subclass","class","neighborhood")
cons_anno <- cons_anno[,cons_cn]
cons_cn <- c("cell_label","CONS_cluster","CONS_supertype","CONS_subclass","CONS_class","neighborhood")
colnames(cons_anno) <- cons_cn

## Decide on WMB columns to include and rename
wmb_cn <- c("cluster_id_label","supertype_id_label","subclass_id_label","class_id_label",
            "x","y")
wmb_anno <- wmb_anno[,wmb_cn]
wmb_cn <- c("WMB_cluster","WMB_supertype","WMB_subclass","WMB_class",
            "WMB_UMAP_1","WMB_UMAP_2")
colnames(wmb_anno) <- wmb_cn

## Merge table and reorder columns
anno <- cbind(cons_anno,wmb_anno)
rownames(anno) <- NULL

cn_anno <- c("CONS_class","WMB_class","neighborhood","CONS_cluster","CONS_supertype","CONS_subclass",
             "WMB_cluster","WMB_supertype","WMB_subclass","WMB_UMAP_1","WMB_UMAP_2")
anno <- anno[,cn_anno]

# Now for ACE, subsample to include 8% of the cells across the data set randomly + 3 cells per merged CONS_cluster, WMB_cluster randomly
set.seed(42)
samp  <- is.element(1:dim(anno)[1],sample(1:dim(anno)[1],round(dim(anno)[1]*0.08)))
samp2 <- subsampleCells_fast(paste(anno$WMB_cluster,anno$CONS_cluster),subSamp=3,seed=42)
kp    <- (samp|samp2)

anno_subset <- anno[kp,]

dim(anno_subset)
#[1] 412345     11

## Write the annotation table
fwrite(anno_subset,"consensus_to_WMB_cell_info.csv.gz",row.names=FALSE)


##########################################################
## We end by creating the cell type annotation table.   ##
##########################################################

# NOT NEEDED--REUSE EXISTING MOUSE WHOLE BRAIN

