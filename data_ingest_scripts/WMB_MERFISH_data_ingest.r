## This script downloads metadata information for the MERFISH data set for whole mouse brain from Yao et al 2023 (https://doi.org/10.1038/s41586-023-06812-z), which is also available on the ABC Atlas (https://portal.brain-map.org/atlases-and-data/bkp/abc-atlas) and on ABC Atlas Access (https://alleninstitute.github.io/abc_atlas_access/notebooks/merfish_imputed_genes_example.html; https://alleninstitute.github.io/abc_atlas_access/notebooks/merfish_ccf_registration_tutorial.html).  Each cell contains coordinates, cell type assignments, and alignments to anatomic parcellations (e.g., brain regions), which allows direct comparison of cell types and brain regions using ACE.

##############################################################
## Whole mouse brain data download and ingest

# Download the "cell_metadata_with_parcellation_annotation.csv" file from https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/MERFISH-C57BL6J-638850-CCF/20231215/views/
# This file includes all of the information we need for ACE, with one exception:
# We need to download parcellation term order from here (used below): https://github.com/AllenInstitute/voxel_service_poc/blob/main/bkp_opensearch/output_20240410/parcellation_term_MBA.csv

library(data.table)

## Read in and subsample MERFISH data with added ROI and omit nonessential columns
anno     <- fread("cell_metadata_with_parcellation_annotation.csv")
anno     <- as.data.frame(anno)
cn_keep  <- c("cluster","supertype","subclass","class",
              "parcellation_division","parcellation_structure","parcellation_substructure",
			  "neurotransmitter")
cn_keepM <- c(cn_keep, "average_correlation_score",
              "x_section","y_section","z_section", # for visualization
              paste0(cn_keep,"_color"),paste0(cn_keep,"_id"))


## Order for all cell type levels
ordC     <- order(anno$cluster)  
cns      <- c("cluster","supertype","subclass","class","neurotransmitter")
anno$neurotransmitter[anno$neurotransmitter==""] = "none"
for (cn in cns)
  anno[,paste0(cn,"_id")] = as.numeric(factor(anno[,cn],unique(anno[,cn][ordC])))


## Reorder based on parcellation order
parc     <- fread("parcellation_term_MBA.csv")
parcs    <- c(intersect(parc$symbol,gsub("-unassigned","",anno$parcellation_structure)),"brain","unassigned")
kpUn     <- setdiff(gsub("-unassigned","",setdiff(anno$parcellation_structure,parc$symbol)),c("unassigned"))
parcs[is.element(parcs,kpUn)] = paste0(parcs[is.element(parcs,kpUn)],"-unassigned")
ordP     <- order(factor(anno$parcellation_structure,levels=parcs),anno$parcellation_substructure)
cns      <- c("parcellation_division","parcellation_structure","parcellation_substructure")
for (cn in cns)
  anno[,paste0(cn,"_id")] = as.numeric(factor(anno[,cn],unique(anno[,cn][ordP])))
		  

# Subsample to include at least 40 cells per cluster AND at least 4000 cells per z-section		  
#subsamp <- subsampleCells(anno$cluster_id_label,40)	 # Super slow, replace with below
set.seed(42)
sampleX  <- function(x,n=40){ln<-min(length(x),n); sort(sample(x,ln));}
indexC   <- sapply(unique(anno$cluster),function(x)which(anno$cluster==x))
subsampC <- sort(unlist(lapply(indexC,function(x)sampleX(x,40))))
indexP   <- sapply(unique(anno$z_section),function(x)which(anno$z_section==x))
subsampP <- sort(unlist(lapply(indexP,function(x)sampleX(x,4000))))
subsampM <- sort(union(subsampP,subsampC))
annoSub  <- anno[subsampM,cn_keepM]
colnames(annoSub)[is.element(colnames(annoSub),cn_keep)] = paste0(colnames(annoSub)[is.element(colnames(annoSub),cn_keep)],"_label")

dim(annoSub)
# [1] 383968     28

## Output the subsetted MERFISH data
fwrite(annoSub,"WMB_MERFISH_subset.csv.gz",compress = "gzip")


#########################################################
## We end by creating the cell type annotation table.  ##
#########################################################

# This data set uses the same whole mouse brain annotation table that is used for the other data sets, so no additional effort is needed here.