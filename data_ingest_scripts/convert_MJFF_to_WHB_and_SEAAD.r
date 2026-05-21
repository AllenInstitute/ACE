## This script downloads files associated with a set of Parkinson's disease (PD) data sets funded by the Michael J. Fox Foundation for Parkinson’s Research (MJFF) (details below) and converts them into tables usable with ACE.  These data sets are all aligned both the the human whole brain and the SEA-AD middle temporal gyrus (MTG) taxonomy (see https://brain-map.org/bkp/analyze/mapmycells/taxonomies) using MapMyCells, and the mapping results are built in to the files downloaded below (as well as in the ABC Atlas GUI itself: https://portal.brain-map.org/atlases-and-data/bkp/abc-atlas)

# MORE ABOUT THE DATA SETS AND ASSOCIATED PROJECT (from: https://alleninstitute.github.io/abc_atlas_access/descriptions/ASAP-PMDBS-10X.html): Aligning Science Across Parkinson’s (ASAP), The MJFF, and the Allen Institute for Brain Science (AIBS) are teaming up to further the mission of the ASAP Collaborative Research Network (CRN) program, to accelerate discoveries in the Parkinson’s disease (PD) and neurodegenerative disease research communities. Together we will annotate, enhance and add knowledge to the growing data catalog in the ASAP CRN Cloud through integration of cell type taxonomies using the Allen Institute’s MapMyCells tool and visualization through the Allen Brain Cell (ABC) Atlas web application. This integration of data and knowledge will allow users to visualize and explore the changes in gene expression of specific, highly resolved brain cell types in the context of a large PD cohort of donors. This initial collaboration focuses on the Human Postmortem-derived Brain Sequencing Collection (PMDBS), a harmonized repository comprised of single nucleus and PolyA RNA-seq data contributed by five ASAP CRN teams (Hafler, Lee, Jakobsson, Scherzer, Hardy). Sequencing data were uniformly aligned to the GRCh38.p13 reference genome (Gencode V32), quality control was performed and low-quality cells were filtered out. A set of highly variable genes were identified and the scVI workflow resulted in an integrated latent variable representation, 2D UMAP coordinates and a set of 30 clusters. Currently, the repository spans roughly 3 millions cells obtained from 9 brain regions and 211 donors with various pathologies (including healthy control). For more details on this dataset and to access the raw data used in its preparation, please visit the ASAP CRN Cloud webpage.


##############################################################
## Download relevant files from ABC Atlas Access and ingest

# First download the "cell_metadata.csv" and "sample.csv" files from the ABC Atlas Access website (https://alleninstitute.github.io/abc_atlas_access/descriptions/ASAP-PMDBS-10X.html; and specifically from this bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/ASAP-PMDBS-10X/20250331/)
# We also need the results of the mapping to the above two data sets from here: https://alleninstitute.github.io/abc_atlas_access/descriptions/ASAP-PMDBS-MapMyCells.html (files are called "mmc_results_seaad.csv" and "mmc_results_siletti_whb.csv" and are in this AWS bucket: https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/ASAP-PMDBS-taxonomy/20250331/).  
# Essentially all this script does is merge files together

# UPDATE May 2026: we now align the matched subset of the data to the Consensus Basal Ganglia taxonomy (CCN20250428)
# For this we need to download the raw data here: https://allen-brain-cell-atlas.s3-us-west-2.amazonaws.com/expression_matrices/ASAP-PMDBS-10X/20250331/ASAP-PMDBS-10X-raw.h5ad
# We then subset and map using the MapMyCells GUI (see below)

##############################################################
## Read in all the data sets
library(data.table)
metadata   <- fread("cell_metadata.csv")
sampleinfo <- fread("sample.csv")
seaad      <- fread("mmc_results_seaad.csv")
siletti    <- fread("mmc_results_siletti_whb.csv")

# Join sample to metadata
sampleinfo <- as.data.frame(sampleinfo)
metadata   <- as.data.frame(metadata)
metadata   <- cbind(metadata,sampleinfo[match(metadata$sample_label,sampleinfo$sample_label),])

# Subset data to relevant columns
meta_kp  <- c("cell_label","cluster_label","region_of_interest_label","source_dataset_label","technique","donor_label","x","y")
metadata <- metadata[,meta_kp]
colnames(metadata) <- c("cell_label","cluster","Brain_region","Study","RNASeq_technique","Donor","UMAP_X","UMAP_Y")

seaad_kp <- c("cell_label","supertype_name","supertype_bootstrapping_probability","subclass_name","subclass_bootstrapping_probability","class_name","class_bootstrapping_probability")
seaad    <- as.data.frame(seaad)[,seaad_kp]
colnames(seaad) <- c("cell_label","SEAAD_supertype","SEAAD_supertype_confidence","SEAAD_subclass","SEAAD_subclass_confidence","SEAAD_class","SEAAD_class_confidence")

siletti_kp <- c("cell_label","subcluster_name","subcluster_bootstrapping_probability","cluster_name","cluster_bootstrapping_probability","supercluster_name","supercluster_bootstrapping_probability")
siletti    <- as.data.frame(siletti)[,siletti_kp]
colnames(siletti) <- c("cell_label","WHB_subcluster","WHB_subcluster_confidence","WHB_cluster","WHB_cluster_confidence","WHB_supercluster","WHB_supercluster_confidence")
siletti$WHB_supercluster <- paste0(siletti$WHB_supercluster,"_WHB") # Append "_WHB" at end of "WHB_supercluster"

# Sample to include only cells with metadata and merge to a single table 
seaad   <- seaad[match(metadata$cell_label,seaad$cell_label),]
siletti <- siletti[match(metadata$cell_label,siletti$cell_label),]
output  <- cbind(metadata,siletti[,colnames(siletti)!="cell_label"],seaad[,colnames(seaad)!="cell_label"])
fwrite(output,"ASAP_CRN_cell_info.csv.gz")

# Subsample to 279,674 cells (10% of the data)

set.seed(42)
keepCells <- sample(1:dim(output)[1],279674)
output_sub <- output[keepCells,c("cell_label",setdiff(colnames(output),"cell_label"))]


########################################################################
## Read in the ASAP data set, subset and write out for mapping

library(anndata)
asap = read_h5ad("ASAP-PMDBS-10X-raw.h5ad")
rn   = rownames(asap)
reorder_keep = match(output$cell_label[keepCells],rn)

asap_subset = asap[reorder_keep,]
write_h5ad(asap_subset, "asap_subset.h5ad", compression="gzip")			 


########################################################################
## Break to run these data in the MapMyCells GUI. Results are         ##
##  downloaded and the csv file is renamed as "asap_bg_mapping.csv"     ##
########################################################################  


########################################################################
## Read back in the mapping, and join with existing results

## Read in and rename mapping columns
mapping <- read.csv("asap_bg_mapping.csv",row.names=1,skip=4)
mapping <- mapping[,c(11,12,8,9,5,6,2,3)]
colnames(mapping) <- c("BG_group","BG_group_confidence","BG_subclass","BG_subclass_confidence",
                       "BG_class","BG_class_confidence","BG_neighborhood","BG_neighborhood_confidence")
metadata_out <- cbind(output_sub, mapping)
metadata_out$BG_subclass <- paste0(metadata_out$BG_subclass,".")
fwrite(metadata_out,"ASAP_CRN_cell_info_subsample.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# This is the same file used for the AD studies ("AD_study_cell_types_for_app.csv"), and it's creation is described in the relevant data_ingest_script.
# NOTE: we have appended all information from the "BG_cross_species_annotation_information.csv" file to the end of this file. This has no effect for any of the other cell type taxonomies, but allows this one to access cell type information from both.