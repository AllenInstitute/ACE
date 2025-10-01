# This file describes how to take align the MTG data from four studies that included the same cells processed using 10x Genomics single nucleus RNA-sequencing technologies.  These studies are (1) the human 'great ape' (GA) study (Jorstad, Song, Exposito-Alonso, et al 2023, DOI:10.1126/science.ade9516), which assesses cell types in MTG across species; (2) the human 'cross-areal' (CA) study (Jorstad et al 2023, DOI:10.1126/science.adf6812), which assesses cell types across multiple neocortical areas in human (including MTG); (3) 'SEA-AD' calls for each level of the taxonomy in MTG (Gabitto, Travaglini, et al 2023; DOI:10.1101/2023.05.08.539485)' and (4) a draft atlas of all cell types in the human brain (Siletti et al 2023; DOI:10.1126/science.add7046). The latter two of these studies are currently available on both MapMyCells and the ABC Atlas. Note: the clusters in the GA study represented the starting point for analysis in generation of the SEA-AD supertypes. 

# For the SEA-AD study, I downloaded all of the 'cell metadata' from  here (https://portal.brain-map.org/atlases-and-data/rnaseq/human-mtg-10x_sea-ad) from the file https://brainmapportal-live-4cc80a57cd6e400d854-f7fdcae.divio-media.net/filer_public/21/37/2137dd6c-efbf-4825-950f-6d13d3f0fb9d/cell_metadata.csv and saved it as "SEAAD_cell_metadata.csv".  This file ALSO includes cell type names from the CA and GA studies included therein.

# For whole human brain I downloaded the "Neurons.h5ad" and "Nonneurons.h5ad" files from https://github.com/linnarsson-lab/adult-human-brain
# The code below then joins together data based on barcode (to the extent possible) using overlapping cells.


######################################################################
## First read in the human whole brain data sets and SEAAD data sets 

## Load the libraries
library(data.table)
library(anndata)
library(reticulate)
options(stringsAsFactors=FALSE)
anndata  <- import("anndata")  # Use python version of anndata to load it backed 

## Read in the WHB metadata
WHB_NN <- anndata$read_h5ad("Nonneurons.h5ad", backed = "r")
WHB_NN <- py_to_r(WHB_NN$obs)
WHB_N  <- anndata$read_h5ad("Neurons.h5ad", backed = "r")
WHB_N  <- py_to_r(WHB_N$obs)
WHB_metadata <- rbind(WHB_N,WHB_NN)

## Read in SEAAD (and other study) metadata
SEAAD_metadata <- fread("SEAAD_cell_metadata.csv")
SEAAD_metadata <- as.data.frame(SEAAD_metadata)


######################################################################
## Next, find overlapping cells, and reformat the WHB data 
## NOTE: there are a lot of cells that we cannot match between studies, so we are  
##       going retain all of the cells, but only label WHB cells that we can

## Join the common cells together
# Cell_ids DO NOT match, even though many of them are the same cells.  Only align cells that I can unambiguously join based on the barcode_donor_region.
# Barcode
SEAAD_bc <- substr(SEAAD_metadata$specimen_name,1,16)
whb_rn   <- rownames(WHB_metadata)
whb_bc   <- substr(whb_rn,nchar(whb_rn)-15,nchar(whb_rn))
# Donor
SEAAD_don<- as.character(SEAAD_metadata$external_donor_name_label)
whb_don  <- as.character(WHB_metadata$donor_id)
# Region
SEAAD_reg<- "MTG"
whb_reg  <- as.character(WHB_metadata$dissection)
whb_reg  <- substr(whb_reg,nchar(whb_reg)-2,1000)
# Join them together
SEAAD_bc <- paste(SEAAD_bc,SEAAD_don,SEAAD_reg,sep="_")
whb_bc   <- paste(whb_bc,whb_don,whb_reg,sep="_")
# See which are unique and intersecting
kp_bc    <- intersect(names(table(SEAAD_bc))[table(SEAAD_bc)==1],names(table(whb_bc))[table(whb_bc)==1])
length(kp_bc)
# [1] 82987

# Define the metadata file
whb_anno <- WHB_metadata[match(kp_bc,whb_bc),]
rownames(whb_anno) <- kp_bc
whb_anno <-  whb_anno[,c("subcluster_id","cluster_id","supercluster_term")]
colnames(whb_anno) <- c("WHB_subcluster", "WHB_cluster", "WHB_supercluster")

## Replace WHB_cluster/subcluster ids with their names, and disambiguate

# This is information from columns 1-2 in Table S3 in Siletti et al 2023 (saved locally as "whb_convert.csv")
whb_convert <- read.csv("whb_convert.csv")
whb_anno$WHB_cluster <- whb_convert[,2][match(whb_anno$WHB_cluster,whb_convert[,1])]

## Convert subcluster to a character vector, by attaching existing id with cluster
whb_anno$WHB_subcluster <- paste0(whb_anno$WHB_cluster,"_",whb_anno$WHB_subcluster)
# NOTE: we have an table matching Table S3 for subclass that was provided through correspondence with Kimberly Siletti that is used for this

## Add modifiers to cell type columns for disambiguation
whb_anno$WHB_supercluster   <- paste0(whb_anno$WHB_supercluster,"_WHB")


######################################################################
## Add the WHB annotations to the SEA_AD metadata table and subset

## Add the WHB annotations 
all_anno <- data.frame(SEAAD_metadata,WHB_subcluster="", WHB_cluster="", WHB_supercluster="")
all_anno[match(kp_bc,SEAAD_bc),c("WHB_subcluster", "WHB_cluster", "WHB_supercluster")] = whb_anno

## Subset and rename the columns
all_anno <- all_anno[,c("cluster_label","subclass_label","class_label","GA_cluster_label","CA_cluster_label",
                        "WHB_subcluster","WHB_cluster","WHB_supercluster",
				        "donor_sex_label","external_donor_name_label")]
colnames(all_anno) <- c("SEAAD_supertype", "subclass", "class", "GA_cluster", "CA_cluster",
                        "WHB_subcluster", "WHB_cluster", "WHB_supercluster",
                        "sex", "donor_name")

## Remove cells not used in the SEA-AD study or CA study 
all_anno <- all_anno[all_anno$SEAAD_supertype!="",]
all_anno <- all_anno[all_anno$CA_cluster!="",]


######################################################################
## Disambiguate cluster names and output results

## Append _CA and _GA to cross areal and great ape clusters, respectively 
all_anno$GA_cluster <- paste0(all_anno$GA_cluster, "_GA")
all_anno$CA_cluster <- paste0(all_anno$CA_cluster, "_CA")

dim(all_anno)
#[1] 135851     10

# Write out the results
fwrite(all_anno,"MTG_cell_metadata.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# This is the same file used for several other studies. See 'align_crossAreal_and_WHB.r' for details.  In addition, information about SEA-AD cell types from 'AD_study_cell_types_for_app.csv' is merged into mtg_whb_annotations.csv.gz so that everything can be in a single file.

