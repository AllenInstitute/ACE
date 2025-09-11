# This file includes annotated script for collecting data from the Allen Institute for Immunology Human Immune Health Atlas available at https://apps.allenimmunology.org/aifi/resources/imm-health-atlas/ and with a preprint (as of September 2025) at https://doi.org/10.1101/2024.09.10.612119.

# First, download data from here on 10 September 2025:
# https://apps.allenimmunology.org/aifi/resources/imm-health-atlas/downloads/scrna/
# We need these files:  
# ==== human_immune_health_atlas_full.h5ad:  The full set of all ~1.8 million cells from the PBMC scRNA-seq data from the Immune Health Atlas. Note that we need this for the metadata, but are NOT using the data itself
# ==== human_immune_health_atlas_metadata_clinical_labs.csv: additional donor metadata (this might already be in the above file).

# Overall process:
# 1) Read in the obs from the h5ad file
# 2) Subsample and write out file
# 3) Read in the donor metadata and join in any additional variables

# Note: this will have to be run in one of the scrattch docker environments (it shouldn't matter which one, but I used alleninst/scrattch:1.1.2)

###############################################################################

library(scrattch.taxonomy)  # I really only need data.table, anndata

## Read in the obs metadata
anndata  <- import("anndata")
adata    <- anndata$read_h5ad("human_immune_health_atlas_full.h5ad", backed = "r")
metadata <- py_to_r(adata$obs)   # converts the pandas DataFrame to an R data.frame


## Subsample substantially
# First keep 1000 random cells per subject
meta_new <- metadata[subsampleCells(metadata$subject.subjectGuid,1000),]
# Next cap at 10000 random cells per L3 cell type
meta_new <- meta_new[subsampleCells(meta_new$AIFI_L3,10000),]


## Merge in additional donor information
donor_info <- read.csv("human_immune_health_atlas_metadata_clinical_labs.csv")
new_cols   <- setdiff(colnames(donor_info),colnames(meta_new))
donor_all_info <- donor_info[match(meta_new$subject.subjectGuid,donor_info$subject.subjectGuid),new_cols]


## Reorder and subset columns and output table
all_info   <- cbind(meta_new,donor_all_info)
# Add "cell." to the cell features
colnames(all_info)[!grepl("\\.", colnames(all_info))] <- paste0("cell.",colnames(all_info)[!grepl("\\.", colnames(all_info))])
# Add an "analysis_type" column for cell-level analysis vs. donor-level analysis
all_info$analysis_type = "Always retain these cells"
all_info$analysis_type[duplicated(all_info$subject.subjectGuid)] = "Retain for cell-level analysis; omit for donor-level analysis."
first_cols <- c("subject.subjectGuid","cell.AIFI_L3","cell.AIFI_L2","cell.AIFI_L1","analysis_type")
first_cols <- c(first_cols,setdiff(colnames(all_info)[substr(colnames(all_info),1,5)=="cell."],first_cols))
omit_cols  <- c("pipeline.fileGuid","sample.sampleKitGuid","specimen.specimenGuid")
# Omit any columns with exactly 1 or >220,000 values (e.g., barcodes)
for (cn in colnames(all_info))
  if((length(unique(all_info[,cn]))<=1)|(length(unique(all_info[,cn]))>220000))
    omit_cols <- c(omit_cols,cn)
use_cols   <- setdiff(c(first_cols,sort(setdiff(colnames(all_info),first_cols))),omit_cols)
all_info   <- all_info[,use_cols]
# Add ".L2/3" to disambiguate cell types
for (i in 2:3){
  cn <- paste0("cell.AIFI_L",i)
  all_info[,cn] <- paste0(all_info[,cn],".L",i)
}

dim(all_info)
# [1] 95136     87

# Write out the results
fwrite(all_info,"HISE_cell_info.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# Start with the donor metadata file for SEA-AD (human_immune_health_atlas_metadata_clinical_labs.csv) and then edit to to add additional CLUSTER and information from HISE.  Note that cell type information and cell type order are taken directly from https://apps.allenimmunology.org/aifi/resources/imm-health-atlas/

# Cell type orders and colors come from the Cell Type Colorset files at https://apps.allenimmunology.org/aifi/resources/imm-health-atlas/downloads/models/