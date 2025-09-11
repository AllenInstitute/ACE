## This file includes annotated script for collecting data from the "PsychENCODE (brainSCOPE)" study listed on ACE. Details about the study are available at https://www.science.org/doi/10.1126/science.adi5199, and data is accessible from https://brainscope.gersteinlab.org/integrative_files.html.

# We downloaded the following files for this script.  
# ==== [sample]-annotated_matrix.txt.gz  (from https://brainscope.gersteinlab.org/output-sample-annotated-matrix.html; we downloaded "all.zip").  This included all of the cell x gene data requred for MapMyCells
# ==== Cell_metadata.tar.gz.  This contains all the cell type assignments for all the studies from the original paper
# Download and unzip both.  NOTE: The annotated matrices do NOT have sample_ids, but they are included in the same order as the cell metadata files for 7 of the studies (see below). I was unable to match data and metadata for the other 5 studies.
# ==== Also https://brainscope.gersteinlab.org/data/sample_metadata/PEC2_sample_metadata.txt = donor metadata, which is joined in both for the cell and the cell type annotation tables.

# Overall process, per data set within brainSCOPE:
# 1) Read in the data for each donor, join into a single matrix
# 2) Read in the metadata to assign cell type names
# --- NOTE: I had to rename a few metadata files (Kriegstein to Velmeshev_et_al and Urban-DLPFC to MultiomeBrain were the only non-obvious name changes).  The one called "AMP-AD_ROSMAP_cell_metadata.tsv" was omitted
# 3) Save as one h5ad file (or split if too big)
# 4) Run using the GUI for MapMyCells, using the SEA-AD MTG taxonomy and the deep generative model
# 5) Join clustering and mapping results and save to single csv file (likely with downsampling) as with previous studies
# --- Be sure to include donor and study information in this table since it looks like multiple diseases are covered here.
# --- SEPARATELY review paper to see which cell type show change in which disease, in the cluster information file.

# Note: this will have to be run in one of the scrattch docker environments (it shouldn't matter which one, but I used alleninst/scrattch:1.1.2)

###############################################################################

## Do steps 1-3 above

library(scrattch.taxonomy)  # I really only need data.table, anndata

studies <- dir("expression_data")

# Successful studies mapping data and metadata: IsoHuBm, DevBrain, LIBD, Ma_et_al, PTSDBrainomics, UCLA-ASD, Velmeshev_et_al

# Omitted studies: 
# AMP-AD (I cannot access the data)
# CMC (Matrices for data and metadata do not match)
# Girgenti-multiome (Matrices for data and metadata do not match) 
# MultiomeBrain (Matrices for data and metadata do not match) 
# SZBDMulti-Seq (Matrices for data and metadata do not match) 

studies <- setdiff(studies,c("Girgenti-multiome","CMC","MultiomeBrain","SZBDMulti-Seq"))

for (stud in studies){
  print(paste("=== STARTING: ",stud))
  metadataIn <- fread(paste0("Cell_metadata/",stud,"_cell_metadata.tsv"))
  metadataIn <- as.data.frame(metadataIn)
  metadata   <- metadataIn[,2:dim(metadataIn)[2]]
  rownames(metadata) <- as.character(metadataIn[,1])
  if(stud=="Ma_et_al")  # Manual hack for inconsistent naming
    metadata$individualID <- substr(metadata$individualID,1,6)
  donors <- unique(metadata$individualID)
  expr <- samples <- NULL
  print(paste(length(donors),"total donors."))
  for (don in donors){
    print(paste(stud,"-",don))
	file_name = paste0("expression_data/",stud,"/",don,"-annotated_matrix.txt.gz")
	if(stud=="Girgenti-multiome")  # Manual hack for inconsistent naming
	  file_name = paste0("expression_data/",stud,"/",substr(gsub("2RT","RT",gsub("_MAH","",don)),1,8),"-annotated_matrix.txt.gz")
	if(file.exists(file_name)){
      expr_don   <- fread(file_name)
      mat        <- as.matrix(expr_don[, -1, with = FALSE])
      sparse_mat <- Matrix(mat, sparse = TRUE)
	  expr       <- cbind(expr, sparse_mat)
	  samples    <- c(samples,rownames(metadata)[metadata$individualID==don])
	  print(paste("Number of samples:",length(rownames(metadata)[metadata$individualID==don])))
	  print(paste("Matrix dimensions:",dim(sparse_mat)[1],"x",dim(sparse_mat)[2]))
	} else {
	  print(paste(file_name,"not found"))
	}
  }
  print(paste("=== COMPILING AND SAVING: ",stud))
  expr <- t(expr)
  expr <- as(expr, "dgCMatrix")
  rownames(expr) <- samples
  colnames(expr) <- expr_don$featurekey

  anndata <- AnnData(X = expr, var = data.frame(gene = colnames(expr)), obs = metadata)
  anndata$var_names <- expr_don$featurekey
  anndata$obs_names <- samples
  write_h5ad(anndata,paste0(stud,".h5ad"), compression="gzip")			 
}

# Split UCLA-ASD into two because it's too big for MapMyCells
anndata  <- read_h5ad("UCLA-ASD.h5ad")	
anndata1 <- anndata[1:200000,]
anndata2 <- anndata[200001:dim(anndata)[1],]
write_h5ad(anndata1,"UCLA_ASD_1.h5ad", compression="gzip")	
write_h5ad(anndata2,"UCLA_ASD_2.h5ad", compression="gzip")	


###############################################################################################
## Here we break for running these data in the MapMyCells GUI. Results are saved in a folder ##
##  called "mapping_results", unzipped, and the csv file is renamed as "[study]_mapping.csv" ##
###############################################################################################

## Rejoin MapMyCells output for UCLA-ASD
in1 <- scan("mapping_results/UCLA_ASD_1_mapping.csv",what="character",sep="\n")
in2 <- scan("mapping_results/UCLA_ASD_2_mapping.csv",what="character",sep="\n")
out <- c(in1,in2[5:length(in2)])  # Skip comments and header for second file
write(out,"mapping_results/UCLA-ASD_mapping.csv")

## Next, join clustering and mapping results and output a cell information table (step 5)
# 1) Read in cell metadata for each (non-omitted) study
# 2) Read in mapping results
# 3) Read in donor metadata for each (non-omitted) study
# 4) Join tables, rename columns of interst, and write out results

# Mapping and metadata
metadata_all <- mapping_all <- NULL
for (stud in studies){
  print(paste("=== STARTING: ",stud))
  # Read in metadata
  metadataIn <- fread(paste0("Cell_metadata/",stud,"_cell_metadata.tsv"))
  metadataIn <- as.data.frame(metadataIn)
  metadata   <- metadataIn[,2:dim(metadataIn)[2]]
  rownames(metadata) <- as.character(metadataIn[,1])
  
  # Manual hack for updated donor names in Ma_et_al
  if(stud=="Ma_et_al") metadata$individualID <- substr(metadata$individualID,1,6)
  
  # Read in and format mapping
  mapping <- read.csv(paste0("mapping_results/",stud,"_mapping.csv"),row.names=1,skip=3)
  mapping <- mapping[,!grepl("_label",colnames(mapping))]
  colnames(mapping) <- gsub("softmax_","",colnames(mapping))
  colnames(mapping) <- paste0("SEAAD_MTG_",colnames(mapping) )
  
  # Subset and format metadata
  metadata <- metadata[rownames(mapping),c("anno","subclass","azimuth","individualID","n_genes","n_counts")]
  colnames(metadata) <- paste0("brainSCOPE_",colnames(metadata))
  
  metadata_all <- rbind(metadata, metadata_all)
  mapping_all  <- rbind(mapping, mapping_all)
}

# Read donor metadata
donor_infoIn <- fread("PEC2_sample_metadata.txt")
donor_infoIn <- as.data.frame(donor_infoIn)
donor_info   <- donor_infoIn[,c(1,3:dim(donor_infoIn)[2])]
rownames(donor_info) <- as.character(donor_infoIn[,2])
donor_info_all <- donor_info[match(metadata_all$brainSCOPE_individualID,rownames(donor_info)),
                             c(1,4,2,3,5)]
donor_info_all$Disorder[donor_info_all$Disorder=="control"] = "Control"

# Combine into a single file, reorder, and rename 
all_info <- cbind(metadata_all, mapping_all, donor_info_all)
all_info <- all_info[,c(1:2,11:12,9:10,7:8,13,14,4,15:17)]
colnames(all_info)[colnames(all_info)=="brainSCOPE_anno"] = "brainSCOPE_cluster"
colnames(all_info)[colnames(all_info)=="brainSCOPE_individualID"] = "Donor_ID"
colnames(all_info) <- gsub("_name","",colnames(all_info))

## Subset the data in a few ways
# Remove cells from donors with missing info
all_info <- all_info[!is.na(all_info$Cohort),]
# subsample each donor to 7,000 cells
all_info <- all_info[subsampleCells(all_info$Donor_ID,7000),]
# subsample each cohort to 60,000 cells
all_info <- all_info[subsampleCells(all_info$Cohort,60000),]
# subsample each cohort x subclass to 7,000 cells
all_info <- all_info[subsampleCells(paste(all_info$Cohort,all_info$brainSCOPE_subclass),7000),]

row.names(all_info) <- NULL  # remove sample ids to save space

# Add "_B24" for "brainSCOPE 2024" to the end of all the subclass and cluster terms for disambiguation
all_info$brainSCOPE_subclass <- paste0(all_info$brainSCOPE_subclass,"_B24")
all_info$brainSCOPE_cluster  <- paste0(all_info$brainSCOPE_cluster,"_B24")

# Add "-1" for all clusters that don't include a "-" symbol.
vec = !grepl("-", all_info$brainSCOPE_cluster)
all_info$brainSCOPE_cluster[vec] <- gsub("_B24","-1_B24",all_info$brainSCOPE_cluster[vec])

# Match study names to paper
all_info$Cohort[all_info$Cohort=="Ma_et_al"] = "Ma-Sestan"
all_info$Cohort[all_info$Cohort=="Velmeshev_et_al"] = "Velmeshev"

dim(all_info)
# [1] 300046     14

# Write out the results
fwrite(all_info,"brainSCOPE_cell_info.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# Start with the metadata file for SEA-AD (https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/AD_study_cell_types_for_app.csv) and then edit to to add additional CLUSTER and DONOR information from this study

# Write a list of all the clusters and subclasses
write(sort(unique(all_info$brainSCOPE_cluster)),"clusters.txt")
write(sort(unique(all_info$brainSCOPE_subclass)),"subclasses.txt")
write(sort(unique(all_info$Donor_ID)),"donors.txt")
