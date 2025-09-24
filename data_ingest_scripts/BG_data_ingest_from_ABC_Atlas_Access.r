## This script reads in the 'HMBA-10xMultiome-BG-Aligned' and 'HMBA-BG-taxonomy-CCN20250428' files from abc_atlas_access (as of May 5, 2025), and merges all of these into a cell and cell type annotation tables, which are then subsampled for inclusion in ACE.  These files are all downloaded from the following AWS directories:
# 1) Cell metadata (HMBA-10XMultiome-BG-Aligned): https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/HMBA-10xMultiome-BG/20250531/ (This includes the cell, donor, and library metadata)
# 2) Cell type metadata (HMBA-BG-taxonomy-CCN20250428) ==> https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/HMBA-BG-taxonomy-CCN20250428/20250531/   (These files have the cell type taxonomy information, but not the cell information)

## In addition, we need to download the 'raw' RNA-seq data files from https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#expression_matrices/HMBA-10xMultiome-BG/20250531/
# These are used for mapping to human and mouse whole brain using MapMyCells, as described below

## All of these are saved into a folder called "20250531", from which the files are read


########################################################################

## Set up the work space
#setwd("\\\\allen\\programs\\celltypes\\workgroups\\humancelltypes\\JeremyM\\github\\annotation_comparison\\example\\BG\\")
library(data.table)
library(anndata)
library(stringr)
source("https://raw.githubusercontent.com/AllenInstitute/mfishtools/977022e8faf0e79a512b3536ff07375ff7ffc33e/R/markerGenesAndMapping.r") # for "subsampleCells" function
data_dir <- "20250531"


########################################################################

## Read in the cell information from a bunch of files
cell       <- fread(file.path(data_dir,"cell_metadata.csv"),data.table=FALSE) 
donor      <- fread(file.path(data_dir,"donor.csv"),data.table=FALSE) 
library    <- fread(file.path(data_dir,"library.csv"),data.table=FALSE) 
value_sets <- fread(file.path(data_dir,"value_sets.csv"),data.table=FALSE) 

## Read in the cell type taxonomy information from a bunch of files
cluster                       <- fread(file.path(data_dir,"cluster.csv"),data.table=FALSE) 
cell_2d_embedding_coordinates <- fread(file.path(data_dir,"cell_2d_embedding_coordinates.csv"),data.table=FALSE) 
cell_to_cluster_membership    <- fread(file.path(data_dir,"cell_to_cluster_membership.csv"),data.table=FALSE) 
cluster_annotation_term       <- fread(file.path(data_dir,"cluster_annotation_term.csv"),data.table=FALSE) 


## Create the cell annotation file
cell_extended = data.frame(
   cell[,c(1,3)],
   donor[match(cell$donor_label,donor$donor_label),c(3,5,7)],
   region_of_interest_name = as.character(library[match(cell$library_label,library$library_label),c(7)]),
   cell_2d_embedding_coordinates[match(cell$cell_label,cell_2d_embedding_coordinates$cell_label),2:3],
   cluster = cell_to_cluster_membership$cluster_alias
)

cell_extended$group = cluster_annotation_term[match(cell_extended$cluster,cluster_annotation_term$name),"parent_term_name"]
cell_extended$subclass = cluster_annotation_term[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$group,cluster_annotation_term$name[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label)])]
cell_extended$class = cluster_annotation_term[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$subclass,cluster_annotation_term$name[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label)])]
cell_extended$neighborhood = cluster_annotation_term[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$class,cluster_annotation_term$name[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label)])]

# Disambiguate subclass from group
cell_extended$subclass = paste0(cell_extended$subclass,".")

# Remove _label in donor_label to avoid issue in ACE
colnames(cell_extended)[colnames(cell_extended)=="donor_label"] = "donor"

# Now for ACE, subsample to include 15% of the cells across the data set randomly + 10 cells per cluster randomly
#---- Before subsampling, omit cells that do not map to any group, subclass, class, or neighborhood.
set.seed(42)
samp  <- is.element(1:dim(cell_extended)[1],sample(1:dim(cell_extended)[1],round(dim(cell_extended)[1]*0.15)))
samp2 <- subsampleCells(cell_extended$cluster,subSamp=10,seed=42)
kp    <- (samp|samp2)&(!is.na(cell_extended$class))
cell_extended <- cell_extended[kp,]


########################################################################

## Subset the h5ad files for alignment to human and mouse whole brain using MapMyCells GUI (https://knowledge.brain-map.org/mapmycells/process/)

species = c("Marmoset","Macaque","Human")
for (spec in species){
  print(spec)
  fn <- paste0(data_dir,"/HMBA-10xMultiome-BG-",spec,"-raw.h5ad")
  expr_data <- read_h5ad(fn)
  keep_cell <- intersect(rownames(expr_data),cell_extended[,1])
  expr_out  <- expr_data[keep_cell,]
  expr_out$var_names <- make.unique(as.character(expr_data$var$gene_symbol))
  write_h5ad(expr_out,paste0(spec,"_expression.h5ad"),compression="gzip")
}


########################################################################
## BEGIN:  THIS SECTION IS DONE OUTSIDE OF R

## We now run MapMyCells by uploading "[Marmoset/Macaque/Human]_expression.csv.gz" to the MapMyCells GUI twice, applying "Hierarchical Mapping" to both "10x Whole Human Brain (CCN202210140)" (called "HumanBrain" in ACE) and to "10x Whole Mouse Brain (CCN20230722)" (called "MouseBrain" in ACE). This was done on September 23, 2025.

# The output files are downloaded into "data_dir", where the mapped csv files are extracted and saved in the format [species]_[HumanBrain/MouseBrain]_mapping.csv

## END:  THIS SECTION IS DONE OUTSIDE OF R
########################################################################


## Read in and append label transfer results

mapping <- NULL
for (spec in species){
  print(spec)
  human_mapping <- read.csv(paste0(data_dir,"/",spec,"_HumanBrain_mapping.csv"), comment.char = "#")
  colnames(human_mapping) <- paste0("HumanBrain_",str_to_sentence(colnames(human_mapping)))
  mouse_mapping <- read.csv(paste0(data_dir,"/",spec,"_MouseBrain_mapping.csv"), comment.char = "#")
  colnames(mouse_mapping) <- paste0("MouseBrain_",str_to_sentence(colnames(mouse_mapping)))
  mapping <- rbind(mapping,cbind(human_mapping,mouse_mapping))
}
mapping <- mapping[match(cell_extended[,1],mapping[,1]),]

# Merge with cell metadata
cell_complete <- cbind(cell_extended,mapping)
cell_complete <- cell_complete[,c(10:13,9,16,19,22,27,30,33,36,6,3,4,7,8,2,5)]
colnames(cell_complete)[16:17] = c("UMAP_x","UMAP_y")
colnames(cell_complete)[1:5]   = paste0("BG_",colnames(cell_complete)[1:5])

# Write out subsampled data set
fwrite(cell_complete,"BG_cross_species_metadata.csv.gz")


########################################################################

## Create the cell type and value annotation file

metadata_values <- value_sets[,c(4,3,5,6,8,1,7)]
cluster_values  <- cluster_annotation_term[,c(2,4,6,1,5,2,9)]
cluster_values[cluster_values[,2]=="Subclass",1] = paste0(cluster_values[cluster_values[,2]=="Subclass",1],".")
colnames(metadata_values) <- colnames(cluster_values) <- c("cell_type","annotation_name","term_order","unique_identifier","color","alias","parent_term")

annotation_values <- rbind(cluster_values,metadata_values)

## Output tables
fwrite(annotation_values,"BG_cross_species_annotation_information.csv")

# Note: This cell type information page only includes BG cell types.  For completeness we are going to manually copy some of the information from the whole human brain and whole mouse brain annotation information tables whose creation is described elsewhere.