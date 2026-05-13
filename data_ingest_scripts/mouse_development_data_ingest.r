## This script reads in the 'Developing-Mouse-Vis-Cortex-10X' and 'Developing-Mouse-Vis-Cortex-taxonomy' files from abc_atlas_access (as of March 31, 2026), and merges all of these into a cell and cell type annotation tables, which are then subsampled for inclusion in ACE.  These files are all downloaded from the following AWS directories:
# 1) Cell metadata (Developing-Mouse-Vis-Cortex-10X): https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/Developing-Mouse-Vis-Cortex-10X/20260131/ (This includes the cell, donor, and library metadata)
# 2) Cell type metadata (Developing-Mouse-Vis-Cortex-taxonomy) ==> https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/index.html#metadata/Developing-Mouse-Vis-Cortex-taxonomy/20260331/ (These files have the cell type taxonomy information, but not the cell information)

## All of these are saved into a folder called "20260131", from which the files are read

## These data correspond to the following study: Gao et al 2025; https://doi.org/10.1038/s41586-025-09644-1

########################################################################

## Set up the work space
library(data.table)
library(stringr)
source("https://raw.githubusercontent.com/AllenInstitute/mfishtools/977022e8faf0e79a512b3536ff07375ff7ffc33e/R/markerGenesAndMapping.r") # for "subsampleCells" function
data_dir <- "20260131"


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
   cell[,c(1,7,8)],
   donor[match(cell$donor_label,donor$donor_label),c(5,6)],
   region_of_interest_name = as.character(library[match(cell$library_label,library$library_label),c(7)]),
   cell_2d_embedding_coordinates[match(cell$cell_label,cell_2d_embedding_coordinates$cell_label),2:3],
   subcluster_label = cell_to_cluster_membership$label
)
postnatal = substr(cell_extended$synchronized_age,1,1)=="P"
cell_extended$donor_age_value[postnatal] = cell_extended$donor_age_value[postnatal]+20

# Add the taxonomy
cell_extended$subcluster = cluster_annotation_term[match(cell_extended$subcluster_label,cluster_annotation_term$label),"name"]
cell_extended$cluster = cluster_annotation_term[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$subcluster,cluster_annotation_term$name[grepl("LEVEL_2",cluster_annotation_term$parent_term_set_label)])]
cell_extended$subclass = cluster_annotation_term[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$cluster,cluster_annotation_term$name[grepl("LEVEL_1",cluster_annotation_term$parent_term_set_label)])]
cell_extended$class = cluster_annotation_term[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label),"parent_term_name"][match(cell_extended$subclass,cluster_annotation_term$name[grepl("LEVEL_0",cluster_annotation_term$parent_term_set_label)])]

# Disambiguate levels 
cell_extended$subclass = paste0(cell_extended$subclass,".")
cell_extended$class = paste0(cell_extended$class,"..")

# Remove _label in donor_label to avoid issue in ACE
colnames(cell_extended)[colnames(cell_extended)=="donor_label"] = "donor"

# Reorder, omit, and rename columns
cell_extended <- cell_extended[,c(1,10:13,6,2,5,4,3,7,8)]
colnames(cell_extended) <- c("cell_label", "subcluster", "cluster", "subclass", "class",
     "brain_region", "age_text", "age_numeric", "donor_sex", "donor", "UMAP_x", "UMAP_y")
rownames(cell_extended) = NULL

# Now for ACE, subsample to include 25% of the cells across the data set randomly + 10 cells per subcluster randomly
#---- Before subsampling, omit cells that do not map to any group, subclass, class, or neighborhood.
set.seed(42)
samp  <- is.element(1:dim(cell_extended)[1],sample(1:dim(cell_extended)[1],round(dim(cell_extended)[1]*0.25)))
samp2 <- subsampleCells(cell_extended$cluster,subSamp=10,seed=42)
kp    <- (samp|samp2)&(!is.na(cell_extended$class))
cell_extended <- cell_extended[kp,]

# Write out subsampled data set
fwrite(cell_extended,"mouse_development_metadata.csv.gz")


########################################################################

## Create the cell type and value annotation file

metadata_values <- value_sets[,c(1,3,5,6,8,2,7)]
cluster_values  <- cluster_annotation_term[,c(2,4,6,1,5,2,9)]
cluster_values[cluster_values[,2]=="subclass",1] = paste0(cluster_values[cluster_values[,2]=="subclass",1],".")
cluster_values[cluster_values[,2]=="class",1]    = paste0(cluster_values[cluster_values[,2]=="class",1],"..")
colnames(metadata_values) <- colnames(cluster_values) <- c("cell_type","annotation_name","term_order","unique_identifier","color","alias","parent_term")

annotation_values <- rbind(cluster_values,metadata_values)
annotation_values <- annotation_values[order(annotation_values[,2],annotation_values[,3]),]

annotation_values <- annotation_values[annotation_values[,2]!="age_bin",]
annotation_values[,2][annotation_values[,2]=="synchronized_age"] = "age_text"

## Output tables
fwrite(annotation_values,"mouse_development_annotation_information.csv")
