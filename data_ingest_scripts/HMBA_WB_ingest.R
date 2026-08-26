## This script is for creating ACE files for the cross-species whole-brain single-cell RNA-seq taxonomy spanning human, macaque, and marmoset, referenced to the mouse and human whole-brain taxonomies (Yao et al., Siletti et al.). It reads in Code Ocean assets which are the "obs" fields for the AIT files available at https://alleninstitute.github.io/HMBA_WB_Atlas/, as well as a cell type table that indicates the order and color of each cell type, brain region, donor, etc..  Since this data set includes ~21 million cells, we need to do a LOT of downsampling.

## Overall this script (1) reads in the obs files for each species along with the cell type information, (2) subsample each species data set, (3) rename data in some levels for global uniqueness, (4) write updated version of four files (to the data folder for inclusion in GitHub).

##############################################################
## Set-up

# Fast implementation of subsampleCells function
subsampleCells <- function(cluster.names, subSamp = 25, seed = 5) { 
  if(length(subSamp)==1) 
    subSamp = rep(subSamp,length(unique(as.character(cluster.names)))) 
  if(is.null(names(subSamp))) 
    names(subSamp) <- unique(as.character(cluster.names)) 
  
  set.seed(seed) 
  cluster_split <- split(seq_along(cluster.names), as.character(cluster.names)) 
  
  kpSamp <- unlist(lapply(names(cluster_split), function(cli) { 
    val <- subSamp[cli] 
    if (!is.na(val)[1]) { 
      kp <- cluster_split[[cli]] 
      kp[sample(length(kp), min(length(kp), val))] 
    } else { 
      integer(0) 
    } 
  })) 
  kpSamp2 <- rep(FALSE, length(cluster.names)) 
  kpSamp2[kpSamp] <- TRUE
  kpSamp2
}

## Load libraries
library(data.table)

##############################################################
## Read in and subsample the metadata files 

human    = fread("HMBA_WB/human_obs_cluster_annotation_filtered.csv")
marmoset = fread("HMBA_WB/marmoset_obs_cluster_annotation_filtered.csv")
macaque  = fread("HMBA_WB/macaque_WB_obs_cluster_annotation_filtered.csv")

# Subsample human to 20 cells/cluster + 80,000 random cells
kp_human1 = subsampleCells(human$cluster,20)
len_human = length(human$cluster)
set.seed(len_human)
kp_human2 = is.element(1:len_human,sample(1:len_human,80000))
kp_human  = kp_human1|kp_human2
sum(kp_human)
# [1] 195084

# Subsample marmoset to 40 cells/cluster + 100,000 random cells
kp_marmoset1 = subsampleCells(marmoset$cluster,40)
len_marmoset = length(marmoset$cluster)
set.seed(len_marmoset)
kp_marmoset2 = is.element(1:len_marmoset,sample(1:len_marmoset,100000))
kp_marmoset  = kp_marmoset1|kp_marmoset2
sum(kp_marmoset)
# [1] 186270

# Subsample macaque to 45 cells/cluster + 110,000 random cells
kp_macaque1 = subsampleCells(macaque$cluster,40)
len_macaque = length(macaque$cluster)
set.seed(len_macaque)
kp_macaque2 = is.element(1:len_macaque,sample(1:len_macaque,110000))
kp_macaque  = kp_macaque1|kp_macaque2
sum(kp_macaque)
# [1] 188238

# Create subsampled data frames
human_sub    <- human[kp_human,]
marmoset_sub <- marmoset[kp_marmoset,]
macaque_sub  <- macaque[kp_macaque,]

# Convert to data.frame
human_sub    <- data.frame(human_sub)
marmoset_sub <- data.frame(marmoset_sub)
macaque_sub  <- data.frame(macaque_sub)
colnames(human_sub) <- colnames(marmoset_sub) <- colnames(macaque_sub) <- colnames(human)


##############################################################
## Read in the cell type file, identify and correct non-uniqueness 

# Read in file
celltype  = fread("HMBA_WB/WB_crossSpecies_cell_types_for_ACE.csv")

# See what isn't unique
nonUnique = names(table(celltype$cell_type))[table(celltype$cell_type)>1]
nonUnique_levels = unique(paste(celltype$source,celltype$level)[is.element(celltype$cell_type,nonUnique)])
nonUnique_levels
# [1] "HMBA_BasalGanglia neighborhood" "HMBA_BasalGanglia class"        "HMBA_BasalGanglia subclass"    
# [4] "HMBA_BasalGanglia group"        "Marmoset_Subcortex class"       "Marmoset_Subcortex group"      
# [7] "Mouse_WB neighborhood"          "Mouse_WB class"                 "Mouse_WB subclass"             
# [10] "Siletti_WB supercluster"        "cross_species_WB neighborhood"  "cross_species_WB class"        
# [13] "cross_species_WB subclass"    

source = setNames(c("_BG", "_sub", "_mWB", "_hWB", "_csHB"),
                  c("HMBA_BasalGanglia", "Marmoset_Subcortex", "Mouse_WB", "Siletti_WB", "cross_species_WB"))

suffix = setNames(c("", "", "_csHB", "_csHB", "_csHB", "", "", "", "", "", "", "_sub", "_sub", "_BG", "_BG", "_BG", "_BG", "_hWB", "", "", "", "", "", "", "", "", ""), colnames(human))

# Replace the relevant items in celltype table
lev = is.element(paste(celltype$source,celltype$level),nonUnique_levels)
celltype$cell_type[lev] <- paste0(celltype$cell_type[lev],source[celltype$source[lev]])

# Now replace all the items in the relevant columns for the human, marmoset, and macaque files

for (cn in colnames(human)) if(suffix[cn]!=""){
  human_sub[,cn]    <- paste0(human_sub[,cn],suffix[cn])
  marmoset_sub[,cn] <- paste0(marmoset_sub[,cn],suffix[cn])
  macaque_sub[,cn]  <- paste0(macaque_sub[,cn],suffix[cn])
}


##############################################################
## Remove unnecessary columns and output gzipped files

# Remove common columns
cn_keep = setdiff(colnames(human),c("V1","cluster_id","species"))
human_sub    <- human_sub[,cn_keep]
marmoset_sub <- marmoset_sub[,cn_keep]
macaque_sub  <- macaque_sub[,cn_keep]

# Remove species columns
human_sub    <- human_sub[,cn_keep!="clustermap_human WB cl:majority vote"]
marmoset_sub <- marmoset_sub[,cn_keep!="clustermap_marmoset WB cl:majority vote"]
macaque_sub  <- macaque_sub[,cn_keep!="clustermap_macaque WB cl:majority vote"]

# Write out all four files to the data directory
fwrite(celltype,"HMBA_WB_celltypes.csv.gz")
fwrite(human_sub,"HMBA_WB_human_cluster_annotation.csv.gz")
fwrite(marmoset_sub,"HMBA_WB_marmoset_cluster_annotation.csv.gz")
fwrite(macaque_sub,"HMBA_WB_macaque_cluster_annotation.csv.gz")

