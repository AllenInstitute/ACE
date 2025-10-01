# This script maps data from the paper “Conserved cell types with divergent features in human versus mouse cortex” (Hodge, Bakken, et al 2019; https://doi.org/10.1038/s41586-019-1506-7) against the taxonomy derived and presented in “Integrated multimodal cell atlas of Alzheimer’s disease” (Gabitto, Travaglini, et al 2024; https://doi.org/10.1101/2023.05.08.539485) using MapMyCells.  MapMyCells enables mapping of single cell and spatial trancriptomics data sets to (in this case) a human middle temporal gyrus (MTG) taxonomy.

# All of the data from Hodge et al 2019 has been on Allen Brain Map ('human_MTG_gene_expression_matrices_2018-06-14.zip' from https://portal.brain-map.org/atlases-and-data/rnaseq/human-mtg-smart-seq).  We download this file and upzip in our working directory

# -- For the Hodge et al 2019 study, the t-SNE coordinates were downloaded from https://raw.githubusercontent.com/AllenInstitute/AllenInstituteTaxonomy/refs/heads/main/conversion_data/Human_MTG_SMART_seq_04042025_tsne.csv
# -- For the Hodge et al 2019 study, the concept of "subclass" was developed post publication, but we want to include them in this ACE table.  This information (along with the cluster order and color, for convenience) we save alongide the scripts in a file called "hodge_taxonomy.csv", which we read in below in generating this ACE table.
# -- For the SEA-AD study, the cluster colors and order are downloaded from here (https://sea-ad-single-cell-profiling.s3.amazonaws.com/index.html#MTG/RNAseq/Supplementary%20Information/), from the file 'cluster_order_and_colors.csv' file, which we also use herein.


######################################################################################
# ========== Download and read in the reference taxonomy from Allen Brain Map ========

## Load libraries
library(scrattch.taxonomy)  # I really only need data.table, anndata

## Read in the data
exons    <- as.matrix(fread("human_MTG_2018-06-14_exon-matrix.csv"),rownames=1)
introns  <- as.matrix(fread("human_MTG_2018-06-14_intron-matrix.csv"),rownames=1)
geneInfo <- read.csv("human_MTG_2018-06-14_genes-rows.csv",row.names=1)
sampInfo <- read.csv("human_MTG_2018-06-14_samples-columns.csv",row.names=1)

## Identify cells with no class. These failed QC.
kp <- sampInfo$cluster!="no class"
sampInfo <- sampInfo[kp,]

## Format and subset counts
taxonomy.counts <- introns + exons
rownames(taxonomy.counts) <- rownames(geneInfo)
taxonomy.counts <- taxonomy.counts[,kp]  # Omit cells from outlier clusters as above

## Convert to h5ad and output for MapMyCells GUI
expr <- t(taxonomy.counts )
expr <- as(expr, "dgCMatrix")
rownames(expr) <- rownames(sampInfo)
colnames(expr) <- rownames(geneInfo)

anndata <- AnnData(X = expr, var = data.frame(gene = colnames(expr)), obs = sampInfo)
anndata$var_names <- rownames(geneInfo)
anndata$obs_names <- rownames(sampInfo)
write_h5ad(anndata, "hodge.h5ad", compression="gzip")			 


########################################################################
## Break to run these data in the MapMyCells GUI. Results are         ##
##  downloaded and the csv file is renamed as "hodge_mapping.csv"     ##
########################################################################  


########################################################################
## Read back in the mapping, and join with colors, TSNE coordinates and original clustering information

## Read in and rename mapping columns
mapping <- read.csv("hodge_mapping.csv",row.names=1,skip=3)
mapping <- mapping[,c(8,9,5,6)]
colnames(mapping) <- c("SEAAD_supertype_label","SEAAD_confidence","SEAAD_subclass","SEAAD_class")

## Add colors and ids (order) for SEAAD taxonomy
seaad <- read.csv("cluster_order_and_colors.csv")
cols  <- seaad[match(mapping$SEAAD_supertype_label,seaad[,1]),2:3]
colnames(cols) <- c("SEAAD_supertype_color","SEAAD_supertype_id")
mapping <- cbind(mapping,cols)

## Read in the Hodge subclass values and cluster color/order
hodge_taxonomy <- read.csv("hodge_taxonomy.csv")
clustering     <- hodge_taxonomy[match(sampInfo$cluster,hodge_taxonomy$Hodge_cluster_label),]

## Subset and rename the columns we want from the sampleInfo file
sampSubset <- sampInfo[,c("donor","brain_subregion","sex","genes_detected_cpm_criterion")]
colnames(sampSubset) <- c("donor","MTG_layer","biological_sex","genes_detected")

## Read in and subset the t-SNE coordinates
tsne <- read.csv("Human_MTG_SMART_seq_04042025_tsne.csv",row.names=1)
tsne <- tsne[match((sampInfo$seq_name),rownames(tsne)),]  # Error correction 8/13/25
rownames(tsne)    <- rownames(sampInfo)
tsne[is.na(tsne)] <- 0   # Set the missing values for 337 cells to 0


########################################################################
## Outpute the file!

metadata_out <- cbind(mapping, clustering, sampSubset, tsne)
fwrite(metadata_out,"hodge_to_seaad.csv.gz")
