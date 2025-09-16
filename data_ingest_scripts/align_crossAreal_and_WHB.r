# This file describes how to take the data from the human cross-areal study (Jorstad et al 2023; http://dx.doi.org/10.1126/science.adf6812) and compares annotation for brain region and cell type with annotations from overlapping cells in whole human brain (Siletti et al 2023; https://www.science.org/doi/10.1126/science.add7046).

# Data from both data sets is downloaded from their respective GitHub repositiories
# For cross-areal I downloaded all five "Supercluster: [...]" Data sets from cellxgene (https://cellxgene.cziscience.com/collections/d17249d2-0e6e-4500-abb8-e6c93fa1ac6f) and renamed them sensibly.
# For whole human brain I downloaded the "Neurons.h5ad" and "Nonneurons.h5ad" files from https://github.com/linnarsson-lab/adult-human-brain
# The code below then joins together data based on barcode (to the extent possible) using overlapping cells.


#glia <- read_h5ad("/allen/programs/celltypes/workgroups/hct/cellTaxonomy/adult-human-brain_v1/additional_files/01_2024/Nonneurons.h5ad", backed="r")
#neuron <- read_h5ad("/allen/programs/celltypes/workgroups/hct/cellTaxonomy/adult-human-brain_v1/additional_files/01_2024/Neurons.h5ad", backed="r")

# Use python version of anndata to load it backed 

## Read in the obs metadata
library(data.table)
library(anndata)
library(reticulate)
options(stringsAsFactors=FALSE)
anndata  <- import("anndata")

## First join the non-neuronal tables, since they are smaller
# Read in the data
WHB_NN <- anndata$read_h5ad("/allen/programs/celltypes/workgroups/hct/cellTaxonomy/adult-human-brain_v1/additional_files/01_2024/Nonneurons.h5ad", backed="r")
#WHB_NN <- anndata$read_h5ad("Nonneurons.h5ad", backed = "r")
WHB_NN <- py_to_r(WHB_NN$obs)
CA_NN  <- anndata$read_h5ad("CA_Non_neurons.h5ad", backed = "r")
CA_NN <- py_to_r(CA_NN$obs)

# Cell_ids DO NOT match, even though many of them are the same cells.  Only align cells that I can unambiguously join based on the barcode_donor_region.
# Barcode
ca_bc   <- substr(rownames(CA_NN),1,16)
whb_rn  <- rownames(WHB_NN)
whb_bc  <- substr(whb_rn,nchar(whb_rn)-15,nchar(whb_rn))
# Donor
ca_don  <- as.character(CA_NN$donor_id)
whb_don <- as.character(WHB_NN$donor_id)
# Region
ca_reg  <- as.character(CA_NN$Subregion)
whb_reg <- as.character(WHB_NN$dissection)
whb_reg <- substr(whb_reg,nchar(whb_reg)-2,1000)
# Join them together
ca_bc   <- paste(ca_bc,ca_don,ca_reg,sep="_")
whb_bc  <- paste(whb_bc,whb_don,whb_reg,sep="_")
# See which are unique and intersecting
kp_bc   <- intersect(names(table(ca_bc))[table(ca_bc)==1],names(table(whb_bc))[table(whb_bc)==1])
length(kp_bc)
# [1] 23761

# Define the metadata file
whb_anno_nn <- WHB_NN[match(kp_bc,whb_bc),]
ca_anno_nn  <- CA_NN[match(kp_bc,ca_bc),]
rownames(whb_anno_nn) <- rownames(ca_anno_nn) <- kp_bc


## Next, join the neuronal tables
# Read in the data

WHB_NN <- anndata$read_h5ad("/allen/programs/celltypes/workgroups/hct/cellTaxonomy/adult-human-brain_v1/additional_files/01_2024/Neurons.h5ad", backed="r")
#WHB_NN <- anndata$read_h5ad("Neurons.h5ad", backed = "r")
WHB_NN <- py_to_r(WHB_NN$obs)

CA_NN <- NULL
fns <- c("CA_CGE_neurons.h5ad","CA_MGE_neurons.h5ad","CA_IT_neurons.h5ad","CA_Non_IT_neurons.h5ad")
for (fn in fns){
  print(fn)
  CA_NNin <- anndata$read_h5ad(fn, backed = "r")
  CA_NNin <- py_to_r(CA_NNin$obs)
  CA_NN <- rbind(CA_NN,as.data.frame(as.matrix(CA_NNin)))
}

# Cell_ids DO NOT match, even though many of them are the same cells.  Only align cells that I can unambiguously join based on the barcode_donor_region.
# Barcode
ca_bc   <- substr(rownames(CA_NN),1,16)
whb_rn  <- rownames(WHB_NN)
whb_bc  <- substr(whb_rn,nchar(whb_rn)-15,nchar(whb_rn))
# Donor
ca_don  <- as.character(CA_NN$donor_id)
whb_don <- as.character(WHB_NN$donor_id)
# Region
ca_reg  <- as.character(CA_NN$Subregion)
whb_reg <- as.character(WHB_NN$dissection)
whb_reg <- substr(whb_reg,nchar(whb_reg)-2,1000)
# Join them together
ca_bc   <- paste(ca_bc,ca_don,ca_reg,sep="_")
whb_bc  <- paste(whb_bc,whb_don,whb_reg,sep="_")
# See which are unique and intersecting
kp_bc   <- intersect(names(table(ca_bc))[table(ca_bc)==1],names(table(whb_bc))[table(whb_bc)==1])
length(kp_bc)
# [1] 270397

# Define the metadata file
whb_anno_n <- WHB_NN[match(kp_bc,whb_bc),]
ca_anno_n  <- CA_NN[match(kp_bc,ca_bc),]
rownames(whb_anno_n) <- rownames(ca_anno_n) <- kp_bc

## Now, merge the neuronal and non-neuronal tables
whb_anno <- rbind(as.data.frame(as.matrix(whb_anno_n)),as.data.frame(as.matrix(whb_anno_nn)))
ca_anno  <- rbind(as.data.frame(as.matrix(ca_anno_n)),as.data.frame(as.matrix(ca_anno_nn)))

## Create a single table with only relevant columns, renamed and reformatted
all_anno <- cbind(ca_anno[,c(1:5, 7, 22, 27:28, 30)],whb_anno[,c(14:15, 17:22)])
colnames(all_anno)[colnames(all_anno)=="tissue"] = "brain_region"
colnames(all_anno)[colnames(all_anno)=="development_stage"] = "age"
colnames(all_anno)[colnames(all_anno)=="sample_id"] = "batch"
colnames(all_anno)[colnames(all_anno)=="supercluster_term"] = "WHB_supercluster"
colnames(all_anno)[colnames(all_anno)=="cluster_id"] = "WHB_cluster"
colnames(all_anno)[colnames(all_anno)=="subcluster_id"] = "WHB_subcluster"
all_anno <- all_anno[,c(1:5,16:18,9,6,7,8,10,15,13,14,11,12)]

## Replace WHB_cluster ids with their names
# This is information from columns 1-2 in Table S3 in Siletti et al 2023 (saved locally as "whb_convert.csv")
whb_convert <- read.csv("whb_convert.csv")
all_anno$WHB_cluster <- whb_convert[,2][match(all_anno$WHB_cluster,whb_convert[,1])]

## Convert subcluster to a character vector, by attaching existing id with cluster
all_anno$WHB_subcluster <- paste0(all_anno$WHB_cluster,"_",all_anno$WHB_subcluster)
# NOTE: we have an table matching Table S3 for subclass that was provided through correspondence with Kimberly Siletti that is used for the .

## Add modifiers to cell type columns for disambiguation
all_anno$WHB_supercluster   <- paste0(all_anno$WHB_supercluster,"_WHB")
all_anno$CrossArea_cluster  <- paste0(all_anno$CrossArea_cluster,"_CA_all")
all_anno$WithinArea_cluster <- paste0(all_anno$WithinArea_cluster,"_CA_",ca_anno[,10])

## Rename some subclasses as full names
old <- c("Astro","Endo","Oligo","Micro/PVM")
new <- c("Astrocyte","Endothelial","Oligodendrocyte","Microglia-PVM")
for (i in 1:4){
  all_anno$CrossArea_subclass[all_anno$CrossArea_subclass==old[i]]   = new[i]
  all_anno$WithinArea_subclass[all_anno$WithinArea_subclass==old[i]] = new[i]
}

## Subsample to include 100,000 cells + at least 50 per within-species cluster
set.seed(42)
kp <- (sample(1:dim(all_anno)[1],dim(all_anno)[1])<=100000)|(scrattch.taxonomy::subsampleCells(all_anno$WithinArea_cluster,50))
all_anno <- all_anno[kp,]
dim(all_anno)
# [1] 104737     18

# Write out the results
fwrite(all_anno,"CAvsWHB_cell_info.csv.gz")

table(ca_anno[,10])
#   A1C    M1C    MTG    S1C    V1C 
# 32037 111830  80236  37332  32723 
regs <- c("A1C","M1C","MTG","S1C","V1C")

# We note that only 5 of the brain regions from the cross-areal study have cells overlapping with the WHB study and therefore only these regions are included in the annotation table.


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# Start with the donor metadata file used for MTG and patch-seq studies (mtg_whb_annotations.csv.gz), and ADD addition columns corresponding to ordered cross-areal cell types from Jorstad et al 2023 (https://www.science.org/doi/10.1126/science.adf6812).  Since I cannot find actual cluster orders in that paper for the within region cell types, we'll sort first by subclass order, and then alphabetically within subclass

so <- scan("subclass_order.txt",what="character",sep="\n")  # This is the subclass order
out_ca <- all_anno[!duplicated(all_anno$WithinArea_cluster),c("WithinArea_cluster","WithinArea_subclass","brain_region")]
out_ca$WithinArea_subclass <- factor(out_ca$WithinArea_subclass,levels=so)
num_ord <- as.numeric(as.character(lapply(out_ca$WithinArea_cluster,function(x) strsplit(x,"_")[[1]][2])))
out_ca <- out_ca[order(out_ca[,3],out_ca[,2],num_ord),]
out_ca$brain_region <- paste("Brain region:",out_ca$brain_region)
write.csv(out_ca,"cell_type_info.csv")

# For the cross-areal types, we use the the order in figure 5B, copied from the pdf into "cluster_order.txt".  It is worth noting that in this study the glutamatergic neurons come before GABAergic, whereas in most other studies GABAergic comes first.  This decision is arbitrary.  Note that this step is done manually in the table.

