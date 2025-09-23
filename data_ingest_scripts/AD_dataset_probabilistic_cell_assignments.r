##################################################################
## This script describes how we went from a series of mapping and clustering files collected from all 10 data sets mapping to the SEA-AT MTG taxonomy along with the SEA-AD data itself, to the cell and cell annotation files presented on ACE under "Alzheimer's disease (SEA-AD vs. community)".


##################################################################
## PREREQUISITES
# Prior to running this script, the following preparations happened:
# 1) Collect all of the data from the 10 external studies, reprocess from fastq files, and align with SEA-AD data. Details of this process and associated scientific outcomes are described in Gabitto, Travaglini et al 2024 (https://doi.org/10.1038/s41593-024-01774-5).  Scripts for mapping the external data using iterative scANVI can be found here: https://github.com/AllenInstitute/SEA-AD_2024/blob/main/Single%20nucleus%20omics/01_Mapping%20and%20Quality%20Control/RNAseq/02_Public%20data%20mapping.ipynb
# NOTE: The outcome of this step was a set of files called [study_name]-cell-annotation.2024-03-27.csv which includes the outputs of the mappings organized by study, and which can be downloaded from https://sea-ad-single-cell-profiling.s3.amazonaws.com/index.html#PFC/RNAseq/public_datasets/
# 2a) Perform basic QC on the external data sets (where such QC is not provided) and organize the data by cell type from the original study. This is done in AD_filter_and_basic_cell_type_labels.py
# 2b) Collect all of the original cluster assignments from the manuscripts, perform QC, rename cells in cases where different files didn't agree, and output all of the data and meta data into files, which are now stored on AWS.  This was also done as part of the SEA-AD project and is available here: https://github.com/AllenInstitute/SEA-AD_2024/blob/main/Single%20nucleus%20omics/01_Mapping%20and%20Quality%20Control/RNAseq/03_Public%20data%20QC.ipynb
# NOTE: for this code we used script in 2a, and ended up with a set of folders in "VilasTain_cellAssignments" corresponding to cell type assigments of each cell passing QC from each broad cell type in each study.  Code 2a is a more recent (and easier to follow) version of this, which also indicates where each file from each study originated from.  We are not allowed to share many of the starting files due to then being in controlled-access repositories.
# 3) Get the SEA-AD cell type annotations ("SEA-AD-cell-annotation.2024-08-27.csv") from https://sea-ad-single-cell-profiling.s3.amazonaws.com/index.html#PFC/RNAseq/public_datasets/ and save it in the current working directory
# 4) Compile this, as well as some minor translation tables in a starting folder. Again we note that we are unfortunately unable to share these startinf files publicly.


##################################################################
## SET UP THE ENVIRONMENT

## Load libraries
library(data.table)
library(mfishtools)
library(dplyr)
library(Seurat)

## Read in the mapping results and original cell type assignments from each data set
mapFolder     <- "[location_of_mapping_files]"  # This is the folder location for the results of mapping external data to SEA-AD taxonomy that were downloaded from https://sea-ad-single-cell-profiling.s3.amazonaws.com/index.html#PFC/RNAseq/public_datasets/ (#1 above)
clusterFolder <- "[location_of_initial_cell_type_files]"  # This is the folder location for the files holding original cell type assignments of external studies from their original publications (#2a above)
studies       <- read.csv("AD_study_names.csv",row.names=1)  # This table includes links between how studies are named in different file structures

SEAAD_annotations <- "SEA-AD-cell-annotation.2024-08-27.csv"  # This is the file name for SEA-AD cell type assignments, which is read in below


##################################################################
## ALIGN MAPPING AND CELL TYPE ASSIGNMENTS

clusters  <- maps <- list()
studyNames <- rownames(studies)

for (nm in studyNames){
  print(nm)
  clTmp  <- NULL
  folder <- paste0(clusterFolder,studies[nm,"Tain"])
  for (ty in dir(folder))
    clTmp <- rbind(clTmp, fread(file.path(folder,ty,"cell_type_label.tsv.gz"))) # cellid [others]
  maps[[nm]] <- fread(file.path(mapFolder,paste0(studies[nm,"Kyle"],"-cell-annotation.2024-03-27.csv")),header=TRUE) # V1  pred  true 
  clusters[[nm]] <- clTmp
  
  # FIX #1  # Kyle has since fixed this one in his file
  if(nm=="Lau_2020") clTmp$cellid <- paste0(substr(clTmp$cellid,1,17),clTmp$donor)
  # FIX #2
  if(nm=="Mathys_2023") maps[[nm]]$`Original Cell ID` <- maps[[nm]]$`Original Cell ID Alternative`
  # FIX #3: ## NOTE: FOR CAIN 2023, I need to add + to each of the cell type numbers in Tain's files (e.g., Ast.0 --> Ast.1)
  if(nm=="Cain_2023"){
    clust <- clTmp$cell_type
	clust[clust=="no.clus"] = "nocluster.0"
    clust <- lapply(clust,function(x) strsplit(x,"\\.")[[1]])
	clust <- matrix(unlist(clust), ncol = 2, byrow = TRUE)
	clust[,2] <- as.numeric(clust[,2])+1
	clust <- as.character(apply(clust,1,function(x) paste(x[1],x[2],sep=".")))
	clust[clust=="nocluster.0"] = "no_cluster"
	clTmp$cell_type <- clust
  }
  # FIX #4: FOR Yang 2022, I need to update the barcodes
  if(nm=="Yang_2022"){
    old = c("NCI 1", "NCI 6", "NCI 7", "NCI 8", "AD 4", "AD 5", "AD 6", "AD 7")
	new = paste0("1_",17:24)
    for (i in 1:8){
	  kp = substr(maps[[nm]]$`Original Cell ID`,18,100)==old[i]
	  maps[[nm]]$`Original Cell ID`[kp] = gsub(old[i],new[i],maps[[nm]]$`Original Cell ID`[kp])
	}
  }
  # FIX #5: For Green 2023, there is an updated set of cell annotations from the paper
  if(nm=="Green_2023"){
    tmp <- fread("Green_updated_cell_annotations.csv",header=TRUE)
	tmp <- tmp[,c("cell","state","cell.type","individualID")]
	colnames(tmp) <- c("cellid","cell_type","broad","individualID")
	clTmp <- tmp
  }
  
  maps[[nm]] <- maps[[nm]][maps[[nm]]$Supertype!="",]
  common     <- intersect(maps[[nm]]$`Original Cell ID`, clTmp$cellid)
  print(paste(nm,"-",length(common)))
  
  clusters[[nm]] <- clTmp[match(common,clTmp$cellid),]
  colnames(clusters[[nm]])[2:dim(clusters[[nm]])[2]] <- paste(colnames(clusters[[nm]])[2:dim(clusters[[nm]])[2]],studies[nm,"Suffix"],sep="_")
  clusters[[nm]] <- as.data.frame(clusters[[nm]])
  maps[[nm]]     <- maps[[nm]][match(common,maps[[nm]]$`Original Cell ID`),]
}
# The output of this code chunk is a set of 'clusters' and a set of 'maps' for each study that include cells passing QC on all fronts (e.g., the set of cells that were assigned a cell type in the original study and that also were also mapped to SEA-AD in Gabitto, Travaglini, et al 2024). 


##################################################
## PROBABILISTIC CELL TYPE ASSIGNMENTS

# For each SEA-AD cell, probabilistically assign a high resolution cell type (and then infer higher levels) from the other studies.  

# NOTE: This is a critical difference between how this cell annotation table is created as compared to every other table in ACE.  ACE requires that all rows correpond to the SAME cells; however, these cells come from multiple studies.  To directly compare cells across all of these studies, we create probablistic cell type assignments for each SEA-AD cell based on the confusion matrix build using mapping and clustering results for cells from each external study. For example, if 70% of the cells in Study #1 mapping to "Sst 1" are assigned to "GABA 3" and 30% are assigned to "GABA 7", then each cell in SEA-AD assigned to "Sst 1" has a 70% chance of being assigned the label "GABA 3" and a 30% chance of being assigned the label "GABA 7".  This is repeated for each cell type in each study. 

print("Create probablistic cell mapping file.")

## Read in the cell type assignments for actual SEA-AD cells
seaadIn <- fread(SEAAD_annotations)
seaadIn <- as.data.frame(seaadIn[,c("Supertype","Subclass","Class")])
colnames(seaadIn) <- c("SEAAD_Supertype","SEAAD_Subclass","SEAAD_Class")
seaadIn <- seaadIn[seaadIn$SEAAD_Supertype!="",]

## Define a downsampled data set for baseline
## -- roughly retain correct proportions, but ensure at least ~60 cells per supertype
set.seed(42)
kpSEAAD <- is.element(1:dim(seaadIn)[1],sample(1:dim(seaadIn)[1],200000))
kpSEAAD <- subsampleCells(seaadIn$SEAAD_Supertype,50)|kpSEAAD
seaad <- seaadIn[kpSEAAD,]

## Now Assign seaad cells as cell types from this data set probabilistically 
for (nm in studyNames){
  print(nm)
  set.seed(42+which(studyNames==nm))  # for reproducibility
  probs <- table(maps[[nm]]$Supertype, clusters[[nm]][,paste0("cell_type_",studies[nm,"Suffix"])])
  probs <- probs/rowSums(probs)
  val <- rep("noMappedCells",dim(seaad)[1])
  names(val) <- rownames(probs)
  for(ct in rownames(probs)){
    kp <- seaad$SEAAD_Supertype==ct
    val[kp] <- sample(colnames(probs), prob = probs[ct,], size = sum(kp), replace = TRUE)
  }
  out <- data.frame(val, clusters[[nm]][,paste0("broad_",studies[nm,"Suffix"])][match(val,clusters[[nm]][,paste0("cell_type_",studies[nm,"Suffix"])])])
  colnames(out) <- paste(nm,c("celltype","broadtype"),sep="_")
  out[is.na(out[,2]),2] <- out[is.na(out[,2]),1]
  out[,1] <- paste(out[,1],studies[nm,"Suffix"],sep="_") # Add suffix
  out[,2] <- paste(out[,2],studies[nm,"Suffix"],sep="_")
  seaad <- cbind(seaad,out)
}

## Output the resulting file
fwrite(seaad,"DLPFC_SEAAD_cell_annotations_for_app.csv.gz")


###############################################################################################
## We end by creating the cell annotation table.  This is largely done manually (see below)  ##
###############################################################################################

# The cell type annotation table was created by joining results from all 11 studies in the same file (with the appended Suffixes listed in AD_study_names.csv included in the "cell_type" column for disambiguation).  The same process was performed for each of the papers:

# The table consists of the following columns:
# * cell_type: cell type name used in the study (with relevant suffix)
# * level: cell type level of resolution (e.g., broad, subclass, class, cluster) 
# * study: Which study was it from (SEA-AD, or [first author]_[year] of study)
# * direction: 'up'/'down'/'unchanged' with disease (see below for more details), OR 'not_assessed' if the cell type was included in the study but did not report whether or not there were changes with disease. In the UI, "Not reported" appears for cell types defined by us (typically very broad types not explictly discribed in the paper).
# * description: A longer name, with potentially more information about the cell type
# * notes: In cases where we indicate up/down with AD, we often use this row to indicate where in the paper we found the reported change in abundance in disease, or in some cases why we said "unchanged".

# Changes with disease were determined by reading the paper and recording, based on text and/or figures, cell types that the manuscript authors indicated were changed with disease.  For example, in Lau_2020, cell type 'a1' (called 'a1_L20') was up with AD because: "fig4; "Only subpopulations a1, a3,and a6 expressed DEGs; specifically, subpopulations a1 and a6 wereenriched with up-regulated signature genes, whereas subpopulation a3 was enriched in down-regulated signature genes (SI Appendix,Fig. S5B). Therefore, we categorized a1 and a6 as theÂ“AD-up-regulatedÂ”subpopulations and a3 as theÂ“AD-down-regulatedÂ”subpopulation"".  For nearly all cases, we partially define increase and decrease with disease baesd on abundance changes, but in this case, we define it using gene expression because the authors do.
