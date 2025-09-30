## This script downloads files from the mouse study of hippocampus + neocortex (Yao et al 2021a; https://doi.org/10.1016/j.cell.2021.04.021), which include comparisons with previous studies of mouse motor (Yao et al 2021b; https://doi.org/10.1038/s41586-021-03500-8) cortex and primary visual cortex/ALM (Tasic et al 2018; https://doi.org/10.1038/s41586-018-0654-5). 

# More specifically, this script starts from supplemental materials in Yao et al 2021 availabe on NeMO here: 
# 1. https://data.nemoarchive.org/biccn/grant/u19_zeng/zeng/transcriptome/scell/SSv4/mouse/processed/YaoHippo2020/
#  - CTX_Hip_anno_SSv4.csv.tar = The annotation information 
# These files are unzipped in the current directory prior to running the script 


##################################################
## READ IN AND ORGANIZE THE ANNOTATIONS

## Read in annotations (#1 above)
anno_all <- read.csv("CTX_Hip_anno_SSv4.csv")

## Subset to the relevant columns
kpCl <- intersect(colnames(anno_all),sort(colnames(anno_all))[c(5:11,22:27,33:39,43:57)])
kpCl <- kpCl[c(1,21:35,3:20)]
anno <- anno_all[,kpCl]

## Rename some of the columns
cn <- colnames(anno)
cn <- gsub("Miniatlas","M1_miniatlas",cn)
cn <- gsub("tasic18","VISp_tasic18",cn)
cn[2:16] <- paste0("CtxHip_",cn[2:16])
colnames(anno) <- cn

## Omit cells not found in M1 or VISp
kpSamp <- !((anno$VISp_tasic18_cluster_label=="absent")&(anno$M1_miniatlas_cluster_label=="absent"))
anno <- anno[kpSamp,]


##################################################
## Output the resulting cell annotation table

write.csv(anno,"CTX_Hip_cellInfoM1VISp.csv",row.names=FALSE)



##################################################
## CELL TYPE ANNOTATION file
##################################################

# The cell type annotation file contains the following:
# The first 392 rows are a flattened and subset version of Table S3 from Yao et al 2021a above, showing the ordering and some relevant information about the CTX+HIP cell types
# Row 393-544: included for the order and color of other taxonomy levels (and are also taken from the same supplemental table)

# The remaining rows are the labels and colors for the cell types in M1 and VISp, derived from the above anno_all file as follows

tasic <- anno_all[,20:22]
tasic <- tasic[match(sort(unique(tasic[,1])),tasic[,1]),]
m1    <- anno_all[,29:31]
m1    <- m1[match(sort(unique(m1[,1])),m1[,1]),]
colnames(m1) <- colnames(tasic) <- c("order","cell type","color")
tasic$name <- "VISp_tasic18_cluster"
m1$name <- "M1_miniatlas_cluster"

out   <- rbind(tasic,m1)
out$order <- paste0("Order: ",as.character(out$order))
write.csv(out[,c(2,3,1,4)],"historic_cell_types.csv",row.names=FALSE)
