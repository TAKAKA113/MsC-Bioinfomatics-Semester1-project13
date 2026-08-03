
getwd()
setwd("/rds/homes/t/txk567/project13_imputed_Dani")


# Read input files
GD <- read.csv("GD_final (1).csv")
GM <- read.csv("GM (1).csv")
DAPC_cov <- read.csv("DAPC_membership_covariates.csv")

# Check dimensions
dim(GD)
dim(GM)
dim(DAPC_cov)

# Preview data
head(GD[,1:6])
head(GM)
head(DAPC_cov)

# Check missing values (should be 0 if imputed)
sum(is.na(GD[,-1]))

########

# Imputation (mode method), this is same way as Danielle
geno_matrix <- as.matrix(GD[,-1])

impute_mode <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode_val
  return(x)
}

geno_imputed <- apply(geno_matrix, 2, impute_mode)

# Confirm no missing values
sum(is.na(geno_imputed))

# Create imputed GD
GD_imputed <- data.frame(Taxa = GD$Taxa, geno_imputed)

# Check
dim(GD_imputed)
head(GD_imputed[,1:6])


###########Done imputation
#項目	           GAPIT	                 GWASpoly
#遺伝子型	   GD（個体×SNP）  	    geno（SNP×個体 + Map情報）
#マップ	    GM（別ファイル）          	geno内に含む
#表現型	     Y（別ファイル）	        pheno（別ファイル）

# Create GWASpoly genotype file
geno_GWAS <- data.frame(
  Marker = GM$SNP,
  Chrom = GM$Chromosome,
  Position = GM$Position,
  t(GD_imputed[,-1])
)
colnames(geno_GWAS)[4:ncol(geno_GWAS)] <- GD_imputed$Taxa

# Load phenotype file
pheno <- read.csv("estimate_phenotype.csv")
colnames(pheno) <- gsub(" ", "_", colnames(pheno))
pheno_GWAS <- pheno[pheno$Taxa %in% GD_imputed$Taxa, ]

# Check matching
dim(geno_GWAS)
dim(pheno_GWAS)
sum(pheno$Taxa %in% GD_imputed$Taxa)

##############Made the geno and pheno dataframe for GWASpoly
#Conduct DAPCA for membership
# Create genlight from imputed GD
library(adegenet)

geno_for_gl <- as.matrix(GD_imputed[,-1])
rownames(geno_for_gl) <- GD_imputed$Taxa

gl <- new("genlight", geno_for_gl)
indNames(gl) <- GD_imputed$Taxa

# Find optimal K
grp <- find.clusters(gl, max.n.clust = 10, n.pca = 100, choose.n.clust = FALSE)
grp$Kstat

# Save BIC plot
pdf("BIC_plot_member2.pdf", width = 8, height = 6)
plot(grp$Kstat, type = "b", pch = 19, xlab = "K", ylab = "BIC", main = "Optimal K by BIC")
dev.off()


#pdf -> plot -> dev.offはセットで覚える



# Run DAPC with K=4
grp <- find.clusters(gl, max.n.clust = 10, n.pca = 100, n.clust = 4)

# DAPC
dapc1 <- dapc(gl, grp$grp, n.pca = 20, n.da = 3)

# Get membership
membership <- dapc1$posterior
dim(membership)
head(membership)

# Save DAPC plots
pdf("DAPC_scatter_member2.pdf", width = 8, height = 6)
scatter(dapc1, col = c("darkblue", "purple", "green", "orange"))
dev.off()

pdf("DAPC_compoplot_member2.pdf", width = 10, height = 6)
compoplot(dapc1, col = c("darkblue", "purple", "green", "orange"), subset = order(dapc1$grp))
dev.off()

# Save DAPC object
saveRDS(dapc1, "dapc1_member2.rds")

# Match membership to pheno_GWAS
membership_df <- as.data.frame(membership)
membership_df$Taxa <- rownames(membership)
membership_matched <- membership_df[membership_df$Taxa %in% pheno_GWAS$Taxa, ]
membership_matched <- membership_matched[match(pheno_GWAS$Taxa, membership_matched$Taxa), ]

# Add Q matrix (exclude Q4)
pheno_GWAS$Q1 <- membership_matched[,1]
pheno_GWAS$Q2 <- membership_matched[,2]
pheno_GWAS$Q3 <- membership_matched[,3]

# Check and save
dim(pheno_GWAS)
head(pheno_GWAS)

# Save files
write.csv(geno_GWAS, "geno_GWAS_2.csv", row.names = FALSE)
write.csv(pheno_GWAS, "pheno_GWAS_2.csv", row.names = FALSE)

#############Done making Dataframe for Gapit

#GWASpoly

# Load GWASpoly
library(GWASpoly)

data2 <- read.GWASpoly(
  ploidy = 2,
  pheno.file = "pheno_GWAS_2.csv",
  geno.file = "geno_GWAS_2.csv",
  format = "numeric",
  n.traits = 14,
  delim = ","
)

# Set K matrix (LOCO)
data2 <- set.K(data2, LOCO = TRUE, n.core = 4)

# Set parameters
N <- 184
params <- set.params(geno.freq = 1 - 5/N)

# Run GWAS
data.scan2 <- GWASpoly(
  data = data2,
  models = c("additive"),
  traits = c("TGW", "Area", "Width", "Length", "Min.Width", "Min.Length",
             "Max.Width", "Max.Length", "DTH", "DTM", "PHT", "SPNO", "SPL", "TKW"),
  params = params,
  n.core = 4
)

# Save intermediate
saveRDS(data.scan2, "data_scan2_member2.rds")

# Set threshold and extract QTL
data.thresh2 <- set.threshold(data.scan2, method = "Bonferroni", level = 0.05)
qtl2 <- get.QTL(data.thresh2)
print(qtl2)

# Save results
write.csv(qtl2, "QTL_results_member2.csv", row.names = FALSE)

############

# QQ plot
pdf("qq_member2.pdf", width = 6, height = 6)
qq.plot(data.scan2, trait = "DTH", model = "additive")
dev.off()

# Manhattan plot
pdf("manhattan_member2.pdf", width = 12, height = 6)
manhattan.plot(data.thresh2, trait = "DTH", model = "additive")
dev.off()

# R² for DTH
qtl_DTH <- subset(qtl2, Trait == "DTH", select = c("Marker", "Model"))
fit_DTH <- fit.QTL(data = data.thresh2, trait = "DTH", qtl = qtl_DTH)
print(fit_DTH)
write.csv(fit_DTH, "QTL_R2_DTH_member2.csv", row.names = FALSE)

# Check files
list.files(pattern = "member2")
