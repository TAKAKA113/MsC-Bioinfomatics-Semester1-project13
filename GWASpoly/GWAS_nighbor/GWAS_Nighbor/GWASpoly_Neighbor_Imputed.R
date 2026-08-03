getwd()
setwd("/rds/homes/t/txk567/Project13_impute_Ela")

# 1. ファイル読み込み
GD <- read.csv("GD_final_imputed.csv")
GM <- read.csv("GM (1).csv")

# 確認
dim(GD)
dim(GM)
head(GD[,1:6])

# 欠損値確認
sum(is.na(GD[,-1]))

###########

# GWASpoly用ファイル作成
geno_GWAS <- data.frame(
  Marker = GM$SNP,
  Chrom = GM$Chromosome,
  Position = GM$Position,
  t(GD[,-1])
)
colnames(geno_GWAS)[4:ncol(geno_GWAS)] <- GD$Taxa

# 表現型ファイル作成
pheno <- read.csv("estimate_phenotype.csv")　#Danielaの表現型ファイル
colnames(pheno) <- gsub(" ", "_", colnames(pheno))  # スペースをアンダースコアに
pheno_GWAS <- pheno[pheno$Taxa %in% GD$Taxa, ]

# 確認-リストの数が一致するか確認
dim(geno_GWAS)　#遺伝子型の個体名リスト＝＞１８４
dim(pheno_GWAS)　#表現型の個体名リスト＝＞１８４
sum(pheno$Taxa %in% GD$Taxa)#GDは１８５個あったので1つ不一致

# 保存
write.csv(geno_GWAS, "geno_GWAS_1.csv", row.names = FALSE)
write.csv(pheno_GWAS, "pheno_GWAS_1.csv", row.names = FALSE)

########

# GWASpoly読み込み
library(GWASpoly)

##小麦は６倍体だが、DartRでは２倍体形式で出力されるのでploidy=2
table(as.matrix(GD[,-1]))

data1 <- read.GWASpoly(
  ploidy = 2,
  pheno.file = "pheno_GWAS_1.csv",
  geno.file = "geno_GWAS_1.csv",
  format = "numeric",
  n.traits = 14,
  delim = ","
)



#######
#集団構造ーmembership
# genlight作成
library(adegenet)

geno_for_gl <- as.matrix(GD[,-1])
rownames(geno_for_gl) <- GD$Taxa

gl <- new("genlight", geno_for_gl)
indNames(gl) <- GD$Taxa

# クラスター数決定（プロットなし）
grp <- find.clusters(gl, max.n.clust = 10, n.pca = 100, choose.n.clust = FALSE)


# BIC確認
grp$Kstat.    #k=4

# BICプロット保存
pdf("BIC_plot_member1.pdf", width = 8, height = 6)
plot(grp$Kstat, type = "b", pch = 19, xlab = "K", ylab = "BIC", main = "Optimal K by BIC")
dev.off()

###########

#DAPCA
#クラスターが重なっている → Q行列の効果が弱い
#クラスターが明確に分離 → Q行列が集団構造を適切に補正
#Membershipを特徴量に入れる根拠になる

# K=4でクラスター決定
grp <- find.clusters(gl, max.n.clust = 10, n.pca = 100, n.clust = 4)

# DAPC実行
dapc1 <- dapc(gl, grp$grp, n.pca = 20, n.da = 3)

# membership取得
membership <- dapc1$posterior

# 確認
dim(membership)
head(membership)

# プロット保存
pdf("DAPC_scatter_member1.pdf", width = 8, height = 6)
scatter(dapc1, col = c("darkblue", "purple", "green", "orange"))
dev.off()

pdf("DAPC_compoplot_member1.pdf", width = 10, height = 6)
compoplot(dapc1, col = c("darkblue", "purple", "green", "orange"), subset = order(dapc1$grp))
dev.off()

# DAPC保存
saveRDS(dapc1, "dapc1_member1.rds")

###########
#Membershipを特徴量として追加
# membership行名確認
rownames(membership)
#DAPCAをするためgenlightをGD,GMから作成したのでmembershipは185列あるため数を一致させる
# pheno_GWASの個体と一致させる
membership_df <- as.data.frame(membership)
membership_df$Taxa <- rownames(membership)

# pheno_GWASの個体だけ抽出
membership_matched <- membership_df[membership_df$Taxa %in% pheno_GWAS$Taxa, ]

# 順序を合わせる
membership_matched <- membership_matched[match(pheno_GWAS$Taxa, membership_matched$Taxa), ]

# Q行列追加（Q4除外）
pheno_GWAS$Q1 <- membership_matched[,1]
pheno_GWAS$Q2 <- membership_matched[,2]
pheno_GWAS$Q3 <- membership_matched[,3]

# 確認
dim(pheno_GWAS)
head(pheno_GWAS)

# 保存
write.csv(pheno_GWAS, "pheno_GWAS_1.csv", row.names = FALSE)



##################
# GWASpoly読み込み
data1 <- read.GWASpoly(
  ploidy = 2,
  pheno.file = "pheno_GWAS_1.csv",
  geno.file = "geno_GWAS_1.csv",
  format = "numeric",
  n.traits = 14,
  delim = ","
)

# K行列計算
data1 <- set.K(data1, LOCO = TRUE, n.core = 4)

# パラメータ設定
N <- 184
params <- set.params(geno.freq = 1 - 5/N)

# GWAS実行
data.scan1 <- GWASpoly(
  data = data1,
  models = c("additive"),
  traits = c("TGW", "Area", "Width", "Length", "Min.Width", "Min.Length",
             "Max.Width", "Max.Length", "DTH", "DTM", "PHT", "SPNO", "SPL", "TKW"),
  params = params,
  n.core = 4
)

# 中間保存
saveRDS(data.scan1, "data_scan1_member1.rds")

# 閾値設定・QTL抽出
data.thresh1 <- set.threshold(data.scan1, method = "Bonferroni", level = 0.05)
qtl1 <- get.QTL(data.thresh1)
print(qtl1)

# 保存
write.csv(qtl1, "QTL_results_member1.csv", row.names = FALSE)


############
# QQ plot保存
pdf("qq_member1.pdf", width = 6, height = 6)
qq.plot(data.scan1, trait = "DTH", model = "additive")
qq.plot(data.scan1, trait = "PHT", model = "additive")
dev.off()

# Manhattan plot保存
pdf("manhattan_member1.pdf", width = 12, height = 6)
manhattan.plot(data.thresh1, trait = "DTH", model = "additive")
manhattan.plot(data.thresh1, trait = "PHT", model = "additive")
dev.off()

# QTL抽出（DTHとPHTのみ）
qtl_DTH <- qtl1[qtl1$Trait == "DTH", ]
qtl_PHT <- qtl1[qtl1$Trait == "PHT", ]

# R²計算
# R2 for DTH
qtl_DTH <- subset(qtl1, Trait == "DTH", select = c("Marker", "Model"))
fit_DTH <- fit.QTL(data = data.thresh1, trait = "DTH", qtl = qtl_DTH)
print(fit_DTH)
write.csv(fit_DTH, "QTL_R2_DTH.csv", row.names = FALSE)

# R2 for PHT
qtl_PHT <- subset(qtl1, Trait == "PHT", select = c("Marker", "Model"))
fit_PHT <- fit.QTL(data = data.thresh1, trait = "PHT", qtl = qtl_PHT)
print(fit_PHT)
write.csv(fit_PHT, "QTL_R2_PHT.csv", row.names = FALSE)
