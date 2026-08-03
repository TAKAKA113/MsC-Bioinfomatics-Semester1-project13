# ============================================================
# 06_BLUE_visualisation.R
# Correlation heatmap, PCA, and temporal trends
# ============================================================

#表のNAの数を確認、NAが多いとデータが歪む
colSums(is.na(blue_all))

####pheatmap  ヒートマップより綺麗にかけるpheatmap
install.packages("pheatmap")
library(dplyr)
library(ggplot2)
library(tidyr)
library(pheatmap)
# --- 形質の列だけ選ぶ ---
blue_traits <- blue_all %>%
  select(-Genotype, -YearOfRelease)

# --- 相関行列を計算 ---
cor_matrix <- cor(blue_traits, use = "pairwise.complete.obs")

# --- ヒートマップを描く ---
pheatmap(
  cor_matrix,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlation heatmap of BLUEs (all traits)",
  border_color = NA
)


#リリースと年代の関係
#まずは練習でSPNOだけ
ggplot(blue_all, aes(x = YearOfRelease, y = SPNO)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SPNO vs Year of Release")


#全体の関係を見る

#1.データを縦型に変換
#横型でも翔が14回書かなければいけない
#縦型だとTraitが列になるので一度で読める
blue_long <- blue_all %>%
  pivot_longer(
    cols = -c(Genotype, YearOfRelease),
    names_to = "Trait",
    values_to = "BLUE"
  )

#2.グラフを書く
ggplot(blue_long, aes(x = YearOfRelease, y = BLUE)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "Temporal trends of BLUEs")


#3.回帰式とP値を追加、統計的に有意か確認
#回帰式の追加はggpubrが必要
install.packages("ggpubr")
library(ggpubr)

#SPNOで練習
ggplot(blue_all, aes(x = YearOfRelease, y = SPNO)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  stat_regline_equation(label.y = 20) +
  stat_cor(label.y = 19) +
  labs(title = "SPNO vs Year of Release")

#全形質で
ggplot(blue_long, aes(x = YearOfRelease, y = BLUE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  stat_regline_equation(size = 3) +
  stat_cor(size = 3) +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "Temporal trends of BLUEs")


#作図した全てをfiguresに保存
# Temporal trends（ggplot）
p <- ggplot(blue_long, aes(x = YearOfRelease, y = BLUE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  stat_regline_equation(size = 2.5, label.y.npc = 0.95) +
  stat_cor(size = 2.5, label.y.npc = 0.85) +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "Temporal trends of BLUEs") +
  theme(strip.text = element_text(size = 8))

ggsave("figures/temporal_trends.png", p, width = 12, height = 10)

# ヒートマップ
png("figures/correlation_heatmap.png", width = 800, height = 800)
pheatmap(
  cor_matrix,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlation heatmap of BLUEs (all traits)",
  border_color = NA
)
dev.off()

# SPNOの単体グラフ
p_spno <- ggplot(blue_all, aes(x = YearOfRelease, y = SPNO)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  stat_regline_equation(label.y = 20) +
  stat_cor(label.y = 19) +
  labs(title = "SPNO vs Year of Release")

ggsave("figures/spno_vs_year.png", p_spno, width = 8, height = 6)

# Q-Qプロット（Location）
png("figures/qqplot_location.png", width = 900, height = 600)
par(mfrow = c(2, 3))
for (trait in loc_traits) {
  res <- resid(loc_models[[trait]])
  qqnorm(res, main = paste("Loc:", toupper(trait)))
  qqline(res, col = "red")
}
par(mfrow = c(1, 1))
dev.off()

# Q-Qプロット（Grain）
png("figures/qqplot_grain.png", width = 900, height = 600)
par(mfrow = c(2, 4))
for (trait in grain_traits) {
  res <- resid(grain_models[[trait]])
  qqnorm(res, main = paste("Grain:", toupper(trait)))
  qqline(res, col = "red")
}
par(mfrow = c(1, 1))
dev.off()


# 行名をリセット（1, 2, 3...にする）
row.names(results) <- NULL

# 結果を確認
print(results)

# Excelファイルとして保存
write.xlsx(results, "data_processed/regression_results.xlsx", rowNames = FALSE)

cat("✓ 回帰分析結果を保存しました: data_processed/regression_results.xlsx\n")



# PCA（主成分分析）- 全形質のBLUEを使用

#1欠損値のある行を除外（PCAは欠損値を扱えない）
pca_data <- na.omit(blue_all)
cat("PCAに使用する品種数:", nrow(pca_data), "\n")

#2形質データだけ取り出す（1列目Genotype、2列目YearOfReleaseを除く）
trait_data <- pca_data[, -c(1, 2)]

#3PCA実行（scale.=TRUEで標準化）
pca_res <- prcomp(trait_data, scale. = TRUE)

#4寄与率を計算（軸ラベル用）
var_explained <- (pca_res$sdev^2) / sum(pca_res$sdev^2)
pc1_pct <- round(var_explained[1] * 100, 1)
pc2_pct <- round(var_explained[2] * 100, 1)

#5プロット用データフレーム作成
pca_df <- data.frame(
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  YearOfRelease = pca_data$YearOfRelease
)

#6PCAプロット（PC1 vs PC2、色=リリース年）
p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = YearOfRelease)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "PCA of BLUEs across all traits",
    x = paste0("PC1 (", pc1_pct, "%)"),
    y = paste0("PC2 (", pc2_pct, "%)"),
    color = "Year of release"
  )

print(p_pca)

#7図を保存
ggsave("figures/06_pca_pc1_pc2.png", p_pca, width = 8, height = 6)
