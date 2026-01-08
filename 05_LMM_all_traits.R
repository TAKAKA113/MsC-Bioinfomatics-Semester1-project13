
# ライブラリを読み込む
library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)
library(tibble)
library(openxlsx)

#Factor型に変更
# Location
location_filtered <- location_filtered %>%
  mutate(
    genotype = as.factor(genotype),
    site     = as.factor(site)
  )

# Grain  
grain_filtered <- grain_filtered %>%
  mutate(
    genotype = as.factor(genotype),
    site     = as.factor(site)
  )


####locationのLMM

library(lme4)
library(dplyr)
library(tibble)

# 分析する形質（tgwを削除）
loc_traits <- c("dth", "dtm", "pht", "spno", "spl", "tkw")

# 結果を入れる箱
loc_models <- list()
loc_blues <- list()

# 各形質を処理
for (trait in loc_traits) {
  
  # 1. モデル式を作る
  formula_LMM <- as.formula(paste0(trait, " ~ 1 + (1 | genotype) + (1 | site)"))
  
  # 2. LMM実行
  model <- lmer(formula_LMM, data = location_filtered, REML = TRUE)
  
  # 3. 全体平均
  fixed_mean <- fixef(model)[["(Intercept)"]]
  
  # 4. ランダム効果を取り出す
  random_effect <- ranef(model)$genotype %>%
    as.data.frame() %>%
    rownames_to_column("Genotype") %>%
    rename(Random_Effect = `(Intercept)`)
  
  # 5. BLUE計算
  blue_table <- data.frame(
    Genotype = random_effect$Genotype,
    BLUE = fixed_mean + random_effect$Random_Effect
  )
  colnames(blue_table)[2] <- toupper(trait)
  
  # 6. 結果を保存
  loc_models[[trait]] <- model
  loc_blues[[trait]] <- blue_table
}

#確認
head(loc_blues[["spno"]])
head(loc_blues[["tkw"]])



####grainのLMM

# 列名のスペースをアンダースコアに変える
#min widtだとスペースが原因で読まれなかったため_を入れる
colnames(grain_filtered) <- gsub(" ", "_", colnames(grain_filtered))

# 確認
colnames(grain_filtered)



# 分析する形質
grain_traits <- c("tgw", "area", "width", "length",
                  "min_width", "min_length",
                  "max_width", "max_length")

# 結果を入れる箱
grain_models <- list()
grain_blues <- list()

# 各形質を処理
for (trait in grain_traits) {
  
  # 1. モデル式を作る
  formula_LMM <- as.formula(paste0(trait, " ~ 1 + (1 | genotype) + (1 | site)"))
  
  # 2. LMM実行
  model <- lmer(formula_LMM, data = grain_filtered, REML = TRUE)
  
  # 3. 全体平均
  fixed_mean <- fixef(model)[["(Intercept)"]]
  
  # 4. ランダム効果を取り出す
  random_effect <- ranef(model)$genotype %>%
    as.data.frame() %>%
    rownames_to_column("Genotype") %>%
    rename(Random_Effect = `(Intercept)`)
  
  # 5. BLUE計算
  blue_table <- data.frame(
    Genotype = random_effect$Genotype,
    BLUE = fixed_mean + random_effect$Random_Effect
  )
  colnames(blue_table)[2] <- toupper(trait)
  
  # 6. 結果を保存
  grain_models[[trait]] <- model
  grain_blues[[trait]] <- blue_table
}

head(grain_blues[["tgw"]])
head(grain_blues[["min_width"]])


#モデル確認
# Location Q-Qプロット（2行×3列）
par(mfrow = c(2, 3))
for (trait in loc_traits) {
  res <- resid(loc_models[[trait]])
  qqnorm(res, main = paste("Loc:", toupper(trait)))
  qqline(res, col = "red")
}
par(mfrow = c(1, 1))

# Grain Q-Qプロット（2行×4列）
par(mfrow = c(2, 4))
for (trait in grain_traits) {
  res <- resid(grain_models[[trait]])
  qqnorm(res, main = paste("Grain:", toupper(trait)))
  qqline(res, col = "red")
}
par(mfrow = c(1, 1))


#ハズレ値の確認

#locationの確認
# DTH
location_filtered %>%
  select(genotype, dth) %>% 
  arrange(desc(dth)) %>% 
  head(10)

# DTM
location_filtered %>% 
  select(genotype, dtm) %>% 
  arrange(desc(dtm)) %>% 
  head(10)

# PHT
location_filtered %>% 
  select(genotype, pht) %>% 
  arrange(desc(pht)) %>% 
  head(10)

# TKW
location_filtered %>% select(genotype, tkw) %>% arrange(desc(tkw)) %>% head(10)

#確認したらtkwだけ以上が見使ったため外れ値の消去
# TKW > 100 を除去
location_filtered <- location_filtered %>%
  filter(tkw < 100 | is.na(tkw))

# 確認
location_filtered %>% 
  select(genotype, tkw) %>% 
  arrange(desc(tkw)) %>% 
  head(10)


#grainの外れ値確認
# MAX_WIDTH
grain_filtered %>% 
  select(genotype, max_width) %>% 
  arrange(desc(max_width)) %>% 
  head(10)

# MAX_LENGTH
grain_filtered %>% 
  select(genotype, max_length) %>% 
  arrange(desc(max_length)) %>% 
  head(10)



#外れ値を除去したtkwだけモデルを見直す
# TKWのLMMを再実行
formula_LMM <- as.formula("tkw ~ 1 + (1 | genotype) + (1 | site)")
model <- lmer(formula_LMM, data = location_filtered, REML = TRUE)

# 全体平均
fixed_mean <- fixef(model)[["(Intercept)"]]

# BLUP
random_effect <- ranef(model)$genotype %>%
  as.data.frame() %>%
  rownames_to_column("Genotype") %>%
  rename(Random_Effect = `(Intercept)`)

# BLUE計算
blue_table <- data.frame(
  Genotype = random_effect$Genotype,
  BLUE = fixed_mean + random_effect$Random_Effect
)
colnames(blue_table)[2] <- "TKW"

# 結果を更新
loc_models[["tkw"]] <- model
loc_blues[["tkw"]] <- blue_table


# TKWのQ-Qプロット
res <- resid(loc_models[["tkw"]])
qqnorm(res, main = "TKW（外れ値除去後）")
qqline(res, col = "red")



# Location BLUEを統合
blue_loc <- loc_blues[["dth"]]
for (trait in c("dtm", "pht", "spno", "spl", "tkw")) {
  blue_loc <- full_join(blue_loc, loc_blues[[trait]], by = "Genotype")
}

# 確認
head(blue_loc)


#推定値つまりBLUEの表の作成と統合
# Location BLUEを統合
library(purrr)
blue_loc <- loc_blues %>%
  reduce(full_join, by = "Genotype")

# 確認
head(blue_loc)

#grainのBLUEを統合
blue_grain <- grain_blues %>%
  reduce(full_join, by = "Genotype")

head(blue_grain)

#locationとgrainの統合　ここで表ができる
blue_all <- full_join(blue_loc, blue_grain, by = "Genotype")

#LMMが同じ品種の複数測定を統合して品種の『真の値』を推定したので行数は下がる
#3375行（測定データ） → 197行（品種ごとのBLUE
head(blue_all)
nrow(blue_all)  # 行数
ncol(blue_all)  # 列数
colnames(blue_all)　#列名
str(blue_all)　#構造


#リリース年データの統合
release <- read_excel("datasets/Year_of_Release_Information.xlsx")

# 確認
head(release)
colnames(release)
colnames(blue_all)


#releaseの列名を統合できるようBlueの表と名前を合わせる
release <- release %>%
  rename(Genotype = Variety)

colnames(release)

#ここでBlueとリリースの表をGenotypeを基準に統合
blue_all <- full_join(blue_all, release, by = "Genotype")

head(blue_all)
# 行数と列数
nrow(blue_all)
ncol(blue_all)

# 列名
colnames(blue_all)

#Excelに出力
write.xlsx(blue_all, "data_processed/blue_all_with_year.xlsx")






