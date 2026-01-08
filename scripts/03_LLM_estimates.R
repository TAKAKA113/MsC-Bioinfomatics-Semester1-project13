install.packages("lme4")
install.packages("lmerTest")
install.packages("readxl")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("tidyverse")
####
library(lme4)
library(lmerTest)
library(dplyr)
library(readxl)
library(tidyverse)


####データの読み込み
loc <- read_excel("data_processed/location_filtered.xlsx")
head(loc)

####Factorに変換
loc <- loc %>%
  mutate(
    genotype = as.factor(genotype),
    site     = as.factor(site),
    rep      = as.factor(rep)
  )

str(loc$genotype)　　#確認



####SPNOのLLM
model_spno <- lmer(
  spno ~ 1 + (1 | genotype) + (1 | site),
  data = loc,
  REML = TRUE
)

summary(model_spno)　　#確認


####BLUMの作成
### 1. 固定効果テーブル
fixed_spno <- fixef(model_spno) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  rename(Fixed_Estimate = ".")

### 2. ランダム効果（BLUP）
random_spno <- ranef(model_spno)$genotype %>%
  as.data.frame() %>%
  rownames_to_column("Genotype") %>%
  rename(Random_Effect = `(Intercept)`)

### 3. BLUE（推定値）
fixed_mean_spno <- fixed_spno$Fixed_Estimate[ fixed_spno$Term == "(Intercept)" ]

blue_spno <- data.frame(
  Genotype = blup_spno$Genotype,
  BLUE_SPno = fixed_mean_spno + blup_spno$BLUP
)

### 確認
head(fixed_spno)
head(random_spno)
head(blue_spno)


####SPLのLLM

model_spl <- lmer(
  spl ~ 1 + (1 | genotype) + (1 | site),
  data = loc,
  REML = TRUE
)

summary(model_spl)　#確認

##1.固定効果（Fixed effects）
fixed_spl <- fixef(model_spl) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  rename(Fixed_Estimate = ".")

#2.ランダム効果（random effect = BLUP）
random_spl <- ranef(model_spl)$genotype %>%
  as.data.frame() %>%
  rownames_to_column("Genotype") %>%
  rename(Random_Effect = `(Intercept)`)

#3.BLUE（推定値 = 固定効果 + ランダム効果）
fixed_mean_spl <- fixed_spl$Fixed_Estimate[ fixed_spl$Term == "(Intercept)" ]

blue_spl <- data.frame(
  Genotype  = random_spl$Genotype,
  BLUE_SPL  = fixed_mean_spl + random_spl$Random_Effect
)

#4.確認
head(fixed_spl)
head(random_spl)
head(blue_spl)


#######TKWのLLM
#1.making LLM model
model_tkw <- lmer(
  tkw ~ 1 + (1 | genotype) + (1 | site),
  data = loc,
  REML = TRUE
)

#2.Fixed Effect
fixed_tkw <- fixef(model_tkw) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  rename(Fixed_Estimate = ".")

#3.Random Effect
random_tkw <- ranef(model_tkw)$genotype %>%
  as.data.frame() %>%
  rownames_to_column("Genotype") %>%
  rename(Random_Effect = `(Intercept)`)

#4.BLUE
fixed_mean_tkw <- fixed_tkw$Fixed_Estimate[ fixed_tkw$Term == "(Intercept)" ]

blue_tkw <- data.frame(
  Genotype  = random_tkw$Genotype,
  BLUE_TKW  = fixed_mean_tkw + random_tkw$Random_Effect
)

#5.Check
head(fixed_tkw)
head(random_tkw)
head(blue_tkw)



nrow(blue_spl) #196
nrow(blue_spno) #197
nrow(blue_tkw) #195


#######################





