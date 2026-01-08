library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)

### === 0. ファイルパス設定 === ###
loc_file <- "datasets/Treated_All_Location_Data_OSandMS-2.xlsx"
grain_file <- "datasets/Marvin_Grain_Size_All Data_20220901.xlsx"

### === 1. 列名クリーニング関数 === ###
clean_colnames <- function(df) {
  names(df) <- df %>%
    names() %>%
    str_trim() %>%    # 前後の空白削除
    tolower()         # 小文字に統一
  return(df)
}

### ============================================================
### === 2. LOCATION データ加工 ==================================
### ============================================================

# --- 2-1 シート一覧（info除外） ---
loc_sheets <- excel_sheets(loc_file)
loc_sheets <- loc_sheets[!grepl("info", loc_sheets, ignore.case = TRUE)]

# --- 2-2 シート別に読み込み＆列名クリーニング ---
loc_raw_list <- lapply(loc_sheets, function(s) {
  df <- read_excel(loc_file, sheet = s, col_types = "text")
  clean_colnames(df)
})

# --- 2-3 必要な列を定義 ---
required_cols_loc <- c(
  "type", "year", "site", "plot", "column", "row", "rep",
  "genotype", "dth", "dtm", "pht", "spno", "spl", "tkw",
  "trialunitcomment"
)

# --- 2-4 必要な列だけに揃える（欠損列は NA 追加） ---
loc_clean_list <- lapply(loc_raw_list, function(df) {
  missing <- setdiff(required_cols_loc, colnames(df))
  df[missing] <- NA
  df <- df[, required_cols_loc]
  df
})

# --- 2-5 縦結合 ---
location_combined <- bind_rows(loc_clean_list)

# --- 2-6 numeric変換（Rを騙さないために重要） ---
location_typed <- location_combined %>%
  mutate(
    year   = as.numeric(year),
    plot   = as.numeric(plot),
    column = as.numeric(column),
    row    = as.numeric(row),
    rep    = as.numeric(rep),
    dth    = as.numeric(dth),
    dtm    = as.numeric(dtm),
    pht    = as.numeric(pht),
    spno   = as.numeric(spno),
    spl    = as.numeric(spl),
    tkw    = as.numeric(tkw)
  )

# --- 2-7 コメント付き行を除外 ---
location_filtered <- location_typed %>%
  filter(is.na(trialunitcomment))

# --- 2-8 保存 ---
dir.create("data_processed", showWarnings = FALSE)
write.xlsx(location_filtered, "data_processed/location_filtered.xlsx")

cat("Location processing finished.\n")


### ============================================================
### === 3. GRAIN SIZE データ加工 ===============================
### ============================================================

# --- 3-1 シート一覧（info除外） ---
grain_sheets <- excel_sheets(grain_file)
grain_sheets <- grain_sheets[!grepl("info", grain_sheets, ignore.case = TRUE)]

# --- 3-2 シート別に読み込み＆列名クリーニング ---
grain_raw_list <- lapply(grain_sheets, function(s) {
  df <- read_excel(grain_file, sheet = s, col_types = "text")
  clean_colnames(df)
})

# --- 3-3 必要な列 ---
required_cols_grain <- c(
  "type", "year", "site", "plot", "column", "row", "rep",
  "genotype", "tgw", "area", "width", "length",
  "min width", "min length", "max width", "max length",
  "remark"
)

# --- 3-4 必要な列だけに揃える（欠損列は NA） ---
grain_clean_list <- lapply(grain_raw_list, function(df) {
  missing <- setdiff(required_cols_grain, colnames(df))
  df[missing] <- NA
  df <- df[, required_cols_grain]
  df
})

# --- 3-5 縦結合 ---
grain_combined <- bind_rows(grain_clean_list)

# --- 3-6 numeric変換（grain特有の値） ---
grain_typed <- grain_combined %>%
  mutate(
    tgw         = as.numeric(tgw),
    area        = as.numeric(area),
    width       = as.numeric(width),
    length      = as.numeric(length),
    `min width`  = as.numeric(`min width`),
    `min length` = as.numeric(`min length`),
    `max width`  = as.numeric(`max width`),
    `max length` = as.numeric(`max length`)
  )

# --- 3-7 コメント付き行を除外 ---
grain_filtered <- grain_typed %>%
  filter(is.na(remark))

# --- 3-8 保存 ---
write.xlsx(grain_filtered, "data_processed/grain_filtered.xlsx")

cat("Grain size processing finished.\n")
