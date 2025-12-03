#Q-Qplotとは残差が正規分布に従っているか確認するための表


par(mfcol = c(2, 3))　#これでplotが2×3で出力される終わったらpar(mfrow(1,1))で戻す.mfcolとmfrowを使い分ける

# SPNO
res_spno <- resid(model_spno)
plot(fitted(model_spno), res_spno)
qqnorm(res_spno); qqline(res_spno)

# SPL
res_spl <- resid(model_spl)
plot(fitted(model_spl), res_spl)
qqnorm(res_spl); qqline(res_spl)

# TKW
res_tkw <- resid(model_tkw)
plot(fitted(model_tkw), res_tkw)
qqnorm(res_tkw); qqline(res_tkw)

par(mfcol = c(1,1))



#####Found TKW plot broken

#The top 20 largest TKW observations were checked using:
loc %>% select(genotype, site, rep, tkw) %>%
         arrange(desc(tkw)) %>%
         head(20)

#I found outliers
#Two entries had extremely unrealistic TKW values:

# A tibble: 20 × 4
#genotype     site  rep      tkw
#<fct>        <fct> <fct>  <dbl>
#1 KENYA HEROE  NJ_MS 3     3667  
#2 Qulqulu      NJ_OS 2     1987  
#3 KENYA IMPALA NJ_OS 3       58.5
#4 R1585        NJ_OS 3       58.4








