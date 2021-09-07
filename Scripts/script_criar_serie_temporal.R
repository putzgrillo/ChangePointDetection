library(tidyverse)
library(lubridate)
df_x <- data.table::fread("EXP_COMPLETA.csv")
df_m <- data.table::fread("IMP_COMPLETA.csv")
# df_ncm <- data.table::fread("/media/bgrillob/DATABASE/R/Comercio Exterior/refs/refNcm.csv")

# SÉRIES RS EXPORTAÇÃO
df_ts <- bind_rows(list(
  X = df_x %>%
    filter(
      SG_UF_NCM == "RS"
      & CO_NCM %in% c(39011092, 24012030)
    ) %>%
    mutate(DT_REF = ymd(CO_ANO * 10000 + CO_MES * 100 + 01)) %>%
    group_by(DT_REF, CO_NCM) %>%
    summarise(KG_LIQUIDO = sum(KG_LIQUIDO)),
  M = df_m %>%
    filter(
      SG_UF_NCM == "RS"
      & CO_NCM %in% c(11010010, 84339090)
    ) %>%
    mutate(DT_REF = ymd(CO_ANO * 10000 + CO_MES * 100 + 01)) %>%
    group_by(DT_REF, CO_NCM) %>%
    summarise(KG_LIQUIDO = sum(KG_LIQUIDO))
)) %>%
  mutate(
    TONELADAS = KG_LIQUIDO / 1000,
    PRODUTO = case_when(
      CO_NCM == 39011092 ~ "X_POLIETILENO",
      CO_NCM == 24012030 ~ "X_TABACO",
      CO_NCM == 11010010 ~ "M_TRIGO",
      CO_NCM == 84339090 ~ "M_COLHEITADEIRA"
    )
  ) %>%
  select(DT_REF, PRODUTO, TONELADAS) %>%
  pivot_wider(names_from = PRODUTO, values_from = TONELADAS, values_fn = sum)

saveRDS(df_ts, file = "/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T4 - Series Temporais/Trabalho/serie.rds")

# 
# df_ts %>%
#   mutate(
#     CO_NCM = as_factor(CO_NCM)
#   ) %>%
#   ggplot(aes(x = DT_REF, y = TONELADAS, colour = PRODUTO)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap(~PRODUTO, ncol = 1, scales = "free_y") +
#     theme_bw()
#   
# 
# 
# 
# # SÉRIES RS EXPORTAÇÃO
# # df_x %>%
# df_m %>%
#   filter(SG_UF_NCM == "RS") %>%
#   mutate(DT_REF = ymd(CO_ANO * 10000 + CO_MES * 100 + 01)) %>%
#   group_by(CO_NCM) %>%
#   summarise(
#     KG_LIQUIDO = sum(KG_LIQUIDO),
#     DT_MIN = min(DT_REF),
#     N_OBS = n_distinct(DT_REF)
#   ) %>% 
#   arrange(-KG_LIQUIDO) %>%
#   slice(1:150) %>% left_join(., df_ncm, by = "CO_NCM") %>% View
