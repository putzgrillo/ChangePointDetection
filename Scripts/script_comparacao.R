library(breakfast)
library(not)

library(tidyverse)
library(lubridate)
# DADOS
setwd("/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T4 - Series Temporais/Trabalho")
df_ts <- readRDS("serie.rds")

# VISUALIZACAO 
df_ts %>%
  pivot_longer(-DT_REF, names_to = "PRODUTO", values_to = "TONELADAS") %>%
  # filter(PRODUTO %in% c("M_TRIGO", "X_TABACO")) %>%
  ggplot(aes(x = DT_REF, y = TONELADAS)) +
    # geom_point() +
    geom_line() +
    scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
    scale_y_continuous(labels = scales::number_format()) +
    facet_wrap(~PRODUTO, ncol = 1, scales = "free_y", strip.position = "left") +
    labs(y = "Peso negociado (em toneladas)", x = "Período") +
    theme_bw() +
    theme(legend.position = "none")

ggsave("grafico_ts.png")

# APLICAÇÃO MÉTODOS ----
pontos_mudanca <- function(y, dt_vector, plotar = FALSE) {
  # RUN NOT MODELS
  not_list <- list(
    pcwsConstMean = not::not(x = y, contrast = "pcwsConstMean"),
    pcwsConstMeanHT = not::not(x = y, contrast = "pcwsConstMeanHT"),
    pcwsLinContMean = not::not(x = y, contrast = "pcwsLinContMean"),
    pcwsLinMean = not::not(x = y, contrast = "pcwsLinMean"),
    pcwsQuadMean = not::not(x = y, contrast = "pcwsQuadMean"),
    pcwsConstMeanVar = not::not(x = y, contrast = "pcwsConstMeanVar")
  )
  # ESCOLHER MELHOR
  ind_melhor <- lapply(not_list, function(x) {
    min(features(x)$ic)
  }) %>% unlist() %>% which.min
  
  not_df <- not_list[ind_melhor] %>%
    lapply(., function(x) {
      data.frame(Method = "not",
                 ModelSelection = "ic",
                 cptp = not::features(x)$cpt)
    }) %>% bind_rows() %>% 
    mutate(cptp = ifelse(is.na(cptp), 0, cptp))
    # bind_rows(., .id = "Constrast")
  
  # RUN TAIL GREEDY AND WILD BINARY
  wb_tg_df <- breakfast::breakfast(x = y, option = "all") %>%
    {.$cptmodel.list} %>%
    lapply(., function (x) {
      if (purrr::is_empty(x$cpts)) {
        data.frame(Method = x$solution.path,
                   ModelSelection = x$model,
                   cptp = NA)
      } else {
        data.frame(Method = x$solution.path,
                   ModelSelection = x$model,
                   cptp = x$cpts)
      }
    }) %>% bind_rows() %>%
    filter(
      Method %in% c("wbs", "tguh") 
      & ModelSelection == "ic"
    )
  
  cpt_df <- bind_rows(list(not_df, wb_tg_df))
  
  if (plotar) {
    df_plot <- list(
      df_not = data.frame(y = y, date_ref = dt_vector, method = "not", cptp = seq_along(y)) %>%
        left_join(., {cpt_df %>% filter(Method == "not")}, by = "cptp") %>%
        mutate(changePoint = ifelse(is.na(Method), NA, as.numeric(date_ref))),
      df_wb = data.frame(y = y, date_ref = dt_vector, method = "wbs", cptp = seq_along(y)) %>%
        left_join(., {cpt_df %>% filter(Method == "wbs")}, by = "cptp") %>%
        mutate(changePoint = ifelse(is.na(Method), NA, as.numeric(date_ref))),
      df_tg = data.frame(y = y, date_ref = dt_vector, method = "tguh", cptp = seq_along(y)) %>%
        left_join(., {cpt_df %>% filter(Method == "tguh")}, by = "cptp") %>%
        mutate(changePoint = ifelse(is.na(Method), NA, as.numeric(date_ref)))
    ) %>% bind_rows()
    
    plot_cp <- df_plot %>%
      ggplot(., aes(y = y, x = date_ref)) +
      geom_line() +
      facet_wrap(~method, ncol = 1) +
      geom_vline(aes(xintercept = changePoint), linetype = 2) +
      theme_bw()
    
    resultado <- list(
      plot = plot_cp,
      df = {cpt_df %>% group_by(Method) %>% summarise(n = sum(cptp > 0), 
                                                      locations = paste(cptp, collapse = ","))},
      location = cpt_df
    )
  } else {
    resultado <- list(
      df = {cpt_df %>% group_by(Method) %>% summarise(n = sum(cptp > 0), 
                                                      locations = paste(cptp, collapse = ","))},
      location = cpt_df)
  }
return(resultado)
}


# SIMULAÇÕES ----
n_boot <- 1000
dt <- seq.Date(from = as.Date("2010-01-01"), length.out = 80, by = "months")

    # SIMULAÇÕES: Piecewise-constant mean with Gaussian noise ----
boot_1 <- vector("list", n_boot)
for (w in seq(n_boot)) {
  x <- c(rep(0, 20), rep(1,20), rep(-1, 20), rep(0.5, 20)) + rnorm(80)    
  boot_1[[w]] <- pontos_mudanca(y = x, dt_vector = dt)
}

lapply(boot_1, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = as_factor(n)) %>%
  ggplot(., aes(x = n, fill = Method)) +
    geom_histogram(alpha = 0.8, stat = "count", colour = "black", position = "identity") +
    geom_vline(xintercept = 4, linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_gaussian_noise_AMOUNT.png")

lapply(boot_1, function(x) {x$location}) %>% 
  bind_rows() %>%
  mutate(cptp = ifelse(cptp == 0, NA, cptp)) %>%
  ggplot(., aes(x = cptp, fill = Method)) +
    geom_histogram(alpha = 0.8, colour = "black", position = "identity") +
    geom_vline(xintercept = c(20.5, 40.5, 60.5), linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_gaussian_noise_LOCATION.png")

lapply(boot_1, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = ifelse(n > 8, 8, n)) %>%
  group_by(Method, ChangePoints = n) %>%
  summarise(Frequency = n()) %>%
  pivot_wider(names_from = Method, values_from = Frequency, values_fn = sum, values_fill = 0)

    # SIMULAÇÕES: Piecewice-constant mean with a heavy-tailed noise. ----
boot_2 <- vector("list", n_boot)
for (w in seq(n_boot)) {
  x <- c(rep(0, 20), rep(1,20), rep(-1, 20), rep(0.5, 20)) + rt(80, df = 2)      
  boot_2[[w]] <- pontos_mudanca(y = x, dt_vector = dt)
}

lapply(boot_2, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = as_factor(n)) %>%
  ggplot(., aes(x = n, fill = Method)) +
    geom_histogram(alpha = 0.8, stat = "count", colour = "black", position = "identity") +
    geom_vline(xintercept = 4, linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_heavy_tail_AMOUNT.png")

lapply(boot_2, function(x) {x$location}) %>% 
  bind_rows() %>%
  mutate(cptp = ifelse(cptp == 0, NA, cptp)) %>%
  ggplot(., aes(x = cptp, fill = Method)) +
    geom_histogram(alpha = 0.8, colour = "black", position = "identity") +
    geom_vline(xintercept = c(20.5, 40.5, 60.5), linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_heavy_tail_LOCATION.png")

lapply(boot_2, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = ifelse(n > 8, 8, n)) %>%
  group_by(Method, ChangePoints = n) %>%
  summarise(Frequency = n()) %>%
  pivot_wider(names_from = Method, values_from = Frequency, values_fn = sum, values_fill = 0) %>%
  apply(., 1, paste, collapse = " & ") %>%
  as.data.frame()


    # SIMULAÇÕES: Piecewise-constant mean and piecewise-constant variance ----
boot_3 <- vector("list", n_boot)
for (w in seq(n_boot)) {
  x <- c(rnorm(20, 5, 1), rnorm(20, 5, 2), rnorm(20, 4, 2), rnorm(20, 4, 1))         
  boot_3[[w]] <- pontos_mudanca(y = x, dt_vector = dt)
}

lapply(boot_3, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = as_factor(n)) %>%
  ggplot(., aes(x = n, fill = Method)) +
    geom_histogram(alpha = 0.8, stat = "count", colour = "black", position = "identity") +
    geom_vline(xintercept = 4, linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_constant_variance_AMOUNT.png")

lapply(boot_3, function(x) {x$location}) %>% 
  bind_rows() %>%
  mutate(cptp = ifelse(cptp == 0, NA, cptp)) %>%
  ggplot(., aes(x = cptp, fill = Method)) +
    geom_histogram(alpha = 0.8, colour = "black", position = "identity") +
    geom_vline(xintercept = c(20.5, 40.5, 60.5), linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/constant_mean_constant_variance_LOCATION.png")

lapply(boot_3, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = ifelse(n > 8, 8, n)) %>%
  group_by(Method, ChangePoints = n) %>%
  summarise(Frequency = n()) %>%
  pivot_wider(names_from = Method, values_from = Frequency, values_fn = sum, values_fill = 0) %>%
  apply(., 1, paste, collapse = " & ") %>%
  as.data.frame()

    # SIMULAÇÕES: ARIMA PROCESS, LINEAR MEAN, HEAVY TAIL AND PIECEWISE VARIANCE ----
boot_4 <- vector("list", n_boot)
for (w in seq(n_boot)) {
                        # PROCESSO ARIMA(1,1,0) COM RESÍDUO T-Student (gl=7)
  x <- arima.sim(n = 20,
                 model = list(order = c(1,1,0), ar = -0.07),
                 start.innov = 5, n.start = 1, innov = rt(20, df = 7))[-1]
                        # (MEDIA(4 ÚLTIMAS) + 2 * SD(4 ÚLTIMAS)) COM RUÍDO GAUSSIANO
  x <- c(x, rep(mean(tail(x, 4)) + 2 * sd(tail(x, 4)), 20) + rnorm(20))  
                        # MÉDIA(4 ÚLTIMAS) + 0.15 * t COM RESÍDUO T-Student (gl=5)
  x <- c(x, (mean(tail(x, 4))) + 0.15 * seq(41, 60) + rt(20, df = 5))
                        # MÉDIA(10 ÚLTIMAS)  - 0.0008 * t^2 COM RUÍDO GAUSSIANO
  x <- c(x, mean(tail(x, 10)) - 0.0008 * seq(61, 80) ** 2 + rnorm(20))  
  boot_4[[w]] <- pontos_mudanca(y = x, dt_vector = dt)
}

lapply(boot_4, function(x) {x$df}) %>%
  bind_rows() %>%
  mutate(n = as_factor(n)) %>%
  ggplot(., aes(x = n, fill = Method)) +
    geom_histogram(alpha = 0.8, stat = "count", colour = "black", position = "identity") +
    geom_vline(xintercept = 4, linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/arima_trend_heavytail_piecewisevariance_AMOUNT.png")

lapply(boot_4, function(x) {x$location}) %>% 
  bind_rows() %>%
  mutate(cptp = ifelse(cptp == 0, NA, cptp)) %>%
  ggplot(., aes(x = cptp, fill = Method)) +
    geom_histogram(alpha = 0.8, colour = "black", position = "identity") +
    geom_vline(xintercept = c(20.5, 40.5, 60.5), linetype = 2) +
    facet_wrap(~Method, ncol = 1) +
    theme_bw()
ggsave(filename = "plots/arima_trend_heavytail_piecewisevariance_LOCATION.png")

    # SIMULAÇÕES: SEASONAL BREAKS ----


# MÉTRICAS DE DESEMPENHO ----
    # ACERTO LOCALIZAÇÃO VIZINHOS 
lapply(boot_4, function(x) {x$location}) %>%
  bind_rows() %>%
  mutate(Correct = ifelse(cptp %in% c(18:22, 38:42, 58:62), TRUE, FALSE)) %>%
  group_by(Method) %>%
  summarise(
    Sensitivity = sum(Correct) / (3 * n_boot),
    Precision = mean(Correct)       
  ) %>% t

# APLICAÇÃO ----
pontos_mudanca(y = df_ts$X_TABACO, dt_vector = df_ts$DT_REF, plotar = TRUE)  # X_TABACO
ggsave(filename = "plots/x_tobacco.png")
pontos_mudanca(y = df_ts$M_TRIGO, dt_vector = df_ts$DT_REF, plotar = TRUE)  # M_TRIGO
ggsave(filename = "plots/m_wheat.png")
pontos_mudanca(y = df_ts$X_POLIETILENO, dt_vector = df_ts$DT_REF, plotar = TRUE)  # X_POLIETILENO
ggsave(filename = "plots/x_polyethylene.png")
pontos_mudanca(y = df_ts$M_COLHEITADEIRA, dt_vector = df_ts$DT_REF, plotar = TRUE)  # M_COLHEITADEIRA
ggsave(filename = "plots/m_combine_harvester.png")
