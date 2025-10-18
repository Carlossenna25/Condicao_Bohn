library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(ARDL)
library(ggplot2)
library(tidyr)
library(scales)
library(lubridate)

read_excel("C:/Users/carlo/OneDrive/Área de Trabalho/Estudos/bohn/dados.xlsx") -> dados

## Formatação das variáveis ##
dados %>% 
  rename(gap = `Hiato do Produto`,
         g = `Crescimento real do PIB (%)`,
         Y = `4380 - PIB mensal - Valores correntes (R$ milhões)`,
         r = `Taxa de Juros Real (% a,a)`,
         i = `4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a,a,`,
         s = `5364 - NFSP sem desvalorização cambial (% PIB) - Fluxo mensal corrente - Resultado primário - Total - Setor público consolidado - %`,
         d = `13762 - Dívida bruta do governo geral (% PIB) - Metodologia utilizada a partir de 2008 - %`,
         s_gf = `5355 - NFSP sem desvalorização cambial (% PIB) - Fluxo mensal corrente - Resultado primário - Total - Governo Federal - %`) %>% 
  mutate(PERÍODO = as.Date(PERÍODO)) %>% 
  filter(PERÍODO >= as.Date("2011-01-01") & PERÍODO <= as.Date("2025-08-01")) %>% 
  mutate(g_d = (d / lag(d) - 1) * 100,
         g_Y = (Y / lag(Y) - 1) * 100,
         diff_dY = g_d - g_Y) -> dados_padronizados

## dummies ##
dados_padronizados %>% 
  mutate(
    NME = ifelse(PERÍODO >= as.Date("2011-01-01") & PERÍODO <= as.Date("2014-12-01"), 1, 0),
    teto_gastos = ifelse(PERÍODO >= as.Date("2016-12-01") & PERÍODO <= as.Date("2023-08-01"), 1, 0),
    covid = ifelse(PERÍODO >= as.Date("2020-01-01") & PERÍODO <= as.Date("2023-05-01"), 1, 0),
    novo_arcabouco = ifelse(PERÍODO >= as.Date("2023-09-01") & PERÍODO <= as.Date("2025-08-01"), 1, 0)
  ) -> dados_padronizados

## controle macro simples ##
lm(s ~ lag(d, 1) + r + g + gap, data = dados_padronizados) -> modelo_macro
summary(modelo_macro)
coeftest(modelo_macro, vcov = vcovHC(modelo_macro, type = "HC1"))

## com dummies ##
lm(s ~ lag(d, 1) + r + g + gap + NME + 
     teto_gastos + covid + novo_arcabouco, data = dados_padronizados) -> modelo_dummy
summary(modelo_dummy)
coeftest(modelo_dummy, vcov = vcovHC(modelo_dummy, type = "HC1"))

## modelo linear com dummy ##
lm(s ~ lag(d, 1) + r + g + gap + d:NME + d:teto_gastos + d:covid + 
     d:novo_arcabouco, data = dados_padronizados) -> bohn_dummy
summary(bohn_dummy)
coeftest(bohn_dummy, vcov = vcovHC(bohn_dummy, type = "HC1"))

## modelo dinâmico ##
lm(s ~ lag(s, 1) + lag(d, 1) + gap + r + teto_gastos + 
                     NME + novo_arcabouco, data = dados_padronizados) -> bohn_dinamico
summary(bohn_dinamico)
coeftest(bohn_dinamico, vcov = NeweyWest(bohn_dinamico, lag = 12, prewhite = FALSE, adjust = TRUE))

## modelo ARDL ##
dados_padronizados %>% 
  select(PERÍODO, s, d, gap, r, teto_gastos, covid, novo_arcabouco, NME) %>% 
  drop_na() -> bohn_ardl

auto_ardl(
  s ~ d + gap + r + teto_gastos + covid + novo_arcabouco, data = bohn_ardl,
  max_order = 4) -> modelo_ardl

modelo_ardl$best_model -> melhor_modelo
summary(melhor_modelo)
bounds_f_test(melhor_modelo, case = 3)

## UECM a partir do melhor ARDL ##
ue <- uecm(melhor_modelo, data = modelo_ardl)
summary(ue)

lmtest::bgtest(ue)     
lmtest::bptest(ue)      
lmtest::coeftest(ue, vcov. = sandwich::vcovHAC(ue))

beta_lr <- - coef(ue)[["L(d, 1)"]] / coef(ue)[["L(s, 1)"]]
Vhac <- sandwich::vcovHAC(ue)  
car::deltaMethod(
  object = coef(ue),
  g = "- `L(d, 1)` / `L(s, 1)`",
  vcov. = Vhac
)

## adicionar sazonalidade ##
bohn_ardl$mes <- factor(lubridate::month(bohn_ardl$PERÍODO))
auto_ardl(
  s ~ d + gap + r + teto_gastos + covid + novo_arcabouco + NME + mes,
  data = bohn_ardl,
  max_order = 12,          
  selection = "AIC"      
) -> bohn_sazonal

best_sazonal <- bohn_sazonal$best_model
ue_sazonal <- uecm(best_sazonal, data = bohn_sazonal)
summary(ue_sazonal)

lmtest::bgtest(ue_sazonal, order = 12)
lmtest::bptest(ue_sazonal)

beta_lr <- - coef(ue_sazonal)[["L(d, 1)"]] / coef(ue_sazonal)[["L(s, 1)"]]
Vhac <- sandwich::vcovHAC(ue_sazonal)  
car::deltaMethod(
  object = coef(ue_sazonal),
  g = "- `L(d, 1)` / `L(s, 1)`",
  vcov. = Vhac
)


## corrigir autocorrelação, adicionando L(s,12) ##
auto_ardl(
  s ~ d + gap + r + teto_gastos + covid + novo_arcabouco + NME + mes,
  data = bohn_ardl,
  max_order = 12,                           
  selection = "BIC",
  fixed_order = c(12, -1, -1, -1, -1, -1, -1, -1, 0)) -> ardl_s12

ardl_s12$best_model -> best_s12
summary(best_s12)
uecm(best_s12, data = ardl_s12) -> ue12
summary(ue12)

lmtest::bgtest(ue12, order = 12)
lmtest::bptest(ue12)
coeftest(ue12, vcov. = NeweyWest(ue12, lag = 12, prewhite = FALSE))


## Gráficos ##
dados_padronizados %>%
  filter(PERÍODO >= as.Date("2011-01-01"),
         PERÍODO <= as.Date("2025-08-01")) %>%
  select(PERÍODO, s, d) %>%
  tidyr::drop_na() -> plot_df

data.frame(
  nome = c("Teto de Gastos", "COVID-19", "Novo Arcabouço Fiscal"),
  inicio = as.Date(c("2016-12-01", "2020-03-01", "2023-09-01")),
  fim    = as.Date(c("2023-08-01", "2023-05-01", "2025-08-01")),
  cor    = c("#00BFFF", "#FFD700", "#FF6347")
)

ggplot(plot_df, aes(x = PERÍODO)) +
  geom_rect(data = faixas, inherit.aes = FALSE,
            aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf, fill = nome),
            alpha = 0.15) +
  geom_line(aes(y = s, color = "Resultado Primário/PIB (s[t])"), linewidth = 1.1) +
  geom_line(aes(y = d, color = "Dívida/PIB (d[t])"), linetype = "longdash", linewidth = 1.2) +
  scale_color_manual(
    NULL,
    values = c("Resultado Primário/PIB (s[t])" = "#1f77b4", "Dívida/PIB (d[t])" = "red")
  ) +
  scale_fill_manual(NULL, values = setNames(faixas$cor, faixas$nome)) +
  labs(
    title = expression("Trajetória de " * s[t] * " e " * d[t] * " — 2011 a 2025"),
    x = "Período",
    y = "% do PIB"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggplot(dados_padronizados, aes(x = PERÍODO)) +
  geom_line(aes(y = g_d, color = "Crescimento da dívida")) +
  geom_line(aes(y = g_Y, color = "Crescimento do PIB")) +
  geom_line(aes(y = diff_dY, color = "Gap (Δd - Δy)"), linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Crescimento da Dívida x Crescimento do PIB — Brasil (2011–2025)",
    subtitle = "Gap positivo indica dominância fiscal (dívida cresce acima do PIB)",
    x = "Período", y = "Taxa de crescimento (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 13)
