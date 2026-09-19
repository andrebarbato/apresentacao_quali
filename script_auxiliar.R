# Dados Gini WDI ----

# Carregando pacotes 
library(WDI)
library(tidyverse)
library(ggrepel)
library(viridis)


# Baixando dados do Gini do WDI para todos os países
gini <- WDI::WDI(country = "all",
                 indicator = "SI.POV.GINI")

# Lista de países 
countries <- WDI::WDI_data$country 

# Países da América-Latina
latin_america <- WDI::WDI_data$country %>% 
  filter(region == "Latin America & Caribbean") %>% 
  select(iso3c)

# Gini médio dos países da AL
gini_la <- gini %>%
  filter(iso3c %in% latin_america$iso3c) %>% 
  group_by(year) %>% 
  summarise(
    gini_la = mean(SI.POV.GINI, na.rm = TRUE)
  ) %>% 
  drop_na()

# Agragador para o mundo
world <- countries %>% 
  filter(region != "Aggregates") %>% 
  select(iso3c)

# Gini médio mundial
gini_wld <- gini %>%
  filter(iso3c %in% world$iso3c) %>% 
  group_by(year) %>% 
  summarise(
    gini_wld = mean(SI.POV.GINI, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  drop_na()

# Países de Renda Média Alta, como o Brasil
umi <- countries %>% 
  filter(income == "Upper middle income") %>% 
  select(iso3c)

# Gini médio para países de renda média alta
gini_umi <- gini %>% 
  filter(iso3c %in% umi$iso3c) %>% 
  group_by(year) %>% 
  summarise(
    gini_umi = mean(SI.POV.GINI, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  drop_na()

# Agrupando as medidas com o gini para o Brasil
gini_comp <- gini %>% 
  filter(iso3c == "BRA") %>% 
  select(year, SI.POV.GINI) %>% 
  rename(
    gini_bra = SI.POV.GINI
  ) %>% 
  left_join(gini_la) %>% 
  left_join(gini_wld) %>% 
  left_join(gini_umi)

# Etiquetas para os dados do gráfico
gini_labels <- gini_comp %>%  
  drop_na() %>%  
  pivot_longer(!year, names_to = "var", values_to = "value") %>%  
  filter(year >= 2000) %>% 
  group_by(var) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  mutate(
    label_text = case_when(
      var == "gini_bra" ~ "Brasil",  
      var == "gini_la"  ~ "América Latina",  
      var == "gini_wld" ~ "Mundo",
      var == "gini_umi" ~ "Países de renda média-alta"
    )
  )

# Gráfico principal
g1 <- gini_comp %>%  
  drop_na() %>%  
  pivot_longer(
    !year,
    names_to = "var",
    values_to = "value"
  ) %>%  
  filter(
    year >= 2000
  ) %>%  
  ggplot(aes(x = year, y = value, color = var, linetype = var)) +  
  geom_line(linewidth = 0.9) + 
  # Adiciona os rótulos diretos na última observação de cada linha
  geom_text_repel(
    data = gini_labels,
    aes(label = label_text),
    nudge_x = 1,          # Desloca ligeiramente para a direita do último ano
    direction = "y",      # Evita que os textos colidem verticalmente
    hjust = 0,
    size = 3.5,           # Tamanho adequado para leitura em artigos
    show.legend = FALSE   # Remove da legenda
  ) +
  # Paleta de cores mais escura e contrastante (ColorBrewer Set2/Dark2)
  scale_color_brewer(
    palette = "Dark2",
    guide = "none" # Remove a legenda de cores, pois temos os rótulos diretos
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "dotted"), 
    guide = "none" # Remove a legenda de linhas
  ) +
  # Expandir o eixo X para dar espaço aos rótulos à direita
  scale_x_continuous(
    limits = c(2000, max(gini_comp$year) + 4),
    breaks = seq(2000, max(gini_comp$year), by = 5)
  ) +
  theme_classic(base_size = 11) + 
  theme(
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(color = "black"),
    # Remove as linhas de eixo individuais para evitar sobreposição
    axis.line = element_blank(),
    # Desenha a caixa completa (as 4 bordas) com a mesma espessura exata
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  ) +  
  labs(
    x = "Ano",
    y = "Coeficiente de Gini"
  )

g1

# Dados do Emprego Industrial ----

# Carregando pacotes
library(ipeadatar)
library(patchwork)


emprego <- ipeadatar::ipeadata(c("PNADC12_ESP12", "PNADC12_OCUPIG12"))

g2 <- emprego %>%  
  select(-c("uname", "tcode")) %>%  
  pivot_wider(
    id_cols = date,
    names_from = code,
    values_from = value
  ) %>%  
  rename(
    emprego_total = PNADC12_ESP12,
    emprego_ind = PNADC12_OCUPIG12
  ) %>%  
  mutate(
    perc_ind = emprego_ind / emprego_total * 100
  ) %>%  
  ggplot(aes(x = date, y = perc_ind)) +
  geom_line(linewidth = 0.9, color = "#52658D") + # Linha com espessura acadêmica e cor sóbria (consistente com o Dark2 anterior)
  theme_classic(base_size = 11) + # Tema clássico (fundo branco, eixos limpos)
  theme(
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(color = "black"),
    # Remove as linhas de eixo individuais para evitar sobreposição
    axis.line = element_blank(),
    # Desenha a caixa completa (as 4 bordas) com a mesma espessura exata
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  ) +
  labs(
    # Título interno removido (será incluído na legenda/caption do artigo)
    x = "Ano",
    y = "Participação do emprego industrial (%)"
  )

painel <- (g1 | g2) + 
  plot_annotation(tag_levels = 'A') + 
  plot_annotation(
    title = "Evolução do Coeficiente de Gini e participação do Emprego Industrial no emprego total",
    caption = "Fonte: Elaboração própria a partir de dados do World Development Indicators (WDI) do Banco Mundial (A) e de dados da PNAD Contínua/IBGE (B)"
    ) & 
  theme(
    plot.caption = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
  )

## Fig1 - Gini e Emp. Industrial ----
ggsave(
  filename = "img/fig1_gini_e_emprego_industrial.svg",
  plot = painel,
  device = "svg",
  units = c("in"),
  width = 10,
  height = 5,
  dpi = 300
  )

# Dados OCE ----
library(ggbump)

eci_ranking <- readr::read_csv(file = "data/growth_proj_eci_rankings.csv")

paises_filtro <- c("USA", "CHN", "BRA", "IND", "ARG")

paises_nselecionados <- eci_ranking |> 
  dplyr::filter(!(country_iso3_code %in% paises_filtro),
                year == 2016, eci_rank_hs92 <= 90)

paises_base <- paises_nselecionados$country_iso3_code |> unique()

paises_amostra <- sample(paises_base, size = 10, replace = FALSE)

paises_filtro <- c(paises_filtro, paises_amostra)

# Defina quais países você quer destacar no gráfico
paises_destaque <- c("EUA", "China", "Brasil", "Índia", "Argentina")


dados_plot <- eci_ranking %>%
  dplyr::select(country_iso3_code, year, eci_rank_hs92) |> 
  dplyr::rename(
    country = country_iso3_code,
    rank = eci_rank_hs92
  ) |>
  dplyr::filter(country %in% paises_filtro) |> 
  dplyr::mutate(
    country = case_when(
      country == "USA" ~ "EUA",
      country == "BRA" ~ "Brasil",
      country == "CHN" ~ "China",
      country == "ARG" ~ "Argentina",
      country == "IND" ~ "Índia",
      TRUE ~ country # Mantém os demais caso existam
    )
  ) |> 
  dplyr::mutate(
    destaque = country %in% paises_destaque)
  
# 3. Defina uma paleta de cores sóbria e refinada (tons pastéis escuros/terrosos/azulados)
paleta_sobria <- c(
  "EUA" = "#3b5998", # Azul corporativo sóbrio
  "China"          = "#a83232", # Vermelho fechado/tijolo
  "Brasil"         = "#2e7d32", # Verde musgo/floresta
  "Índia"         = "#d97706", # Mostarda/âmbar elegante
  "Argentina"      = "#0284c7"  # Azul claro acinzentado
)


# 4. Gráfico com o eixo X controlado
rangking <- ggplot(dados_plot, aes(x = year, y = rank, group = country)) +
  # Linhas de fundo esmaecidas
  geom_bump(data = filter(dados_plot, !destaque), color = "gray75", smooth = 8, size = 0.5, alpha = 0.7) +
  
  # Linhas em destaque
  geom_bump(data = filter(dados_plot, destaque), aes(color = country), smooth = 8, size = 1.3) +
  
  # Rótulos à esquerda (ano inicial)
  geom_text(data = filter(dados_plot, year == min(dados_plot$year) & destaque),
            aes(label = paste(country, rank), color = country), hjust = 1.1, size = 4, fontface = "bold") +
  
  # Rótulos à direita (ano final)
  geom_text(data = filter(dados_plot, year == max(dados_plot$year) & destaque),
            aes(label = paste(rank, country), color = country), hjust = -0.1, size = 4, fontface = "bold") +
  
  # Controle das escalas
  scale_color_manual(values = paleta_sobria) +
  scale_y_reverse() +
  scale_x_continuous(
    breaks = c(min(dados_plot$year), 2005, 2015, max(dados_plot$year))
  ) +
  labs(
    title = "Evolução do Ranking Global de Complexidade Econômica",
    caption = "Fonte: Elaboração própria com base nos dados de The Growth Lab at Harvard University"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray60"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold", color = "gray55"),
    axis.title = element_blank(),
    plot.margin = margin(t = 15, r = 70, b = 15, l = 70),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_cartesian(clip = "off")

## Fig1 - Gini e Emp. Industrial ----
ggsave(
  filename = "img/fig2_ranking_complexidade.svg",
  plot = rangking,
  device = "svg",
  units = c("in"),
  width = 10,
  height = 9,
  dpi = 300
)
