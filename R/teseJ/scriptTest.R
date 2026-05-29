# Instalar pacman se ainda não estiver instalado
if (!require("pacman")) install.packages("pacman")

# Carregar todos os pacotes necessários com pacman
pacman::p_load(
  stringr,        # Manipulação de strings
  dplyr,          # Manipulação de dados
  writexl,        # Exportar para Excel
  ggplot2,        # Visualização de dados
  easyScieloPack, # Busca de artigos na SciELO
  ggtext,         # Textos ricos em gráficos
  ggpubr,         # Publicação de gráficos bonitos
  ggrepel,        # Evita sobreposição de labels
  arrow,          # Leitura e escrita de dados em formato Parquet/feather
  purrr           # Programação funcional (map, walk, etc.)
)


palavra_chave = "vulnerabilidade"

### Exemplo ###
pp <- search_scielo(
  palavra_chave, 
  n_max = 300
) # Vamos buscar pelo termo "vulnerabilidade", mas limitando a busca a 1.000 registros.

# Se demorar muito, vá tomar uma água para se hidratar, :)

# Visualizar os primeiros resultados
#View(pp)
#names(pp) # variáveis da base de dados

# Padronizar título para minúsculas e verificar se contém o termo "vulnerabilidade"
pp <- pp %>%
  mutate(
    titulo_min = tolower(title),
    tema = str_detect(titulo_min, palavra_chave)
  )

# Filtrar artigos que mencionam "emendas" no título
df_tematica_scielo  <- pp %>%
  filter(tema  == "TRUE") %>%
  arrange(year) %>%
  dplyr::select(-tema)

# Salvar os resultados filtrados em novo arquivo
write_xlsx(df_tematica_scielo, paste("scielo_",gsub(" ","_",palavra_chave),".xlsx", sep=""))

## Vamos agora repetir o processo com os dados da CAPES

install.packages("capesR")
library(capesR)
# Para mais informações, ver:
# https://hugoavmedeiros.github.io/capesR/
# https://cran.r-project.org/web/packages/capesR/index.html

capes <- download_capes_data(c(1987:2022)) # Vai pegar todos os dados.

# Pode ir passar um café pq vai demorar um pouco.

df_capes <- map_dfr(
  capes,
  read_parquet
)

# aqui vai demorar um pouquinho, não muito!

df_capes$titulo_min <- tolower(df_capes$titulo) # colocar tudo em letra minuscula

df_capes$tema <- stringr::str_detect(df_capes$titulo_min, palavra_chave)

df_tematica_capes  <- df_capes %>%
  filter(tema  == "TRUE") %>%
  arrange(ano_base) %>%
  dplyr::select(-tema) # verifica se tem ou nao


# Salvar os resultados filtrados em novo arquivo
write_xlsx(df_tematica_capes, paste("capes_",gsub(" ","_",palavra_chave),".xlsx", sep=""))


## Gráficos

## Gráfico CAPES
df_agg_ano_capes <- df_tematica_capes %>%
  group_by(ano_base) %>%
  summarise(n_casos = n())

sum(df_agg_ano_capes$n_casos) # total de teses e dissertações que citam pp no título

dev.off() 

ano_min = min(df_agg_ano_capes$ano_base, na.rm = TRUE)
ano_max = max(df_agg_ano_capes$ano_base, na.rm = TRUE)

#png(filename = paste("capes_",gsub(" ","_",palavra_chave), sep=""))
df_agg_ano_capes %>%
  ggplot(aes(ano_base, n_casos)) +
  geom_line(alpha = .3) +
  geom_point(size = 5, alpha = .5, color = "darkblue") +
  ggrepel::geom_text_repel(aes(label = n_casos), size = 5) +
  scale_x_continuous(
    breaks = seq(ano_min,
                 ano_max,
                 by = 2)
  ) +
  labs(
    x = "",
    y = "Número",
    title = paste("Teses e Dissertações que citam <b><span style='color:darkblue;'>",palavra_chave,"</span></b> no título"),
    subtitle = paste("Brasil (n = ", count(df_capes), ", ", ano_min,"–",ano_max,")", sep=""),
    caption = "Fonte: elaboração própria a partir do capesR"
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = ggtext::element_markdown())

## Gráfico Scielo
df_agg_ano_scielo <- df_tematica_scielo %>%
  group_by(year) %>%
  summarise(n_casos = n())

df_agg_ano_scielo$year <- as.numeric(df_agg_ano_scielo$year)

sum(df_agg_ano_scielo$n_casos) # total de publicações filtradas

dev.off() 

ano_min = min(df_agg_ano_scielo$year, na.rm = TRUE)
ano_max = max(df_agg_ano_scielo$year, na.rm = TRUE)

df_agg_ano_scielo %>%
  ggplot(aes(year, n_casos)) +
  geom_line(alpha = .3) +
  geom_point(size = 5, alpha = .5, color = "darkblue") +
  ggrepel::geom_text_repel(aes(label = n_casos), size = 5) +
  scale_x_continuous(
    breaks = seq(ano_min, ano_max)
  ) +
  labs(
    x = "",
    y = "Publicações",
    title = paste("Publicações que citam <b><span style='color:darkblue;'>",palavra_chave,"</span></b> no título"),
    subtitle = paste("Brasil (n = ", count(pp), ", ", ano_min,"–",ano_max,")", sep=""),
    caption = "Fonte: elaboração própria a partir do easyScieloPack"
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = ggtext::element_markdown())

  dev.off()
