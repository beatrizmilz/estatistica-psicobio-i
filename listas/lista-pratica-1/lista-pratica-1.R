# Carregando pacotes

library(haven)
library(tidyverse)

# Carregando os dados
dados <- haven::read_sav("listas/lista-pratica-1/dados.sav")

dados |> 
  writexl::write_xlsx("listas/lista-pratica-1/dados-lista-1.xlsx")


# A tabela apresenta os dados de 36 trabalhadores de uma empresa.

glimpse(dados)

count(dados, ESTADO_C)

count(dados, GRAU_DE)

count(dados, N_MERO_D)

max(dados$SAL_RIO)

max(dados$SAL_RIO2)

count(dados, ORIGEM) |> View()

janitor::tabyl(dados, GRAU_DE, ESTADO_C) 




dados |> 
  ggplot() +
  aes(x = ESTADO_C, fill = ESTADO_C) + 
  geom_bar(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(
    title = "Trabalhadores por estado civil",
    x = "Estado Civil",
    y = "Número de trabalhadores"
  )

# Idade

dados |> 
  group_by(ESTADO_C) |> 
  summarise(
    media_salario = mean(SAL_RIO),
    media_idade = mean(IDADE)
  )

media_idade <- mean(dados$IDADE)
sd_idade <- sd(dados$IDADE)

dados |> 
  ggplot() +
  aes(x = IDADE) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  geom_vline(aes(xintercept = media_idade), color = "red",
             linewidth = 2, linetype = 2) +
  
  geom_vline(aes(xintercept = 31), color = "blue",
             linewidth = 2, linetype = 3) +
  theme_light() +
  labs(
    title = "Distribuição da idade dos trabalhadores",
    x = "Idade",
    y = "Número de trabalhadores"
  )

# -----

idade_minima_ic = 32
idade_maxima_ic = 37

tibble(
  grupo = c("Trabalhadores", "População de SP"),
  idade_media = c(media_idade, 31)
) |> 
  ggplot() +
  aes(x = grupo, y = idade_media) + 
  geom_errorbar(aes(x = "Trabalhadores", ymin = idade_minima_ic, ymax = idade_maxima_ic), width = 0.2) +
  geom_point() + 
    theme_light() +
  scale_y_continuous(limits = c(30, 40))
  labs(
    title = "Distribuição da idade dos trabalhadores",
    x = "Idade",
    y = "Número de trabalhadores"
  )

  # ###  
  
salario_tempo <- dados |> 
  select(salario_06 = SAL_RIO, salario_07 = SAL_RIO1, salario_08 = SAL_RIO2) |> 
  tibble::rowid_to_column() |> 
  pivot_longer(cols = c("salario_06", "salario_07", "salario_08") )

salario_tempo |> 
  ggplot() +
  aes(x = value) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  facet_wrap(~name, ncol = 1) +
  theme_light()
  

# Calcular z score
# Quantos desvios padrão de distancia o valor está da media?
# Calcular z score
# z-score = (x-μ)/σ
# 
# x is a raw score to be standardized;
# 
# μ is the mean of the population;
# 
# σ is the standard deviation of the population.

dados_z_score <- dados |> 
  rowid_to_column() |> 
  rename(numero_filhos = N_MERO_D) |> 
  dplyr::mutate(
    media_numero_filhos = mean(numero_filhos),
    sd_numero_filhos = sd(numero_filhos),
    z_score = (numero_filhos - media_numero_filhos)/sd_numero_filhos
  ) 

dados_z_score |> 
  dplyr::filter(abs(z_score)  >= 3) |> View()

dados_z_score |> 
  dplyr::count(numero_filhos) |> 
  ggplot() +
  aes(x = numero_filhos, y = n) +
  geom_col()
  


# 6
infer::chisq_test(dados, formula = ESTADO_C ~ GRAU_DE)

  infer::t_test(dados, )
