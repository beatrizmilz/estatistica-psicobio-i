# Carregando pacotes e dados ----------------------------------------------

library(tidyverse)

dados_cfa <- read_csv2("dados/CFA.csv")


dados_cfa_longo <- dados_cfa |> 
  pivot_longer(cols = starts_with("x"), 
               names_to = "prova",
               values_to = "nota")


# Criando o gráfico -------------------------------------------------------

# Boxplot comum

dados_cfa_longo |> 
  ggplot() +
  aes(x = prova, y = nota) +
  geom_boxplot(fill = "lightblue") +
  theme_light() +
  labs(title = "Distribuição das notas das provas",
       x = "Prova",
       y = "Nota")


# boxplot "acinturado" - causado pelo notch = TRUE
dados_cfa_longo |> 
  ggplot() +
  aes(x = prova, y = nota) +
  geom_boxplot(fill = "lightblue", notch = TRUE) + # A DIFERENÇA ESTÁ AQUI
  theme_light() +
  labs(title = "Distribuição das notas das provas",
       x = "Prova",
       y = "Nota")
