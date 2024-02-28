# Na aula, o Altay (professor) utiliza o Jamovi
# para fazer as análises. Para praticar, vou fazer em R.

# carregando os pacotes
library(tidyverse)
library(moments) # função skewness e kurtosis
library(jmv) # funções do jamovi


# Os dados estão disponíveis neste arquivo do Google Drive:
# https://drive.google.com/file/d/1B4noZ3KS20JoQ9oMa2eTO5zwQ2EJj3zs/view
# Fiz download manual por ser mais rápido.


# Carregando os dados --------------------------------------------------

# usei o read_csv2 pois o arquivo é separado por ;
dados_cfa <- read_csv2("dados/CFA.csv")

# Explicação da base de dados:
# Conjunto de alunos que aplicamos provas.
# variaveis x1, x2, x3 são provas.


# quais são as variáveis?
names(dados_cfa)
 # [1] "...1"   "id"     "sex"    "ageyr"  "agemo"  "school" "grade"  "x1"    
 # [9] "x2"     "x3"     "x4"     "x5"     "x6"     "x7"     "x8"     "x9"   


glimpse(dados_cfa)
# Rows: 198
# Columns: 16
# $ ...1   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
# $ id     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, …
# $ sex    <dbl> 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2,…
# $ ageyr  <dbl> 13, 13, 13, 13, 12, 14, 12, 12, 13, 12, 12, 12, 12, 12, 12,…
# $ agemo  <dbl> 1, 7, 1, 2, 2, 1, 1, 2, 0, 5, 2, 11, 7, 8, 6, 1, 11, 5, 8, …
# $ school <chr> "Pasteur", "Pasteur", "Pasteur", "Pasteur", "Pasteur", "Pas…
# $ grade  <dbl> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,…
# $ x1     <dbl> 3.00, 5.33, 4.50, 5.33, 4.83, 5.33, 2.83, 5.67, 4.50, 3.50,…
# $ x2     <dbl> 7.75, 5.25, 5.25, 7.75, 4.75, 5.00, 6.00, 6.25, 5.75, 5.25,…
# $ x3     <dbl> 0.38, 2.13, 1.88, 3.00, 0.00, 2.25, 1.00, 1.88, 1.50, 0.00,…
# $ x4     <dbl> 2.33, 1.67, 1.00, 2.67, 2.67, 1.00, 3.33, 3.67, 2.67, 2.67,…
# $ x5     <dbl> 5.75, 3.00, 1.75, 4.50, 4.00, 3.00, 6.00, 4.25, 5.75, 5.00,…
# $ x6     <dbl> 1.29, 1.29, 0.00, 2.43, 2.57, 0.00, 2.86, 1.29, 2.71, 2.57,…
# $ x7     <dbl> 3.39, 3.78, 3.26, 3.00, 3.70, 4.35, 4.70, 3.39, 4.52, 4.13,…
# $ x8     <dbl> 5.75, 6.25, 3.90, 5.30, 6.30, 6.65, 6.20, 5.15, 4.65, 4.55,…
# $ x9     <dbl> 6.36, 7.92, 4.42, 4.86, 5.92, 7.50, 4.86, 3.67, 7.36, 4.36,…

# Estatísticas descritivas da variáveis ----------------

# Vamos calcular as variáveis descritivas da prova 1

descritiva_x1 <- dados_cfa |> 
  summarise(
    quantidade_observacoes_total = n(),
    quantidade_faltantes = sum(is.na(x1)),
    n = quantidade_observacoes_total - quantidade_faltantes,
    mediana = median(x1, na.rm = TRUE),
    media = mean(x1, na.rm = TRUE),
    desvio_padrao = sd(x1, na.rm = TRUE),
    min = min(x1, na.rm = TRUE),
    max = max(x1, na.rm = TRUE),
    assimetria = skewness(x1, na.rm = TRUE),
    # A curtose deu diferente do que aparece para o Altay no Jamovi!! :O
    # Entender o motivo.
    curtose = kurtosis(x1, na.rm = TRUE),
  ) 

View(descritiva_x1)

# Entendendo o motivo da curtose ser diferente no jamovi

# Jamovi: https://www.jamovi.org/
# Documentação: https://docs.jamovi.org/_pages/jmv_overview.html

# Fui fuçar no GitHub do jamovi: 
# https://github.com/jamovi/jmv/blob/92fc56f74cdf647c8f79e6bab3f6aaa670f781b8/R/descriptives.b.R#L1630
# Indicam essa página:
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-whats-with-the-different-formulas-for-kurtosis/
# Nessa página mostram exemplos com SAS, Stata, e SPSS.


lista_x1_descritiva <- jmv::descriptives(dados_cfa, 
                  vars = x1,
                  skew = TRUE,
                  kurt = TRUE) 

curtose_jmv <- lista_x1_descritiva$descriptives$asDF$`x1[kurt]`

# Confuso, vou seguir...

# A versão do pacote moments do R foi atualizada no CRAN pela
# última vez 2 anos atrás:
# https://github.com/cran/moments

# Rodando novamente a kurtosis com o moments
curtose_moments <- moments::kurtosis(dados_cfa$x1, na.rm = TRUE)
# aqui conseguimos ver a fórmula usada:
# https://github.com/cran/moments/blob/master/R/kurtosis.R

# Acho que um exercício legal para fazer com calma depois
# é entender as diferenças entre as fórmulas de kurtosis
# entre o jamovi e o moments.

# Pensar o que faz mais sentido. Mas tô sem tempo agora...

# O ALTAY CHEGOU EM ALGO QUE PARECE OFECERER UMA PISTA
# ALGUNS SOFTWARES UTILIZAM A CURTOSE PARA A NORMAL 
# K=3 E OUTROS K=0

curtose_moments - curtose_jmv
# a diferença é quase 3, então deve ser isso mesmo.


# Gráficos -------------------------------------------------

# Gráfico de histograma da prova 1, com a densidade.

# Histograma com R Base
hist(dados_cfa$x1)

# Já o histograma tem algumas diferenças do jamovi...
# será o tamanho dos bins?
dados_cfa |>
  # começar o gráfico
  ggplot() +
  # variável que queremos colocar no eixo x é a prova 1 (x1)
  aes(x = x1) +
  geom_histogram(bins = 19,
                 colour = "black", 
                 fill = "lightblue") +
  theme_light() +
  labs(title = "Distribuição das notas da prova 1",
       x = "Nota",
       y = "Frequência")

# Gráfico de densidade
dados_cfa |>
  ggplot() +
  aes(x = x1) +
  geom_density(fill = "lightblue") +
  theme_light() +
  labs(title = "Distribuição das notas da prova 1",
       x = "Nota",
       y = "Densidade")


# Os dois gráficos juntos (densidade e histograma)
grafico_prova_1 <- dados_cfa |>
  ggplot() +
  aes(x = x1) +
  geom_histogram(aes(y = ..density..), 
                 bins = 19,
                 colour = "black", 
                 fill = "lightblue") +
  geom_density(alpha = 0.2, fill = "lightblue") +
  theme_light() +
  labs(title = "Distribuição das notas da prova 1",
       x = "Nota",
       y = "Densidade")

grafico_prova_1


ggsave(
  filename = "output/aula-3-histograma_densidade_prova_1.png",
  plot = grafico_prova_1
)


# Existe outro pacote "interno" do jamovi
# https://github.com/jamovi/jmvcore
# que pode ser útil para explorar essas diferenças

# Extra -----------------------------------------------------
# Extra: E se a gente quisesse fazer isso para todas as provas?
# Vamos calcular as variáveis descritivas de cada prova

dados_cfa_longo <- dados_cfa |> 
  pivot_longer(cols = starts_with("x"), 
               names_to = "prova",
               values_to = "nota")


descritiva_varias_provas <- dados_cfa_longo |> 
  group_by(prova) |> 
  summarise(
    quantidade_observacoes_total = n(),
    quantidade_faltantes = sum(is.na(nota)),
    n = quantidade_observacoes_total - quantidade_faltantes,
    mediana = median(nota, na.rm = TRUE),
    media = mean(nota, na.rm = TRUE),
    desvio_padrao = sd(nota, na.rm = TRUE),
    min = min(nota, na.rm = TRUE),
    max = max(nota, na.rm = TRUE),
    assimetria = skewness(nota, na.rm = TRUE),
    curtose_moments = moments::kurtosis(nota, na.rm = TRUE)
  ) 

View(descritiva_varias_provas)


# Gráfico
histograma_densidade_provas <- dados_cfa_longo |> 
  ggplot() + 
  aes(x = nota) +
  geom_histogram(aes(y = ..density..), 
                 bins = 19,
                 colour = "black", 
                 fill = "lightblue") +
  geom_density(alpha = 0.2, fill = "lightblue") +
  theme_light() +
  labs(title = "Distribuição das notas das provas",
       x = "Nota",
       y = "Densidade") +
  facet_wrap(~prova)

histograma_densidade_provas

ggsave(
  filename = "output/aula-3-histograma_densidade_provas.png",
  plot = histograma_densidade_provas
)
