
intervalo_de_confianca_media <- function(media, desvio_padrao, tamanho_amostra, confianca = 95){

  if(confianca == 99){
    z <- 2.58
  } else if(confianca == 95){
    z <- 1.96
  } else if(confianca == 90){
    z <- 1.65
  } else {
    usethis::ui_stop("Escolha dentre os seguintes níveis de confiança: 90%, 95% ou 99%.")
  }

    erro_padrao_calc <- desvio_padrao / sqrt(tamanho_amostra)
  
    valor_intervalo <- erro_padrao_calc * z
    
    c(ic_min = media - valor_intervalo, ic_max = media + valor_intervalo)
    
}
# ------

#  http://www.scielo.br/pdf/rgenf/v34n1/04.pdf

# 4 - Veja o seguinte extrato do artigo acima:"Foram distribuídos 120
# questionários, destes 109 foram respondidos pelos trabalhadores de enfermagem,
# distribuídos por categoria profissional: enfermeiros (n=12), técnicos de 
# enfermagem (n=85) e auxiliares de enfermagem (n=12). 


# Quereremos comparar se existe diferença na frequencia das categorias
# profissionais entre as 109 pessoas que aceitaram participar deste estudo.

# Utilizando o teste qui-quadrado para uma amostra, responda se existe
# diferença significativa entre as categorias profissionais 
# (a frequencia esperada é que todas as categorias tenham o mesmo numero de 
# pessoas)

# valor de qui_quadrado para 5% = 5.99


# Qual é a H0?
# Qual é a Ha?

# Ho: Não existe associação entre a categoria profissional 
# e a frequência das respostas.

# Ha: Existe associação entre a categoria profissional 
# e a frequência das respostas.



tabela <- tibble::tribble(
  ~ categoria, ~ frequencia_observada, 
  "enfermeiros", 12, 
  "tecnicos de enfermagem", 85, 
  "auxiliares de enfermagem", 12
)  


chi_quadrado <- tabela |> 
  dplyr::mutate(
    frequencia_esperada = sum(frequencia_observada)/3,
    residuo = frequencia_observada - frequencia_esperada,
    qui_quadrado = residuo^2/frequencia_esperada
  ) 

# Precisamos somar o valor:
sum(chi_quadrado$qui_quadrado)

# E como calcular o p?

# Calculando o qui-quadrado via R:
# options(scipen = 999)
tabela |>
  tibble::column_to_rownames("categoria") |>
  chisq.test()

# 	Chi-squared test for given probabilities
# 
# data:  tibble::column_to_rownames(tabela, "categoria")
# X-squared = 97.78, df = 2, p-value < 0.00000000000000022




# 5 - Veja o seguinte extrato do artigo acima: "Em relação à alocação nos turnos:
# n=68 estavam no diurno e n=41 no turno noturno".

# Queremos comparar se existe diferença na frequência do número de pessoas nos
# grupos que estavam no período diurno e noturno, entre as 109 pessoas que aceitaram
# participar deste estudo.

# Utilizando o teste qui-quadrado para uma amostra, responda se existe diferença
# significativa entre estes grupos (a frequencia esperada é que todas as 
# categorias tenham o mesmo número de pessoas).

tabela_5 <- tibble::tribble(
  ~ categoria, ~ frequencia_observada, 
  "noturno", 41, 
  "diurno", 68
)  

chi_quadrado_5 <- tabela_5 |> 
  dplyr::mutate(
    frequencia_esperada = sum(frequencia_observada)/dplyr::n(),
    residuo = frequencia_observada - frequencia_esperada,
    qui_quadrado = residuo^2/frequencia_esperada
  ) 


sum(chi_quadrado_5$qui_quadrado)

# E como calcular o p?




tabela_5 |>
  tibble::column_to_rownames("categoria") |>
  chisq.test()


# 	Chi-squared test for given probabilities
# 
# data:  tibble::column_to_rownames(tabela_5, "categoria")
# X-squared = 6.6881, df = 1, p-value = 0.009706



# 6 - Em São Paulo, segundo estudo realizado em 2007, a média de horas de sono 
# da população da cidade é 6,9 horas. Um pesquisador coletou uma amostra de 40 
# participantes e após a realização do diário de sono, ele obteve a média de
# horas de sono de sua amostra (8,3 h com DP=1,1). Podemos afirmar que sua
# amostra possui um número de horas de sono comparável com a população de referência?

# teste t


ic <- intervalo_de_confianca_media(media = 8.3,
                                   desvio_padrao = 1.1, 
                                   tamanho_amostra = 40)
# Pelo IC, minha resposta seria não.
#   ic_min   ic_max 
# 7.959106 8.640894 

# Pelo IC, me parece que a resposta é não.

# Se a hipótese nula for verdadeira, os dois grupos teriam numero de horas de 
# sono iguais.
# Então,  A média da população que eu tive a amostra seria igual à média da população 
# de são paulo.

# media_amostral = tem uma distribuição pelo TLC

desvio_padrao = 1.1
n = 40
media_amostra = 8.3
media_pop = 6.9


cima <- media_amostra - media_pop

baixo <- desvio_padrao/ (sqrt(n))

teste_t <- cima/baixo







