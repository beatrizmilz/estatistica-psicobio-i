# Lista teórica - SEMANA 2 ----------

# Questões 3 até 6
a <- c(8, 6.5, 7.5, 9.5, 10)
b <- c(6, 8.5, 3.5, 7.5, 9)



# média ----
media_aritmetica <- function(vetor){
  sum(vetor)/length(vetor)
}

media_aritmetica(a)

media_aritmetica(b)


# desvio padrão -----------------

desvio_padrao <- function(vetor){
  desvios <- vetor - media_aritmetica(vetor)
  desvios_ao_quadrado <- desvios ^ 2
  media_desvios_ao_quadrado <- media_aritmetica(desvios_ao_quadrado)
  variancia <- media_desvios_ao_quadrado
  result_desvio_padrao <- sqrt(variancia)
  result_desvio_padrao
}

desvio_padrao(a)
desvio_padrao(b)


# Erro padrão -------

erro_padrao <- function(vetor){
  desvio_padrao(vetor)/sqrt(length(vetor))
}

erro_padrao(a)

erro_padrao(b)



# Coeficiente de variação -------------

# desvio padrão dividido pela média


coeficiente_de_variacao <- function(vetor){
  (desvio_padrao(vetor)/media_aritmetica(vetor))*100
}

coeficiente_de_variacao(a)
coeficiente_de_variacao(b)


# Questão 9 --------------------------

# 9 e 10) IC para creatinina dos pacientes 

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
    
    c(media - valor_intervalo, media + valor_intervalo)
    
}


# 9) Antes da cirurgia

ic_antes <- intervalo_de_confianca_media(
  media = 3.7,
  desvio_padrao = 0.3,
  tamanho_amostra = 75,
  confianca = 95
)



# 10) Depois da cirurgia

ic_depois <- intervalo_de_confianca_media(
  media = 0.9,
  desvio_padrao = 0.4,
  tamanho_amostra = 75,
  confianca = 95
)


# 11)  Podemos afirmar que existe diferença significativa entre o valor da 
# Creatinina antes e depois da Cirurgia?

# Com 95%:
ic_antes
ic_depois


# 12) ---------
# Conjunto A e B


ic_conjunto_a <- intervalo_de_confianca_media(
  media = 10,
  desvio_padrao = 2,
  tamanho_amostra = 40,
  confianca = 95
)

ic_conjunto_b <- intervalo_de_confianca_media(
  media = 10,
  desvio_padrao = 2,
  tamanho_amostra = 10,
  confianca = 95
)


ic_conjunto_a[2] - ic_conjunto_a[1]
ic_conjunto_b[2] - ic_conjunto_b[1]


# testando se o tamanho da amostra muda algo
# acredito que nao

ic_conjunto_a_v2 <- intervalo_de_confianca_media(
  media = 10,
  desvio_padrao = 2,
  tamanho_amostra = 400,
  confianca = 95
)

ic_conjunto_b_v2 <- intervalo_de_confianca_media(
  media = 10,
  desvio_padrao = 2,
  tamanho_amostra = 100,
  confianca = 95
)


ic_conjunto_a_v2[2] - ic_conjunto_a_v2[1]
ic_conjunto_b_v2[2] - ic_conjunto_b_v2[1]


# 14)

n = 1001
prop_favor = 0.54
prop_contra = 0.41
margem_de_erro = 3
z = 3


# ic a favor -------------------------
valor_ic_favor <- z*sqrt((prop_favor * (1 - prop_favor))/n)

ic_favor <- c(prop_favor - valor_ic_favor,
prop_favor + valor_ic_favor)


# ic contra -----------------------

valor_ic_contra <- z*sqrt((prop_contra* (1 - prop_contra))/n)

ic_contra <- c(prop_contra - valor_ic_contra,
prop_contra + valor_ic_contra)

