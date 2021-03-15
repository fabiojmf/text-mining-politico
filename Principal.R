rm(list = ls())

library(tidyverse)
library(tidytext)
library(wordcloud2)
library(rvest)

url.principal <- "http://www.broadcast.com.br"

pagina <- read_html(paste(url.principal,"/cadernos/politico/",sep = ""))

noticias <- pagina %>% html_nodes(".materia")

limpa_string <- function(parametro){
  ### Remove numeros(datas), barras, pontuação e quebra de linha
  parametro <- str_remove_all(parametro,pattern = '[:digit:]')
  parametro <- str_remove_all(parametro,pattern = '[:punct:]')
  parametro <- str_replace_all(parametro, pattern = '\n',replacement = " ")
  return(parametro)
}

texto_sub_pagina <- function(parametro){
  retorno.sub.pagina <- session(parametro)
  retorno.sub.pagina <- read_html(retorno.sub.pagina)
  texto <- str_to_lower(retorno.sub.pagina %>% html_nodes(".integra-materia") %>% html_text())
  texto <- limpa_string(texto)
  return(texto)
}

contador <- 0
vetor.textos <- c()

### Ler todas as matérias da página principal
for (i in noticias) {
  if(!is.na(i %>% html_element("a"))){
    contador = contador + 1
    vetor.textos[contador] <- texto_sub_pagina(paste(url.principal, i %>% html_element("a") %>%html_attr(name = "href"),sep = ""))
  }
}

texto.df <- tibble(line=contador,text=vetor.textos)
qtd.palavras <- texto.df %>% unnest_tokens(word, text) %>% count(word, sort = T)

### Customiza as 'stop words' para o português do Brasil
custom_stop_words <- read.table(file = "~/Documents/stop-words-brazil.txt")
names(custom_stop_words) = "word"

### Retira as 'stop words'
texto.sem.stop <- qtd.palavras %>% anti_join(custom_stop_words)
words = as.data.frame(texto.sem.stop)

### Cria a nuvem de palavras
# wordcloud2(data = words, size = .5, shape = "oval", rotateRatio = 0.5, ellipticity = 0.9, color = "brown")

### Lematização - Identificar e converter formas flexionadas das palavras para as suas versões dicionarizadas
lemma_dic <- read.delim(file = "~/Documents/lemmatization-pt.txt")
names(lemma_dic) <- c("stem", "term")

palavras <- words$word

for (j in 1:length(palavras)){
  comparacao <- palavras[j] == lemma_dic$term
  if (sum(comparacao) == 1){
    palavras[j] <- as.character(lemma_dic$stem[comparacao])
  } else {
    palavras[j] <- palavras[j]
  }
}

palavras.lematizadas.df <- as.data.frame(palavras)
colnames(palavras.lematizadas.df) <- 'palavras'
palavras.lematizadas.df <- palavras.lematizadas.df %>% group_by(palavras) %>% summarise_all(sum)

### Analise de sentimentos
sentimentos <- read.table(file = "~/Documents/sentimentos-pt.txt", sep = "\t", header = TRUE, encoding = "latin1")

sentimentos <- sentimentos %>%
  group_by(Termo) %>%
  summarise(positivo = max(PosScore), negativo = max(NegScore)) %>%
  mutate(Termo = trimws(Termo, which = "left"))

pos <- sentimentos[, c(1, 2)]
neg <- sentimentos[, c(1, 3)]

cont.sentimento.positivo <- 0
cont.sentimento.negativo <- 0

for (j in 1:nrow(words)){
  aux <- data.frame(Termo = words$word[j], Ocorrencias = words$n[j])
  row.names(aux) <- NULL
  x <- left_join(aux, pos, by = "Termo") %>% na.omit()
  
  sentimento_positivo <- sum(x$Ocorrencias*x$positivo)/sum(a$Ocorrencias)
  
  x <- left_join(aux, neg, by = "Termo") %>% na.omit()
  
  sentimento_negativo <- sum(x$Ocorrencias*x$negativo)/sum(a$Ocorrencias)
  
  cont.sentimento.positivo[j] <- sentimento_positivo
  cont.sentimento.negativo[j] <- sentimento_negativo
}

