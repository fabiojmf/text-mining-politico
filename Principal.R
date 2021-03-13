rm(list = ls())

library(tidyverse)
library(tidytext)
library(wordcloud2)

url.principal <- "http://www.broadcast.com.br"

pagina <- read_html(paste(url.principal,"/cadernos/politico/",sep = ""))

noticias <- pagina %>% html_nodes(".materia")

limpa_string <- function(parametro){
  ### Remove numeros(datas), barras, pontuação e quebra de linha
  parametro <- str_remove_all(parametro,pattern = '[:digit:]')
  parametro <- str_remove_all(parametro,pattern = '[:punct:]')
  parametro <- str_replace_all(parametro, pattern = '\n',replacement = " ")
  return(str_trim(parametro))
}

texto_sub_pagina <- function(parametro){
  retorno.sub.pagina <- session(parametro)
  retorno.sub.pagina <- read_html(retorno.sub.pagina)
  texto <- retorno.sub.pagina %>% html_nodes(".integra-materia") %>% html_text()
  texto <- limpa_string(texto)
  return(texto)
}

contador <- 0
vetor.textos <- c()

for (i in noticias) {
  if(!is.na(i %>% html_element("a"))){
    contador = contador + 1
    texto.sub.paginas <- 

    vetor.textos[contador] <- texto_sub_pagina(paste(url.principal, i %>% html_element("a") %>%html_attr(name = "href"),sep = ""))

  }
}

### Analise dos textos
texto.df <- tibble(line=contador,text=vetor.textos)
qtd.palavras <- texto.df %>% unnest_tokens(word, text) %>% count(word, sort = T)

### Customiza as 'stop words' para o português do Brasil
custom_stop_words <- read.table(file = "~/Documents/stop_words_brazil.txt")
names(custom_stop_words) = "word"

### Retira as 'stop words'
texto.sem.stop <- qtd.palavras %>% anti_join(custom_stop_words)

words = as.data.frame(texto.sem.stop)

wordcloud2(data = words, size = .5, 
           shape = "oval",
           rotateRatio = 0.5, 
           ellipticity = 0.9, color = "brown")
