library(rvest)
library(stringr)

url.principal <- "http://www.broadcast.com.br/cadernos/politico/"

pagina <- read_html(url.principal)

noticias <- pagina %>% html_nodes(".materia")

enderecos <- c()
contador <- 1

for (i in noticias) {
    enderecos[contador] <- str_replace_all(string = i %>%
                                             html_element("a") %>% 
                                             html_attr(name = "href"), 
                                           "/cadernos/politico/","")
    contador = contador + 1
  
}

retorno.sub.pagina <- session(toString(paste(url.principal,enderecos[1],sep = "")))
retorno.sub.pagina <- read_html(retorno.sub.pagina)

retorno.sub.pagina %>% html_nodes(".integra-materia") %>% html_text()