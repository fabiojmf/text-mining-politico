library(rvest)
library(stringr)

url.principal <- "http://www.broadcast.com.br/cadernos/politico/"

pagina <- read_html(url.principal)
pagina <- pagina %>% html_nodes(".mais_noticias")  %>% html_nodes(".row") %>% html_nodes(".col-md-12 ") %>% html_children()

enderecos <- c()
contador <- 0

for (i in pagina) {
  if(i %>% html_name() == "div" &&  i %>% html_attrs() == "noticia morenews"){
    ### Captura os links das matérias
    enderecos[contador] <- str_replace_all(string = i %>%
                                             html_node(".materia") %>%
                                             html_element("h5") %>% 
                                             html_element("a") %>% 
                                             html_attr(name = "href"), 
                                           "/cadernos/politico/","")
    contador = contador + 1
  }
}

retorno.sub.pagina <- session(toString(paste(url.principal,enderecos[1],sep = "")))
retorno.sub.pagina <- read_html(retorno.sub.pagina)

### Ler conteúdo da matéria
retorno.sub.pagina %>% html_nodes(".integra-materia") %>% html_text()
                