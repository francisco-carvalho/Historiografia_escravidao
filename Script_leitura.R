install.packages("pdftools")
install.packages("stringr")

library(pdftools)
library(stringr)
setwd("C:/Users/Kiko/Desktop/R/PDFs")

diss = "2017_DouglasGuimaraesLeite"
pginic = 230
pgfin = 254

CriarListaPalRef <- function(diss, pginic, pgfin) {
  
  Nome_Dissertacao <- list.files(pattern = diss) #Seleciona arq pdf
  
  texto <- lapply(Nome_Dissertacao, pdf_text) #transforma pdf em texto corrido
  palFontes <- texto[[1]][pginic] #Seleciona as palavras que estão nas Referências
  
  while (pginic < pgfin) {
    
    pginic <- pginic + 1
    palFontes <- paste0(palFontes, texto[[1]][pginic])
    
  }
  
  #tirar os espaços e criar lista de palavras
  palFontes <- strsplit(palFontes, " ") 
  
  return(palFontes)
}

TirarRN <- function(ListaPal){
  
  aux <- c()
  palavras_separadas <- c()
  i <- 1 #index da lista sem espaço
  j <- 1 #index da nova lista
  while (i < length(ListaPal[[1]])) {
    
    if (str_detect(ListaPal[[1]][i], "\r\n")) {
      
      aux <- strsplit(ListaPal[[1]][i], "\r\n")
      iaux <- 1 #tamanho da nova lista formada pela separação do \r\n
      
      while (iaux <= length(aux[[1]])) {
        
        #adicionando cada nova palavra na lista
        palavras_separadas[[1]][j] <- aux[[1]][iaux]
        
        #não pode adicionar +1 j na última iteração
        if (iaux < length(aux[[1]])) {
          j <- j + 1
        }
        
        iaux <- iaux + 1
        
      }
      
      
    } else 
      
      #adicionando palavras que não tem \r\n
      palavras_separadas[[1]][j] <- ListaPal[[1]][i]
    j <- j + 1 #posição da próxima palavra na lista nova
    i <- i + 1 #próxima palavra da lista sem espaço
  }
  
  return(palavras_separadas)
}

DescobrirAutores <- function(palsepFontes) {
  
  k = 1
  m = 0
  autores <- c()
  while (k < length(palsepFontes[[1]])) {
    palavra = palsepFontes[[1]][k]
    
    #ver se né maiúscula, não é número nem In: AUTOR
    if (palavra == str_to_upper(palavra) && str_sub(palavra, -1) == ","){
      if(str_detect(palavra, "[1-9]") == FALSE){
        #if (str_detect(palsepFontes[[1]][m], ":$") == FALSE ){
          #if (str_detect(palsepFontes[[1]][m], "^In$") == FALSE ){
          
          #adicionado autores
            autores[[1]][length(autores[[1]]) + 1] <- palavra
          
          #}
        #}
      }
    }   
    
    #Ver se é autor repetido
    if (str_detect(palavra, "_{3,}")) {
      autores[[1]][length(autores[[1]]) + 1] <- autores[[1]][length(autores[[1]])]
      
    }
    
    k <- k + 1
    m <- m + 1
  }
  
  return(autores)
}

CriarListaAutores <- function(diss, pginic, pgfin){
  
 
  palFontes <- CriarListaPalRef(diss, pginic, pgfin)
  
  #tirar /R/N
  palFontes <- TirarRN(palFontes)
  #selecionar autores em formato de lista de strings
  autores <- DescobrirAutores(palFontes)
  
  return(autores)
}









funcaosqrt <- function(n) {
  
  x <- n*n
  
  return(x)
  
}








