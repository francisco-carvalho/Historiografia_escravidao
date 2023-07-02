install.packages("pdftools")
install.packages("stringr")
install.packages("tm")
install.packages("openxlsx")


## Sys.getlocale()
## Sys.setlocale("LC_ALL","pt_BR.UTF-8") ###para acentos funcionarem

library(pdftools)
library(stringr)
library(tm)
library(openxlsx)

setwd("../Desktop/Pós/Artigos/Escravidao_hoje")

CriarListaPalRef <- function(diss, pginic, pgfin) {
  #recebe a dissertacao/tese j? em texto, p?gina inicial e final das refer?ncias
  #Nome_Dissertacao <- list.files(pattern = diss) #Seleciona arq pdf
  
  palFontes <- diss[[1]][pginic] #Seleciona as palavras que est?o nas Refer?ncias

  while (pginic < pgfin) {
    
    pginic <- pginic + 1
    palFontes <- paste0(palFontes, diss[[1]][pginic])
    
  }
  
  #tirar os espa?os e criar lista de palavras
  palFontes <- gsub("[\r\n]", " ", palFontes)
  palFontes <- strsplit(palFontes, " ")
  return(palFontes)
}

TirarRN <- function(ListaPal){
  
  aux <- c()
  palavras_separadas <- c()
  i <- 1 #index da lista sem espa?o
  j <- 1 #index da nova lista
  while (i < length(ListaPal[[1]])) {
    
    if (str_detect(ListaPal[[1]][i], "\r\n")) {
      
      aux <- strsplit(ListaPal[[1]][i], "\r\n")
      iaux <- 1 #tamanho da nova lista formada pela separa??o do \r\n
      
      while (iaux <= length(aux[[1]])) {
        
        #adicionando cada nova palavra na lista
        palavras_separadas[[1]][j] <- aux[[1]][iaux]
        
        #n?o pode adicionar +1 j na ?ltima itera??o
        if (iaux < length(aux[[1]])) {
          j <- j + 1
        }
        
        iaux <- iaux + 1
        
      }
      
      
    } else 
      
      #adicionando palavras que n?o tem \r\n
    palavras_separadas[[1]][j] <- ListaPal[[1]][i]
    j <- j + 1 #posi??o da pr?xima palavra na lista nova
    i <- i + 1 #pr?xima palavra da lista sem espa?o
  }
  
  return(palavras_separadas)
}

CriarREf <- function(sobrenome, primeiroNome, palAnt) {
  
  Ruidos <- c("A.,", "B.,", "C.,", "D.,", "E.,", "F.,", "G.,", "H.,", "I.,", "J.,", "K.,", "L.,", "M.,", "N.,", "O.,", "P.,", "Q.,", "R.,", "S.,", "T.,", "U.,", "V.,", "W.,", "X.,", "Y.,", "Z.,","A,", "B,", "C,", "D,", "E,", "F,", "G,", "H,", "I,", "J,", "K,", "L,", "M,", "N,", "O", "P,", "Q,", "R,", "S,", "T,", "U,", "V", "W", "X", "Y,", "Z,", "AC,", "AL,", "AP,", "AM,", "BA,", "CE,", "ES,", "GO,", "MA,", "MT,", "MS,", "MG,", "PA,", "PB,", "PE,", "PI,", "PR,", "RJ,", "RN,", "RS,", "RO,", "RR,", "SC,", "SP,", "SE,", "TO,", "DF,", "ONU,", "UNESCO,", "AEL,", "FGV,", "FURB,","IUPERJ,", "PUC,", "PUC-CAMPINAS,", "PUC/CAMPINAS,", "PUC/MG,", "PUC-MG,", "PUC/PR,", "PUC-PR,", "PUC-RJ,", "PUC/RJ,", "PUC-RS,", "PUC/RS,", "PUC/SP,", "PUC-SP,", "PUCCAMP,", "UA,", "UAM,", "UBC,", "UCB,", "UCDB,", "UCG,", "UCP,", "UCPEL,", "UCS,", "UCSAL,", "UDESC,", "UECE,", "UEFS,", "UEL,", "UEM,", "UEMA,", "UEMG,", "UEMS,", "UEPB,", "UEPG,", "UERJ,", "UESB,", "UFAC,", "UFAL,", "UFBA,", "UFC,", "UFES,", "UFF,", "UFG,", "UFGD,", "UFGD,", "UFJF,", "UFLA,", "UFMA,", "UFMG,", "UFMS,", "UFMT,", "UFOP,", "UFPA,", "UFPB,", "UFPE,", "UFPEL,", "UFPI,", "UFPR,", "UFRGS,", "UFRJ,", "UFRN,", "UFRPE,", "UFRR,", "UFRRJ,", "UFS,", "UFSCAR,", "UFSM,", "UFU,", "UFV,", "UGF,", "ULBRA,", "UM,", "UMC,", "UNAERP,", "UNAMA,", "UNB,", "UNESA,", "UNESP,", "UNG,", "UNIABC,", "UNIANA,", "UNIARA,", "UNIB,", "UNIBAN,", "UNICAMP,", "UNICAP,", "UNICID,", "UNICSUL,", "UNIDERP,", "UNIFENAS,", "UNIFESP,", "UNIFOR,", "UNIFRAN,", "UNIG,", "UNIGRANRIO,", "UNIJUI,", "UNIJU?,", "UNIMAR,", "UNIMARCO,", "UNIMEP,", "UNIMESP,", "UNIOESTE,", "UNIP,", "UNIPE,", "UNIR,", "UNIRIO,", "UNISA,", "UNISANTA,", "UNISANTOS,", "UNISC,", "UNISINOS,", "UNISUL,", "UNIT,", "UNITAU,", "UNIVALI,", "UNIVAP,", "UNIVERSO,", "UNOESTE,", "UPE,", "UPF,", "URCA,", "URCAMP,", "URG,", "URI,", "URRN,", "USC,", "USF,", "USJT,", "USP,", "USU,", "UVA,")
  sobrenomesFakes <- c("FILHO,", "NETO,", "JR.,", "J?NIOR", "JUNIOR")
  romanos <- c("I,", "II,", "III", "IV,", "V,", "VI,", "VII,", "VIII,", "IX,", "X,", "XI,", "XII,", "XIII,", "XIV,", "XV,", "XVI,", "XVII,", "XVIII,", "XIX,", "XX,", "XXI,", "XXII,", "XXIII,", "XXIV,", "XXV,", "XXVI,", "XXVII,", "XXVIII,", "XXIX,", "XXX,")
  
  
  if (is.element(sobrenome, Ruidos)){
      return("Ruido")
    }
  
  if (is.element(sobrenome, romanos)) {
    
    return("NumeroRomano")
  }
  
  ref <- paste0(sobrenome, " ", primeiroNome)
  #Tirando ponto e ponto e v?rgula do primeiroNome
  
  if (is.element(sobrenome, sobrenomesFakes)){
    
    ref <- paste0(palAnt, " ", sobrenome, " ", primeiroNome ) 
  }
  
  if (str_detect(sobrenome, "_")) {
    
    return("underscore")
  }
  

  #Tirando ponto e ponto e v?rgula do primeiro nome ou palAnt
  ref <- str_remove(ref, "[.]")
  ref <- str_remove(ref, "[;]")
  
  return(ref)
  
}

DescobrirAutoresTotal <- function(palsepFontes) {
 
  iPalAnt <- 0
  iSobrenome <- 1
  iPrimeiroNome <- 2
  autores <- c()

  while (iSobrenome < length(palsepFontes[[1]])) {
    sobrenome <- palsepFontes[[1]][iSobrenome]
    primeiroNome <- palsepFontes[[1]][iPrimeiroNome]
    palAnt<- palsepFontes[[1]][iPalAnt]
    #referencia <- paste0(sobrenome, " ", primeiroNome)
    referencia <- CriarREf(sobrenome, primeiroNome, palAnt)
    
    #ver se n?o ? mai?scula, n?mero nem Instituicao nem n?mero romano
    if (sobrenome == str_to_upper(sobrenome) && str_sub(sobrenome, -1) == ","){
      if(str_detect(sobrenome, "[1-9]") == FALSE){
        if(referencia != "Instituicao" && referencia != "NumeroRomano"){ 
        #if (str_detect(palsepFontes[[1]][iPalAnt], ":$") == FALSE ){
          #if (str_detect(palsepFontes[[1]][iPalAnt], "^In$") == FALSE ){
          
          #adicionado autores
            autores[[1]][length(autores[[1]]) + 1] <- referencia
          #}
        
          }  #}
        }
      }   
    
    #Ver se ? autor repetido
    if (str_detect(sobrenome, "_{3,}")) {
      autores[[1]][length(autores[[1]]) + 1] <- autores[[1]][length(autores[[1]])]
      
    }
    
    # i++
    iSobrenome <- iSobrenome + 1
    iPalAnt <- iPalAnt + 1
    iPrimeiroNome <- iPrimeiroNome + 1
  }
  
  return(autores)
}

DescobrirAutoresEntUni <- function(palsepFontes) {
  
  iPalAnt <- 0
  iSobrenome <- 1
  iPrimeiroNome <- 2
  autores <- c()
  
  while (iSobrenome < length(palsepFontes[[1]])) {
    sobrenome <- palsepFontes[[1]][iSobrenome]
    primeiroNome <- palsepFontes[[1]][iPrimeiroNome]
    palAnt<- palsepFontes[[1]][iPalAnt]
    referencia <- CriarREf(sobrenome, primeiroNome, palAnt)

    #ver se n?o ? mai?scula, n?mero nem Instituicao nem n?mero romano
    if (sobrenome == str_to_upper(sobrenome) && str_sub(sobrenome, -1) == ","){
      if(str_detect(sobrenome, "[1-9]") == FALSE){
        if(referencia != "Instituicao" && referencia != "NumeroRomano"&& referencia != "Ruido"){
          #primeiro item da lista
          if (length(autores[[1]]) == 0) {
            autores[[1]][length(autores[[1]]) + 1] <- referencia
          }
          if (is.element(referencia, autores[[1]]) == FALSE) {
            #adicionado autores
            autores[[1]][length(autores[[1]]) + 1] <- referencia
        
          }
        } 
      }
    }
    
    # i++
    iSobrenome <- iSobrenome + 1
    iPalAnt <- iPalAnt + 1
    iPrimeiroNome <- iPrimeiroNome + 1
  }
  
  return(autores)
}

CriarListaAutores <- function(diss, pginic, pgfin){
  
 
  palFontes <- CriarListaPalRef(diss, pginic, pgfin)
  
  #tirar /R/N
  palFontes <- TirarRN(palFontes)
  #selecionar autores em formato de lista de strings
  autores <- DescobrirAutoresEntUni(palFontes)
  
  
  return(autores)
}

CriarColRef <- function(dfRaw) {
  
  #criando df final
  dfFin <- dfRaw
  dfRaw$Ref <- NA
  #preenchendo coluna das ref
  i <- 1
  
  
  while (i<length(dfRaw$Autor) + 1){
    print(i)
    print(dfRaw$endereco[i])
    print(dfRaw$Nome_leitura[i])
    dissTitulo <- list.files(path = dfRaw$endereco[i], pattern = dfRaw$Nome_leitura[i])
    diss <- lapply(paste0(dfRaw$endereco[i], "\\", dissTitulo), pdf_text) #transforma pdf em texto corrido
    pginic <- dfRaw$pginic[i]
    pgfin <- dfRaw$pgfin[i]
    referencias <- CriarListaAutores(diss, pginic, pgfin)
    
    if (length(referencias[[1]]) == 0 ){
      dfRaw$Ref[i] <- "vazio"
    }
    
    if (length(referencias[[1]]) > 0){
      dfRaw$Ref[i] <- referencias
    }
    i <- i + 1
  }
  return(dfRaw)
}

unificarPrincipaisRef <- function(lista_autores) {

  desambfinal <- read.table("desambfinal.csv", 
                            header = TRUE, 
                            sep = ";", 
                            encoding = "latin1")
  dfDesamb <- c()
  dfRef <- CriarColRef(lista_autores)
  dfRefDesambNovo <- dfRef
  i <- 1
  while (i < 450) {
    print(i)
    #Lista que vai ser inserida na nova coluna do DF para cada um dos 450
    autoresDesamb <- c()
    
    for (autor in dfRefDesambNovo$Ref[[i]]){
      autorRefinado <- tolower(str_remove(autor, " "))
      
      for (col in desambfinal){
        
        if (is.element(autorRefinado, col)) {
          
          autorRefinado <- col[[1]][1]
          
        }
        
      }
      
      if (!is.element(autorRefinado, autoresDesamb[[1]])){ 
        autoresDesamb[[1]][length(autoresDesamb[[1]]) + 1] <- autorRefinado
      }
    }
    dfRefDesambNovo$RefDesamb[i] <- autoresDesamb
    i <- i + 1
  }
   
  return(dfRefDesambNovo)
}



#Criar o primeiro df, com autores repetidos
enderecos <- read.table("../../../R/Excel/DadosLeituraR11.csv", 
                        header = TRUE, 
                        sep = ";", 
                        check.names = FALSE,
                        encoding = "latin1")
dfRef <- CriarColRef(enderecos)
  
dfTeste <- CriarColRef(enderecos[1,])

dfDesambTeste <- unificarPrincipaisRef(enderecos)  


###pós tratamentos e subsests
descobrirPrincipaisRef <- function(dfRef, nRef, anoDef = NULL, nivelDef = NULL, instituicaoDef = NULL, tipoInstDef = NULL, notaCapesDef = NULL, UFDef = NULL, RegiaoDef = NULL){
  
  
  if (!is.null(anoDef)){
    
    dfRef <- subset(dfRef, Ano == anoDef)
    
  }
  
  if (!is.null(nivelDef)){
    
    dfRef <- subset(dfRef, N?vel == nivelDef)
    
  }
  
  if (!is.null(instituicaoDef)){
    
    dfRef <- subset(dfRef, Instituicao == instituicaoDef)
    
  }
  
  if (!is.null(tipoInstDef)){
    
    dfRef <- subset(dfRef, TipoInst == tipoInstDef)
    
  }
  
  if (!is.null(notaCapesDef)){
    
    dfRef <- subset(dfRef, nota_Capes == notaCapesDef)
    
  }
  
  if (!is.null(UFDef)){
    
    dfRef <- subset(dfRef, UF == UFDef)
    
  } 
  
  if (!is.null(RegiaoDef)){
    
    dfRef <- subset(dfRef, Regi?o == RegiaoDef)
    
  }
  
  
  dfColunaRef <- dfRef$RefDesamb
  
  
  #adicionando referencias na lista
  k <- 1
  lista_ref <- c()
  ultimo_trabalho <- length(dfRef$RefDesamb)
  while (k < ultimo_trabalho + 1){
    
    lista_ref <- c(lista_ref, dfColunaRef[[k]])
    k <- k + 1 
  }
  
  
  matriz_ref <- termFreq(lista_ref)
  principais_referencias <- findMostFreqTerms(matriz_ref, n = nRef)
  
  return(principais_referencias)
  
}


#Salvar e carregar DFS
##saveRDS(dfRef, "~/../Desktop/R/DFREF.rds")
##dfRef <- readRDS("~/../Desktop/R/DFREF.rds")

#DF final sem repeti??o e formato certo! (jan/2020)
saveRDS(dfDesambTeste, "~/../Desktop/R/DF_definitivo.rds")
dfRefDesamb <- readRDS("~/../Desktop/R/DFREFDESAMB.rds")


# #Rotina para criar coluna de autores j? em formato padr?o e sem repeti??o
# dfDesamb <- c()
# dfRefDesambNovo <- dfRef
# i <- 1
# while (i < 450) {
#   print(i)
#   #Lista que vai ser inserida na nova coluna do DF para cada um dos 450
#   autoresDesamb <- c()
#   
#   for (autor in dfRefDesambNovo$Ref[[i]]){
#     autorRefinado <- tolower(str_remove(autor, " "))
#     
#     for (col in desambfinal){
#       
#       if (is.element(autorRefinado, col)) {
#         
#         autorRefinado <- col[[1]][1]
#         
#       }
#       
#     }
#     
#     if (!is.element(autorRefinado, autoresDesamb[[1]])){ 
#       autoresDesamb[[1]][length(autoresDesamb[[1]]) + 1] <- autorRefinado
#     }
#   }
#   dfRefDesambNovo$RefDesamb[i] <- autoresDesamb
#   i <- i + 1
# }

#Testar quantas vezes aparece um autor
k <- 1
j <- 0
while (k < 450){
  
  if (is.element("mattos,hebe", dfDesambTeste$RefDesamb[k][[1]]) ){
    j <- j + 1
    print(paste0(dfDesambTeste$endereco[k], dfDesambTeste$Nome_leitura[k]))
    
    #intersec <- c(intersec, k)
  }
  k <- k + 1 
}
print(j)


###
###testes
#####

