

vacinados2021 <- read_delim("vacinados2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
vacinados2022 <- read_delim("vacinados2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Limpeza de Data Set
vacinados <- rbind(vacinados2021,vacinados2022)

vacinadosOrigial <- rbind(vacinados2021,vacinados2022)
#install.packages("tidyverse")

# Removendo Colunas
vacinados$categoria = NULL
vacinados$faixa_etaria = NULL
vacinados$lote = NULL
vacinados$sistema_origem = NULL

# Função grupoEtario 
grupo_Etario= function (idade ) {
  
  if(is.na(idade)){ return(NULL)}
  else if(idade>=0 & idade<12){ return ("CRIANÇA")}
  else if(idade>=13 & idade<=17){return ("ADOLESCENTE")}
  else if(idade>=18 & idade<=39){return  ("JOVEM ADULTO")}
  else if(idade>=40 & idade<=65){return ("MEIA IDADE")}
  else{return ("IDOSO")}
}

# Criando coluna vazio de Grupo Etario
vacinados["grupoEtario"] <-c("")

# Preenchendo coluna GrupoEtario
vacinados$grupoEtario <- sapply(vacinados$idade,grupo_Etario)


## Removendo '1' e '2' da coluna $Sexo
a <- filter(vacinados,vacinados$sexo != '1' & vacinados$sexo != '2')
#Filtrando os is.na
b <- filter(vacinados, is.na(sexo))
#Concatenando de volta para 'a'
a <- rbind(a,b)
#Jogando vacinados para data set principal
vacinados <- a

#Limpando coluna racca_cor , Indigena estava duplicado
vacinados$raca_cor <- replace(vacinados$raca_cor, vacinados$raca_cor == "ÍNDIGENA","INDÍGENA")
 
#Transformando em Factor 
vacinados$raca_cor <- factor(vacinados$raca_cor)
vacinados$sexo <- factor(vacinados$sexo)
vacinados$vacina_fabricante <- factor(vacinados$vacina_fabricante)


#Removendo Na's
vacinados <- filter (vacinados,!is.na(idade))
vacinados <- filter (vacinados,!is.na(cnes))

#update da dataset Copia
vacinadosCopia <- vacinados









