
## Limpeza de Data Set
vacinados2021Copia <- vacinados2021

# Removendo Colunas
vacinados2021$categoria = NULL
vacinados2021$faixa_etaria = NULL
vacinados2021$lote = NULL
vacinados2021$sistema_origem = NULL

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
vacinados2021["grupoEtario"] <-c("")

# Preenchendo coluna GrupoEtario
vacinados2021$grupoEtario <- sapply(vacinados2021$idade,grupo_Etario)


## Removendo '1' e '2' da coluna $Sexo
a <- filter(vacinados2021,vacinados2021$sexo != '1' & vacinados2021$sexo != '2')
#Filtrando os is.na
b <- filter(vacinados2021, is.na(sexo))
#Concatenando de volta para 'a'
a <- rbind(a,b)
#Jogando vacinados para data set principal
vacinados2021 <- a

#Limpando coluna racca_cor , Indigena estava duplicado
vacinados2021$raca_cor <- replace(vacinados2021$raca_cor, vacinados2021$raca_cor == "ÍNDIGENA","INDÍGENA")
 
#Transformando em Factor 
vacinados2021$raca_cor <- factor(vacinados2021$raca_cor)
vacinados2021$sexo <- factor(vacinados2021$sexo)
vacinados2021$vacina_fabricante <- factor(vacinados2021$vacina_fabricante)


#Removendo Na's
vacinados2021 <- filter (vacinados2021,!is.na(idade))
vacinados2021 <- filter (vacinados2021,!is.na(cnes))

#update da dataset Copia
vacinados2021Copia <- vacinados2021