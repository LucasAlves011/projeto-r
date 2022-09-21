unique(vaciandos2021$sexo)
table(vaciandos2021$sexo)

a <- slice(vaciandos2021$sexo,vaciandos2021$sexo != '1' || vaciandos2021$sexo != '2' )


unique(vacinados2021$sexo)
vacinados2021Copia <- vacinados2021

a <- replace(vacinados2021$raca_cor, vacinados2021$raca_cor == "ÍNDIGENA","INDÍGENA")
vacinados2021$raca_cor <- a
table(vacinados2021$raca_cor)
vacinados2021$raca_cor <- factor(vacinados2021$raca_cor)
vacinados2021$sexo <- factor(vacinados2021$sexo)


vacinados2021Copia <- vacinados2021
table(vacinados2021$vacina_fabricante)
vacinados2021$vacina_fabricante <- factor(vacinados2021$vacina_fabricante)f 

sapply(vacinados2021$idade,grupo_Etario)
grupo_Etario(vacinados2021$idade)

grupo_Etario= function (idade ) {
  
  if(is.na(idade)){
    return(NULL)
  }
  else if(idade>=0 & idade<12){
    return ("CRIANÇA")
  }
  else if(idade>=13 & idade<=17){
    return ("ADOLESCENTE")
    
  }
  else if(idade>=18 & idade<=39){
    return  ("JOVEM ADULTO")
    
  }
  else if(idade>=40 & idade<=65){
    return ("MEIA IDADE")
    
  }
  else{
    return ("IDOSO")
    
  }
}

unique(vacinados2021$grupo)


a <- filter(vacinados2021,is.na(idade))
table(is.na(vacinados2021$idade))

grupo_Etario(18)

grupo_Etario(c(13,5,2,6,8,45,24,77,9,3,4,34,13,2))
vacinados2021$grupoIdade <- sapply(vacinados2021$idade, grupo_Etario)


filter()
table(vacinados2021Copia$sistema_origem)


vacinados2021$categoria = NULL
vacinados2021$faixa_etaria = NULL
vacinados2021$lote = NULL
vacinados2021$sistema_origem = NULL

vacinados2021 <- filter (vacinados2021,!is.na(idade))
filter (vacinados2021,is.na(raca_cor))
 filter (vacinados2021,is.na(municipio))
a <- filter (vacinados2021,!is.na(cnes))



