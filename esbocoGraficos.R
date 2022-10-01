# GRÁFICO NUMERO 1

vacinados2021 <- read_delim("vacinados2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
vacinados2022 <- read_delim("vacinados2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
vacinados <- rbind(vacinados2021,vacinados2022)
rm(vacinados2021,vacinados2022)

## separando por anos 
v2021 <- vacinados %>% filter(vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01')
v2022 <- vacinados %>% filter(vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01')

# Fiz alguma coisa para contar as ocorrencias
d2021 <- v2021 %>% group_by(data_vacinacao) %>% summarise(repetido = n()) %>% arrange(repetido) 
d2022 <- v2022 %>% group_by(data_vacinacao) %>% summarise(repetido = n()) %>% arrange(repetido) 
rm(v2021,v2022)
#d <- recorte %>% group_by(data_vacinacao) %>% summarise(repetido = n()) %>% arrange(repetido) 

## ordenei por data
d2021Ordenado <-d2021[order(d2021$data_vacinacao),]
copyd2022 <-d2022[order(d2022$data_vacinacao),]

#d2022Ordenado <-d2022[order(d2022$data_vacinacao),]
rm(d2021,d2022)

## GRÁFICO DE PESSOAS VACINADAS EM FUNÇÃO DO TEMPO NO ANO DE 2021 
fig <- plot_ly(d2021Ordenado,x=~d2021Ordenado$data_vacinacao,y=~d2021Ordenado$repetido,type = "scatter" , mode= 'lines+markes', name = '2021') %>% 
layout(title = 'Vacinação ao longo dos anos de 2021 e 2022', plot_bgcolor = "#ffffff", xaxis = list(title = "Tempo"),yaxis = list (title = "Vacinas aplicadas")) %>%
add_trace(copyd2022, x = ~copyd2022$data_vacinacao, y = ~copyd2022$repetido, type = 'scatter', mode = 'lines', name = '2022')
fig

####################################################################################################

# GRÁFICO Nº2 
"AJEITAR POR ORDEM ETARIA "

criança2021 <- vacinados %>% filter(vacinados$grupoEtario == "CRIANÇA" & vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
adolescente2021 <- vacinados %>% filter(vacinados$grupoEtario == "ADOLESCENTE" & vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01')  %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
jovemAdulto2021 <- vacinados %>% filter(vacinados$grupoEtario == "JOVEM ADULTO" & vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01')  %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
meiaIdade2021 <- vacinados %>% filter(vacinados$grupoEtario == "MEIA IDADE" & vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
idoso2021 <- vacinados %>% filter(vacinados$grupoEtario == "IDOSO" & vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)

criança2022 <- vacinados %>% filter(vacinados$grupoEtario == "CRIANÇA" & vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
adolescente2022 <- vacinados %>% filter(vacinados$grupoEtario == "ADOLESCENTE" & vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01')  %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
jovemAdulto2022 <- vacinados %>% filter(vacinados$grupoEtario == "JOVEM ADULTO" & vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01')  %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
meiaIdade2022 <- vacinados %>% filter(vacinados$grupoEtario == "MEIA IDADE" & vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)
idoso2022 <- vacinados %>% filter(vacinados$grupoEtario == "IDOSO" & vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01') %>% group_by(grupoEtario) %>% summarise(repetido = n()) %>% arrange(repetido)


Grupos <- c("CRIANÇA", "ADOLESCENTE", "JOVEM ADULTO", "MEIA IDADE", "IDOSO")

d21 <- rbind(criança2021,adolescente2021,jovemAdulto2021,meiaIdade2021,idoso2021)
d22 <- rbind(criança2022,adolescente2022,jovemAdulto2022,meiaIdade2022,idoso2022)

data <- data.frame(Grupos, d21, d22)

fig <- plot_ly(data, x = ~Grupos, y = ~d21$repetido, type = 'bar', name = '2021') %>% 
  add_trace(y = ~d22$repetido, name = '2022')

fig <- fig %>% layout(title = 'Vacinação por grupos', plot_bgcolor = "#ffffff", yaxis = list(title = 'Quantidade de Vacinados'), barmode = 'group')

fig

####################################################################################################

v2021 <- vacinados %>% filter(vacinados$data_vacinacao > '2021-01-01' & vacinados$data_vacinacao < '2022-01-01')
v2022 <- vacinados %>% filter(vacinados$data_vacinacao > '2022-01-01' & vacinados$data_vacinacao < '2023-01-01')

a <- v2021
a$grupo <- replace(v2021$grupo, v2021$grupo == "TRABALHADORES DE TRANSPORTE AÉREO","TRABALHADORES DE TRANSPORTE") 
a$grupo <- replace(a$grupo, a$grupo == "TRABALHADORES DE TRANSPORTE COLETIVO RODOVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a$grupo <- replace(a$grupo, a$grupo == "TRABALHADORES DE TRANSPORTE METROVIÁRIO E FERROVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a$grupo <- replace(a$grupo, a$grupo == "CAMINHONEIROS","TRABALHADORES DE TRANSPORTE") 
a$grupo <- replace(a$grupo, a$grupo == "TRABALHADORES DE TRANSPORTE AQUAVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a$grupo <- replace(a$grupo, a$grupo == "GESTANTES E PUÉRPERAS NÃO RESIDENTES EM RECIFE","GESTANTES E PUÉRPERAS") 

a1 <- v2022
a1$grupo <- replace(v2022$grupo, v2022$grupo == "TRABALHADORES DE TRANSPORTE AÉREO","TRABALHADORES DE TRANSPORTE") 
a1$grupo <- replace(a1$grupo, a1$grupo == "TRABALHADORES DE TRANSPORTE COLETIVO RODOVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a1$grupo <- replace(a1$grupo, a1$grupo == "TRABALHADORES DE TRANSPORTE METROVIÁRIO E FERROVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a1$grupo <- replace(a1$grupo, a1$grupo == "CAMINHONEIROS","TRABALHADORES DE TRANSPORTE") 
a1$grupo <- replace(a1$grupo, a1$grupo == "TRABALHADORES DE TRANSPORTE AQUAVIÁRIO","TRABALHADORES DE TRANSPORTE") 
a1$grupo <- replace(a1$grupo, a1$grupo == "GESTANTES E PUÉRPERAS NÃO RESIDENTES EM RECIFE","GESTANTES E PUÉRPERAS") 

b <- a %>% group_by(grupo) %>% summarise(repetido = n()) %>% arrange(repetido) 
b1 <- a1 %>% group_by(grupo) %>% summarise(repetido = n()) %>% arrange(repetido) 

b[20,] <- NA # PUBLICO GERAL
b[1,] <- NA # CRIANÇA 6 A 11
b[17,] <- NA # PUBLICO GERAL -18 ANOS

b1[22,] <- NA # PUBLICO GERAL
b1[19,] <- NA # PUBLICO GERAL -18
b1[14,] <- NA #CRIANÇA 3 A 4
b1[20,] <- NA # CRIANÇA 5 A 11

fig <- plot_ly()
fig <- fig %>% add_pie(data = b, labels = ~b$grupo, values = ~b$repetido,
                       name = "2021", domain = list(row = 0, column = 0),hole = 0.6)
fig <- fig %>% add_pie(data = b1, labels = ~b1$grupo, values = ~b1$repetido,
                       name = "2022", domain = list(row = 0, column = 1),hole = 0.6)
fig <- fig %>% layout(title = "Grupos prioritários", showlegend = F, 
                      grid=list(rows=2, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F) )
fig
















