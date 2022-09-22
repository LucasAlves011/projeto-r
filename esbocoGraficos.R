recorte <- vacinados2021

d <- recorte %>% group_by(data_vacinacao) %>% summarise(repetido = n()) %>% arrange(repetido) 

d <-d[order(d$data_vacinacao),]

## GRÁFICO DE PESSOAS VACINADAS EM FUNÇÃO DO TEMPO NO ANO DE 2021 
fig <- plot_ly(d,x=~d$data_vacinacao,y=~d$repetido,type = "scatter" , mode= 'lines+markes') %>% 
layout(title = 'Vacinação ao longo do ano de 2021', plot_bgcolor = "#ffffff", xaxis = list(title = "Tempo"), yaxis = list (title = "Pessoas vacinadas"))
fig
