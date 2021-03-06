# Rede agentes penitenciários e presos
# Profa. Ludmila
# Script: Neylson Crepalde
#######################################

library(xlsx)
library(igraph)
library(dplyr)
library(descr)

setwd('~/Documentos/CRISP/redes_corrigidas_atualizadas')
arquivos <- list.files('~/Documentos/CRISP/redes_corrigidas_atualizadas',
                       '.xls')
nomes_arquivos = sapply(arquivos, gsub, pattern=' ', replacement='_')
nomes_arquivos = sapply(nomes_arquivos, gsub, pattern='.xlsx', replacement='')

le_matrizes_xlsx <- function(x){
  cat(paste0(as.character(x), '\n'))
  dados = read.xlsx(x, 2)
  nomes = names(dados)
  rownames(dados) = nomes[-1]
  dados = as.matrix(dados)
  dados = dados[,-1]
  dados = apply(dados, 1, gsub, pattern = "X", replacement = "0")
  return(dados)
}

#dado1 = le_matrizes_xlsx(arquivos[17])
#dado1 = read.xlsx(arquivos[18], 2)
#View(dado1)

#Só tive que mudar 03 AGENTE NELSON HUNGRIA
dados = lapply(arquivos, le_matrizes_xlsx)
###############################################################

g1 <- graph_from_adjacency_matrix(dados[[1]], mode = "directed", weighted = T)
#plot(g1); title(nomes_arquivos[1])

g2 <- graph_from_adjacency_matrix(dados[[2]], mode = "directed", weighted = T)
#plot(g2); title(nomes_arquivos[2])

g3 <- graph_from_adjacency_matrix(dados[[3]], mode = "directed", weighted = T)
#plot(g3); title(nomes_arquivos[3])

g4 <- graph_from_adjacency_matrix(dados[[4]], mode = "directed", weighted = T)
#plot(g4); title(nomes_arquivos[4])

#par(mfrow=c(2,2))
#plot(g1, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[1])
#plot(g2, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[2])
#plot(g3, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[3])
#plot(g4, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[4])
#par(mfrow=c(1,1))

g5 <- graph_from_adjacency_matrix(dados[[5]], mode = "directed", weighted = T)
g6 <- graph_from_adjacency_matrix(dados[[6]], mode = "directed", weighted = T)
g7 <- graph_from_adjacency_matrix(dados[[7]], mode = "directed", weighted = T)
g8 <- graph_from_adjacency_matrix(dados[[8]], mode = "directed", weighted = T)
g9 <- graph_from_adjacency_matrix(dados[[9]], mode = "directed", weighted = T)
g10 <- graph_from_adjacency_matrix(dados[[10]], mode = "directed", weighted = T)
g11 <- graph_from_adjacency_matrix(dados[[11]], mode = "directed", weighted = T)
g12 <- graph_from_adjacency_matrix(dados[[12]], mode = "directed", weighted = T)
g13 <- graph_from_adjacency_matrix(dados[[13]], mode = "directed", weighted = T)
g14 <- graph_from_adjacency_matrix(dados[[14]], mode = "directed", weighted = T)
g15 <- graph_from_adjacency_matrix(dados[[15]], mode = "directed", weighted = T)
g16 <- graph_from_adjacency_matrix(dados[[16]], mode = "directed", weighted = T)
g17 <- graph_from_adjacency_matrix(dados[[17]], mode = "directed", weighted = T)
g18 <- graph_from_adjacency_matrix(dados[[18]], mode = "directed", weighted = T)
g19 <- graph_from_adjacency_matrix(dados[[19]], mode = "directed", weighted = T)
g20 <- graph_from_adjacency_matrix(dados[[20]], mode = "directed", weighted = T)
g21 <- graph_from_adjacency_matrix(dados[[21]], mode = "directed", weighted = T)
g22 <- graph_from_adjacency_matrix(dados[[22]], mode = "directed", weighted = T)
g23 <- graph_from_adjacency_matrix(dados[[23]], mode = "directed", weighted = T)
g24 <- graph_from_adjacency_matrix(dados[[24]], mode = "directed", weighted = T)
g25 <- graph_from_adjacency_matrix(dados[[25]], mode = "directed", weighted = T)
g26 <- graph_from_adjacency_matrix(dados[[26]], mode = "directed", weighted = T)
g27 <- graph_from_adjacency_matrix(dados[[27]], mode = "directed", weighted = T)
g28 <- graph_from_adjacency_matrix(dados[[28]], mode = "directed", weighted = T)
g29 <- graph_from_adjacency_matrix(dados[[29]], mode = "directed", weighted = T)
g30 <- graph_from_adjacency_matrix(dados[[30]], mode = "directed", weighted = T)
g31 <- graph_from_adjacency_matrix(dados[[31]], mode = "directed", weighted = T)
g32 <- graph_from_adjacency_matrix(dados[[32]], mode = "directed", weighted = T)
g33 <- graph_from_adjacency_matrix(dados[[33]], mode = "directed", weighted = T)

grafos = list(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
              g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,
              g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,
              g31,g32,g33)

#####################################################
### Montando uma tabela com as métricas de cada 

# Vendo se há algum não conectado
sapply(grafos, is.connected)

densidades = sapply(grafos, edge_density, loops = F)
diametros = sapply(grafos, diameter)
distancia_media = sapply(grafos, mean_distance)
n = sapply(grafos, function(x) length(V(x)))
transitividade = sapply(grafos, transitivity, type="global")

banco <- data.frame(ego = nomes_arquivos,
                    densidades = densidades,
                    diametros = diametros,
                    distancia_media = distancia_media,
                    n = n,
                    transitividade = transitividade,
                    stringsAsFactors = F)
View(banco)
#write.xlsx(banco, "metricas_redes.xlsx", sheetName = "Métricas", 
#           row.names = F, showNA = F)

#########################################
# Lendo o banco de atributos SPSS
library(foreign)
atributos.spss <- read.spss("Banco redes.sav", to.data.frame = T)
View(atributos.spss)
#att1 = atributos[atributos$Q.3==1,]
#View(att1)
#att2 = atributos[atributos$Q.3==2,]
#View(att2)

# Tem probleminhas aqui. Precisamos verificar

#########################################
# Pegando os atributos
le_atributos_xlsx <- function(x){
  cat(paste0(as.character(x), '\n'))
  dados = read.xlsx(x, 1, stringsAsFactors=F)
  return(dados)
}

atributos = lapply(arquivos, le_atributos_xlsx)

#Corrigindo numero de linhas nos atributos
for (i in 1:length(atributos)){
  atributos[[i]] <- atributos[[i]][1:banco$n[i],]
}

names(atributos[[1]])

##### Recodificar os missing values...
for (i in 1:length(atributos)){
  atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES[is.na(atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES) == T] = 0
  atributos[[i]]$TIPOS.DE.RELAÇÃO[is.na(atributos[[i]]$TIPOS.DE.RELAÇÃO) == T] = 0  #0 = desconhecido
  atributos[[i]]$PRESO[is.na(atributos[[i]]$PRESO) == T] = 0   #0 = desconhecido
}

#Colocando o atributo preso
for (i in 1:length(grafos)){
  V(grafos[[i]])$preso = atributos[[i]]$PRESO
  V(grafos[[i]])$tipo_relacao = atributos[[i]]$TIPOS.DE.RELAÇÃO
  V(grafos[[i]])$frequencia_de_contatos = atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES
}

#Plotando com atributos
#par(mfrow=c(2,2))
plot(grafos[[20]], vertex.color = (V(grafos[[20]])$preso)+2,
     #vertex.size = grauin33/2,
     vertex.size = as.numeric(V(grafos[[20]])$frequencia_de_contatos),
     edge.arrow.size=.2, vertex.label.cex = 1,
     xlab="Tamanho = Freq de contato\nCor = Preso/Não-Preso")
     #layout = layout_with_kk)
title(nomes_arquivos[33])
#par(mfrow=c(1,1))
###########################
# Calculando a proporção de conhecidos presos e não-presos
for (i in 1:33){
  cat(i)
  print(atributos[[i]]$PRESO)
}

#Limpando uma variável
atributos[[16]]$PRESO[atributos[[16]]$PRESO == "1 Já esteve"] = 1
atributos[[16]]$PRESO[atributos[[16]]$PRESO == "1 Já foi"] = 1

#Transformando em numeric
for (i in c(16,17,31)){
  atributos[[i]]$PRESO <- as.numeric(atributos[[i]]$PRESO)
}


# Calculando a proporção de presos X não-presos
prop_presos <- c()
for (i in 1:length(atributos)){
  if (NA %in% atributos[[i]]$PRESO){
    prop_presos[i] = NA
  }
  else{
    mat <- freq(atributos[[i]]$PRESO,plot=F)
    x = mat[1,2]
    prop_presos[i] = x
  }
}
prop_presos # PONDERAR PELA QTD DE ENCONTROS

banco$prop_presos <- prop_presos
#write.xlsx(banco, "metricas_redes.xlsx",
#           sheetName = "Métricas", row.names = F, showNA = F)

#################################################
#################################################

### Consertando a variável frequencia de contatos no mes
for (i in 1:33){
  cat(i)
  print(atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES)
}

atributos[[5]]$FREQUENCIA.DE.CONTADOS.NO.MES = 0
atributos[[11]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[11]]$FREQUENCIA.DE.CONTADOS.NO.MES == "(falecido)"] = 0
atributos[[11]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[11]]$FREQUENCIA.DE.CONTADOS.NO.MES == "telefone"] = 0
atributos[[22]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[22]]$FREQUENCIA.DE.CONTADOS.NO.MES == "0 (TRES CARTAS)"] = 0
atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES == "20-25"] = 25
atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES == "12-15"] = 15
atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[31]]$FREQUENCIA.DE.CONTADOS.NO.MES == "1-2"] = 2
atributos[[32]]$FREQUENCIA.DE.CONTADOS.NO.MES[atributos[[32]]$FREQUENCIA.DE.CONTADOS.NO.MES == "(faleceu)"] = 0

#transforma em numeric
for (i in 1:33){
  atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES = as.numeric(atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES)
}


#######

# limpando a variável tipos de relação
for (i in 1:33){
  cat(i)
  print(atributos[[i]]$TIPOS.DE.RELAÇÃO)
}

atributos[[3]]$TIPOS.DE.RELAÇÃO[atributos[[3]]$TIPOS.DE.RELAÇÃO == "2-3"] = 99
atributos[[12]]$TIPOS.DE.RELAÇÃO[atributos[[12]]$TIPOS.DE.RELAÇÃO == "3-6"] = 99
atributos[[13]]$TIPOS.DE.RELAÇÃO[atributos[[13]]$TIPOS.DE.RELAÇÃO == "(2) 3"] = 99
atributos[[16]]$TIPOS.DE.RELAÇÃO[atributos[[16]]$TIPOS.DE.RELAÇÃO == "5-2"] = 99
atributos[[17]]$TIPOS.DE.RELAÇÃO[atributos[[17]]$TIPOS.DE.RELAÇÃO == "2-6"] = 99
atributos[[25]]$TIPOS.DE.RELAÇÃO[atributos[[25]]$TIPOS.DE.RELAÇÃO == "5-6"] = 99
atributos[[26]]$TIPOS.DE.RELAÇÃO[atributos[[26]]$TIPOS.DE.RELAÇÃO == "PROMOTOR DA VEC"] = 0
atributos[[28]]$TIPOS.DE.RELAÇÃO[atributos[[28]]$TIPOS.DE.RELAÇÃO == "5 (COORD. DE RESSOC.)"] = 5
atributos[[28]]$TIPOS.DE.RELAÇÃO[atributos[[28]]$TIPOS.DE.RELAÇÃO == "2 (PRESO DE OUTRA UNIDADE)"] = 2
atributos[[31]]$TIPOS.DE.RELAÇÃO[atributos[[31]]$TIPOS.DE.RELAÇÃO == "3(2)"] = 99
atributos[[31]]$TIPOS.DE.RELAÇÃO[atributos[[31]]$TIPOS.DE.RELAÇÃO == "6(3)"] = 99

#transforma em numeric
for (i in 1:33){
  atributos[[i]]$TIPOS.DE.RELAÇÃO = as.numeric(atributos[[i]]$TIPOS.DE.RELAÇÃO)
}

#for (i in 1:33){
#  atributos[[i]] = atributos[[i]] %>% 
#    recode(TIPOS.DE.RELAÇÃO, `0`="Sem informação",
#          `1`= "Família", `2`="Amigo/Conhecido Interno",
#          `3`="Amigo/Conhecido Externo", `4`="Inimigo",
#          `5`="Agente/Equipe psicossocial", 
#          `6`="Relação Profissional",
#          `99` = "Múltiplas relações")
#}


# Proporção de presos ponderado pelo número de encontros
#for (att in 1:length(atributos)){
#  atributos[att]$preso_qtd_encontros = atributos[att]$PRESO * atributos[att]$FREQUENCIA.DE.CONTADOS.NO.MES
#  
#}


#########################################
### Algumas análises a nível individual  AVANÇAR!
#mais_central_grau = c()
#mais_central_between = c()
#mais_central_constraint = c()

#for (grafo in grafos){
#  grau = degree(grafo)
#  inter = betweenness(grafo)
#  const = constraint(grafo)


#}

#primeiro_cent_grau <- sapply(grafos, pega_centrais, FUN=degree, rank=1)

#grau = degree(g1)
#class(which.max(grau))
#grau[22]


################################
# Construindo a variável shape
for (i in 1:length(atributos)){
  atributos[[i]]$SHAPE = "none"
}


for (i in 1:length(atributos)){
  for (row in 1:nrow(atributos[[i]])){
    if (atributos[[i]]$PRESO[row] == 1){
      atributos[[i]]$SHAPE[row] = "square"
    }
    if (atributos[[i]]$PRESO[row] == 2){
      atributos[[i]]$SHAPE[row] = "circle"
    }
  }
}

for (i in 1:length(atributos)){
  print(atributos[[i]]$SHAPE)
}

#Construindo a variável CORES

for (i in 1:length(atributos)){
  atributos[[i]]$CORES = "white"
}

for (i in 1:length(atributos)){
  for (row in 1:nrow(atributos[[i]])){
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 1){
      atributos[[i]]$CORES[row] = "red"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 2){
      atributos[[i]]$CORES[row] = "green"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 3){
      atributos[[i]]$CORES[row] = "yellow"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 4){
      atributos[[i]]$CORES[row] = "black"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 5){
      atributos[[i]]$CORES[row] = "orange"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 6){
      atributos[[i]]$CORES[row] = "blue"
    }
    if (atributos[[i]]$TIPOS.DE.RELAÇÃO[row] == 99){
      atributos[[i]]$CORES[row] = "pink"
    }
  }
}

for (i in 1:length(atributos)){
  print(atributos[[i]]$CORES)
}



################################################
# Plotando os grafos com as informações disponíveis
### COLOCAR LEGENDA!!!

#for (i in 1:32){              
#  #exclui 33 pq não tem informação
#  plot(grafos[[i]], vertex.shape = atributos[[i]]$SHAPE,
#       vertex.color = adjustcolor(atributos[[i]]$CORES, .6),
#       vertex.size = atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES+2,
#       edge.arrow.size=.3, vertex.label = NA,
#       xlab="Tamanho = Frequência de Contato\nCor = Tipo de Relação\nForma = Preso")
#  #layout = layout_with_kk)
#  title(nomes_arquivos[i])
#}


##################################################
# Roda as análises descritivas dos escores sem APAC

index = grep("APAC", banco$ego)
banco$ego[index]

banco2 = banco[-index,]
names(banco2)

preso = grep("PRES", banco2$ego)

banco2$preso = 0
banco2$preso[preso] = 1

cbind(banco2$ego, banco2$preso)
freq(banco2$preso, plot=F)

summary(banco2$diametros[banco2$preso == 0])
sd(banco2$diametros[banco2$preso == 0])
summary(banco2$diametros[banco2$preso == 1])
sd(banco2$diametros[banco2$preso == 1])

summary(banco2$densidades[banco2$preso== 0])
sd(banco2$densidades[banco2$preso== 0])
summary(banco2$densidades[banco2$preso== 1])
sd(banco2$densidades[banco2$preso== 1])

# Transforma em proporção (não porcentagem)
banco2$prop_presos = banco2$prop_presos/100
summary(banco2$prop_presos[banco2$preso== 0])
sd(banco2$prop_presos[banco2$preso== 0])
summary(banco2$prop_presos[banco2$preso== 1])
sd(banco2$prop_presos[banco2$preso== 1])

#write.xlsx(banco2, "metricas_redes_sem_APAC.xlsx", row.names = FALSE)

