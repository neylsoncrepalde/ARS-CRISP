# Rede agentes penitenciários e presos
# Profa. Ludmila
# Script: Neylson Crepalde
#######################################

library(xlsx)
library(igraph)
library(dplyr)
library(descr)

setwd('~/Documentos/Neylson Crepalde/Doutorado/CRISP/Tabelas de Redes')
arquivos <- list.files('~/Documentos/Neylson Crepalde/Doutorado/CRISP/Tabelas de Redes',
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

g1 <- graph_from_adjacency_matrix(dados[[1]])
plot(g1); title(nomes_arquivos[1])

g2 <- graph_from_adjacency_matrix(dados[[2]])
plot(g2); title(nomes_arquivos[2])

g3 <- graph_from_adjacency_matrix(dados[[3]])
plot(g3); title(nomes_arquivos[3])

g4 <- graph_from_adjacency_matrix(dados[[4]])
plot(g4); title(nomes_arquivos[4])

par(mfrow=c(2,2))
plot(g1, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[1])
plot(g2, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[2])
plot(g3, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[3])
plot(g4, edge.arrow.size=.2, vertex.label=NA); title(nomes_arquivos[4])
par(mfrow=c(1,1))

g5 <- graph_from_adjacency_matrix(dados[[5]])
g6 <- graph_from_adjacency_matrix(dados[[6]])
g7 <- graph_from_adjacency_matrix(dados[[7]])
g8 <- graph_from_adjacency_matrix(dados[[8]])
g9 <- graph_from_adjacency_matrix(dados[[9]])
g10 <- graph_from_adjacency_matrix(dados[[10]])
g11 <- graph_from_adjacency_matrix(dados[[11]])
g12 <- graph_from_adjacency_matrix(dados[[12]])
g13 <- graph_from_adjacency_matrix(dados[[13]])
g14 <- graph_from_adjacency_matrix(dados[[14]])
g15 <- graph_from_adjacency_matrix(dados[[15]])
g16 <- graph_from_adjacency_matrix(dados[[16]])
g17 <- graph_from_adjacency_matrix(dados[[17]])
g18 <- graph_from_adjacency_matrix(dados[[18]])
g19 <- graph_from_adjacency_matrix(dados[[19]])
g20 <- graph_from_adjacency_matrix(dados[[20]])
g21 <- graph_from_adjacency_matrix(dados[[21]])
g22 <- graph_from_adjacency_matrix(dados[[22]])
g23 <- graph_from_adjacency_matrix(dados[[23]])
g24 <- graph_from_adjacency_matrix(dados[[24]])
g25 <- graph_from_adjacency_matrix(dados[[25]])
g26 <- graph_from_adjacency_matrix(dados[[26]])
g27 <- graph_from_adjacency_matrix(dados[[27]])
g28 <- graph_from_adjacency_matrix(dados[[28]])
g29 <- graph_from_adjacency_matrix(dados[[29]])
g30 <- graph_from_adjacency_matrix(dados[[30]])
g31 <- graph_from_adjacency_matrix(dados[[31]])
g32 <- graph_from_adjacency_matrix(dados[[32]])
g33 <- graph_from_adjacency_matrix(dados[[33]])

grafos = list(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
              g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,
              g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,
              g31,g32,g33)

#####################################################
### Montando uma tabela com as métricas de cada 

# Vendo se há algum não conectado
sapply(grafos, is.connected)

densidades = sapply(grafos, edge_density)
diametros = sapply(grafos, diameter)
distancia_media = sapply(grafos, mean_distance)
n = sapply(grafos, function(x) length(V(x)))
transitividade = sapply(grafos, transitivity, type="global")

banco <- data.frame(ego = nomes_arquivos,
                    densidades = densidades,
                    diametros = diametros,
                    n = n,
                    transitividade = transitividade,
                    stringsAsFactors = F)
View(banco)

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
    atributos[[i]]$TIPOS.DE.RELAÇÃO[is.na(atributos[[i]]$TIPOS.DE.RELAÇÃO) == T] = 0
    atributos[[i]]$PRESO[is.na(atributos[[i]]$PRESO) == T] = 0
}

#Colocando o atributo preso
for (i in 1:length(grafos)){
  V(grafos[[i]])$preso = atributos[[i]]$PRESO
  V(grafos[[i]])$tipo_relacao = atributos[[i]]$TIPOS.DE.RELAÇÃO
  V(grafos[[i]])$frequencia_de_contatos = atributos[[i]]$FREQUENCIA.DE.CONTADOS.NO.MES
}

#Plotando com atributos
plot(grafos[[6]], vertex.color = (V(grafos[[6]])$preso)+2,
     vertex.size = as.numeric(V(grafos[[6]])$frequencia_de_contatos),
     edge.arrow.size=.2)
title(nomes_arquivos[6])

###########################
# Calculando a proporção de conhecidos presos e não-presos
atributos[[1]]$PRESO[atributos[[1]]$PRESO==0] = 2

prop_presos <- c()
for (i in 1:length(atributos)){
  mat <- freq(atributos[[i]]$PRESO,plot=F)
  x = mat[1,2]
  prop_presos[i] = x
}
prop_presos # Tem viés!


#########################################
### Algumas análises a nível individual
mais_central_grau = c()
mais_central_between = c()
mais_central_constraint = c()

#for (grafo in grafos){
#  grau = degree(grafo)
#  inter = betweenness(grafo)
#  const = constraint(grafo)
  
  
#}

#primeiro_cent_grau <- sapply(grafos, pega_centrais, FUN=degree, rank=1)

grau = degree(g1)
class(which.max(grau))
grau[22]






