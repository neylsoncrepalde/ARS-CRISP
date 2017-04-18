# Rede agentes penitenciários e presos
# Profa. Ludmila
# Script: Neylson Crepalde
#######################################

library(xlsx)
library(igraph)

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


dados = lapply(arquivos, le_matrizes_xlsx)
###############################################################

g1 <- graph_from_adjacency_matrix(dados[[1]])
plot(g1); title(nomes_arquivos[1])

g2 <- graph_from_adjacency_matrix(dados[[2]])
plot(g2); title(nomes_arquivos[2])

g3 <- graph_from_adjacency_matrix(dados[[3]])
plot(g3); title(nomes_arquivos[3])
v
g4 <- graph_from_adjacency_matrix(dados[[4]])
plot(g4); title(nomes_arquivos[4])

par(mfrow=c(2,2))
plot(g1, edge.arrow.size=.2); title(nomes_arquivos[1])
plot(g2, edge.arrow.size=.2); title(nomes_arquivos[2])
plot(g3, edge.arrow.size=.2); title(nomes_arquivos[3])
plot(g4, edge.arrow.size=.2); title(nomes_arquivos[4])
par(mfrow=c(1,1))
