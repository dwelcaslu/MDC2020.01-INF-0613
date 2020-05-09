#------------------------------------------------#
# INF-0613 Aprendizado de Maquina nao            #
#          Supervisionado                        #
#                                                #
# Trabalho Avaliativo 3                          #
#------------------------------------------------#
# Nome COMPLETO Aluna (o) 1: Karla Fátima Calvoso#
#                            Simões              #
# Nome COMPLETO Aluna (o) 3: Weld Lucas Cunha    #
#                                                #
#------------------------------------------------#


# Limpando o ambiente antes de iniciar a execução do código:
rm(list = ls())

# Configurando a semente:
set.seed(0)

# Load libraries
library(fpc)
library(cluster)
library(NbClust)
library(factoextra)

# Configurando o diretÃ³rio de trabalho:
setwd("C:\\MDC 2020\\INF-0613 - Aprendizado de Máquina não supervisionado\\Trabalho 3") # configure o caminho antes de descomentar essa linha


############ Carregando os dados ############
carros <- read.csv("imports-85.data", sep=',', header=FALSE)
summary(carros)
dim(carros)
names(carros) <- c("symboling","normalized_losses","make","fuel_type","aspiration","num_of_doors","body_style","drive_wheels","engine_location","wheel_base","length","width","height","curb_weight","engine_type","num_of_cylinders","engine_size","fuel_system","bore","stroke","compression_rate","horsepower","peak_rpm","city_mpg","highway_mpg","price")


############ Atividade 1: Análise e Preparação dos Dados ############
source("Preparando_dados_inf0613-trabalho3.R")
correlation <- cor(carros[4:55]); 
correlation


############ Atividade 2: Agrupamento com K-means ############
### Gráfico Elbow Curve
graf_elbow_curve <- function(df_carros) {
  wss <- (nrow(df_carros)-1)*sum(apply(df_carros,2,var))
 
  for (i in 2:30) wss[i] <- sum(kmeans(df_carros,centers=i)$withinss)
  plot(1:30, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main="Assessing the Optimal Number of Clusters with the Elbow Method",
       pch=20, cex=2)
}

# Retirando apenas as features symboling e make
carros_1 <- carros[,-c(1,3)]
dim(carros_1)
graf_elbow_curve(carros_1)
summary(carros_1)

# Retirando tambem as features wheel_base, length, width e height
carros_2 <- carros[,-c(1,3,4,5,6,7)]
dim(carros_2)
graf_elbow_curve(carros_2) 

# Retirando as features symboling, make e wheel_base
carros_3 <- carros[,-c(1,3,4)]
dim(carros_3)
graf_elbow_curve(carros_3)

# Retirando as features de 1 a 11
carros_4 <- carros[,-c(1:11)]
dim(carros_4)
graf_elbow_curve(carros_4)

### Gráfico da Silhueta

fviz_nbclust(carros_1, kmeans , method ="silhouette", k.max=30)
fviz_nbclust(carros_2, kmeans , method ="silhouette", k.max=30)
fviz_nbclust(carros_3, kmeans , method ="silhouette", k.max=30)
fviz_nbclust(carros_4, kmeans , method ="silhouette", k.max=30)
  
############ Atividade 3: Agrupamento com DBscan ############  
db_carros_1<-dbscan::dbscan (carros_1 , eps = 0.15 , minPts =5)
print(db_carros_1)

dbscan :: kNNdistplot (carros_1 , k =20)

fviz_cluster(db_carros_1, data=carros_1,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())

###################
db_carros_2<-dbscan::dbscan (carros_2 , eps = 0.15 , minPts =5)
print(db_carros_2)

dbscan :: kNNdistplot (carros_2 , k =20)

fviz_cluster(db_carros_2, data=carros_2,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())

###################
db_carros_3<-dbscan::dbscan (carros_3 , eps = 0.15 , minPts =5)
print(db_carros_3)

dbscan :: kNNdistplot (carros_3 , k =20)

fviz_cluster(db_carros_3, data=carros_3,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())

###################
db_carros_4<-dbscan::dbscan (carros_4 , eps = 0.15 , minPts =5)
print(db_carros_4)

dbscan :: kNNdistplot (carros_4 , k =20)

fviz_cluster(db_carros_4, data=carros_4,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
