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
carros_1 <- carros[,c(4:55)]
dim(carros_1)


wss <- (nrow(carros_1)-1)*sum(apply(carros_1,2,var))

for (i in 2:30) wss[i] <- sum(kmeans(carros_1,
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


### Gráfico da Silhueta

  fviz_nbclust(carros_1, kmeans , method ="silhouette", k.max=30)
  
############ Atividade 3: Agrupamento com DBscan ############  
db_carros<-dbscan::dbscan (carros_1 , eps = 0.15 , minPts =5)
print(db_carros)

dbscan :: kNNdistplot (carros_1 , k =20)

fviz_cluster(db_carros, data=carros_1,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
