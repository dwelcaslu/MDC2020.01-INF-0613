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
source("preprocessing.R")

############ Carregando os dados ############
carros <- read.csv("imports-85.data", sep=',', header=FALSE)
names(carros) <- c("symboling", "normalized_losses", "make", "fuel_type", "aspiration", "num_of_doors",
                   "body_style", "drive_wheels", "engine_location", "wheel_base", "length", "width", "height",
                   "curb_weight", "engine_type", "num_of_cylinders", "engine_size", "fuel_system", "bore",
                   "stroke", "compression_rate", "horsepower", "peak_rpm", "city_mpg", "highway_mpg", "price")
summary(carros)
dim(carros)


############ Atividade 1: Análise e Preparação dos Dados ############
# Identificando a coluna que contém o target:
target_col = 1
# Identificando as colunas que não são features:
nonfeat_cols = c(1, 3)

### Tratando os dados incompletos:
carros <- prep_incomplete_data(carros)
### Aplicando one-hot enconding nas features categóricas:
carros <- encode_features(carros)
### Normalizando as features:
carros <- min_max_normalize(carros, nonfeat_cols)
summary(carros)

# Analisando a variância das features:
variances_matrix <- var(carros[-nonfeat_cols])
features_var <- NULL
for (name in row.names(variances_matrix)){
  if (variances_matrix[name, name] > 0.05){
    print(paste(name, variances_matrix[name, name]))
    features_var <- c(features_var, name)
  }
}

# Print the correlation between features:
correlation <- cor(carros[-3]); correlation
# Analisando as correlações das as features e a saída:
max_cor_y <- sort(correlation[correlation[, target_col] < 1, target_col], decreasing=TRUE); print(max_cor_y)
features_cor_y <- names(max_cor_y[abs(max_cor_y) > 0.15])

# Analisando as correlações entre as features:
feat_cor <- correlation[-target_col, -target_col]
for (n in names(max_cor_y)){
  selected_vals <- feat_cor[abs(feat_cor[, n]) < 0.999, n]
  sqr_cor <- selected_vals ^ 2
  max_cor <- selected_vals[sqr_cor == max(sqr_cor)][1]
  max_cor_var <- names(selected_vals)[selected_vals == max_cor]
  print(paste(n, '- max feat_cor (abs.):', round(max_cor, 6), '- with', max_cor_var))
}

# Selecionando as colunas:
# carros_new <- carros[, features_var] 
carros_new <- carros[, features_cor_y]
carros_new$four_doors <- NULL

summary(carros_new)


############ Atividade 2: Agrupamento com K-means ############
### Gráfico Elbow Curve
graf_elbow_curve <- function(df_carros){
  wss <- (nrow(df_carros)-1)*sum(apply(df_carros,2,var))
  for (i in 2:30) wss[i] <- sum(kmeans(df_carros, centers=i, nstart=25)$withinss)
  plot(1:30, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main="Assessing the Optimal Number of Clusters with the Elbow Method",
       pch=20, cex=2)
}

# Retirando apenas as features symboling e make
graf_elbow_curve(carros_new)
### Gráfico da Silhueta
fviz_nbclust(carros_new, kmeans , method ="silhouette", k.max=30)


###################
### Versão final K-means:
###################
carros$cluster <- kmeans(carros_new, centers=6, nstart=25)$cluster
for (c in sort(unique(carros$cluster))){
  select <- carros[carros$cluster == c, ]
  print(paste("cluster:", c, " - symboling(s):"))
  print(sort(unique(select$symboling)))
}

############ Atividade 3: Agrupamento com DBscan ############
## Análise do Raio da Vizinhança de Pontos:
db_carros_1 <- dbscan::dbscan(carros_new , eps=0.5 , minPts=5)
print(db_carros_1)
fviz_cluster(db_carros_1, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
dbscan :: kNNdistplot(carros_new , k=20)
###################
db_carros_2 <- dbscan::dbscan(carros_new , eps=1 , minPts=5)
print(db_carros_2)
fviz_cluster(db_carros_2, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
###################
db_carros_3 <- dbscan::dbscan(carros_new , eps=1.5 , minPts=5)
print(db_carros_3)
fviz_cluster(db_carros_3, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())

## Determinando Ruídos::
###################
db_carros_4 <- dbscan::dbscan(carros_new , eps=0.8 , minPts=2)
print(db_carros_4)
fviz_cluster(db_carros_4, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
###################
db_carros_5 <- dbscan::dbscan(carros_new , eps=0.8 , minPts=5)
print(db_carros_5)
fviz_cluster(db_carros_5, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())
###################
db_carros_6 <- dbscan::dbscan(carros_new , eps=0.8 , minPts=10)
print(db_carros_6)
fviz_cluster(db_carros_6, data=carros_new,stand=FALSE, ellipse=FALSE, show.clust.cent=FALSE,
             geom="point", palette="jco", ggtheme=theme_classic())

###################
### Versão final DBSCAN:
###################
db_carros_final <- dbscan::dbscan (carros_new, eps=0.7, minPts=3)
print(db_carros_final)
carros$cluster <- db_carros_final$cluster; carros$cluster
for (c in sort(unique(carros$cluster))){
  select <- carros[carros$cluster == c, ]
  print(paste("cluster:", c, " - symboling(s):"))
  print(sort(unique(select$symboling)))
} 

for (c in sort(unique(carros$cluster))){
  select <- carros[carros$cluster == c, ]
  print(paste("cluster:", c, " - maker(s):", length(unique(select$make))))
} 
