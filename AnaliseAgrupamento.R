# Instalando Pacotes
install.packages("dplyr")
install.packages("cluster")
install.packages("ggplot2")
library(dplyr)
library(cluster) 
library(ggplot2)

# Transformação das variáveis em categóricas e numérica
med <- med %>% mutate_if(is.numeric,as.factor)
med$IDADE=as.numeric(med$IDADE)
str(med)

# Cálculo da distância de Gower
gower_dist <- daisy(med,
                    metric = "gower",
                    type = list(logratio = 4))

# Cálculo do silhouette width com método PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Gráfico sihouette width (número de clusters)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

# Atribuindo grupos na base de dados
med$grupos <- pam_fit$clustering