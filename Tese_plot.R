V(AP)$name
E(AP)$weight <- ars_ap_all$Peso
V(AP)$gender <- c("M", "M", "M", "F", 
                  "F", "F", "F", "F", 
                  "M", "F", "M", "F", 
                  "M", "F", "M", "F", 
                  "F", "M", "F", "F", 
                  "F", "M", "M", "F", 
                  "F", "F", "M", "F",
                  "M", "F", "F", "M", 
                  "F", "M", "F", "F", 
                  "F")

# Atributo dos sujeitos sem nomes
V(AP)$suj <- paste("s", 1:37, sep = "")

# Colocando as métricas no grafo
V(AP)$aut <-  authority_score(AP)$vector


plot(AP, 
     vertex.color = c("gold", "skyblue")[1+(V(AP)$gender=="M")], 
     vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     #vertex.size = sqrt(V(AP)$nota)*5,
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.color = E(AP)$weight,
     edge.width = E(AP)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Amizade')



plot(AP, 
     vertex.color = c("gold", "skyblue")[1+(V(AP)$gender=="M")], 
     vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     edge.arrow.size=.1/2,
     edge.curved=0.2,  
     edge.width = E(AP)$weight, 
     layout = layout_components)


## Distância

plot(AP_d, vertex.color = "gold", vertex.label.color = "black", vertex.label.cex = 0.6, edge.arrow.size=.4,
     edge.curved=0.2)


E(AP_d)$weight <- ars_ap_d_all$Peso
V(AP_d)$gender <- c("M", "M", "M", "F", 
                    "F", "F", "F", "F", 
                    "M", "F", "M", "F", 
                    "M", "F", "M", "F", 
                    "F", "M", "F", "F", 
                    "F", "M", "M", "F", 
                    "F", "F", "M", "F",
                    "M", "F", "F", "M", 
                    "F", "M", "F", "F", 
                    "F")

# Colocando as métricas no grafo
V(AP_d)$degree <- AP_d_centralidade$Grau_Entrada_D
V(AP_d)$eig <- AP_d_centralidade$Proximidade_D
V(AP_d)$bw <- AP_d_centralidade$Intermediação_D


plot(AP_d, 
     vertex.color = c("gold", "skyblue")[1+(V(AP_d)$gender=="M")], 
     #vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size = V(AP)$bw,
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(AP_d)$weight, 
     layout = layout.fruchterman.reingold)

V(AP_p)$name
E(AP_p)$weight <- ars_ap_p_all$Peso
V(AP_p)$gender <- c("M", "M", "M", "F", 
                    "F", "F", "F", "F", 
                    "M", "F", "F", "M", 
                    "F", "M", "F", "M", 
                    "F", "F", "M", "F", 
                    "F", "M", "M", "F", 
                    "F", "F", "F", "M",
                    "F", "F", "M", "F", 
                    "M", "F", "M", "F", 
                    "F")

# Colocando as métricas no grafo
V(AP_p)$degree <- AP_centralidade$Grau_geral
V(AP_p)$eig <- AP_centralidade$Proximidade
V(AP_p)$bw <- AP_centralidade$Intermediação


plot(AP_p, 
     vertex.color = c("gold", "skyblue")[1+(V(AP_p)$gender=="M")], 
     #vertex.label = V(AP_p)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     #vertex.size = sqrt(V(AP_p)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(AP_p)$weight, 
     layout = layout.fruchterman.reingold)

V(SER_)$name
V(SER_)$gender <- c("F", "F", "F", "M", 
                    "F", "F", "F", "F", 
                    "F", "F", "M", "F", 
                    "F", "F", "F", "F", 
                    "F", "M", "F", "F", 
                    "M", "F", "F", "F", 
                    "F", "F", "F", "F", 
                    "F", "F", "F", "F", 
                    "F", "F", "F", "F", 
                    "F", "F", "M", "F", 
                    "F", "F", "M", "F", 
                    "F", "F", "F")

# Atributo dos sujeitos sem nomes
V(SER_)$suj <- paste("s", 1:47, sep = "")

plot(SER_, 
     vertex.color = c("gold", "skyblue")[1+(V(SER_)$gender=="M")], 
     #vertex.label = V(SER_)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, 
     #vertex.size = sqrt(V(SER_)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(SER_)$weight, 
     layout = layout.fruchterman.reingold)

V(SER_d)$name
V(SER_d)$gender <- c("F", "F", "F", "M", 
                     "F", "F", "F", "F", 
                     "F", "F", "M", "F", 
                     "F", "F", "F", "F", 
                     "F", "M", "F", "F", 
                     "M", "F", "F", "F", 
                     "F", "F", "F", "F", 
                     "F", "F", "F", "F", 
                     "F", "F", "F", "F", 
                     "F", "F", "M", "F", 
                     "F", "F", "M", "F", 
                     "F", "F", "F")

# Atributo dos sujeitos sem nomes
V(SER_d)$suj <- paste("s", 1:49, sep = "")

plot(SER_d, 
     vertex.color = c("gold", "skyblue")[1+(V(SER_d)$gender=="M")], 
     #vertex.label = V(SER_)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, 
     #vertex.size = sqrt(V(SER_)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(SER_d)$weight, 
     layout = layout.fruchterman.reingold)

plot(SER_p, 
     vertex.color = c("gold", "skyblue")[1+(V(SER_p)$gender=="M")], 
     #vertex.label = V(SER_)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, 
     #vertex.size = sqrt(V(SER_)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(SER_p)$weight, 
     layout = layout.fruchterman.reingold)


## AP 2022 ####
AP_2022
E(AP_2022)$weight <- ars_ap_2022_all$Peso
V(AP_2022)$suj <- paste("s", 1:38, sep = "")
V(AP_2022)$name

plot(AP_2022, 
     vertex.color = "pink", 
     vertex.label = V(AP_2022)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, 
     #vertex.size = sqrt(V(AP_2022)$bw),
     edge.arrow.size= 0.4,
     edge.curved=0.2, 
     edge.color = E(AP_2022)$weight,
     edge.width = E(AP_2022)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Amizade')

hist(V(AP_2022)$degree,
     col = 'green')

barplot(AP_2022_dis)



V(AP_2022_p)$suj <- paste("s", 1:36, sep = "")
E(AP_2022_p)$weight <- ars_ap_2022_p_all$Peso
V(AP_2022_p)$name

plot(AP_2022_p, 
     vertex.color = "pink", 
     vertex.label = V(AP_2022_p)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, 
     #vertex.size = sqrt(V(AP_2022)$bw),
     edge.arrow.size= 0.4,
     edge.curved=0.2, 
     edge.color = E(AP_2022_p)$weight,
     edge.width = E(AP_2022_p)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Profissão')


V(AP_2022_d)$suj <- paste("s", 1:39, sep = "")
E(AP_2022_d)$weight <- ars_ap_2022_d_all$Peso
V(AP_2022_d)$name
plot(AP_2022_d, 
     vertex.color = "pink", 
     vertex.label = V(AP_2022_d)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, 
     #vertex.size = sqrt(V(AP_2022)$bw),
     edge.arrow.size= 0.4,
     edge.curved=0.2, 
     edge.color = E(AP_2022_d)$weight,
     edge.width = E(AP_2022_d)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Distância')


## SERV 2022 ####
SER_2022
E(SER_2022)$weight <- df_ser_2022_all$Peso
V(SER_2022)$suj <- paste("s", 1:60, sep = "")
V(SER_2022)$name

plot(SER_2022, 
     vertex.color = "pink", 
     vertex.label = V(SER_2022)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, 
     #vertex.size = sqrt(V(AP_2022)$bw),
     edge.arrow.size= 0.4,
     edge.curved=0.2, 
     edge.color = E(SER_2022)$weight,
     edge.width = E(SER_2022)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Amizade')

SER_2022_p
E(SER_2022_p)$weight <- df_p_ser_2022_all$Peso
V(SER_2022_p)$suj <- paste("s", 1:58, sep = "")
V(SER_2022_p)$name

plot(SER_2022_p, 
     vertex.color = "pink", 
     vertex.label = V(SER_2022_p)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, 
     #vertex.size = sqrt(V(AP_2022)$bw),
     edge.arrow.size= 0.4,
     edge.curved=0.2, 
     edge.color = E(SER_2022_p)$weight,
     edge.width = E(SER_2022_p)$weight, 
     layout = layout.fruchterman.reingold,
     main = 'Rede de Profissão')

