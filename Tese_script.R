### Análises tese ####
library(tidyverse)
library(readxl)
library(igraph)
library(arsenal)
library(expss)
library(RColorBrewer)

# Carregando banco de dados no R
Dados_ser_ <- readxl::read_xlsx("HSE_DOC_SERSO_2.xlsx")
Dados_AP_ <- readxl::read_xlsx("HSE_AP.xlsx") 
Dados_mau_ <- readxl::read_xlsx("HSE_DOC_MAU_2.xlsx")
Dados_Odo <- readxl::read_xlsx("HSE_DOC_Odonto.xlsx")

# retirada da linha 33 por ser repetida com a linha 35 (Natalia)
Dados_AP_ <- Dados_AP_ %>% slice(-33)

# transformações dos dados de AP para padronizar com SC
Dados_AP_2 <- Dados_AP_ %>% rename("1ª Amizade" = "1ª amizade", 
                                   "2ª Amizade" = "2ª amizade",
                                   "3ª Amizade" = "3ª amizade",
                                   "4ª Amizade" = "4ª amizade",
                                   "5ª Amizade" = "5ª amizade",
                                   "1º Distanciamento" = "1ª distanciamento",
                                   "2º Distanciamento" = "2ª distanciamento",
                                   "3º Distanciamento" = "3ª distanciamento",
                                   "4º Distanciamento" = "4ª distanciamento",
                                   "5º Distanciamento" = "5ª distanciamento")


# transformações dos dados do mau para padronizar com SC
Dados_mau_2 <- Dados_mau_ %>% rename("1ª Amizade" = "1ª Proximidade", 
                                   "2ª Amizade" = "2ª Proximidade",
                                   "3ª Amizade" = "3ª Proximidade",
                                   "4ª Amizade" = "4ª Proximidade",
                                   "5ª Amizade" = "5ª Proximidade")


# Unir todos os bancos----
df_geral <- rbind(Dados_AP_2, Dados_ser, Dados_mau_2)

# Correção de respostas dos sujeitos de psicologia RV_03, pois houve uma duplicação.
recode(df_geral[, 48]) <- c("Primavera"~"Médico", "Férias"~"Hospital", "Sol"~"Doença", "Verão"~"Dor", "Ventilador"~"Coração")

# rename de raciocínio verbal
df_geral <- df_geral %>% rename(RV_01 = 46,
                                RV_02 = 47,
                                RV_03 = 48,
                                RV_04 = 49,
                                RV_05 = 50,
                                RV_06 = 51,
                                RV_07 = 52,
                                RV_08 = 53,
                                RV_09 = 54,
                                RV_10 = 55,
                                RV_11 = 56,
                                RV_12 = 57)


# Correção do raciocínio verbal
df_geral$rv01 <- ifelse(df_geral$RV_01 == "Dia",1,0)
df_geral$rv02 <- ifelse(df_geral$RV_02 == "Verão",1,0) 
df_geral$rv03 <- ifelse(df_geral$RV_03 == "Doença",1,0) 
df_geral$rv04 <- ifelse(df_geral$RV_04 == "Derrota",1,0) 
df_geral$rv05 <- ifelse(df_geral$RV_05 == "Certeza",1,0) 
df_geral$rv06 <- ifelse(df_geral$RV_06 == "Vender",1,0) 
df_geral$rv07 <- ifelse(df_geral$RV_07 == "Literatura",1,0) 
df_geral$rv08 <- ifelse(df_geral$RV_08 == "Cume",1,0) 
df_geral$rv09 <- ifelse(df_geral$RV_09 == "Futuro",1,0) 
df_geral$rv10 <- ifelse(df_geral$RV_10 == "Precoce",1,0) 
df_geral$rv11 <- ifelse(df_geral$RV_11 == "Avaliar",1,0) 
df_geral$rv12 <- ifelse(df_geral$RV_12 == "Perfume",1,0) 

# escore total
df_geral$rvtotal <- df_geral %>% select(123:134) %>% rowSums()

########################## ANÁLISE DE REDES PSICOLOGIA (Amizade) ######################################


df_ap <- Dados_AP_ %>% select(4, 75:89)

ars_ap_1 <- df_ap %>% select(1,2) %>% rename(Amizade = "1ª amizade")
ars_ap_1$Peso <- 5
ars_ap_2 <- df_ap %>% select(1,3) %>% rename(Amizade = "2ª amizade")
ars_ap_2$Peso <- 4
ars_ap_3 <- df_ap %>% select(1,4) %>% rename(Amizade = "3ª amizade")
ars_ap_3$Peso <- 3
ars_ap_4 <- df_ap %>% select(1,5) %>% rename(Amizade = "4ª amizade")
ars_ap_4$Peso <- 2
ars_ap_5 <- df_ap %>% select(1,6) %>% rename(Amizade = "5ª amizade")
ars_ap_5$Peso <- 1

ars_ap_all <- rbind(ars_ap_1, ars_ap_2, ars_ap_3, ars_ap_4, ars_ap_5)

# retirada das linhas com Na's
ars_ap_all <- na.omit(ars_ap_all)

# Elaboração do grafo da turma de avaliação psicológica
AP <- graph_from_data_frame(ars_ap_all, directed = TRUE, vertices = NULL)
AP

plot(AP, vertex.color = "gold", vertex.label.color = "black", vertex.label.cex = 0.8, edge.arrow.size=.4,
     edge.curved=0.2)

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


# Nível de centralidade
AP_centralidade <- degree(AP, mode = c("in")) %>% as.data.frame() %>% rename(Grau_geral = ".")
AP_centralidade$Proximidade <- evcent(AP)$vector %>% as.data.frame() 
AP_centralidade$Intermediação <- betweenness(AP, directed = TRUE) 

# Colocando as métricas no grafo
V(AP)$degree <- AP_deg
V(AP)$eig <- AP_deg
V(AP)$bw <- AP_bw


plot(AP, 
     vertex.color = c("gold", "skyblue")[1+(V(AP)$gender=="M")], 
     vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     #vertex.size = sqrt(V(AP)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(AP)$weight, 
     layout = layout.fruchterman.reingold)



plot(AP, 
     vertex.color = c("gold", "skyblue")[1+(V(AP)$gender=="M")], 
     vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     edge.arrow.size=.1/2,
     edge.curved=0.2,  
     edge.width = E(AP)$weight, 
     layout = layout_components)



#### DISTANCIAMENTO PSICOLOGIA 
ars_ap_d_1 <- df_ap %>% select(1,7) %>% rename(Distância = "1ª distanciamento")
ars_ap_d_1$Peso <- 5
ars_ap_d_2 <- df_ap %>% select(1,8) %>% rename(Distância = "2ª distanciamento")
ars_ap_d_2$Peso <- 4
ars_ap_d_3 <- df_ap %>% select(1,9) %>% rename(Distância = "3ª distanciamento")
ars_ap_d_3$Peso <- 3
ars_ap_d_4 <- df_ap %>% select(1,10) %>% rename(Distância = "4ª distanciamento")
ars_ap_d_4$Peso <- 2
ars_ap_d_5 <- df_ap %>% select(1,11) %>% rename(Distância = "5ª distanciamento")
ars_ap_d_5$Peso <- 1

ars_ap_d_all <- rbind(ars_ap_d_1,
                      ars_ap_d_2,
                      ars_ap_d_3,
                      ars_ap_d_4,
                      ars_ap_d_5)
  
ars_ap_d_all <- na.omit(ars_ap_d_all)

AP_d <- graph_from_data_frame(ars_ap_d_all, directed = TRUE, vertices = NULL)


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


# Nível de centralidade
AP_d_centralidade <- degree(AP_d, mode = c("in")) %>% as.data.frame() %>% rename(Grau_geral = ".")
AP_d_centralidade$Proximidade <- evcent(AP_d)$vector %>% as.data.frame() # EIGENVECTOR CENTRALITY
AP_d_centralidade$Intermediação <- betweenness(AP_d, directed = TRUE) # Betweenness centrality

# Colocando as métricas no grafo
V(AP_d)$degree <- AP_d_deg
V(AP_d)$eig <- AP_d_eig
V(AP_d)$bw <- AP_d_bw


plot(AP_d, 
     vertex.color = c("gold", "skyblue")[1+(V(AP_d)$gender=="M")], 
     #vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     #vertex.size = sqrt(V(AP)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(AP_d)$weight, 
     layout = layout.fruchterman.reingold)

###### PROFISSIONAL PSICOLOGIA
ars_ap_p_1 <- df_ap %>% select(1,12) %>% rename(Profissional = "1º Profissional")
ars_ap_p_1$Peso <- 5
ars_ap_p_2 <- df_ap %>% select(1,13) %>% rename(Profissional = "2º Profissional")
ars_ap_p_2$Peso <- 4
ars_ap_p_3 <- df_ap %>% select(1,14) %>% rename(Profissional = "3º Profissional")
ars_ap_p_3$Peso <- 3
ars_ap_p_4 <- df_ap %>% select(1,15) %>% rename(Profissional = "4º Profissional")
ars_ap_p_4$Peso <- 2
ars_ap_p_5 <- df_ap %>% select(1,16) %>% rename(Profissional = "5º Profissional")
ars_ap_p_5$Peso <- 1

ars_ap_p_all <- rbind(ars_ap_p_1,
                      ars_ap_p_2,
                      ars_ap_p_3,
                      ars_ap_p_4,
                      ars_ap_p_5)

ars_ap_p_all <- na.omit(ars_ap_p_all)

AP_p <- graph_from_data_frame(ars_ap_p_all, directed = TRUE, vertices = NULL)


E(AP_p)$weight <- ars_ap_p_all$Peso
V(AP_p)$gender <- c("M", "M", "M", "F", 
                    "F", "F", "F", "F", 
                    "M", "F", "M", "F", 
                    "M", "F", "M", "F", 
                    "F", "M", "F", "F", 
                    "F", "M", "M", "F", 
                    "F", "F", "M", "F",
                    "M", "F", "F", "M", 
                    "F", "M", "F", "F", 
                    "F")

# Nível de centralidade
AP_p_centralidade <- degree(AP_p, mode = c("in")) %>% as.data.frame() %>% rename(Grau_geral = ".")
AP_p_centralidade$Proximidade <- evcent(AP_p)$vector %>% as.data.frame() # EIGENVECTOR CENTRALITY
AP_p_centralidade$Intermediação <- betweenness(AP_p, directed = TRUE) # Betweenness centrality
AP_p_centralidade %>% 


# Colocando as métricas no grafo
V(AP_p)$degree <- AP_centralidade$Grau_geral
V(AP_p)$eig <- AP_centralidade$Proximidade
V(AP_p)$bw <- AP_centralidade$Intermediação


plot(AP_p, 
     vertex.color = c("gold", "skyblue")[1+(V(AP_p)$gender=="M")], 
     #vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     #vertex.size = sqrt(V(AP)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(AP_p)$weight, 
     layout = layout.fruchterman.reingold)







# Usar o pacote arsenal

## retirar os escores e tirar as precisões



## Análise de redes Serviço Social(Amizade) -----

df_ser <- Dados_ser %>% select(4, 75:89)

df_ser_1 <- df_ser %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
df_ser_1$Peso <- 5
df_ser_2 <- df_ser %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
df_ser_2$Peso <- 4
df_ser_3 <- df_ser %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
df_ser_3$Peso <- 3
df_ser_4 <- df_ser %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
df_ser_4$Peso <- 2
df_ser_5 <- df_ser %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
df_ser_5$Peso <- 1

df_ser_all <- rbind(df_ser_1, df_ser_2, df_ser_3, df_ser_4, df_ser_5)

df_ser_all_2 <- df_ser_all %>% slice(1:133, 135:138, 140:174, 177:178, 181:188, 191:194, 196:205)

SER_ <- graph_from_data_frame(df_ser_all_2, directed = TRUE, vertices = NULL)

V(SER_)$gender <- c("F", "F", "F", 
                    "M", "F", "F", 
                    "F", "F", "F", 
                    "F", "M", "F", 
                    "F", "F", "F", 
                    "F", "F", "M", 
                    "F", "F", "M", 
                    "F", "F", "F", 
                    "F", "F", "F", 
                    "F", "F", "F", 
                    "F", "F", "F", 
                    "F", "F", "F", 
                    "F", "F", "M", 
                    "F", "F", "M", 
                    "F", "F", "F",
                    "F")

# Atributo dos sujeitos sem nomes
V(SER_)$suj <- paste("s", 1:46, sep = "")

plot(SER_, 
     vertex.color = c("gold", "skyblue")[1+(V(SER_)$gender=="M")], 
     vertex.label = V(SER_)$suj,
     vertex.label.color = "black",
     vertex.label.cex = 0.8, 
     #vertex.size = sqrt(V(SER_)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(SER_)$weight, 
     layout = layout.fruchterman.reingold)

## Escrevendo uma linha para teste no git



