# Análises tese 
library(tidyverse)
library(readxl)
library(igraph)
library(arsenal)
library(expss)
library(RColorBrewer)

# Carregando banco de dados no R
Dados_ser <- readxl::read_xlsx("HSE_SERSO.xlsx") 
Dados_AP_ <- readxl::read_xlsx("HSE_AP.xlsx") 

# retirada da linha 33 por ser repetida com a linha 35 (Natalia)
Dados_AP_ <- Dados_AP_ %>% slice(-33)

### Análise de redes Psicologia ----
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
ars_ap_all_2 <- na.omit(ars_ap_all)

# Elaboração do grafo da turma de avaliação psicológica
AP <- graph_from_data_frame(ars_ap_all_2, directed = TRUE, vertices = NULL)
AP

plot(AP, vertex.color = "gold", vertex.label.color = "black", vertex.label.cex = 0.8, edge.arrow.size=.4,
     edge.curved=0.2)

AP
V(AP)$name
E(AP)$weight <- ars_ap_all_2$Peso
V(AP)$gender <- c("M", "M", "M", "F", "F", "F", "F", "F", "M", "F", "M", "F", "M", "F", "M", "F", "F", "M", 
                  "F", "F", "F", "M", "M", "F", "F", "F", "M", "F", "M", "F", "F", "M", "F", "M", "F", "F", "F")

# Atributo dos sujeitos sem nomes
V(AP)$suj <- paste("s", 1:37, sep = "")

# Nível de centralidade
AP_deg <- degree(AP, mode = c("in"))
AP_deg

# EIGENVECTOR CENTRALITY
AP_eig <- evcent(AP)$vector
AP_eig

# Betweenness centrality
AP_bw <- betweenness(AP, directed = TRUE)
AP_bw

# Colocando as métricas no grafo
V(AP)$degree <- AP_deg
V(AP)$eig <- AP_deg
V(AP)$bw <- AP_bw


plot(AP, 
     vertex.color = c("gold", "skyblue")[1+(V(AP)$gender=="M")], 
     vertex.label = V(AP)$suj,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size = sqrt(V(AP)$bw),
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

# Usar o pacote arsenal

## retirar os escores e tirar as precisões

# transformações dos dados de AP para padronizar com o outro banco
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
# Unir todos os bancos
df_geral <- rbind(Dados_AP_2, Dados_ser)

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

## Análise de redes Serviço Social -----

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
     vertex.label.cex = 0.7, 
     #vertex.size = sqrt(V(SER_)$bw),
     edge.arrow.size=.1,
     edge.curved=0.2, 
     edge.width = E(SER_)$weight, 
     layout = layout.fruchterman.reingold)


