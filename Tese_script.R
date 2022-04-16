### Análises tese ####
library(tidyverse)
library(readxl)
library(igraph)
library(arsenal)
library(expss)
library(RColorBrewer)
library(lmtest)
library(rstatix)
library(stats)
library(psych)
library(ggplot2)
#library(MASS)

# Carregando banco de dados no R   ---------------------------------
Dados_ser_ <- readxl::read_xlsx("HSE_DOC_SERSO_2.xlsx") #1
Dados_ser_2 <- readxl::read_xlsx("HSE_DOC_SERV_2022.xlsx") #2
Dados_AP_ <- readxl::read_xlsx("HSE_AP.xlsx") #3
Dados_mau_ <- readxl::read_xlsx("HSE_DOC_MAU_2.xlsx") #4
Dados_Odo <- readxl::read_xlsx("HSE_DOC_Odonto.xlsx") #5
Dados_AP_2022 <- readxl::read_xlsx("HSE_DOC_PSI_2022.xlsx") #6
Dados_Petrolina_1 <- readxl::read_xlsx("HSE_DOC_DEB_UNIVASF_1.xlsx") #7
Dados_Petrolina_3 <- readxl::read_xlsx("HSE_DOC_DEB_UNIVASF_3.xlsx") #8

# retirada da linha 33 por ser repetida com a linha 35 (Natalia)
Dados_AP_ <- Dados_AP_ %>% slice(-33)

# transformações dos dados de AP para padronizar com SC
Dados_AP_ <- Dados_AP_ %>% rename("1ª Amizade" = "1ª amizade", 
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
Dados_mau_ <- Dados_mau_ %>% rename("1ª Amizade" = "1ª Proximidade", 
                                    "2ª Amizade" = "2ª Proximidade",
                                    "3ª Amizade" = "3ª Proximidade",
                                    "4ª Amizade" = "4ª Proximidade",
                                    "5ª Amizade" = "5ª Proximidade")


# Unir todos os bancos     ------------------------------
df_geral <- rbind(Dados_AP_, Dados_ser_, Dados_ser_2, 
                  Dados_mau_, Dados_Odo, Dados_AP_2022, 
                  Dados_Petrolina_1, Dados_Petrolina_3)


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


## Correção do raciocínio verbal ----
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


## Correção competências emocionais ----

# F1/F2/F3/F4/F5/FG
# INVERSÃO DOS ITENS 3, 16 e 28

# FATOR 1 (8 ITENS) - 1,5,8,15,23,26,29,31  -- >>> +11
# FATOR 2 (7 ITENS) - 4,11,14,16,21,24,27
# FATOR 3 (4 ITENS) - 6,18,28,32
# FATOR 4 (7 ITENS) - 2,7,10,12,19,22,33
# FATOR 5 (8 ITENS) - 3,9,13,17,20,25,30,34

#
df_geral <- df_geral %>% rename(C3 = 'Por mais que tente, não consigo controlar a expressão do que estou sentindo.')
df_geral <- df_geral %>% rename(C16 = 'Frustrações deixam-me desanimado/a por bastante tempo.')
df_geral <- df_geral %>% rename(C28 = 'Tenho vergonha de expressar os meus sentimentos.')



df_geral$C3_inv <- ifelse(df_geral$C3 %in% c(5), 1,
                   ifelse(df_geral$C3 %in% c(4), 2,
                   ifelse(df_geral$C3 %in% c(3), 3,
                   ifelse(df_geral$C3 %in% c(2), 4,
                   ifelse(df_geral$C3 %in% c(1), 5, df_geral$C3)))))

df_geral$C16_inv <- ifelse(df_geral$C16 %in% c(5), 1,
                    ifelse(df_geral$C16 %in% c(4), 2,
                    ifelse(df_geral$C16 %in% c(3), 3,
                    ifelse(df_geral$C16 %in% c(2), 4,
                    ifelse(df_geral$C16 %in% c(1), 5, df_geral$C16)))))

df_geral$C28_inv <- ifelse(df_geral$C28 %in% c(5), 1,
                    ifelse(df_geral$C28 %in% c(4), 2,
                    ifelse(df_geral$C28 %in% c(3), 3,
                    ifelse(df_geral$C28 %in% c(2), 4,
                    ifelse(df_geral$C28 %in% c(1), 5, df_geral$C28)))))



df_geral$CF1 <- df_geral %>% select(12,16,19,26,34,37,40,42) %>% rowMeans()
df_geral$CF2 <- df_geral %>% select(15,22,25,137,32,35,38) %>% rowMeans()
df_geral$CF3 <- df_geral %>% select(17,19,138,43) %>% rowMeans()
df_geral$CF4 <- df_geral %>% select(13,18,21,23,30,33,44) %>% rowMeans()
df_geral$CF5 <- df_geral %>% select(136,20,24,28,31,36,45) %>% rowMeans()
df_geral$C_GERAL <- df_geral %>% select(139:143) %>% rowMeans()

### Correção HSE adultos -----
names(df_geral) %>% as.data.frame()
## Detalhamento do HSE
# H1,H2,H3,H4,H5
# INVERSÃO DOS ITENS 9,15,17,20,24,26,29

# H1(6 ITENS) - (3,7,8,9*,16,17*)
# H2(6 ITENS) - (1,2,6,12,15*,20*)
# H3(6 ITENS) - (24*,26*,27,28,29*,30) 
# H4(6 ITENS) - (4,10,13,18,19,22)
# H5(6 ITENS) - (5,11,14,21,23,25) 

# Fator 1 - neuroticismo 
# itens invertidos - 98, 106
df_geral$H98_inv <- ifelse(df_geral$`Controlo meus sentimentos.` %in% c(5), 1,
                    ifelse(df_geral$`Controlo meus sentimentos.` %in% c(4), 2,
                    ifelse(df_geral$`Controlo meus sentimentos.` %in% c(3), 3,
                    ifelse(df_geral$`Controlo meus sentimentos.` %in% c(2), 4,
                    ifelse(df_geral$`Controlo meus sentimentos.` %in% c(1), 5, df_geral$`Controlo meus sentimentos.`)))))

df_geral$H106_inv <- ifelse(df_geral$`Sou calmo(a) e controlo bem meu estresse.` %in% c(5), 1,
                     ifelse(df_geral$`Sou calmo(a) e controlo bem meu estresse.` %in% c(4), 2,
                     ifelse(df_geral$`Sou calmo(a) e controlo bem meu estresse.` %in% c(3), 3,
                     ifelse(df_geral$`Sou calmo(a) e controlo bem meu estresse.` %in% c(2), 4,
                     ifelse(df_geral$`Sou calmo(a) e controlo bem meu estresse.` %in% c(1), 5, df_geral$`Sou calmo(a) e controlo bem meu estresse.`)))))



## Fator 2 - inverter: 104,109
df_geral$H104_inv <- ifelse(df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.` %in% c(5), 1,
                     ifelse(df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.` %in% c(4), 2,
                     ifelse(df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.` %in% c(3), 3,
                     ifelse(df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.` %in% c(2), 4,
                     ifelse(df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.` %in% c(1), 5, 
                            df_geral$`Sou meio desleixado(a), não tenho cuidado na hora de fazer as coisas.`)))))

df_geral$H109_inv <- ifelse(df_geral$`Costumo ser desorganizado(a).` %in% c(5), 1,
                     ifelse(df_geral$`Costumo ser desorganizado(a).` %in% c(4), 2,
                     ifelse(df_geral$`Costumo ser desorganizado(a).` %in% c(3), 3,
                     ifelse(df_geral$`Costumo ser desorganizado(a).` %in% c(2), 4,
                     ifelse(df_geral$`Costumo ser desorganizado(a).` %in% c(1), 5, 
                            df_geral$`Costumo ser desorganizado(a).`)))))






## Fator 3 - Inverter: 113,115,118
df_geral$H113_inv <- ifelse(df_geral$`Costumo ser quieto(a).` %in% c(5), 1,
                     ifelse(df_geral$`Costumo ser quieto(a).` %in% c(4), 2,
                     ifelse(df_geral$`Costumo ser quieto(a).` %in% c(3), 3,
                     ifelse(df_geral$`Costumo ser quieto(a).` %in% c(2), 4,
                     ifelse(df_geral$`Costumo ser quieto(a).` %in% c(1), 5, 
                            df_geral$`Costumo ser quieto(a).`)))))

df_geral$H115_inv <- ifelse(df_geral$`Sou reservado(a), fico mais na minha.` %in% c(5), 1,
                     ifelse(df_geral$`Sou reservado(a), fico mais na minha.` %in% c(4), 2,
                     ifelse(df_geral$`Sou reservado(a), fico mais na minha.` %in% c(3), 3,
                     ifelse(df_geral$`Sou reservado(a), fico mais na minha.` %in% c(2), 4,
                     ifelse(df_geral$`Sou reservado(a), fico mais na minha.` %in% c(1), 5, 
                            df_geral$`Sou reservado(a), fico mais na minha.`)))))

df_geral$H118_inv <- ifelse(df_geral$`Sou tímido(a), inibido(a).` %in% c(5), 1,
                     ifelse(df_geral$`Sou tímido(a), inibido(a).` %in% c(4), 2,
                     ifelse(df_geral$`Sou tímido(a), inibido(a).` %in% c(3), 3,
                     ifelse(df_geral$`Sou tímido(a), inibido(a).` %in% c(2), 4,
                     ifelse(df_geral$`Sou tímido(a), inibido(a).` %in% c(1), 5, 
                            df_geral$`Sou tímido(a), inibido(a).`)))))

## Fator 4

## Fator 5

## Escores médios HSE
df_geral$HF1 <- df_geral %>% select(145,92,96,97,105,146) %>% rowMeans()
df_geral$HF2 <- df_geral %>% select(90,91,95,101,147,148) %>% rowMeans()
df_geral$HF3 <- df_geral %>% select(149,150,116,117,151,119) %>% rowMeans()
df_geral$HF4 <- df_geral %>% select(93,99,102,107,108,111) %>% rowMeans()
df_geral$HF5 <- df_geral %>% select(94,100,103,110,112,114) %>% rowMeans()
df_geral$H_GERAL <- df_geral %>% select(152:156) %>% rowMeans()


## Correção metas para realização -----

df_geral$MF1 <- df_geral %>% select(58:64, 71:74) %>% rowMeans()
df_geral$MF2 <- df_geral %>% select(65:70) %>% rowMeans()
df_geral$MF_GERAL <- df_geral %>% select(158, 159) %>% rowMeans()

########################## ANÁLISE DE REDES PSICOLOGIA (Amizade) ######################################


df_ap <- Dados_AP_ %>% select(4, 75:89)

ars_ap_1 <- df_ap %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
ars_ap_1$Peso <- 5
ars_ap_2 <- df_ap %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
ars_ap_2$Peso <- 4
ars_ap_3 <- df_ap %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
ars_ap_3$Peso <- 3
ars_ap_4 <- df_ap %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
ars_ap_4$Peso <- 2
ars_ap_5 <- df_ap %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
ars_ap_5$Peso <- 1

ars_ap_all <- rbind(ars_ap_1, ars_ap_2, ars_ap_3, ars_ap_4, ars_ap_5)

rm(ars_ap_1, ars_ap_2, ars_ap_3, ars_ap_4, ars_ap_5)

# retirada das linhas com Na's
ars_ap_all <- na.omit(ars_ap_all)

# Elaboração do grafo da turma de avaliação psicológica
AP <- graph_from_data_frame(ars_ap_all, directed = TRUE, vertices = NULL)
AP



# Nível de centralidade
AP_centralidade <- data.frame(Nome = V(AP)$name, Grau_Entrada = degree(AP, mode = c("in")))
AP_centralidade$Centralidade_distancia <- closeness(AP, mode = "all") 
AP_centralidade$Proximidade <- evcent(AP)$vector 
AP_centralidade$Intermediação <- betweenness(AP, directed = TRUE) 
# Nível de autoridade
AP_centralidade$Autoridade <- authority_score(AP)$vector

AP_centralidade <- AP_centralidade %>% arrange(Nome)


## Identificando comunidades
## não funcionou muito bem
teste <- as.undirected(AP, mode = "collapse", 
                       edge.attr.comb = list(weight = "sum", "ignore"))
teste_2 <- cluster_edge_betweenness(teste)
teste_3 <- cluster_edge_betweenness(AP)

plot(teste_3, AP)


#### DISTANCIAMENTO PSICOLOGIA 
ars_ap_d_1 <- df_ap %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
ars_ap_d_1$Peso <- 5
ars_ap_d_2 <- df_ap %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
ars_ap_d_2$Peso <- 4
ars_ap_d_3 <- df_ap %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
ars_ap_d_3$Peso <- 3
ars_ap_d_4 <- df_ap %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
ars_ap_d_4$Peso <- 2
ars_ap_d_5 <- df_ap %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
ars_ap_d_5$Peso <- 1

ars_ap_d_all <- rbind(ars_ap_d_1,
                      ars_ap_d_2,
                      ars_ap_d_3,
                      ars_ap_d_4,
                      ars_ap_d_5)

rm(ars_ap_d_1,
   ars_ap_d_2,
   ars_ap_d_3,
   ars_ap_d_4,
   ars_ap_d_5)
  
ars_ap_d_all <- na.omit(ars_ap_d_all)

AP_d <- graph_from_data_frame(ars_ap_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_d_centralidade <- data.frame(Nome = V(AP_d)$name, Grau_Entrada_D = degree(AP_d, mode = c("in")))
AP_d_centralidade$Centralidade_distancia_D <- closeness(AP_d, mode = "all") 
AP_d_centralidade$Proximidade_D <- evcent(AP_d)$vector 
AP_d_centralidade$Intermediação_D <- betweenness(AP_d, directed = TRUE) 
AP_d_centralidade$Autoridade_D <- authority_score(AP_d)$vector

AP_d_centralidade <- AP_d_centralidade %>% arrange(Nome)


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

rm(ars_ap_p_1,
   ars_ap_p_2,
   ars_ap_p_3,
   ars_ap_p_4,
   ars_ap_p_5)

ars_ap_p_all <- na.omit(ars_ap_p_all)

AP_p <- graph_from_data_frame(ars_ap_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_p_centralidade <- data.frame(Nome = V(AP_p)$name, Grau_Entrada_P = degree(AP_p, mode = c("in")))
AP_p_centralidade$Centralidade_distancia_P <- closeness(AP_p, mode = "all") 
AP_p_centralidade$Proximidade_P <- evcent(AP_p)$vector  # EIGENVECTOR CENTRALITY
AP_p_centralidade$Intermediação_P <- betweenness(AP_p, directed = TRUE) 

# Nível de autoridade
AP_p_centralidade$Autoridade_P <- authority_score(AP_p)$vector

AP_p_centralidade <- AP_p_centralidade %>% arrange(Nome)


#### UNIR MÉTRICAS PSICOLOGIA

# Retirada de Marcella e de Wesley
AP_d_centralidade <- AP_d_centralidade %>% slice(-20, -39)

AP_centralidade_geral <- inner_join(AP_centralidade, AP_p_centralidade, by = "Nome")
AP_centralidade_geral <- inner_join(AP_centralidade_geral, AP_d_centralidade, by = "Nome")

########################## ANÁLISE DE REDES PSICOLOGIA_MAU (Amizade) ######################################
names(Dados_mau_)

## Retirar Gabriela da análise

df_ap_mau <- Dados_mau_ %>% select(4, 75:89)

ars_ap_mau_1 <- df_ap_mau %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
ars_ap_mau_1$Peso <- 5
ars_ap_mau_2 <- df_ap_mau %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
ars_ap_mau_2$Peso <- 4
ars_ap_mau_3 <- df_ap_mau %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
ars_ap_mau_3$Peso <- 3
ars_ap_mau_4 <- df_ap_mau %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
ars_ap_mau_4$Peso <- 2
ars_ap_mau_5 <- df_ap_mau %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
ars_ap_mau_5$Peso <- 1

ars_ap_mau_all <- rbind(ars_ap_mau_1, ars_ap_mau_2, ars_ap_mau_3, ars_ap_mau_4, ars_ap_mau_5)

rm(ars_ap_mau_1, ars_ap_mau_2, ars_ap_mau_3, ars_ap_mau_4, ars_ap_mau_5)

# retirada das linhas com Na's
ars_ap_mau_all <- na.omit(ars_ap_mau_all)

# Elaboração do grafo da turma de avaliação psicológica
AP_mau <- graph_from_data_frame(ars_ap_mau_all, directed = TRUE, vertices = NULL)




# Nível de centralidade
AP_mau_centralidade <- data.frame(Nome = V(AP_mau)$name, Grau_Entrada = degree(AP_mau, mode = c("in")))
AP_mau_centralidade$Centralidade_distancia <- closeness(AP_mau, mode = "all") 
AP_mau_centralidade$Proximidade <- evcent(AP_mau)$vector 
AP_mau_centralidade$Intermediação <- betweenness(AP_mau, directed = TRUE) 
# Nível de autoridade
AP_mau_centralidade$Autoridade <- authority_score(AP_mau)$vector

AP_mau_centralidade <- AP_mau_centralidade %>% arrange(Nome)


#### DISTANCIAMENTO PSICOLOGIA MAU
ars_ap_mau_d_1 <- df_ap_mau %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
ars_ap_mau_d_1$Peso <- 5
ars_ap_mau_d_2 <- df_ap_mau %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
ars_ap_mau_d_2$Peso <- 4
ars_ap_mau_d_3 <- df_ap_mau %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
ars_ap_mau_d_3$Peso <- 3
ars_ap_mau_d_4 <- df_ap_mau %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
ars_ap_mau_d_4$Peso <- 2
ars_ap_mau_d_5 <- df_ap_mau %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
ars_ap_mau_d_5$Peso <- 1

ars_ap_mau_d_all <- rbind(ars_ap_mau_d_1,
                      ars_ap_mau_d_2,
                      ars_ap_mau_d_3,
                      ars_ap_mau_d_4,
                      ars_ap_mau_d_5)

rm(ars_ap_mau_d_1,
   ars_ap_mau_d_2,
   ars_ap_mau_d_3,
   ars_ap_mau_d_4,
   ars_ap_mau_d_5)

ars_ap_mau_d_all <- na.omit(ars_ap_mau_d_all)

AP_mau_d <- graph_from_data_frame(ars_ap_mau_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_mau_d_centralidade <- data.frame(Nome = V(AP_mau_d)$name, Grau_Entrada_D = degree(AP_mau_d, mode = c("in")))
AP_mau_d_centralidade$Centralidade_distancia_D <- closeness(AP_mau_d, mode = "all") 
AP_mau_d_centralidade$Proximidade_D <- evcent(AP_mau_d)$vector 
AP_mau_d_centralidade$Intermediação_D <- betweenness(AP_mau_d, directed = TRUE) 
AP_mau_d_centralidade$Autoridade_D <- authority_score(AP_mau_d)$vector

AP_mau_d_centralidade <- AP_mau_d_centralidade %>% arrange(Nome)


###### PROFISSIONAL PSICOLOGIA
ars_ap_mau_p_1 <- df_ap_mau %>% select(1,12) %>% rename(Profissional = "1º Profissional")
ars_ap_mau_p_1$Peso <- 5
ars_ap_mau_p_2 <- df_ap_mau %>% select(1,13) %>% rename(Profissional = "2º Profissional")
ars_ap_mau_p_2$Peso <- 4
ars_ap_mau_p_3 <- df_ap_mau %>% select(1,14) %>% rename(Profissional = "3º Profissional")
ars_ap_mau_p_3$Peso <- 3
ars_ap_mau_p_4 <- df_ap_mau %>% select(1,15) %>% rename(Profissional = "4º Profissional")
ars_ap_mau_p_4$Peso <- 2
ars_ap_mau_p_5 <- df_ap_mau %>% select(1,16) %>% rename(Profissional = "5º Profissional")
ars_ap_mau_p_5$Peso <- 1

ars_ap_mau_p_all <- rbind(ars_ap_mau_p_1,
                      ars_ap_mau_p_2,
                      ars_ap_mau_p_3,
                      ars_ap_mau_p_4,
                      ars_ap_mau_p_5)

rm(ars_ap_mau_p_1,
   ars_ap_mau_p_2,
   ars_ap_mau_p_3,
   ars_ap_mau_p_4,
   ars_ap_mau_p_5)

ars_ap_mau_p_all <- na.omit(ars_ap_mau_p_all)

AP_mau_p <- graph_from_data_frame(ars_ap_mau_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_mau_p_centralidade <- data.frame(Nome = V(AP_mau_p)$name, Grau_Entrada_P = degree(AP_mau_p, mode = c("in")))
AP_mau_p_centralidade$Centralidade_distancia_P <- closeness(AP_mau_p, mode = "all") 
AP_mau_p_centralidade$Proximidade_P <- evcent(AP_mau_p)$vector  # EIGENVECTOR CENTRALITY
AP_mau_p_centralidade$Intermediação_P <- betweenness(AP_mau_p, directed = TRUE) 

# Nível de autoridade
AP_mau_p_centralidade$Autoridade_P <- authority_score(AP_mau_p)$vector

AP_mau_p_centralidade <- AP_mau_p_centralidade %>% arrange(Nome)


#### UNIR MÉTRICAS PSICOLOGIA

AP_mau_centralidade_geral <- inner_join(AP_mau_centralidade, AP_mau_p_centralidade, AP_mau_d_centralidade, by = "Nome")
AP_mau_centralidade_geral <- inner_join(AP_mau_centralidade_geral, AP_mau_d_centralidade, by = "Nome")

# Usar o pacote arsenal


########### ANÁLISE DE REDES SERVIÇO SOCIAL (Amizade) -----

# IMPORTANTE
# Situações que o respondente não desejava marcar todas as opções de amizade foi marcada de modo repetido. 
# Retirar as repetições e colocar Na no lugar.

df_ser <- Dados_ser_ %>% select(4, 75:89)


df_ser$`4ª Amizade` <- ifelse(df_ser$`3ª Amizade` == df_ser$`4ª Amizade`, NA, df_ser$`4ª Amizade`)
df_ser$`5ª Amizade` <- ifelse(df_ser$`4ª Amizade` == df_ser$`5ª Amizade`, NA, df_ser$`5ª Amizade`)
df_ser$`2º Distanciamento` <- ifelse(df_ser$`1º Distanciamento` == df_ser$`2º Distanciamento`, NA, df_ser$`2º Distanciamento`)
df_ser$`3º Distanciamento` <- ifelse(df_ser$`2º Distanciamento` == df_ser$`3º Distanciamento`, NA, df_ser$`3º Distanciamento`)
df_ser$`4º Distanciamento` <- ifelse(df_ser$`3º Distanciamento` == df_ser$`4º Distanciamento`, NA, df_ser$`4º Distanciamento`)
df_ser$`5º Distanciamento` <- ifelse(df_ser$`4º Distanciamento` == df_ser$`5º Distanciamento`, NA, df_ser$`5º Distanciamento`)                              
df_ser$`5º Profissional` <- ifelse(df_ser$`4º Profissional` == df_ser$`5º Profissional`, NA, df_ser$`5º Profissional`)                            
                     
                      
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

df_ser_all <- na.omit(df_ser_all)

rm(df_ser_1, df_ser_2, df_ser_3, df_ser_4, df_ser_5)

SER_ <- graph_from_data_frame(df_ser_all, directed = TRUE, vertices = NULL)



# Nível de centralidade
SER_centralidade <- data.frame(Nome = V(SER_)$name, Grau_Entrada = degree(SER_, mode = c("in")))
SER_centralidade$Centralidade_distancia <- closeness(SER_, mode = "all") 
SER_centralidade$Proximidade <- evcent(SER_)$vector 
SER_centralidade$Intermediação <- betweenness(SER_, directed = TRUE) 

# Nível de autoridade
SER_centralidade$Autoridade <- authority_score(SER_)$vector

SER_centralidade <- SER_centralidade %>% arrange(Nome)

### Distância SERVIÇO SOCIAL

df_d_ser_1 <- df_ser %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
df_d_ser_1$Peso <- 5
df_d_ser_2 <- df_ser %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
df_d_ser_2$Peso <- 4
df_d_ser_3 <- df_ser %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
df_d_ser_3$Peso <- 3
df_d_ser_4 <- df_ser %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
df_d_ser_4$Peso <- 2
df_d_ser_5 <- df_ser %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
df_d_ser_5$Peso <- 1

df_d_ser_all <- rbind(df_d_ser_1, df_d_ser_2, df_d_ser_3, df_d_ser_4, df_d_ser_5)

rm(df_d_ser_1, df_d_ser_2, df_d_ser_3, df_d_ser_4, df_d_ser_5)

df_d_ser_all <- na.omit(df_d_ser_all)

SER_d <- graph_from_data_frame(df_d_ser_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
SER_d_centralidade <- data.frame(Nome = V(SER_d)$name, Grau_Entrada_D = degree(SER_d, mode = c("in")))
SER_d_centralidade$Centralidade_distancia_D <- closeness(SER_d, mode = "all") 
SER_d_centralidade$Proximidade_D <- evcent(SER_d)$vector 
SER_d_centralidade$Intermediação_D <- betweenness(SER_d, directed = TRUE) 

# Nível de autoridade
SER_d_centralidade$Autoridade_D <- authority_score(SER_d)$vector

SER_d_centralidade <- SER_d_centralidade %>% arrange(Nome)

## Jean e Jessica Menezes

## Serviço Social - Profissional

df_p_ser_1 <- df_ser %>% select(1,12) %>% rename(Profissional = "1º Profissional")
df_p_ser_1$Peso <- 5
df_p_ser_2 <- df_ser %>% select(1,13) %>% rename(Profissional = "2º Profissional")
df_p_ser_2$Peso <- 4
df_p_ser_3 <- df_ser %>% select(1,14) %>% rename(Profissional = "3º Profissional")
df_p_ser_3$Peso <- 3
df_p_ser_4 <- df_ser %>% select(1,15) %>% rename(Profissional = "4º Profissional")
df_p_ser_4$Peso <- 2
df_p_ser_5 <- df_ser %>% select(1,16) %>% rename(Profissional = "5º Profissional")
df_p_ser_5$Peso <- 1

df_p_ser_all <- rbind(df_p_ser_1,df_p_ser_2,df_p_ser_3,df_p_ser_4,df_p_ser_5)

rm(df_p_ser_1,df_p_ser_2,df_p_ser_3,df_p_ser_4,df_p_ser_5)

df_p_ser_all <- na.omit(df_p_ser_all)

SER_p <- graph_from_data_frame(df_p_ser_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
SER_p_centralidade <- data.frame(Nome = V(SER_p)$name, Grau_Entrada_P = degree(SER_p, mode = c("in")))
SER_p_centralidade$Centralidade_distancia_P <- closeness(SER_p, mode = "all") 
SER_p_centralidade$Proximidade_P <- evcent(SER_p)$vector 
SER_p_centralidade$Intermediação_P <- betweenness(SER_p, directed = TRUE) 

# Nível de autoridade
SER_p_centralidade$Autoridade_P <- authority_score(SER_p)$vector
SER_p_centralidade <- SER_p_centralidade %>% arrange(Nome)

#### UNIR MÉTRICAS SERVIÇO SOCIAL

## Observações
# 42 pessoas responderam ao teste, porém 47 pessoas foram citadas em todos os momentos. 2 pessoas foram citadas apenas em distância
# Retirar Jean e Jessica Menezes

# Retirada de Marcella e de Wesley
SER_d_centralidade <- SER_d_centralidade %>% slice(-20, -23)
SER_p_centralidade <- SER_p_centralidade %>% slice(-20)

SER_centralidade_geral <- inner_join(SER_centralidade, SER_p_centralidade, SER_d_centralidade, by = "Nome")
SER_centralidade_geral <- inner_join(SER_centralidade_geral, SER_d_centralidade, by = "Nome")

########### ANÁLISE DE REDES SERVIÇO SOCIAL 2022 -----
# AMIZADE
df_ser_2022 <- Dados_ser_2 %>% select(4, 75:89)

df_ser_2022$`1ª Amizade` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`1ª Amizade`, NA, df_ser_2022$`1ª Amizade`)
df_ser_2022$`2ª Amizade` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`2ª Amizade`, NA, df_ser_2022$`2ª Amizade`)
df_ser_2022$`3ª Amizade` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`3ª Amizade`, NA, df_ser_2022$`3ª Amizade`)
df_ser_2022$`4ª Amizade` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`4ª Amizade`, NA, df_ser_2022$`4ª Amizade`)
df_ser_2022$`5ª Amizade` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`5ª Amizade`, NA, df_ser_2022$`5ª Amizade`)
                             
df_ser_2022$`1º Profissional` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`1º Profissional`, NA, df_ser_2022$`1º Profissional`)
df_ser_2022$`2º Profissional` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`2º Profissional`, NA, df_ser_2022$`2º Profissional`)
df_ser_2022$`3º Profissional` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`3º Profissional`, NA, df_ser_2022$`3º Profissional`)
df_ser_2022$`4º Profissional` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`4º Profissional`, NA, df_ser_2022$`4º Profissional`)
df_ser_2022$`5º Profissional` <- ifelse(df_ser_2022$`Seu nome` == df_ser_2022$`5º Profissional`, NA, df_ser_2022$`5º Profissional`)


df_ser_2022_1 <- df_ser_2022 %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
df_ser_2022_1$Peso <- 5
df_ser_2022_2 <- df_ser_2022 %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
df_ser_2022_2$Peso <- 4
df_ser_2022_3 <- df_ser_2022 %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
df_ser_2022_3$Peso <- 3
df_ser_2022_4 <- df_ser_2022 %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
df_ser_2022_4$Peso <- 2
df_ser_2022_5 <- df_ser_2022 %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
df_ser_2022_5$Peso <- 1

df_ser_2022_all <- rbind(df_ser_2022_1, df_ser_2022_2, df_ser_2022_3, df_ser_2022_4, 
                    df_ser_2022_5)

df_ser_2022_all <- na.omit(df_ser_2022_all)

rm(df_ser_2022_1, df_ser_2022_2, df_ser_2022_3, df_ser_2022_4, 
   df_ser_2022_5)

SER_2022 <- graph_from_data_frame(df_ser_2022_all, directed = TRUE, vertices = NULL)



# Nível de centralidade
SER_2022_centralidade <- data.frame(Nome = V(SER_2022)$name, Grau_Entrada = degree(SER_2022))
SER_2022_centralidade$Centralidade_distancia <- closeness(SER_2022, mode = "all") 
SER_2022_centralidade$Proximidade <- evcent(SER_2022)$vector 
SER_2022_centralidade$Intermediação <- betweenness(SER_2022, directed = TRUE) 

# Nível de autoridade
SER_2022_centralidade$Autoridade <- authority_score(SER_2022)$vector

# SER_2022_centralidade <- SER_2022_centralidade %>% arrange(Nome)

### Distância SERVIÇO SOCIAL

df_d_ser_2022_1 <- df_ser_2022 %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
df_d_ser_2022_1$Peso <- 5
df_d_ser_2022_2 <- df_ser_2022 %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
df_d_ser_2022_2$Peso <- 4
df_d_ser_2022_3 <- df_ser_2022 %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
df_d_ser_2022_3$Peso <- 3
df_d_ser_2022_4 <- df_ser_2022 %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
df_d_ser_2022_4$Peso <- 2
df_d_ser_2022_5 <- df_ser_2022 %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
df_d_ser_2022_5$Peso <- 1

df_d_ser_2022_all <- rbind(df_d_ser_2022_1, df_d_ser_2022_2, df_d_ser_2022_3, df_d_ser_2022_4, 
                      df_d_ser_2022_5)

rm(df_d_ser_2022_1, df_d_ser_2022_2, df_d_ser_2022_3, df_d_ser_2022_4, 
   df_d_ser_2022_5)

df_d_ser_2022_all <- na.omit(df_d_ser_2022_all)

SER_2022_d <- graph_from_data_frame(df_d_ser_2022_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
SER_2022_d_centralidade <- data.frame(Nome = V(SER_2022_d)$name, Grau_Entrada_D = degree(SER_2022_d, mode = c("in")))
SER_2022_d_centralidade$Centralidade_distancia_D <- closeness(SER_2022_d, mode = "all") 
SER_2022_d_centralidade$Proximidade_D <- evcent(SER_2022_d)$vector 
SER_2022_d_centralidade$Intermediação_D <- betweenness(SER_2022_d, directed = TRUE) 

# Nível de autoridade
SER_2022_d_centralidade$Autoridade_D <- authority_score(SER_2022_d)$vector

# SER_d_centralidade <- SER_d_centralidade %>% arrange(Nome)

## Serviço Social - Profissional

df_p_ser_2022_1 <- df_ser_2022 %>% select(1,12) %>% rename(Profissional = "1º Profissional")
df_p_ser_2022_1$Peso <- 5
df_p_ser_2022_2 <- df_ser_2022 %>% select(1,13) %>% rename(Profissional = "2º Profissional")
df_p_ser_2022_2$Peso <- 4
df_p_ser_2022_3 <- df_ser_2022 %>% select(1,14) %>% rename(Profissional = "3º Profissional")
df_p_ser_2022_3$Peso <- 3
df_p_ser_2022_4 <- df_ser_2022 %>% select(1,15) %>% rename(Profissional = "4º Profissional")
df_p_ser_2022_4$Peso <- 2
df_p_ser_2022_5 <- df_ser_2022 %>% select(1,16) %>% rename(Profissional = "5º Profissional")
df_p_ser_2022_5$Peso <- 1

df_p_ser_2022_all <- rbind(df_p_ser_2022_1,df_p_ser_2022_2,df_p_ser_2022_3,df_p_ser_2022_4,
                      df_p_ser_2022_5)

rm(f_p_ser_2022_1,df_p_ser_2022_2,df_p_ser_2022_3,df_p_ser_2022_4,
   df_p_ser_2022_5)

df_p_ser_2022_all <- na.omit(df_p_ser_2022_all)

SER_2022_p <- graph_from_data_frame(df_p_ser_2022_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
SER_2022_p_centralidade <- data.frame(Nome = V(SER_2022_p)$name, Grau_Entrada_P = degree(SER_2022_p, mode = c("in")))
SER_2022_p_centralidade$Centralidade_distancia_P <- closeness(SER_2022_p, mode = "all") 
SER_2022_p_centralidade$Proximidade_P <- evcent(SER_2022_p)$vector 
SER_2022_p_centralidade$Intermediação_P <- betweenness(SER_2022_p, directed = TRUE) 

# Nível de autoridade
SER_2022_p_centralidade$Autoridade_P <- authority_score(SER_2022_p)$vector
# SER_2022_p_centralidade <- SER_p_centralidade %>% arrange(Nome)

#### UNIR MÉTRICAS SERVIÇO SOCIAL 2022

SER_2022_centralidade_geral <- inner_join(SER_2022_centralidade, SER_2022_p_centralidade, SER_2022_d_centralidade, by = "Nome")
SER_2022_centralidade_geral <- inner_join(SER_2022_centralidade_geral, SER_2022_d_centralidade, by = "Nome")



####### ANÁLISE DE REDES ODONTO (Rede Amizade) ----
df_odo <- Dados_Odo %>% select(4, 75:89)
Dados_Odo <- Dados_Odo %>% rename(Idade = "Idade (apenas números)")

Dados_Odo_1 <- df_odo %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
Dados_Odo_1$Peso <- 5
Dados_Odo_2 <- df_odo %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
Dados_Odo_2$Peso <- 4
Dados_Odo_3 <- df_odo %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
Dados_Odo_3$Peso <- 3
Dados_Odo_4 <- df_odo %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
Dados_Odo_4$Peso <- 2
Dados_Odo_5 <- df_odo %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
Dados_Odo_5$Peso <- 1

Dados_Odo_all <- rbind(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

rm(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

Dados_Odo_all <- na.omit(Dados_Odo_all)

ODO_ <- graph_from_data_frame(Dados_Odo_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
ODO_centralidade <- data.frame(Nome = V(ODO_)$name, Grau_Entrada = degree(ODO_, mode = c("in")))
ODO_centralidade$Centralidade_distancia <- closeness(ODO_, mode = "all") 
ODO_centralidade$Proximidade <- evcent(ODO_)$vector 
ODO_centralidade$Intermediação <- betweenness(ODO_, directed = TRUE) 

# Nível de autoridade
ODO_centralidade$Autoridade <- authority_score(ODO_)$vector
ODO_centralidade <- ODO_centralidade %>% arrange(Nome)

### Odonto - Distância ----

Dados_Odo_1 <- df_odo %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
Dados_Odo_1$Peso <- 5
Dados_Odo_2 <- df_odo %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
Dados_Odo_2$Peso <- 4
Dados_Odo_3 <- df_odo %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
Dados_Odo_3$Peso <- 3
Dados_Odo_4 <- df_odo %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
Dados_Odo_4$Peso <- 2
Dados_Odo_5 <- df_odo %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
Dados_Odo_5$Peso <- 1

Dados_Odo_d_all <- rbind(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

rm(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

Dados_Odo_d_all <- na.omit(Dados_Odo_d_all)

ODO_d <- graph_from_data_frame(Dados_Odo_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
ODO_d_centralidade <- data.frame(Nome = V(ODO_d)$name, Grau_Entrada_D = degree(ODO_d, mode = c("in")))
ODO_d_centralidade$Centralidade_distancia_D <- closeness(ODO_d, mode = "all") 
ODO_d_centralidade$Proximidade_D <- evcent(ODO_d)$vector 
ODO_d_centralidade$Intermediação_D <- betweenness(ODO_d, directed = TRUE) 

# Nível de autoridade
ODO_d_centralidade$Autoridade_D <- authority_score(ODO_d)$vector
ODO_d_centralidade <- ODO_d_centralidade %>% arrange(Nome)

## Odonto - Profissional ----

Dados_Odo_1 <- df_odo %>% select(1,12) %>% rename(Profissional = "1º Profissional")
Dados_Odo_1$Peso <- 5
Dados_Odo_2 <- df_odo %>% select(1,13) %>% rename(Profissional = "2º Profissional")
Dados_Odo_2$Peso <- 4
Dados_Odo_3 <- df_odo %>% select(1,14) %>% rename(Profissional = "3º Profissional")
Dados_Odo_3$Peso <- 3
Dados_Odo_4 <- df_odo %>% select(1,15) %>% rename(Profissional = "4º Profissional")
Dados_Odo_4$Peso <- 2
Dados_Odo_5 <- df_odo %>% select(1,16) %>% rename(Profissional = "5º Profissional")
Dados_Odo_5$Peso <- 1

Dados_Odo_p_all <- rbind(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

rm(Dados_Odo_1, Dados_Odo_2, Dados_Odo_3, Dados_Odo_4, Dados_Odo_5)

Dados_Odo_p_all <- na.omit(Dados_Odo_p_all)

ODO_p <- graph_from_data_frame(Dados_Odo_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
ODO_p_centralidade <- data.frame(Nome = V(ODO_p)$name, Grau_Entrada_P = degree(ODO_p, mode = c("in")))
ODO_p_centralidade$Centralidade_distancia_P <- closeness(ODO_p, mode = "all") 
ODO_p_centralidade$Proximidade_P <-  evcent(ODO_p)$vector 
ODO_p_centralidade$Intermediação_P<- betweenness(ODO_p, directed = TRUE) 

# Nível de autoridade
ODO_p_centralidade$Autoridade_P <- authority_score(ODO_p)$vector
ODO_p_centralidade <- ODO_p_centralidade %>% arrange(Nome)

## Unir Odonto

ODO_centralidade_geral <- inner_join(ODO_centralidade, ODO_p_centralidade, ODO_d_centralidade, by = "Nome")
ODO_centralidade_geral <- inner_join(ODO_centralidade_geral, ODO_d_centralidade, by = "Nome")


### ANÁLISE DE REDES PSICOLOGIA_2022 #####

df_ap_2022 <- Dados_AP_2022 %>% select(4, 75:89)

ars_ap_2022_1 <- df_ap_2022 %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
ars_ap_2022_1$Peso <- 5
ars_ap_2022_2 <- df_ap_2022 %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
ars_ap_2022_2$Peso <- 4
ars_ap_2022_3 <- df_ap_2022 %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
ars_ap_2022_3$Peso <- 3
ars_ap_2022_4 <- df_ap_2022 %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
ars_ap_2022_4$Peso <- 2
ars_ap_2022_5 <- df_ap_2022 %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
ars_ap_2022_5$Peso <- 1

ars_ap_2022_all <- rbind(ars_ap_2022_1,
                    ars_ap_2022_2,
                    ars_ap_2022_3,
                    ars_ap_2022_4,
                    ars_ap_2022_5)

rm(ars_ap_2022_1,
   ars_ap_2022_1,
   ars_ap_2022_2,
   ars_ap_2022_2,
   ars_ap_2022_3,
   ars_ap_2022_3,
   ars_ap_2022_4,
   ars_ap_2022_4,
   ars_ap_2022_5,
   ars_ap_2022_5)

# retirada das linhas com Na's
ars_ap_2022_all <- na.omit(ars_ap_2022_all)

# Elaboração do grafo da turma de avaliação psicológica
AP_2022 <- graph_from_data_frame(ars_ap_2022_all, directed = TRUE, vertices = NULL)
AP_2022



# Nível de centralidade
AP_2022_centralidade <- data.frame(Nome = V(AP_2022)$name, Grau_Entrada = degree(AP_2022, mode = c("in")))
AP_2022_centralidade$Centralidade_distancia <- closeness(AP_2022, mode = "all") 
AP_2022_centralidade$Proximidade <- evcent(AP_2022)$vector 
AP_2022_centralidade$Intermediação <- betweenness(AP_2022, directed = TRUE) 
# Nível de autoridade
AP_2022_centralidade$Autoridade <- authority_score(AP_2022)$vector

#AP_2022_centralidade <- AP_2022_centralidade %>% arrange(Nome)

AP_2022_dis <- degree.distribution(AP_2022, cumulative = FALSE)

V(AP_2022)$degree <- AP_2022_centralidade$Grau_Entrada
V(AP_2022)$name
#### DISTANCIAMENTO PSICOLOGIA 
ars_ap_2022_d_1 <- df_ap_2022 %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
ars_ap_2022_d_1$Peso <- 5
ars_ap_2022_d_2 <- df_ap_2022 %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
ars_ap_2022_d_2$Peso <- 4
ars_ap_2022_d_3 <- df_ap_2022 %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
ars_ap_2022_d_3$Peso <- 3
ars_ap_2022_d_4 <- df_ap_2022 %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
ars_ap_2022_d_4$Peso <- 2
ars_ap_2022_d_5 <- df_ap_2022 %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
ars_ap_2022_d_5$Peso <- 1

ars_ap_2022_d_all <- rbind(ars_ap_2022_d_1,
                      ars_ap_2022_d_2,
                      ars_ap_2022_d_3,
                      ars_ap_2022_d_4,
                      ars_ap_2022_d_5)

rm(ars_ap_2022_d_1,
   ars_ap_2022_d_2,
   ars_ap_2022_d_3,
   ars_ap_2022_d_4,
   ars_ap_2022_d_5)

ars_ap_2022_d_all <- na.omit(ars_ap_2022_d_all)

AP_2022_d <- graph_from_data_frame(ars_ap_2022_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_2022_d_centralidade <- data.frame(Nome = V(AP_2022_d)$name, Grau_Entrada_D = degree(AP_2022_d, mode = c("in")))
AP_2022_d_centralidade$Centralidade_distancia_D <- closeness(AP_2022_d, mode = "all") 
AP_2022_d_centralidade$Proximidade_D <- evcent(AP_2022_d)$vector 
AP_2022_d_centralidade$Intermediação_D <- betweenness(AP_2022_d, directed = TRUE) 
AP_2022_d_centralidade$Autoridade_D <- authority_score(AP_2022_d)$vector

AP_2022_d_centralidade <- AP_2022_d_centralidade %>% arrange(Nome)


###### PROFISSIONAL PSICOLOGIA
ars_ap_2022_p_1 <- df_ap_2022 %>% select(1,12) %>% rename(Profissional = "1º Profissional")
ars_ap_2022_p_1$Peso <- 5
ars_ap_2022_p_2 <- df_ap_2022 %>% select(1,13) %>% rename(Profissional = "2º Profissional")
ars_ap_2022_p_2$Peso <- 4
ars_ap_2022_p_3 <- df_ap_2022 %>% select(1,14) %>% rename(Profissional = "3º Profissional")
ars_ap_2022_p_3$Peso <- 3
ars_ap_2022_p_4 <- df_ap_2022 %>% select(1,15) %>% rename(Profissional = "4º Profissional")
ars_ap_2022_p_4$Peso <- 2
ars_ap_2022_p_5 <- df_ap_2022 %>% select(1,16) %>% rename(Profissional = "5º Profissional")
ars_ap_2022_p_5$Peso <- 1

ars_ap_2022_p_all <- rbind(ars_ap_2022_p_1,
                      ars_ap_2022_p_2,
                      ars_ap_2022_p_3,
                      ars_ap_2022_p_4,
                      ars_ap_2022_p_5)

rm(ars_ap_2022_p_1,
   ars_ap_2022_p_2,
   ars_ap_2022_p_3,
   ars_ap_2022_p_4,
   ars_ap_2022_p_5)

ars_ap_2022_p_all <- na.omit(ars_ap_2022_p_all)

AP_2022_p <- graph_from_data_frame(ars_ap_2022_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
AP_2022_p_centralidade <- data.frame(Nome = V(AP_2022_p)$name, Grau_Entrada_P = degree(AP_2022_p, mode = c("in")))
AP_2022_p_centralidade$Centralidade_distancia_P <- closeness(AP_2022_p, mode = "all") 
AP_2022_p_centralidade$Proximidade_P <- evcent(AP_2022_p)$vector  # EIGENVECTOR CENTRALITY
AP_2022_p_centralidade$Intermediação_P <- betweenness(AP_2022_p, directed = TRUE) 

# Nível de autoridade
AP_2022_p_centralidade$Autoridade_P <- authority_score(AP_2022_p)$vector

AP_2022_p_centralidade <- AP_2022_p_centralidade %>% arrange(Nome)


#### UNIR MÉTRICAS PSICOLOGIA

AP_2022_centralidade_geral <- inner_join(AP_2022_centralidade, AP_2022_p_centralidade, by = "Nome")
AP_2022_centralidade_geral <- inner_join(AP_2022_centralidade_geral, AP_2022_d_centralidade, by = "Nome")


### ANÁLISE DE REDES PSICOLOGIA_PETROLINA 1 #####

df_pet1 <- Dados_Petrolina_1 %>% select(4, 75:89)

ars_pet1_1 <- df_pet1 %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
ars_pet1_1$Peso <- 5
ars_pet1_2 <- df_pet1 %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
ars_pet1_2$Peso <- 4
ars_pet1_3 <- df_pet1 %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
ars_pet1_3$Peso <- 3
ars_pet1_4 <- df_pet1 %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
ars_pet1_4$Peso <- 2
ars_pet1_5 <- df_pet1 %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
ars_pet1_5$Peso <- 1

ars_pet1_all <- rbind(ars_pet1_1,
                      ars_pet1_2,
                      ars_pet1_3,
                      ars_pet1_4,
                      ars_pet1_5)

rm(ars_pet1_1,
   ars_pet1_2,
   ars_pet1_3,
   ars_pet1_4,
   ars_pet1_5)

# retirada das linhas com Na's
ars_pet1_all <- na.omit(ars_pet1_all)

# Elaboração do grafo da turma de avaliação psicológica
PET1 <- graph_from_data_frame(ars_pet1_all, directed = TRUE, vertices = NULL)
PET1



# Nível de centralidade
PET1_centralidade <- data.frame(Nome = V(PET1)$name, Grau_Entrada = degree(PET1, mode = c("in")))
PET1_centralidade$Centralidade_distancia <- closeness(PET1, mode = "all") 
PET1_centralidade$Proximidade <- evcent(PET1)$vector 
PET1_centralidade$Intermediação <- betweenness(PET1, directed = TRUE) 
# Nível de autoridade
PET1_centralidade$Autoridade <- authority_score(PET1)$vector

#AP_2022_centralidade <- AP_2022_centralidade %>% arrange(Nome)

#PET1_dis <- degree.distribution(PET1, cumulative = FALSE)

V(PET1)$degree <- PET1_centralidade$Grau_Entrada
V(PET1)$name
#### DISTANCIAMENTO PSICOLOGIA 
ars_pet1_d_1 <- df_pet1 %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
ars_pet1_d_1$Peso <- 5
ars_pet1_d_2 <- df_pet1 %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
ars_pet1_d_2$Peso <- 4
ars_pet1_d_3 <- df_pet1 %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
ars_pet1_d_3$Peso <- 3
ars_pet1_d_4 <- df_pet1 %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
ars_pet1_d_4$Peso <- 2
ars_pet1_d_5 <- df_pet1 %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
ars_pet1_d_5$Peso <- 1

ars_pet1_d_all <- rbind(ars_pet1_d_1,
                           ars_pet1_d_2,
                           ars_pet1_d_3,
                           ars_pet1_d_4,
                           ars_pet1_d_5)

rm(ars_pet1_d_1,
   ars_pet1_d_2,
   ars_pet1_d_3,
   ars_pet1_d_4,
   ars_pet1_d_5)

ars_pet1_d_all <- na.omit(ars_pet1_d_all)

PET1_d <- graph_from_data_frame(ars_pet1_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
PET1_d_centralidade <- data.frame(Nome = V(PET1_d)$name, Grau_Entrada_D = degree(PET1_d, mode = c("in")))
PET1_d_centralidade$Centralidade_distancia_D <- closeness(PET1_d, mode = "all") 
PET1_d_centralidade$Proximidade_D <- evcent(PET1_d)$vector 
PET1_d_centralidade$Intermediação_D <- betweenness(PET1_d, directed = TRUE) 
PET1_d_centralidade$Autoridade_D <- authority_score(PET1_d)$vector

# AP_2022_d_centralidade <- AP_2022_d_centralidade %>% arrange(Nome)


###### PROFISSIONAL PSICOLOGIA
ars_pet1_p_1 <- df_pet1 %>% select(1,12) %>% rename(Profissional = "1º Profissional")
ars_pet1_p_1$Peso <- 5
ars_pet1_p_2 <- df_pet1 %>% select(1,13) %>% rename(Profissional = "2º Profissional")
ars_pet1_p_2$Peso <- 4
ars_pet1_p_3 <- df_pet1 %>% select(1,14) %>% rename(Profissional = "3º Profissional")
ars_pet1_p_3$Peso <- 3
ars_pet1_p_4 <- df_pet1 %>% select(1,15) %>% rename(Profissional = "4º Profissional")
ars_pet1_p_4$Peso <- 2
ars_pet1_p_5 <- df_pet1 %>% select(1,16) %>% rename(Profissional = "5º Profissional")
ars_pet1_p_5$Peso <- 1

ars_pet1_p_all <- rbind(ars_pet1_p_1,
                       ars_pet1_p_2,
                       ars_pet1_p_3,
                       ars_pet1_p_4,
                       ars_pet1_p_5)

rm(ars_pet1_p_1,
   ars_pet1_p_2,
   ars_pet1_p_3,
   ars_pet1_p_4,
   ars_pet1_p_5)

ars_pet1_p_all <- na.omit(ars_pet1_p_all)

PET1_p <- graph_from_data_frame(ars_pet1_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
PET1_p_centralidade <- data.frame(Nome = V(PET1_p)$name, Grau_Entrada_P = degree(PET1_p, mode = c("in")))
PET1_p_centralidade$Centralidade_distancia_P <- closeness(PET1_p, mode = "all") 
PET1_p_centralidade$Proximidade_P <- evcent(PET1_p)$vector  # EIGENVECTOR CENTRALITY
PET1_p_centralidade$Intermediação_P <- betweenness(PET1_p, directed = TRUE) 

# Nível de autoridade
PET1_p_centralidade$Autoridade_P <- authority_score(PET1_p)$vector

# PET1_p_centralidade <- AP_2022_p_centralidade %>% arrange(Nome)


#### UNIR MÉTRICAS PSICOLOGIA

PET1_centralidade_geral <- inner_join(PET1_centralidade, PET1_p_centralidade, by = "Nome")
PET1_centralidade_geral <- inner_join(PET1_centralidade_geral, PET1_d_centralidade, by = "Nome")

### ANÁLISE DE REDES PSICOLOGIA_PETROLINA 3 #####

df_pet3 <- Dados_Petrolina_3 %>% select(4, 75:89)

ars_pet3_1 <- df_pet3 %>% select(1,2) %>% rename(Amizade = "1ª Amizade")
ars_pet3_1$Peso <- 5
ars_pet3_2 <- df_pet3 %>% select(1,3) %>% rename(Amizade = "2ª Amizade")
ars_pet3_2$Peso <- 4
ars_pet3_3 <- df_pet3 %>% select(1,4) %>% rename(Amizade = "3ª Amizade")
ars_pet3_3$Peso <- 3
ars_pet3_4 <- df_pet3 %>% select(1,5) %>% rename(Amizade = "4ª Amizade")
ars_pet3_4$Peso <- 2
ars_pet3_5 <- df_pet3 %>% select(1,6) %>% rename(Amizade = "5ª Amizade")
ars_pet3_5$Peso <- 1

ars_pet3_all <- rbind(ars_pet3_1,
                      ars_pet3_2,
                      ars_pet3_3,
                      ars_pet3_4,
                      ars_pet3_5)

rm(ars_pet3_1,
   ars_pet3_2,
   ars_pet3_3,
   ars_pet3_4,
   ars_pet3_5)

# retirada das linhas com Na's
ars_pet3_all <- na.omit(ars_pet3_all)

# Elaboração do grafo da turma de avaliação psicológica
PET3 <- graph_from_data_frame(ars_pet3_all, directed = TRUE, vertices = NULL)
PET3



# Nível de centralidade
PET3_centralidade <- data.frame(Nome = V(PET3)$name, Grau_Entrada = degree(PET3, mode = c("in")))
PET3_centralidade$Centralidade_distancia <- closeness(PET3, mode = "all") 
PET3_centralidade$Proximidade <- evcent(PET3)$vector 
PET3_centralidade$Intermediação <- betweenness(PET3, directed = TRUE) 
# Nível de autoridade
PET3_centralidade$Autoridade <- authority_score(PET3)$vector

#AP_2022_centralidade <- AP_2022_centralidade %>% arrange(Nome)

#PET1_dis <- degree.distribution(PET1, cumulative = FALSE)

V(PET3)$degree <- PET3_centralidade$Grau_Entrada
V(PET3)$name
#### DISTANCIAMENTO PSICOLOGIA 
ars_pet3_d_1 <- df_pet3 %>% select(1,7) %>% rename(Distância = "1º Distanciamento")
ars_pet3_d_1$Peso <- 5
ars_pet3_d_2 <- df_pet3 %>% select(1,8) %>% rename(Distância = "2º Distanciamento")
ars_pet3_d_2$Peso <- 4
ars_pet3_d_3 <- df_pet3 %>% select(1,9) %>% rename(Distância = "3º Distanciamento")
ars_pet3_d_3$Peso <- 3
ars_pet3_d_4 <- df_pet3 %>% select(1,10) %>% rename(Distância = "4º Distanciamento")
ars_pet3_d_4$Peso <- 2
ars_pet3_d_5 <- df_pet3 %>% select(1,11) %>% rename(Distância = "5º Distanciamento")
ars_pet3_d_5$Peso <- 1

ars_pet3_d_all <- rbind(ars_pet3_d_1,
                        ars_pet3_d_2,
                        ars_pet3_d_3,
                        ars_pet3_d_4,
                        ars_pet3_d_5)

rm(ars_pet3_d_1,
   ars_pet3_d_2,
   ars_pet3_d_3,
   ars_pet3_d_4,
   ars_pet3_d_5)

ars_pet3_d_all <- na.omit(ars_pet3_d_all)

PET3_d <- graph_from_data_frame(ars_pet3_d_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
PET3_d_centralidade <- data.frame(Nome = V(PET3_d)$name, Grau_Entrada_D = degree(PET3_d, mode = c("in")))
PET3_d_centralidade$Centralidade_distancia_D <- closeness(PET3_d, mode = "all") 
PET3_d_centralidade$Proximidade_D <- evcent(PET3_d)$vector 
PET3_d_centralidade$Intermediação_D <- betweenness(PET3_d, directed = TRUE) 
PET3_d_centralidade$Autoridade_D <- authority_score(PET3_d)$vector

# AP_2022_d_centralidade <- AP_2022_d_centralidade %>% arrange(Nome)


###### PROFISSIONAL PSICOLOGIA
ars_pet3_p_1 <- df_pet3 %>% select(1,12) %>% rename(Profissional = "1º Profissional")
ars_pet3_p_1$Peso <- 5
ars_pet3_p_2 <- df_pet3 %>% select(1,13) %>% rename(Profissional = "2º Profissional")
ars_pet3_p_2$Peso <- 4
ars_pet3_p_3 <- df_pet3 %>% select(1,14) %>% rename(Profissional = "3º Profissional")
ars_pet3_p_3$Peso <- 3
ars_pet3_p_4 <- df_pet3 %>% select(1,15) %>% rename(Profissional = "4º Profissional")
ars_pet3_p_4$Peso <- 2
ars_pet3_p_5 <- df_pet3 %>% select(1,16) %>% rename(Profissional = "5º Profissional")
ars_pet3_p_5$Peso <- 1

ars_pet3_p_all <- rbind(ars_pet3_p_1,
                        ars_pet3_p_2,
                        ars_pet3_p_3,
                        ars_pet3_p_4,
                        ars_pet3_p_5)

rm(ars_pet3_p_1,
   ars_pet3_p_2,
   ars_pet3_p_3,
   ars_pet3_p_4,
   ars_pet3_p_5)

ars_pet3_p_all <- na.omit(ars_pet3_p_all)

PET3_p <- graph_from_data_frame(ars_pet3_p_all, directed = TRUE, vertices = NULL)


# Nível de centralidade
PET3_p_centralidade <- data.frame(Nome = V(PET3_p)$name, Grau_Entrada_P = degree(PET3_p, mode = c("in")))
PET3_p_centralidade$Centralidade_distancia_P <- closeness(PET3_p, mode = "all") 
PET3_p_centralidade$Proximidade_P <- evcent(PET3_p)$vector  # EIGENVECTOR CENTRALITY
PET3_p_centralidade$Intermediação_P <- betweenness(PET3_p, directed = TRUE) 

# Nível de autoridade
PET3_p_centralidade$Autoridade_P <- authority_score(PET3_p)$vector

# PET1_p_centralidade <- AP_2022_p_centralidade %>% arrange(Nome)


#### UNIR MÉTRICAS PSICOLOGIA

## Novas informacoes

PET3_centralidade_geral <- inner_join(PET3_centralidade, PET3_p_centralidade, by = "Nome")
PET3_centralidade_geral <- inner_join(PET3_centralidade_geral, PET3_d_centralidade, by = "Nome")

############################# Análises Estatísticas ###################################

# Seleção de todos os subfatores e fatores gerais
df_principal <- df_geral %>% select(4,10,120,139:144,152:160)

# Centralidade geral de todas as salas
df_centralidade <- rbind(AP_centralidade_geral, SER_centralidade_geral, SER_2022_centralidade_geral, 
                         ODO_centralidade_geral, AP_mau_centralidade_geral,
                         AP_2022_centralidade_geral, PET3_centralidade_geral,
                         PET1_centralidade_geral)

df_principal <- df_principal %>% rename(Nome = "Seu nome")
df_principal_join <- inner_join(df_centralidade, df_principal, by = "Nome")
df_principal_join <- df_principal_join %>% rename(nota = 'Desempenho no último semestre')
df_principal_join$nota <- ifelse(df_principal_join$nota == "9.50 - 10",19,
                          ifelse(df_principal_join$nota == "9 - 9.49",18,
                          ifelse(df_principal_join$nota == "8.5 - 8.99",17,
                          ifelse(df_principal_join$nota == "8 - 8.49",16,
                          ifelse(df_principal_join$nota == "7.5 - 7.99",15,
                          ifelse(df_principal_join$nota == "7 - 7.49",14,
                          ifelse(df_principal_join$nota == "6.5 - 6.99",13,
                          ifelse(df_principal_join$nota == "6 - 6.49",12,
                          ifelse(df_principal_join$nota == "5.5 - 5.99",11,
                          ifelse(df_principal_join$nota == "3 - 3.49",6,
                          ifelse(df_principal_join$nota == "0 - 1",1, df_principal_join$nota)))))))))))
df_principal_join$nota <- as.numeric(df_principal_join$nota)


glimpse(df_principal_join)
## Análise de regressão

# Fazer análise de correlação antes da regressão.
# Olhar entre salas.

mod <- lm(Centralidade_distancia ~ HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)
mod2 <- lm(Intermediação ~ HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)
mod3 <- lm(C_GERAL ~ Intermediação + Centralidade_distancia, df_principal_join)
mod4 <- lm(Autoridade ~ HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)
mod5 <- lm(Proximidade ~ CF1 + CF2 + CF3 + CF4 + CF5 +
             HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)
mod6 <- lm(Proximidade_P ~ CF1 + CF2 + CF3 + CF4 + CF5 +
             HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)
mod7 <- lm(nota ~ CF1 + CF2 + CF3 + CF4 + CF5 +
             HF1 + HF2 + HF3 + HF4 +HF5 + MF1 + MF2, df_principal_join)



summary(mod) # Hse é significativo
summary(mod2)
summary(mod4) # Hse é significativo
summary(mod5) # Hse é significativo
summary(mod6) # Hse é significativo
summary(mod7) # Quanto menor amabilidade, maior a nota

# Observar multicolinearidade a partir da correlação
df_principal_join %>% select(19:33) %>% pairs.panels()
df_principal_join %>% select(18, 22, 28) %>% pairs.panels()

names(df_principal_join)
range(AP_centralidade_geral$Proximidade)
range(SER_centralidade_geral$Proximidade)

## Possivelmente a regressão multinível será mais adequada por assumir que os valores não são independentes, ou seja,
## há uma relação entre o resultado da pesquisa e a turma que o sujeito faz parte
## análise com efeitos aleatórios 




