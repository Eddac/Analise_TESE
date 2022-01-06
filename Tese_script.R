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


# Unir todos os bancos----
df_geral <- rbind(Dados_AP_, Dados_ser_, Dados_mau_, Dados_Odo)


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


## Correção competências emocionais

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

### Correção HSE adultos
names(df_geral)
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


## Correção metas para realização
### ESSE INSTRUMENTO ESTÁ UM POUCO CONFUSO -- ANALISAR POSTERIORMENTE
# Meta aprender
# df_geral$MF1 <- df_geral %>% select(58,59,60,61,62,63,64) %>% rowMeans()

# Meta perfomance aproximação

# Meta perfomance evitação

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

AP_centralidade_geral <- cbind(AP_centralidade, AP_p_centralidade, AP_d_centralidade)


# Usar o pacote arsenal

## retirar os escores e tirar as precisões



########### Análise de redes SERVIÇO SOCIAL (Amizade) -----

df_ser <- Dados_ser_ %>% select(4, 75:89)

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

df_ser <- Dados_ser_ %>% select(4, 75:89)

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

df_ser <- Dados_ser_ %>% select(4, 75:89)

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

SER_centralidade_geral <- cbind(SER_centralidade, SER_p_centralidade, SER_d_centralidade)


#### ODONTO


Dados_Odo <- Dados_Odo %>% rename(Idade = "Idade (apenas números)")

#### Análises Estatísticas

# Dados gerais apenas com AP e Serviço Social
df_principal <- df_geral %>% slice(1:77) %>% select(4,10,120,139:144,152:157)

# Centralidade geral de AP e SS
df_centralidade <- rbind(AP_centralidade_geral, SER_centralidade_geral)

df_principal <- df_principal %>% rename(Nome = "Seu nome")

df_centralidade <- df_centralidade %>% select(-7, -13)

df_principal_join <- inner_join(df_centralidade, df_principal, by = "Nome")



