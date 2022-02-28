#### Análise Fatorial Metas #####
library(knitr)  #pacote para tabelas
library(GPArotation)

#KMO
names(df_geral) # Itens do 58:74

df_geral %>% select(58:74) %>% KMO()

#Bartlett

df_geral %>% select(58:74) %>% bartlett.test()

# Paralell analysis

poly_meta <- df_geral %>% select(58:74) %>% polychoric(.)   # matriz de correlações policóricas para análise paralela.
rho_meta <- poly_meta$rho # guardando apenas o Rho de Spearman da matriz de correlações policóricas
scree_meta <-scree(rho_meta)

pa_meta <- fa.parallel(rho_meta, n.obs=209, fa="fa") # análise paralela sobre a matriz de correlações policóricas
pa_meta$fa.values #mostra os eigenvalues com os dados experimentais
pa_meta$fa.sim #mostra os eigenvalues com os dados simulados

# OBS.: a análise paralela indicou 3 fatores, o scree-plot 3 ou 4 fatores,
# e pelo critério de Kaiser-Guttman (eigenvalue > 1), formariam-se três fatores.
# Assim, optou-se pela extração de 3 fatores.

# Fatores

meta_efa <- df_geral %>% select(58:74) %>% fa(nfactors = '3', cor='poly', 
            fm='wls', rotate = 'geominQ')

meta_efa$e.values
meta_efa$loadings %>% view()

# A análise indicou que o fator 3 não apresenta itens que sustente adequadamente sua existência.
# Por isso, é indicado a formação de apenas 2 fatores

meta_efa <- df_geral %>% select(58:74) %>% fa(nfactors = '2', cor='poly', 
                                              fm='wls', rotate = 'geominQ')

meta_efa$e.values
meta_efa$loadings %>% view()

kable(meta_efa$loadings[1:17, 1:2],digits = 2)

df_geral %>% select(58:64, 71:74) %>% psych::omega(digits=3,poly=TRUE) # Omega 0.93 alfa 0.89
df_geral %>% select(65:70) %>% omega(digits=3,poly=TRUE) # omega 0.9 alfa 0.84




