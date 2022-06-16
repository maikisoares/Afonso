library(readxl)

####Obtendo os dados da planilha####
dados <- read_excel("2º Pré-ensaio caldo de agave 17-03-22.xlsx", sheet = "Resultado GC")

####Convertendo os valores de pressao e %mol####
dados$pressao_atm <- dados$`Pressão (mbar)`/1013 #convertendo mbar pra atm
dados$CH4 <- dados$`CH4 (%molmol)`/100 #convertendo % pra decimal

####Excluindo colunas de outros gases####
dados$`H2(%molmol)`<- NULL
dados$`CO2 (%molmol)`<- NULL
dados$`H2S(%molmol)`<- NULL
dados$`Pressão (mbar)` <- NULL
dados$`CH4 (%molmol)`<- NULL

####Filtrando as colunas por amostra####
library(dplyr)

Branco <- dados %>% filter(Amostra == "Branco")
A1 <- dados %>% filter(Amostra == "A1")
A2 <- dados %>% filter(Amostra == "A2")
A3 <- dados %>% filter(Amostra == "A3")
B1 <- dados %>% filter(Amostra == "B1")
B2 <- dados %>% filter(Amostra == "B2")
B3 <- dados %>% filter(Amostra == "B3")
C1 <- dados %>% filter(Amostra == "C1")
C2 <- dados %>% filter(Amostra == "C2")
C3 <- dados %>% filter(Amostra == "C3")

####Juntando as colunas####
dados_totais <- left_join(Branco, A1,
                          by=c("Coleta" = "Coleta"), 
                          suffix = c("_branco", "_A1"))
dados_totais <- left_join(dados_totais, A2,
                          by=c("Coleta" = "Coleta"),
                          suffix = c(" ","_A2"))
dados_totais <- left_join(dados_totais, A3,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_A3"))
dados_totais <- left_join(dados_totais, B1,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_B1"))
dados_totais <- left_join(dados_totais, B2,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_B2"))
dados_totais <- left_join(dados_totais, B3,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_B3"))
dados_totais <- left_join(dados_totais, C1,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_C1"))
dados_totais <- left_join(dados_totais, C2,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_C2"))
dados_totais <- left_join(dados_totais, C3,
                          by=c("Coleta" = "Coleta"),
                          suffix = c("", "_C3"))

#renomeando as colunas das amostras A2
dados_totais <- dplyr::rename(dados_totais,
                           "Amostra_A2" = "Amostra",
                           "Data_A2" = "Data",
                           "Hora_A2" = "Hora",
                           "elapsed time_A2" = "elapsed time",
                           "pressao_atm_A2" = "pressao_atm",
                           "CH4_A2" = "CH4")

####Criando as medias das amostras A, B e C####
#Medias A
dados_totais$media_A_hora <- rowMeans(dados_totais[,c("Hora_A1","Hora_A2","Hora_A3")], na.rm = TRUE)
dados_totais$media_A_pressao <- rowMeans(dados_totais[,c("pressao_atm_A1","pressao_atm_A2","pressao_atm_A3")], na.rm = TRUE)
dados_totais$media_A_CH4 <- rowMeans(dados_totais[,c("CH4_A1","CH4_A2","CH4_A3")], na.rm = TRUE)

#Medias B
dados_totais$media_B_hora <- rowMeans(dados_totais[,c("Hora_B1","Hora_B3")], na.rm = TRUE)
dados_totais$media_B_pressao <- rowMeans(dados_totais[,c("pressao_atm_B1","pressao_atm_B3")], na.rm = TRUE)
dados_totais$media_B_CH4 <- rowMeans(dados_totais[,c("CH4_B1","CH4_B3")], na.rm = TRUE)

#Medias C
dados_totais$media_C_hora <- rowMeans(dados_totais[,c("Hora_C1","Hora_C2","Hora_C3")], na.rm = TRUE)
dados_totais$media_C_pressao <- rowMeans(dados_totais[,c("pressao_atm_C1","pressao_atm_C2","pressao_atm_C3")], na.rm = TRUE)
dados_totais$media_C_CH4 <- rowMeans(dados_totais[,c("CH4_C1","CH4_C2","CH4_C3")], na.rm = TRUE)

####Gerando uma planilha####
library(writexl)

write_xlsx(dados_totais, "pre_ensaio_2_1_1.xlsx")

