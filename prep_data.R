library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)
setwd("/home/germano/Documents/lgbtapp/")

path = "/home/germano/Documents/lgbtapp/raw_data/"




# CASOS/MÊS ----
lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "dados_uf_mes.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
                select(., 2:14)



lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$UF <- lst[[1]][1]

colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'UF') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["UF",])
result2 <- result2[!rownames(result2) %in% "UF", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL
df = as.data.table(df)
df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Estado", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=Estado, group=Estado)) + geom_point()+geom_line() #+ scale_y_continuous(breaks=pretty_breaks(n=5))

saveRDS(df2, "data/casos_mes_uf.rds")

# PERFIL/MÊS/IDADE ----

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "perfil_vitima_mes_idade.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
  select(., 2:14)



lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$`Faixa Etária` <- lst[[1]][1]


colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'Faixa Etária') 



#colnames(result) <- c('Não Informado', '4 a 7', '8 a 11', '12 a 14', '15 a 17', '18 a 24', '25 a 30', '31 a 35', '36 a 40', '41 a 45', '46 a 50', '51 a 55', '56 a 60', '61 a 65', '66 a 70', '71 a 75', '76 a 80', '81 a 85', '85 a 90', '91+', 'Faixa Etária') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["Faixa Etária",])
result2 <- result2[!rownames(result2) %in% "Faixa Etária", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL

df = as.data.table(df)

df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Idade", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=Idade, group=Idade)) + geom_point()+geom_line() #+ scale_y_continuous(breaks=pretty_breaks(n=5))

saveRDS(df2, "data/perfil_mes_idade.rds")


# PERFIL/MÊS/SEXO ----

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "perfil_vitima_mes_sexo.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
  select(., 2:14)



lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$`Faixa Etária` <- lst[[1]][1]


colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'Sexo') 



#colnames(result) <- c('Não Informado', '4 a 7', '8 a 11', '12 a 14', '15 a 17', '18 a 24', '25 a 30', '31 a 35', '36 a 40', '41 a 45', '46 a 50', '51 a 55', '56 a 60', '61 a 65', '66 a 70', '71 a 75', '76 a 80', '81 a 85', '85 a 90', '91+', 'Faixa Etária') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["Sexo",])
result2 <- result2[!rownames(result2) %in% "Sexo", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL
df = as.data.table(df)

df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Sexo", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=Sexo, group=Sexo)) + geom_point()+geom_line() #+ scale_y_continuous(breaks=pretty_breaks(n=5))


saveRDS(df2, "data/perfil_mes_sexo.rds")


# PERFIL/MÊS/GENERO ----

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "perfil_vitima_mes_genero.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
  select(., 2:14)

lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$`Gênero` <- lst[[1]][1]

colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'Gênero') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["Gênero",])
result2 <- result2[!rownames(result2) %in% "Gênero", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL
df = as.data.table(df)

df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Gênero", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=`Gênero`, group=`Gênero`)) + geom_point()+geom_line() #+ scale_y_continuous(breaks=pretty_breaks(n=5))


saveRDS(df2, "data/perfil_mes_genero.rds")


# PERFIL/MÊS/DEFICIENCIA ----

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "perfil_vitima_mes_deficiencia.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
  select(., 2:14)

lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$`Deficiência` <- lst[[1]][1]

colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'Deficiência') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["Deficiência",])
result2 <- result2[!rownames(result2) %in% "Deficiência", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL
df = as.data.table(df)

df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Deficiência", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=`Deficiência`, group=`Deficiência`)) + geom_point()+geom_line() #+ scale_y_continuous(breaks=pretty_breaks(n=5))


saveRDS(df2, "data/perfil_mes_deficiencia.rds")


# PERFIL/MÊS/RAÇA ----

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "perfil_vitima_mes_raca.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:13) %>% rowSums(., na.rm=T)))

teste <- lst[[1]] %>% as.data.frame() %>%
  select(., 2:14)

lst2 <- lapply(lst, function(x) x[14])
result = lapply(lst2, "[", , 'Total') %>% as.data.frame()
result$`Raça` <- lst[[1]][1]


colnames(result) <- c("2011", "2012", '2013', 
                      '2014', '2015', '2016', 
                      '2017', '2018', "2019", 'Raça') 

result2 <- result %>% t()
colnames(result2)<-unlist(result2["Raça",])
result2 <- result2[!rownames(result2) %in% "Raça", ]

result2 <-  as.data.frame(result2)

df <- tibble::rownames_to_column(result2, "Ano")

df$`NA` <- NULL
df = as.data.table(df)

df2 <- melt(df, id.vars="Ano")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Ano", "Raça", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=`Raça`, group=`Raça`)) + geom_point()+geom_line() 


saveRDS(df2, "data/perfil_mes_raca.rds")


# TIPOS VIOLAÇÃO ----
# Nao pode apenas somar as colunas em um "total". Cada coluna é um resultado próprio

lst <- lapply(1:9, function(i) read.xlsx(paste0(path, "tipo_violacao.xlsx"), sheetName = i,encoding='UTF-8'))

lst <- lapply(1:9, function(i) lst[i] %>% as.data.frame() %>%
                mutate(Total = select(., 2:12) %>% rowSums(., na.rm=T)))

type_violence <- function(lista, coluna){
    dataframe <-  cbind(lista[[1]][,1] %>% as.data.frame(.),do.call(cbind,lapply(1:9, function(i) lista[[i]][,coluna])))
    rownames(dataframe) <- dataframe[,1]
    dataframe[,1] <- NULL
    colnames(dataframe) <- c('2011', '2012', '2013 ', 
                            '2014', '2015', '2016', 
                            '2017', '2018', '2019') 
    return(dataframe)
}

abuso <- type_violence(lst,2)
discriminacao <- type_violence(lst,3)
negligencia <- type_violence(lst,4)
outras <- type_violence(lst,5)
tortura <- type_violence(lst,6)
escravos <- type_violence(lst,7)
trafico <- type_violence(lst,8)
fisica <- type_violence(lst,9)
institucional <- type_violence(lst,10)
psicologica <- type_violence(lst,11)
sexual <- type_violence(lst,12)

types_list = list(abuso, discriminacao,
             negligencia, tortura, escravos,
             trafico, fisica,
             institucional,
             psicologica,
             sexual, outras)

types_list_df <- lapply(1:11, function(i) as.data.table(tibble::rownames_to_column(types_list[[i]],"Estado"))) 


types_list_melt <- lapply(1:11, function(i) melt(types_list_df[[i]],id.vars="Estado"))
saveRDS(types_list_melt, "data/tipos_violencia.rds")

df <- lapply(1:11, function(i) colnames(types_list_melt[[i]]) <- c("Estado", "Ano", 'Casos') ) 

types_list_melt[1]
types_list_melt[1]$value <- as.numeric(types_list_melt[1]$value)
colnames(df2) <- c("Estado", "Ano", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=`Estado`, group=`Estado`)) + geom_point()+geom_line() 


df2 <- melt(df, id.vars="Estado")
df2$value <- as.numeric(df2$value)
colnames(df2) <- c("Estado", "Ano", 'Casos') 
ggplot(df2, aes(x=Ano, y=Casos,color=`Estado`, group=`Estado`)) + geom_point()+geom_line() 


saveRDS(df2, "data/tipos_violacao.rds")
