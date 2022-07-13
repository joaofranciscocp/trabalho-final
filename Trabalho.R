library(tidyverse)
library(data.table)
library(readxl)
library(stargazer)
library(performance)
library(lmtest)
library(sandwich)
library(commarobust)
library(estimatr)
library(car)
library(geobr)
library(sf)
library(rio)
library(readr)
library(psych)
library(xtable)

summary(lm_robust(log(despesas_pc+1) ~ log(median_income+1) + log(price+1) 
                + log(populacao+1) + idhm + 
                  pop_65_mais + pop_17_menos  + log(densidade) + coligacao,
                data6, se_type = 'HC0'))

coeftest(reg_despesas, vcov = vcovHC)

setwd('C:/Users/joaofrancisco/Desktop/USP/Economia/5º período/Econometria I (Fabiana Rocha)/Trabalho')

#Pegar os dados

ibge2010 <- fread('microdados_pessoa_2010.csv', 
                   select = c('id_municipio', 
                              'v6531'))

by_municipios <- ibge2010 %>% 
  group_by(id_municipio)

data <- by_municipios %>% summarise(
  mean_income = mean(v6531, na.rm = TRUE),
  median_income = median(v6531, na.rm = TRUE)) %>%  
  mutate(price = median_income/mean_income)

areas <- read_excel('areas.xls') %>% 
  rename(area = 'AR_MUN_2010', id_municipio = 'GEOCODIGO') %>% 
  select(id_municipio, area)

socioeconomico <- read_csv('idhm.csv') %>% 
  filter(ano == 2010) %>% 
  mutate(pop_17_menos = populacao_1_menos + populacao_1_3 + 
           populacao_4 + populacao_5 + populacao_6_17) %>% 
  rename(gini = 'indice_gini', theil = 'indice_theil',
         pop_65_mais = 'populacao_65_mais') %>% 
  select(id_municipio, gini, theil, pop_65_mais, idhm, pop_17_menos)

despesas_consulta <- read_excel('Despesas_Consulta.xlsx')

funcoes_consulta <- read_excel('Funcao Consulta.xlsx')

estados_partidos <- list()
i <- 1

for(estado in list.files('Partidos')){
  dado <- read.csv(paste0('Partidos/', estado), sep = ';')
  estados_partidos[[i]] <- dado %>% 
    filter(id_municipio != '#N/D') %>% 
    select(id_municipio, Partido)
  estados_partidos[[i]]$id_municipio <- as.character(estados_partidos[[i]]$id_municipio)
  i <- i +1
  
}

partidos <- bind_rows(estados_partidos)

#Adicionei manualmente no Excel uma coluna id_municipio igual à dos microdados dos IBGE

despesas_municipios <- despesas_consulta %>% 
  select('id_municipio', 'Despesas Orçamentárias', 'Populacao')

funcoes_municicpios <- funcoes_consulta %>% 
  select('id_municipio','Educação', 'Saúde', 'Assistência Social',
         'Desporto e Lazer',
         'Transporte', 'Saneamento', 'Habitação', 'Urbanismo')

data2 <- merge(x = data, y = despesas_municipios, by = 'id_municipio',
               all.x = TRUE) %>% 
  drop_na('Despesas Orçamentárias') %>% 
  rename(despesas = 'Despesas Orçamentárias') %>% 
  filter(mean_income != 0) %>% 
  mutate(price = price*100) %>% 
  rename(populacao = 'Populacao')

data3 <- merge(x = data2, y = funcoes_municicpios, by = 'id_municipio') %>% 
  drop_na('Educação', 'Saúde', 'Assistência Social',
          'Desporto e Lazer',
          'Transporte', 'Saneamento', 'Habitação',
          'Urbanismo', 'id_municipio') %>% 
  rename(educ = 'Educação',
         saude = 'Saúde', 
         assist_social = 'Assistência Social',
         esprt_lazer = 'Desporto e Lazer',
         transporte = 'Transporte',
         saneamento = 'Saneamento',
         habitacao = 'Habitação',
         urbanismo = 'Urbanismo') %>%
  distinct(id_municipio, .keep_all = TRUE)

data4 <- merge(x=data3, y=areas, by = 'id_municipio') %>% 
  mutate(densidade = populacao/area)

data5 <- merge(x = data4, y = socioeconomico, by = 'id_municipio') %>% 
  mutate(despesas_pc = despesas/populacao, 
         educ_pc = educ/populacao,
         saude_pc = saude/populacao,
         urbanismo_pc = urbanismo/populacao,
         habitacao_pc = habitacao/populacao,
         saneamento_pc = saneamento/populacao,
         assist_pc = assist_social/populacao,
         pop_17_menos = pop_17_menos/populacao,
         pop_65_mais = pop_65_mais/populacao)


data6 <- merge(x=data5, y = partidos, by = 'id_municipio') %>% 
  mutate(coligacao = case_when(Partido %in% c('PMDB','PT', 'PCdoB', 'PSB') ~ 'situacao',
    Partido %in% c('PSDB','DEM', 'PPS') ~ 'oposicao',
    TRUE ~ 'nenhuma')) %>% 
  mutate(coligacao = as.factor(coligacao)) %>% 
  within(., coligacao <- relevel(coligacao, ref = 'nenhuma'))


reg_despesas <- lm(log(despesas_pc+1) ~ log(median_income+1) + log(price+1) 
            + log(populacao+1) + idhm + 
              pop_65_mais + pop_17_menos  + log(densidade) + coligacao,
            data6)

coeftest(reg_despesas, vcov = vcovHC)

reg_educ <- lm(log(educ_pc+1) ~ log(median_income+1) + log(price+1) 
               + log(populacao+1) + idhm + pop_17_menos + log(densidade+1) + coligacao,
               data6)

coeftest(reg_educ, vcov = vcovHC)

reg_saude <- lm(log(saude_pc+1) ~ log(median_income+1) + log(price+1) 
                + log(populacao+1) + pop_65_mais + pop_17_menos
                  idhm + coligacao,
                data6)

coeftest(reg_saude, vcov = vcovHC)

reg_saneamento <- lm(log(saneamento_pc+1) ~ log(median_income+1) + log(price+1) 
                     + log(populacao+1) + idhm + coligacao,
                     data6)

coeftest(reg_saneamento, vcov = vcovHC)

reg_assist <- lm(log(assist_pc+1) ~ log(median_income+1) + log(price+1) 
                    + log(populacao+1) + idhm + pop_17_menos
                 + pop_65_mais + coligacao,
                    data6)

coeftest(reg_assist, vcov = vcovHC)

mod_stargazer('teste.txt', reg_despesas,
          reg_educ,
          reg_saude,
          reg_saneamento,
          reg_assist,
          omit = c('Constant'), 
          dep.var.labels = c('Despesa total',
                             'Educação',
                             'Saúde',
                             'Saneamento',
                             'Assistência Social'),
          covariate.labels = c('Renda mediana', 'Preço','População', 'IDHM', 'População mais de 65',
                               'População mais de 17'))


summary(reg_assist)
mod_stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}

municipios <- read_municipality(
  code_muni = "all",
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE
)

data7 <- merge(x = data, y = municipios %>% rename(id_municipio = code_muni) %>% 
                 select(geom, id_municipio), by = 'id_municipio')

mapa <- data7 %>% 
  ggplot() +
  geom_sf(aes(fill = median_income, geometry = geom),  color = NA) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 1200)) + 
  ggtitle('Municípios brasileiros por renda mediana') +
  labs(fill = 'Renda mediana (R$)', colour = '') +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 11, hjust = 0.2), # title
        axis.text.x = element_blank(), # remove x axis labels
        axis.text.y = element_blank(), # remove y axis labels  
        axis.ticks = element_blank(), # remove axis ticks
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove grid lines
ggsave("mapa.tiff", width = 10, height = 10, dpi=600, compression = "lzw")

  
estatisticas <- describe(data6 %>% select(mean_income, median_income, price, despesas_pc)) %>% 
  select(n, mean, sd, min, max)

xtable(estatisticas)




