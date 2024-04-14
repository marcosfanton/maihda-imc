library(tidyverse) 
library(here) 
library(PNSIBGE) # Extração de microdados PNS

# Construção do banco de dados####
# Seleção de variáveis
vars <- c(
  "V00291", # peso do morador selecionado
  "P005", # gravidez (2 = NÃO)
  "C006", # sexo/gênero
  "C008", # idade 
  "C009", # raça/idade
  "P00102", # "o sr. sabe o seu peso?" (1 = SIM)
  "P00103", # peso informado
  "P00402", # "o Sr. sabe a sua altura?" (1 = SIM)
  "P00403", # altura informada 
  "VDF003", # renda per capita
  "VDD004A") # nível de instrução

# Importação de dados via pnsibge
raw_19 <- get_pns(year = 2019,
                  vars = vars, # seleção das variáveis 
                  selected = TRUE, # seleção de respondentes do questionário
                  design = FALSE) # banco desestruturado (sem modo survey)

# Salvar banco
write.csv(raw_19, 
          "dados/raw_19.csv", 
          row.names = FALSE)
