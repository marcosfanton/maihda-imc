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

# Importação de dados via PNSIBGE
raw_19 <- get_pns(year = 2019,
                  vars = vars, # seleção das variáveis 
                  selected = TRUE, # seleção de respondentes do questionário
                  design = FALSE) # banco desestruturado (sem modo survey)
# Salvar banco
write.csv(raw_19, 
          "dados/raw/raw_19.csv", 
          row.names = FALSE)

# Carregar banco
raw_19 <- read.csv("dados/raw_19.csv")

# Construção de variáveis
pns19 <- raw_19 |> 
  dplyr::filter(C008 >= 18)  |>  # maiores de 18 anos
  dplyr::filter(C009 != "Ignorado")  |>  # exclusão de "Ignorados" da variável raça
  dplyr::filter(P00102 == "Sim" & P00402 == "Sim")  |>  # sabe o peso & a altura
  dplyr::filter(P005 != "Sim"| is.na(P005))  |>  # exclusão de mulheres grávidas
  dplyr::mutate(
    peso_original = V00291,
    age = factor(
      cut(C008, c(breaks = quantile(C008, 
                                    probs = seq(0, 1, 1/4), 
                                    na.rm = TRUE)), # variável idade por quartil
          labels = c("low", "middle_low", "middle_high", "high"), 
          right = FALSE, 
          include.lowest = TRUE)),
    gender = factor(case_when( 
      C006 == "Homem" ~ "male",
      C006 == "Mulher" ~ "female"),
      levels = c("male", "female")), # variável gênero
    race = factor(case_when(
      C009 == "Branca" ~ "white",
      C009 == "Preta" ~ "black",
      C009 == "Parda" ~ "brown",
      C009 == "Indígena" ~ "indigenous"),
      levels = c("white", "black", "brown", "indigenous")), # variável raça/etnia
    income = factor(
      cut(VDF003, c(breaks = quantile(VDF003, 
                                      probs = seq(0, 1, 1/4), 
                                      na.rm = TRUE)), 
          labels = c("ilow", "imiddle_low", "imiddle_high", "ihigh"), # variável renda por tercil
          right = FALSE, 
          include.lowest = TRUE)),
    education = factor(case_when(
      VDD004A %in% c("Sem instrução", "Fundamental incompleto ou equivalente") ~ "elow",
      VDD004A %in% c("Fundamental completo ou equivalente", 
                     "Médio incompleto ou equivalente") ~ "emiddle_low",
      VDD004A == "Médio completo ou equivalente" ~ "emiddle_high",
      VDD004A %in% c("Superior incompleto ou equivalente", "Superior completo") ~ "ehigh"),
      levels = c("elow", "emiddle_low", "emiddle_high", "ehigh")), # variável escolaridade por quartil
    bmi = P00103/((P00403/100)^2), # variável IMC
    obesity = factor(case_when(  
      bmi >= 30 ~ 1,
      TRUE ~ 0), 
      levels = c(0, 1)))  |>  # variável estado nutricional (obesidade = sim/não)
  select(age, gender, race, education, income, bmi, obesity, peso_original) # seleção das variáveis

pns19 <- drop_na(pns19)

# Criação de estratos sociais
pns19 <- pns19  |> 
  dplyr::mutate(strata = paste(gender, race, income, education, sep = ""))

# Salvar banco
write.csv(pns19, 
          "dados/pns19.csv", 
          row.names = FALSE)

# Carregar banco
raw_19 <- read.csv("dados/raw_19.csv")