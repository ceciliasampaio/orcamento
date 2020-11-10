# OBTENDO PACOTES ---------------------------------------------------------

library(dplyr)

# OBTENDO DADOS E APLICANDO TRATAMENTO PREVIO -----------------------------

#observar se tabela já esta em memoria

diretorio <- "P:\\DBAHNSN"

ESTOQUE <-
  readRDS(paste(diretorio,"\\vw_bi_mvto_estoque.rds", sep = "")) %>%
  mutate(DATA = as.Date(DATA)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
      DATA > "2018-12-31"
  ) %>%
  group_by(
    CD_AVISO_CIRURGIA,
    CD_ITMVTO_ESTOQUE,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    UNIDADE,
    CONTA_CUSTO
  ) %>%
  tally() %>%
  unique()

ATENDIMENTOS <-
  readRDS(paste(diretorio,"\\vw_bi_atendimento.rds", sep = "")) %>%
  mutate(DATA_HORA = as.Date(DATA_HORA)) %>%
  filter(CD_MULTI_EMPRESA == 1 &
           DATA_HORA > "2018-12-31"
  )

FATURAMENTO_MAT_MED <-
  readRDS(paste(diretorio,"\\vw_bi_faturamento.rds", sep = "")) %>%
  mutate(DATA_LANCAMENTO = as.Date(DATA_LANCAMENTO)) %>%
  mutate(CD_PRO_FAT = as.numeric(CD_PRO_FAT)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
      DATA_LANCAMENTO > "2018-12-31" &
      !SETOR_EXECUTANTE %in% c("CENTRO CIRURGICO", "HEMODINAMICA") &
      TP_MVTO == "Produto"
  )


dados <- FATURAMENTO_MAT_MED %>%
  select(
    CD_ATENDIMENTO,
    SETOR_EXECUTANTE,
    PROCEDIMENTO
  ) %>%
  unique()


total_setor <- dados %>% # total de atendimentos por setor
  group_by(SETOR_EXECUTANTE) %>%
  summarise(
    total = n()
  ) %>%
  mutate(
    freq_setor = (total/sum(total_setor$total)*100)
  ) %>%
  arrange(desc(freq_setor)) %>%
  mutate(
    acumulado_setor = cumsum(freq_setor),
    abc_setor = ifelse(acumulado_setor <= 80, "A", ifelse(acumulado_setor >= 95, "C", "B"))
  )


total_itens <- dados %>% # total de itens por procedimento e por setor
  group_by(
    SETOR_EXECUTANTE,
    PROCEDIMENTO
  ) %>%
  summarise(
    itens = n()
  )

total <- total_setor %>%
  inner_join(total_itens, by = "SETOR_EXECUTANTE") %>%
  mutate(freq = itens/total)


