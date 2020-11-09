# OBTENDO PACOTES ---------------------------------------------------------

library(dplyr)

# OBTENDO DADOS E APLICANDO TRATAMENTO PREVIO -----------------------------

#observar se tabela já esta em memoria
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
