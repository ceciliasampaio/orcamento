# OBTENDO PACOTES ---------------------------------------------------------

library(dplyr)

# OBTENDO DADOS E APLICANDO TRATAMENTO PREVIO -----------------------------

#observar se tabela já esta em memoria

diretorio <- "C:\\Users\\herico.souza\\Documents\\DBAHNSN"

ESTOQUE <-
  readRDS(paste(diretorio,"\\vw_bi_mvto_estoque.rds", sep = "")) %>%
  mutate(
    DATA = as.Date(DATA),
    CD_ATENDIMENTO = as.character(CD_ATENDIMENTO)
  ) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
      DATA > "2018-12-31"
  ) %>%
  group_by(
    DATA,
    CD_MULTI_EMPRESA,
    CD_ATENDIMENTO,
    CD_AVISO_CIRURGIA,
    CD_ITMVTO_ESTOQUE,
    PRODUTO,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    UNIDADE,
    CONTA_CUSTO
  ) %>%
  tally()

ATENDIMENTOS <-
  readRDS(paste(diretorio,"\\vw_bi_atendimento.rds", sep = "")) %>%
  mutate(DATA_HORA = as.Date(DATA_HORA)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
    DATA_HORA > "2018-12-31" &
    !grepl("TESTE", NM_PACIENTE)
  )

FATURAMENTO_MAT_MED <-
  readRDS(paste(diretorio,"\\vw_bi_faturamento.rds", sep = "")) %>%
  mutate(DATA_LANCAMENTO = as.Date(DATA_LANCAMENTO)) %>%
  mutate(CD_PRO_FAT = as.numeric(CD_PRO_FAT)) %>%
  mutate(CD_ATENDIMENTO = as.character(CD_ATENDIMENTO)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
      DATA_LANCAMENTO > "2018-12-31" &
      !SETOR_EXECUTANTE %in% c("CENTRO CIRURGICO", "HEMODINAMICA") &
      TP_MVTO == "Produto"
  )


dados <- FATURAMENTO_MAT_MED %>%
  inner_join(
    select(
      ATENDIMENTOS,
      CD_ATENDIMENTO,
      CD_CID_ENTRADA,
      DS_CID_ENTRADA,
      CD_CID,
      DS_CID
    ),
    by = "CD_ATENDIMENTO"
  ) %>%
  inner_join(
      ESTOQUE,
    by = c(
      "CD_ATENDIMENTO"="CD_ATENDIMENTO",
      "CD_ITMVTO"="CD_ITMVTO_ESTOQUE",
      "PROCEDIMENTO"="PRODUTO"
    )
  ) %>%
  mutate(
    qtd = rowSums(.[c("QT_FAT_PRODUCAO", "QT_FAT_CREDENCIADO", "QT_LANC_PACOTE")], na.rm = TRUE),
    faturamento = rowSums(.[c("VL_FAT_PRODUCAO", "VL_FAT_CREDENCIADO", "VL_LANC_PACOTE")], na.rm = TRUE)
  ) %>%
  select(
    CD_CID_ENTRADA,
    DS_CID_ENTRADA,
    CD_CID,
    DS_CID,
    CD_ATENDIMENTO,
    SETOR_EXECUTANTE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    CONTA_CUSTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    qtd,
    faturamento
  )


total_setor <- dados %>% # total de atendimentos por setor
  mutate(
    SETOR_EXECUTANTE =
      replace(
        SETOR_EXECUTANTE,
        SETOR_EXECUTANTE %in% c( "INTERNACAO 1º PAVIMENTO",
          "INTERNACAO 2º PAVIMENTO",
          "INTERNACAO 3º PAVIMENTO",
          "INTERNACAO 4º PAVIMENTO"
        ),
        "INTERNACAO"
      ),
    SETOR_EXECUTANTE =
      replace(
        SETOR_EXECUTANTE,
        SETOR_EXECUTANTE == "RECEPCAO DO CDI",
        "CDI"
      )
  ) %>%
  group_by(
    CD_CID_ENTRADA,
    CD_CID,
    SETOR_EXECUTANTE
  ) %>%
  mutate(
    ATENDIMENTOS = length(unique(CD_ATENDIMENTO))
  ) %>%
  ungroup() %>%
  group_by(
    CD_CID_ENTRADA,
    DS_CID_ENTRADA,
    CD_CID,
    DS_CID,
    SETOR_EXECUTANTE,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    ATENDIMENTOS
  ) %>%
  summarise(
    freq = length(unique(CD_ATENDIMENTO)),
    Fat = sum(faturamento),
    QTD = sum(qtd)
  ) %>%
  mutate(
    per_freq = round(freq/ATENDIMENTOS*100,9),
    Qtd_Perc = round(QTD/ATENDIMENTOS,9)
  ) %>%
  arrange(desc(ATENDIMENTOS),CD_CID_ENTRADA, CD_CID, SETOR_EXECUTANTE, desc(per_freq))
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


