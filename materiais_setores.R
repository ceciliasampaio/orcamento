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

FATURAMENTO <-
  readRDS(paste(diretorio,"\\vw_bi_faturamento.rds", sep = "")) %>%
  mutate(DATA_LANCAMENTO = as.Date(DATA_LANCAMENTO)) %>%
  mutate(CD_PRO_FAT = as.numeric(CD_PRO_FAT)) %>%
  mutate(CD_ATENDIMENTO = as.character(CD_ATENDIMENTO)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
      DATA_LANCAMENTO > "2018-12-31" &
      !SETOR_EXECUTANTE %in% c("CENTRO CIRURGICO", "HEMODINAMICA")
  ) %>%
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
  )

FATURAMENTO_MAT_MED <- FATURAMENTO %>%
  filter( TP_MVTO == "Produto" )


FATURAMENTO_SADT_AUDITORIA <- FATURAMENTO %>%
  filter(
    TP_MVTO != "Produto" &
    GRUPO_FATURAMENTO != "PACOTES ESPECIAIS"
  )


PROTOCOLOS <- FATURAMENTO_MAT_MED %>%
  inner_join(
      ESTOQUE,
    by = c(
      "CD_ATENDIMENTO"="CD_ATENDIMENTO",
      "CD_ITMVTO"="CD_ITMVTO_ESTOQUE",
      "PROCEDIMENTO"="PRODUTO"
    )
  ) %>% #JOIN COM TABELA DE ESTOQUE
  bind_rows(FATURAMENTO_SADT_AUDITORIA) %>% #JOIN DEMAIS ITENS DA CONTA
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
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
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
  ) %>%
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
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    CONTA_CUSTO,
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
    per_freq = round(freq/ATENDIMENTOS*100,5),
    Qtd_Perc = round(QTD/ATENDIMENTOS,5)
  ) %>%
  arrange(desc(ATENDIMENTOS),CD_CID_ENTRADA, CD_CID, SETOR_EXECUTANTE, desc(per_freq))
