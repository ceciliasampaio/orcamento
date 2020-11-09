# OBTENDO PACOTES ---------------------------------------------------------

library(dplyr)
# library(reshape2)

# OBTENDO DADOS E APLICANDO TRATAMENTO PREVIO -----------------------------

diretorio <- "P:\\DBAHNSN"

CIRURGIAS_WORKFLOW <-
  readRDS(paste(diretorio,"\\vw_bi_cirurgia_workflow.rds", sep = "")) %>%
  mutate(DH_INICIO_CIRURGIA = as.Date(DH_INICIO_CIRURGIA)) %>%
  filter(CD_MULTI_EMPRESA == 1 &
           CD_CEN_CIR %in% c(1,3) &
           DH_INICIO_CIRURGIA > "2018-12-31" &
           SITUACAO_AVISO == "Realizado"
  )

FATURAMENTO <-
  readRDS(paste(diretorio,"\\vw_bi_faturamento.rds", sep = "")) %>%
  mutate(DATA_LANCAMENTO = as.Date(DATA_LANCAMENTO)) %>%
  mutate(CD_PRO_FAT = as.numeric(CD_PRO_FAT)) %>%
  filter(CD_MULTI_EMPRESA == 1 & DATA_LANCAMENTO > "2018-12-31" &
           SETOR_EXECUTANTE %in% c("CENTRO CIRURGICO", "HEMODINAMICA")
  )

ESTOQUE <-
  readRDS("P:\\DBAHNSN\\vw_bi_mvto_estoque.rds") %>%
  mutate(DATA = as.Date(DATA)) %>%
  filter(CD_MULTI_EMPRESA == 1 & DATA > "2018-12-31" & !is.na(CD_AVISO_CIRURGIA)) %>%
  group_by(CD_AVISO_CIRURGIA, CD_ITMVTO_ESTOQUE, CONTA_CUSTO) %>%
  tally() %>%
  unique()

# FRAGMENTACAO DE TABELAS -------------------------------------------------

CIRURGIAS <- CIRURGIAS_WORKFLOW %>%
  select(
    CD_AVISO_CIRURGIA,
    CD_ATENDIMENTO,
    NM_PACIENTE,
    COD_CIRURGIA,
    CIRURGIA,
    CD_CIRURGIAO,
    CIRURGIAO,
    ESPECIALID_PRINCIPAL_CIRURGIAO,
    CIRURGIA_GRUPO,
    CIRURGIA_SUB_GRUPO,
    PROCEDIMENTOS,
    CIRURGIA_PORTE
  ) %>%
  mutate(
    CD_MVTO = CD_AVISO_CIRURGIA
  )


FAT_CIRURGIA <- FATURAMENTO %>%
  filter(TP_MVTO %in% c("Cirurgia", "Equipamento")) %>%
  inner_join(CIRURGIAS, by = "CD_MVTO")


ITENS_CIRURGIA <- CIRURGIAS_WORKFLOW %>%
  inner_join(ESTOQUE, by = "CD_AVISO_CIRURGIA") %>%
  select(
    CD_AVISO_CIRURGIA,
    CD_ATENDIMENTO,
    NM_PACIENTE,
    COD_CIRURGIA,
    CIRURGIA,
    CD_CIRURGIAO,
    CIRURGIAO,
    ESPECIALID_PRINCIPAL_CIRURGIAO,
    CIRURGIA_GRUPO,
    CIRURGIA_SUB_GRUPO,
    PROCEDIMENTOS,
    CIRURGIA_PORTE,
    CD_ITMVTO_ESTOQUE,
    CONTA_CUSTO
  ) %>%
  rename(
    CD_ITMVTO = CD_ITMVTO_ESTOQUE
  )


FAT_PRODUTO <- FATURAMENTO %>%
  filter(TP_MVTO == "Produto") %>%
  inner_join(ITENS_CIRURGIA, by = "CD_ITMVTO" )


FAT_AUDITORIA <- FATURAMENTO %>%
  filter(!TP_MVTO %in% c("Produto", "Cirurgia", "Equipamento"))


# UNIAO DE FRAGMENTOS -----------------------------------------------------

FATURMENTO_CIRURGIAS <- bind_rows(FAT_CIRURGIA, FAT_PRODUTO, FAT_AUDITORIA)


# ANALISE E DELINEAMENTO DO PROTOCLO --------------------------------------

dados <- FATURMENTO_CIRURGIAS %>%
  filter(!is.na(CD_AVISO_CIRURGIA)) %>%
  select(
    CD_ATENDIMENTO.x,
    CD_AVISO_CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    COD_CIRURGIA,
    CIRURGIA,
    QT_FAT_PRODUCAO,
    QT_FAT_CREDENCIADO,
    QT_LANC_PACOTE,
    VL_FATOR_PRO_FAT,
    VL_FAT_PRODUCAO,
    VL_FAT_CREDENCIADO,
    VL_LANC_PACOTE
  ) %>%
  mutate(
    qtd = rowSums(.[c("QT_FAT_PRODUCAO", "QT_FAT_CREDENCIADO", "QT_LANC_PACOTE")], na.rm = TRUE)
  ) %>%
  mutate(
    faturamento = rowSums(.[c("VL_FAT_PRODUCAO", "VL_FAT_CREDENCIADO", "VL_LANC_PACOTE")], na.rm = TRUE)
  ) %>%
  mutate(
    QT = qtd/VL_FATOR_PRO_FAT
  ) %>%
  rename(
    CD_ATENDIMENTO = CD_ATENDIMENTO.x
  ) %>%
  select(
    CD_ATENDIMENTO,
    CD_AVISO_CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    COD_CIRURGIA,
    CIRURGIA,
    VL_FATOR_PRO_FAT,
    QT,
    faturamento
  ) %>%
  group_by(
    CD_ATENDIMENTO,
    CD_AVISO_CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    COD_CIRURGIA,
    CIRURGIA,
    VL_FATOR_PRO_FAT
  ) %>%
  summarise(
    QTD = sum(QT),
    FAT = sum(faturamento)
  )

# Frequencia de realizacao de cirurgias.
total_cirurgias <- CIRURGIAS_WORKFLOW %>% # quantidade de cirurgias
  group_by(CIRURGIA) %>%
  summarise(
    cirurgias = n()
  ) %>%
  mutate(
    freq = (cirurgias/sum(total_cirurgias$cirurgias)*100)
  ) %>%
  arrange(desc(freq)) %>%
  mutate(
    acumulado_cir = cumsum(freq),
    abc_cirurgias = ifelse(acumulado_cir <= 80, "A",ifelse(acumulado_cir >= 95, "C", "B")) # curva abc das cirurgias

  )


total_itens <- dados %>%  # quantidade de itens
  group_by(
    COD_CIRURGIA,
    CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    VL_FATOR_PRO_FAT,
    FAT
  ) %>%
  summarise(itens = n(), QT = sum(QTD), FATURAMENTO = sum(FAT)) %>%
  mutate(COD_CIRURGIA = as.character(COD_CIRURGIA))


total <- total_itens %>% # quantidade de itens por cirurgia
  inner_join(total_cirurgias, by = "CIRURGIA") %>%
  mutate(freq = itens/cirurgias) %>%
  mutate(qtd_perc = round(QT/cirurgias, 9) ) %>%
  select(
    COD_CIRURGIA,
    CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    VL_FATOR_PRO_FAT,
    freq,
    qtd_perc
  )


abc <- total_itens %>%
  inner_join(total_cirurgias, by = "CIRURGIA") %>%
  select(
    COD_CIRURGIA,
    CIRURGIA,
    cirurgias,
    FATURAMENTO
  ) %>%
  group_by(
    CIRURGIA
  ) %>%
  summarise(
    Qtd_Cirurgias = mean(cirurgias),
    Fat = sum(FATURAMENTO)
  ) %>%
  mutate(
    freq_fat = (Fat/sum(abc$Fat)*100)
  ) %>%
  arrange(desc(freq_fat)) %>%
  mutate(
    acumulado_fat = cumsum(freq_fat),
    abc_fat = ifelse(acumulado_fat <= 80, "A", ifelse(acumulado_fat >= 95, "C", "B"))
    # curva abc do faturamento
  )


curva <- abc %>%  # curva AA AB ...
  inner_join(total_cirurgias, by = "CIRURGIA") %>%
  mutate(
    abc = paste(abc_fat, abc_cirurgias, sep = ""))



