# OBTENDO PACOTES ---------------------------------------------------------

library(dplyr)


# OBTENDO DADOS E APLICANDO TRATAMENTO PREVIO -----------------------------

diretorio <- "P:\\DBAHNSN"

CIRURGIAS_WORKFLOW <-
  readRDS(paste(diretorio,"vw_bi_cirurgia_workflow.rds", sep = "")) %>%
  mutate(DH_INICIO_CIRURGIA = as.Date(DH_INICIO_CIRURGIA)) %>%
  filter(CD_MULTI_EMPRESA == 1 &
           CD_CEN_CIR %in% c(1,3) &
           DH_INICIO_CIRURGIA > "2018-12-31" &
           SITUACAO_AVISO == "Realizado"
  )


FATURAMENTO <-
  readRDS(paste(diretorio,"DBAHNSN\\vw_bi_faturamento.rds", sep = "")) %>%
  mutate(DATA_LANCAMENTO = as.Date(DATA_LANCAMENTO)) %>%
  mutate(CD_PRO_FAT = as.numeric(CD_PRO_FAT)) %>%
  filter(
    CD_MULTI_EMPRESA == 1 &
    DATA_LANCAMENTO > "2018-12-31" &
    SETOR_EXECUTANTE %in% c("CENTRO CIRURGICO", "HEMODINAMICA")
  )



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
    CONTA_CUSTO,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    UNIDADE
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
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    COD_CIRURGIA,
    CIRURGIA,
    QT_FAT_PRODUCAO,
    QT_FAT_CREDENCIADO,
    QT_LANC_PACOTE,
    VL_FATOR_PRO_FAT,
    VL_FAT_PRODUCAO,
    VL_FAT_CREDENCIADO,
    VL_LANC_PACOTE,
    CUSTO_UNITARIO
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
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    COD_CIRURGIA,
    CIRURGIA,
    VL_FATOR_PRO_FAT,
    qtd,
    faturamento,
    CUSTO_UNITARIO,
    QT
  ) %>%
  group_by(
    CD_ATENDIMENTO,
    CD_AVISO_CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    ESPECIE,
    CLASSE,
    SUB_CLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    COD_CIRURGIA,
    CIRURGIA,
    VL_FATOR_PRO_FAT,
    CUSTO_UNITARIO,
    QT
  ) %>%
  summarise(
    QTD = sum(qtd),
    FAT = sum(faturamento),
    custo = CUSTO_UNITARIO*QT
  )

# Frequencia de realizacao de cirurgias

# curva abc das cirurgias
CIRURGIAS_ABC <- CIRURGIAS_WORKFLOW %>%
  group_by(CIRURGIA) %>%
  summarise(
    cirurgias = n()
  ) %>%
  mutate(
    freq = (cirurgias/sum(cirurgias)*100)
  ) %>%
  arrange(desc(freq)) %>%
  mutate(
    acumulado_cir = cumsum(freq),
    abc_cirurgias = ifelse(
      acumulado_cir <= 80, "A",
      ifelse(
        acumulado_cir >= 95, "C", "B"
        )
      )
  )


# quantidade de itens
total_itens <- dados %>%
  group_by(
    COD_CIRURGIA,
    CIRURGIA,
    GRUPO_FATURAMENTO,
    GRUPO_PROCEDIMENTO,
    PRODUTO_ESPECIE,
    PRODUTO_CLASSE,
    PRODUTO_SUBCLASSE,
    ESPECIE,
    CLASSE,
    CONTA_CUSTO,
    CD_PRO_FAT,
    PROCEDIMENTO,
    UNIDADE,
    VL_FATOR_PRO_FAT,
    custo
  ) %>%
  summarise(
    itens = n(),
    QT = sum(QTD),
    FATURAMENTO = sum(FAT)
  ) %>%
  mutate(
    COD_CIRURGIA = as.character(COD_CIRURGIA)
  )


# quantidade de itens por cirurgia
total <- total_itens %>%
  inner_join(CIRURGIAS_ABC, by = "CIRURGIA") %>%
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
    UNIDADE,
    VL_FATOR_PRO_FAT,
    freq,
    qtd_perc
  )

tabela <- total_itens %>%
  inner_join(CIRURGIAS_ABC, by = "CIRURGIA") %>%
  mutate(freq = itens/cirurgias) %>%
  mutate(qtd_perc = round(QT/cirurgias, 9) ) %>%
  select(
    COD_CIRURGIA,
    CIRURGIA,
    ESPECIE,
    CLASSE,
    PROCEDIMENTO,
    cirurgias,
    itens,
    freq,
    qtd_perc,
    custo
  )

# curva abc do faturamento
FATURAMENTO_ABC <- total_itens %>%
  inner_join(CIRURGIAS_ABC, by = "CIRURGIA") %>%
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
    freq_fat = (Fat/sum(Fat)*100)
  ) %>%
  arrange(desc(freq_fat)) %>%
  mutate(
    acumulado_fat = cumsum(freq_fat),
    abc_fat = ifelse(acumulado_fat <= 80, "A", ifelse(acumulado_fat >= 95, "C", "B"))
  )

# curva abc do tiket médio
TIKET_ABC <- total_itens %>%
  inner_join(CIRURGIAS_ABC, by = "CIRURGIA") %>%
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
    Fat = sum(FATURAMENTO),
    Tk_Medio = round(Fat/Qtd_Cirurgias,2)
  ) %>%
  arrange(desc(Tk_Medio)) %>%
  mutate(
    Tk_Abc = ifelse(
      Tk_Medio >= quantile(Tk_Medio, 0.75), "A",
      ifelse(
        Tk_Medio < quantile(Tk_Medio, 0.50), "C", "B")
      )
  ) %>%
  select(
    CIRURGIA,
    Tk_Medio,
    Tk_Abc
  )


# curva abc dos procedimentos por qtd, fat e ticket medio
curva <- FATURAMENTO_ABC %>%  # curva AA AB ...
  inner_join(CIRURGIAS_ABC, by = "CIRURGIA") %>%
  inner_join(TIKET_ABC, by = "CIRURGIA") %>%
  mutate(
    abc = paste(abc_fat, abc_cirurgias, Tk_Abc, sep = "")
  ) %>%
  select(
    CIRURGIA,
    Qtd_Cirurgias,
    Fat,
    Tk_Medio,
    abc
  )
