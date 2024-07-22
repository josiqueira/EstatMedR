# DOBR_arruma2.R
# Segunda etapa: ler o arquivo csv, 
# e usar dic.xlsx para rearranjar o database

pausa <- FALSE

cat("\nLendo csv\n")
ini_time <- Sys.time()
dados <- as.data.frame(data.table::fread("DOBR2020.csv"))
end_time <- Sys.time()
cat("... tempo: ", difftime(end_time,ini_time),"\n")

cat("\nLendo dicionario\n")
ini_time <- Sys.time()
dt_type <- readxl::read_excel("DOBR2020dic.xlsx")
end_time <- Sys.time()
cat("... tempo: ", difftime(end_time,ini_time),"\n")

# Acertar os campos de acordo com o dicionario
namesdic <- names(dt_type)
dt_dic <- data.frame(matrix(nrow=0,ncol=length(namesdic)))
names(dt_dic) <- namesdic
dt_guia <- dt_type[!is.na(dt_type$class),]
dt_dados <- data.frame(matrix(1:nrow(dados),ncol=1,nrow=nrow(dados)))
names(dt_dados) <- "ref.line"

cat("\nIntegrando dados e dicionario\n")
ini_time <- Sys.time()
for (g.aux in 1:nrow(dt_guia))
{
  if (dt_guia$class[g.aux]=="remove")
  {
    next
  }
  dt_tmp <- dt_type[dt_type$variable==dt_guia$variable[g.aux] & 
                      !is.na(dt_type$variable),]
  c.aux <- which(names(dados)==dt_guia$variable[g.aux])
  values <- as.character(unlist(dados[,c.aux]))
  
  # recodifica os dados
  if (dt_guia$class[g.aux]=="factor")
  {
    if (nrow(dt_tmp)>1)
    {
      for (r.aux in 2:nrow(dt_tmp))
      {
        if (is.na(dt_tmp$short.str[r.aux]))
        {
          values[values==dt_tmp$levels[r.aux]] <- NA
        } else
        {
          values[values==dt_tmp$levels[r.aux]] <- dt_tmp$short.str[r.aux]
        }
      }
    }
  }
  
  # converte ao tipo solicitado
  dt_tmp2 <- data.frame(values)
  names(dt_tmp2) <- dt_guia$variable[g.aux]
  if (dt_guia$class[g.aux]=="factor")
  {
    if (nrow(dt_tmp)>1)
    {
      labels <- dt_tmp$short.str[2:nrow(dt_tmp)]
      labels[is.na(labels)] <- ""
      dt_tmp2[,1] <- factor(dt_tmp2[,1],
                            levels=labels,
                            labels=labels)
    }
  }
  if (dt_guia$class[g.aux]=="integer")
  {
    dt_tmp2[,1] <- as.integer(dt_tmp2[,1])
  }
  if (dt_guia$class[g.aux]=="numeric")
  {
    dt_tmp2[,1] <- as.numeric(dt_tmp2[,1])
  }
  dt_dados <- cbind(dt_dados,dt_tmp2)
}
end_time <- Sys.time()
cat("... tempo: ", difftime(end_time,ini_time),"\n")

# remove os dados originais
# rm(dados) # valores originais nao mais necessarios

# Excecoes precisam ser trabalhadas individualmente

# IDADE esta confuso, sem solucao por enquanto
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="0"])))
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="1"])))
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="2"])))
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="3"])))
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="4"])))
# as.character(sort(unique(dt_dados$IDADE[substr(dt_dados$IDADE,1,1)=="5"])))
# sort(unique(dt_dados$IDADE))
# table(dt_dados$IDADE)


# IDADEMAE	Idade da mãe (anos)	numeric	47	---	---	Alterar para numeric, 99 trocar para NA
dt_dados$IDADEMAE[dt_dados$IDADEMAE==99] <- NA
# tem mae de 8, 10 anos ?

# QTDFILVIVO	Número de filhos tidos (nascidos vivos)	numeric	21	---	---	Alterar para numeric, 99 trocar para NA
dt_dados$QTDFILVIVO[dt_dados$QTDFILVIVO==99] <- NA
# tem mulheres com 24 e mais

# QTDFILVIVO	Número de filhos tidos (nascidos vivos)	numeric	21	---	---	Alterar para numeric, 99 trocar para NA
dt_dados$QTDFILMORT[dt_dados$QTDFILMORT==99] <- NA
# tem mulheres com 24 e mais

# QTDFILVIVO	Número de filhos tidos (nascidos vivos)	numeric	21	---	---	Alterar para numeric, 99 trocar para NA
dt_dados$SEMAGESTAC[dt_dados$SEMAGESTAC==99] <- NA
# tem mulheres com 24 e mais

# Falta, ainda, encontrar os codigos em campos que os utilizam
# Os campos com data ddmmaaaa podem ser convertidos em data para calcular diferenças de tempo
# Há campos calculados pelo sistema, que podem ser recalculados ou eliminados

# coluna ref.line, temporaria, removida
dt_dados$ref.line <- NULL

cat("\nSalvando modificacoes em DOBR2020analise.csv\n")
ini_time <- Sys.time()
data.table::fwrite(dt_dados,"DOBR2020analise.csv")
end_time <- Sys.time()
cat("... tempo: ", difftime(end_time,ini_time),"\n")

# Como sao muitas colunas, melhor ver uma por uma
colunas <- sapply(dt_dados,class)
for (i.aux in 1:length(colunas))
{
  nome.var <- names(colunas)[i.aux]
  # coluna nos dados
  c.aux <- which(names(dt_dados)==nome.var)
  # tipo da variavel
  tipo.var <- as.character(colunas[i.aux])
  # descricao por extenso
  ext.var <- dt_guia$description[which(dt_guia$variable==nome.var)]
  
  cat("\n--------------------\n")
  cat(nome.var,": ",ext.var," (",tipo.var,")\n",sep="")
  cat("obs: ",dt_guia$obs[i.aux])  

  cat("\nnumero de valores diversos: ",length(unique(dt_dados[,c.aux])))
  nas <- sum(is.na(dt_dados[,c.aux]))
  cat("\nNA: ",nas," (",round(nas/nrow(dt_dados)*100,2),"%)",sep="")
  
  analisado <- FALSE
  if (tipo.var=="factor")
  {
    l <- levels(dt_dados[,c.aux])
    cat("\nníveis (",length(l),"): | ",sep="")
    cat(l,"\n",sep=" | ")
    tabela <- table(dt_dados[,c.aux])
    print(tabela)
    barplot(tabela, main=nome.var)
    analisado <- TRUE
  }

  if (tipo.var=="character")
  {
    v <- unique(dt_dados[,c.aux])
    dots <- ""
    if (length(v)>200) {v <- v[1:200]; dots <- "..."}
    cat("\nvalores: ",v,dots)
  }
  
  if (tipo.var=="integer" | tipo.var=="numeric")
  {
    if (tipo.var=="integer")
    {
      tabela <- table(dt_dados[,c.aux])
      print(tabela)
    }
    densidade <- density(dt_dados[,c.aux],na.rm=TRUE) 
    plot(densidade, main=ext.var,
         xlab=nome.var, ylab="densidade")
    analisado <- TRUE
  }
  
  if (!analisado)
  {
    cat("\nVariavel ainda nao analisada.\n")
  }
  
  if (pausa)
  {
    readline(prompt="Press [enter] to continue")
  }
}

