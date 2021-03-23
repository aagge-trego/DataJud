#configuracoes =================================================================
rm(list = ls())

library(stringr)
library(stringi)

source('funcoes.R')
#===============================================================================

#funcoes =======================================================================
trataTexto = function(txt){
  lista_serven = c('ALDERICO ROCHA SANTOS - Juiz Federal',
                   'ÁTILA NAVES AMARAL - Juiz de Direito 1',
                   'DFEACC',
                   'DFEANS',
                   'DFEGNA',
                   'DFELZA',
                   'JOSÉ PROTO DE OLIVEIRA - Juiz Auxiliar 2',
                   'JOSÉ PROTO DE OLIVEIRA - Juiz de Direito 2',
                   'JULIANO TAVEIRA BERNARDES - Juiz Auxiliar 1',
                   'LEANDRO CRISPIM - Presidente',
                   'LUIZ EDUARDO DE SOUSA - Corregedor Regional Eleitoral',
                   'LUIZ EDUARDO DE SOUSA - Vice-Presidente',
                   'MARCIO ANTONIO DE SOUSA MORAES JUNIOR - Jurista 1',
                   'OVIDIO MARTINS DE ARAUJO - Juiz Auxiliar 3',
                   'PA095ITA',
                   'PA132HID',
                   'VICENTE LOPES DA ROCHA JÚNIOR - Jurista 2')
  
  if(txt %in% lista_serven){
    txt = switch(txt,
                 'ALDERICO ROCHA SANTOS - Juiz Federal' = 'GBJF',
                 'ÁTILA NAVES AMARAL - Juiz de Direito 1' = 'GBJDI',
                 'JOSÉ PROTO DE OLIVEIRA - Juiz Auxiliar 2' = 'GBJAII',
                 'JOSÉ PROTO DE OLIVEIRA - Juiz de Direito 2' = 'GBJDII',
                 'JULIANO TAVEIRA BERNARDES - Juiz Auxiliar 1' = 'GBJAI',
                 'LEANDRO CRISPIM - Presidente' = 'PRES',
                 'LUIZ EDUARDO DE SOUSA - Corregedor Regional Eleitoral' = 'VPCRE',
                 'LUIZ EDUARDO DE SOUSA - Vice-Presidente' = 'VPCRE',
                 'MARCIO ANTONIO DE SOUSA MORAES JUNIOR - Jurista 1' = 'GBJUI',
                 'OVIDIO MARTINS DE ARAUJO - Juiz Auxiliar 3' = 'GBJAIII',
                 'VICENTE LOPES DA ROCHA JÚNIOR - Jurista 2' = 'GBJUII',
                 txt)
  } else {
    for(letra in LETTERS[1:26]) txt = gsub(letra, '', txt)
    txt = gsub(' ', '', txt)
    txt = gsub('Á', '', txt)
    txt = gsub('ª', '', txt)
    txt = gsub('Â', '', txt)
    txt = gsub('Ô', '', txt)
    txt = gsub('Í', '', txt)
    txt = gsub('Ç', '', txt)
    txt = gsub('Ú', '', txt)
    txt = gsub('Ã', '', txt)
    txt = gsub('ÃÍ', '', txt)
    txt = gsub('Ó', '', txt)
    txt = gsub('Ô', '', txt)
    txt = gsub('É', '', txt)
    
    txt = as.numeric(txt)
  }
  
  return(txt)
}
#===============================================================================

#tabelas =======================================================================
assuntos = read.csv('01 dados brutos/assuntos.csv',
                    header = T,
                    encoding = 'UTF-8')
complementos = read.csv('01 dados brutos/complementos.csv',
                        header = T,
                        encoding = 'UTF-8')
processos = read.csv('01 dados brutos/processos.csv',
                     header = T, 
                     encoding = 'UTF-8')
movimentos = read.csv('01 dados brutos/movimentos.csv',
                      header = T, 
                      encoding = 'UTF-8')

assuntos_cnj = read.csv('01 dados brutos/assuntos-cnj.csv',
                        header = T,
                        encoding = 'UTF-8')
classes_cnj = read.csv('01 dados brutos/classes-cnj.csv',
                       header = T,
                       encoding = 'UTF-8')
movimentos_cnj = read.csv('01 dados brutos/movimentos-cnj.csv',
                          header = T,
                          encoding = 'UTF-8')
#===============================================================================

#regra 1 (classes) =============================================================
processos = verificaFolhaCnj(processos, classes_cnj, 'classe')
#===============================================================================

#regra 2 (assuntos) ============================================================
assuntos = verificaFolhaCnj(assuntos, assuntos_cnj, 'assunto')
#===============================================================================

#regra 3 (movimentos) ==========================================================
movimentos = verificaFolhaCnj(movimentos, movimentos_cnj, 'movimento')
#===============================================================================

#regra 4 (pelos menos um assunto folha) ========================================
dat = aggregate(folha ~ nr_unico, FUN = sum, data = assuntos)
colnames(dat)[2] = 'qtd_assuntos_folha'
assuntos = merge(assuntos, dat, by = 'nr_unico', all.x = T)
#===============================================================================

#regra 5 (nr unico) ============================================================
dat_nr_unico = unique(processos['nr_unico'])
dat_nr_unico = transform(dat_nr_unico, NNNNNNN = cSubStr(nr_unico, 1, 7),
                                       DD = cSubStr(nr_unico, 9, 10),
                                       AAAA = cSubStr(nr_unico, 12, 15),
                                       J = cSubStr(nr_unico, 17, 17),
                                       TR = cSubStr(nr_unico, 19, 20),
                                       OOOO = cSubStr(nr_unico, 22, 25))
dat_nr_unico = transform(dat_nr_unico,
                         vTAMANHO = ifelse(str_length(nr_unico) != 25, 0, 1),
                         vNNNNNNN = ifelse(is.na(NNNNNNN), 0, 1),
                         vDD = ifelse(verificaAg97(nr_unico), 1, 0),
                         vAAAA = ifelse(is.na(AAAA) | AAAA < 2010 |
                                         AAAA > as.numeric(format(Sys.Date(),
                                                                  '%Y')),
                                       0, 1),
                         vJ = ifelse(J != 6, 0, 1),
                         vTR = ifelse(TR != 9, 0, 1),
                         vOOOO = ifelse(is.na(OOOO), 0, 1))

dat_nr_unico = cbind(dat_nr_unico,
                     status_nr_unico = ifelse(rowSums(dat_nr_unico[,8:14]) == 7,
                                              1, 0))

processos = merge(processos, dat_nr_unico[,c('nr_unico', 'status_nr_unico')],
                  by = 'nr_unico', all = T)
#===============================================================================

#regra 55 (repeticao de nr unico ) =============================================
#adicionando uma indicadora da duplicidade de numero unico:
dat = data.frame(nr_unico = names(table(processos['nr_unico'])
                                [table(processos['nr_unico']) > 1]),
                 qtd_repeticoes = as.numeric(table(processos['nr_unico'])
                                             [table(processos['nr_unico']) > 1]))
processos = merge(processos, dat, by = 'nr_unico', all.x = T)

processos = transform(processos,
                      qtd_repeticoes = ifelse(is.na(qtd_repeticoes),
                                                0, qtd_repeticoes))
#===============================================================================

#tabela de-para ================================================================
dat_aux = subset(processos, select = c('origem', 'id_orgao_julgador_origem',
                                       'nome_orgao_julgador_origem'))
dat = aggregate(ind ~ origem + id_orgao_julgador_origem +
                  nome_orgao_julgador_origem,
                FUN = sum, data = data.frame(dat_aux, ind = 1))[,1:3]

dat['de_para'] = apply(dat['nome_orgao_julgador_origem'], 1, trataTexto)

serventias_1g = read.csv('01 dados brutos/serventias-1g.csv',
                         header = T, sep = ';', encoding = 'UTF-8')
serventias_1g = transform(serventias_1g, 
                          de_para = as.numeric(substr(serventia_nome, 1, 3)))
serventias_2g = read.csv('01 dados brutos/serventias-2g.csv',
                         header = T, sep = ';', encoding = 'UTF-8')
serventias_2g['de_para'] = c('VPCRE','GBJI','GBJII','GBJDI','GBJDII','GBJF',
                             'GBJAI','GBJAII','GBJAIII','PRES')

serventias = rbind(serventias_1g[,c('serventia_id', 'de_para')],
                   serventias_2g[,c('serventia_id', 'de_para')])

dat = merge(dat, serventias, by = 'de_para', all = T)

rm(dat_aux, serventias, serventias_1g, serventias_2g)
#===============================================================================
