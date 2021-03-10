#configuracoes =================================================================
rm(list = ls())

library(stringr)
library(stringi)

source('funcoes.R')
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

head(processos)
