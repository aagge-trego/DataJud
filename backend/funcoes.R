verificaFolhaCnj = function(df, df_cnj, str_tipo){
  dat = subset(df_cnj, folha == 1)
  df = merge(df, dat[,c('cod', 'folha')],
             by.x = paste0('cod_', str_tipo, '_cnj'), by.y = 'cod', all.x = T)
  
  df = transform(df, folha = ifelse(is.na(folha), 0, folha))
  
  table(df$folha)
  
  return(df)
}

cSubStr = function(x, i, f) as.numeric(substr(x, i, f))

verificaAg97 = function(nr){
  NNNNNNN = substr(nr, 1, 7)
  DD = substr(nr, 9, 10)
  AAAA = substr(nr, 12, 15)
  J = substr(nr, 17, 17)
  TR = substr(nr, 19, 20)
  OOOO = substr(nr, 22, 25)
  
  nr = as.numeric(paste0(NNNNNNN, AAAA, J, TR, OOOO, 00))
  
  verifica = ifelse(nr %% 97 == as.numeric(DD), T, F)
  
  return(verifica)
}
