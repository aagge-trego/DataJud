
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