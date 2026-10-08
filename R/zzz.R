# Defaults de sessao: cache do usuario; nada de estado global
# obrigaorio - todas as funcoes recebem ou derivam caminhos.
#
# data.table-aware: sem isto, DT[, j, by] chamado a partir deste
# pacote cai no metodo [.data.frame (cedta() FALSE) e quebra.
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  op <- options()
  if (is.null(op$tsebr.cache_dir)) {
    options(tsebr.cache_dir =
              file.path(tools::R_user_dir("tsebr", "cache")))
  }
  invisible()
}
