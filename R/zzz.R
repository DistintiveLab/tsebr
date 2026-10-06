# Defaults de sessao: cache do usuario; nada de estado global
# obrigaorio - todas as funcoes recebem ou derivam caminhos.
.onLoad <- function(libname, pkgname) {
  op <- options()
  if (is.null(op$tsebr.cache_dir)) {
    options(tsebr.cache_dir =
              file.path(tools::R_user_dir("tsebr", "cache")))
  }
  invisible()
}
