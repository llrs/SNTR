planes_estrategicos <- function(...) {
  req <- prepare_api() |>
    req_url_path_append("planesestrategicos/busqueda") |>
    req_url_query(page = 0, pageSize = 100, order ="fechaConcesion", direccion = "desc")
}
