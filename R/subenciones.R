subvenciones <- function(...) {
    req <- prepare_api() |>
      req_url_path_append("concesiones/busqueda") |>
      req_url_query(vpd = "GE", page = 100, pageSize = 100, order="fechaConcesion", direccion = "desc")


}

# p <- jsonlite::read_json("https://www.pap.hacienda.gob.es/bdnstrans/api/concesiones/busqueda?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc")
# https://www.pap.hacienda.gob.es/bdnstrans/api/convocatorias?numConv=737599&vpd=GE
