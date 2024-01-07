de_minimis <- function() {
  req <- prepare_api() |>
    req_url_path_append("minimis/busqueda") |>
    req_url_query(vpd = "GE", page = 100, pageSize = 100, order="fechaConcesion", direccion = "desc")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/minimis/busqueda?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc
  # https://www.pap.hacienda.gob.es/bdnstrans/api/minimis/busqueda?vpd=GE&numeroConvocatoria=12345798&page=0&pageSize=100&order=fechaConcesion&direccion=desc
}
