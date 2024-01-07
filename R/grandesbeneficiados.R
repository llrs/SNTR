grandes_beneficiados <- function() {
    req <- prepare_api() |>
      req_url_path_append("grandesbeneficiarios")
# https://www.pap.hacienda.gob.es/bdnstrans/api/grandesbeneficiarios/busqueda?vpd=GE&anios=2020,2021&page=0&pageSize=100&order=ejercicio&direccion=desc

}

partidos_politicos <- function() {
    req <- prepare_api() |>
      req_url_path_append("partidospoliticos/busqueda")
# https://www.pap.hacienda.gob.es/bdnstrans/api/grandesbeneficiarios/busqueda?vpd=GE&anios=2020,2021&page=0&pageSize=100&order=ejercicio&direccion=desc

}


instrumentos <- function() {
    req <- prepare_api() |>
      req_url_path_append("instrumentos")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/instrumentos?vpd=GE
}

reglamentos <- function() {
    req <- prepare_api() |>
      req_url_path_append("reglamentos") |>
      req_url_query(ambito = "M")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/reglamentos?ambito=M&vpd=GE
}

sectores <- function() {
    req <- prepare_api() |>
      req_url_path_append("sectores") |>
      req_url_query(ambito = "C")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/sectores?ambito=C&vpd=GE
}

rc <- "https://www.pap.hacienda.gob.es/bdnstrans/api/partidospoliticos/exportar?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc&tipoDoc=csv"


