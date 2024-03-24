#' Grandes beneficiados
#'
#' Those that benefited most.
#' @return A data.frame with the most benefited people.
#' @export
grandes_beneficiados <- function() {
  req <- prepare_api() |>
    req_url_path_append("grandesbeneficiarios")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/grandesbeneficiarios/busqueda?vpd=GE&anios=2020,2021&page=0&pageSize=100&order=ejercicio&direccion=desc

}

#' Search concessions to political parties
#'
#' @return A data.frame with political parties
#' @export
political_parties <- function() {
  req <- prepare_api() |>
    req_url_path_append("partidospoliticos/busqueda")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/grandesbeneficiarios/busqueda?vpd=GE&anios=2020,2021&page=0&pageSize=100&order=ejercicio&direccion=desc

  rr <- req |> req_perform() |> resp_body_json()
  list2DF2(rr$content)
}


#' Instruments
#'
#' Instruments used
#'
#' @return A data.frame with instruments used
#' @export
instruments <- function() {

  req <- prepare_api() |>
    req_url_path_append("instrumentos")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/instrumentos?vpd=GE
  req |> req_perform() |> resp_body_json()
}

reglamentos <- function() {
  req <- prepare_api() |>
    req_url_path_append("reglamentos") |>
    req_url_query(ambito = "M")

  # https://www.pap.hacienda.gob.es/bdnstrans/api/reglamentos?ambito=M&vpd=GE
  list2DF2(resp_body_json(req_perform(req)))
}

#' Sectors
#'
#' @return
#' @export
#'
#' @examples
sectors <- function(area = NULL) {
  # area <- match.arg(area, c("C", "M", "L"))

  req <- prepare_api() |>
    req_url_path_append("sectores") |>
    req_url_query(ambito = area)
  list2DF2(resp_body_json(req_perform(req)))
  # https://www.pap.hacienda.gob.es/bdnstrans/api/sectores?ambito=C&vpd=GE
}

rc <- "https://www.pap.hacienda.gob.es/bdnstrans/api/partidospoliticos/exportar?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc&tipoDoc=csv"


