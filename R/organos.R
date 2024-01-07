#' Organism
#'
#' Look up for public organism.
#' @param admin Level of the organization: one of `c(Estatal = "C", Local = "L", Autonomica = "A", Otros = "O")`.
#' @param ... Other parameters to search them.
#'
#' @return A data.frame with the description, id, parent_id and admin level.
#' @export
#' @examples
#' oc <- organism("C")
#' ol <- organism("L")
#' oa <- organism("A")
#' oo <- organism("O")
organism <- function(admin = "C", ...) {
  admin <- match.arg(admin, c(Estatal = "C", Local = "L", Autonomica = "A", Otros = "O"))
  out <- prepare_api() |>
    req_url_path_append("organos") |>
    req_url_query(idAdmon = admin, ...) |>
    req_perform() |>
    resp_body_json()

  l <- lapply(out, org_node)
  o <- do.call(rbind, l)
  o$admin <- admin
  o
}

org_node <- function(x, parent_id = NA) {
  if (!is.list(x)) {
    return(x)
  }
  parent <- list2DF(x[c("id", "descripcion")])
  if (all(lengths(x) == 3L)) {
    out_a <- lapply(x, org_node)
    return(do.call(rbind, out_a))
  }
  parent$parent_id <- parent_id
  if (!is.null(x[["children"]]) && all(lengths(x[["children"]]) == 2L)) {
    children <- list2DF2(x[["children"]])
    children$parent_id <- parent$id
  } else if (!is.null(x[["children"]])) {
    children <- org_node(x[["children"]], parent_id)
  } else {
    children <- NULL
  }
  rbind(parent, children)
}

instrumentos <- function() {
  req <- prepare_api() |>
    req_url_path_append("instrumentos")
}

actividades <- function() {
  req <- prepare_api() |>
    req_url_path_append("actividades")

}

concesiones <- function() {
  req <- prepare_api() |>
    req_url_path_append("concesiones")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/concesiones/busqueda?vpd=GE&codConcesion=asfd%20aafd&fechaDesde=1/10/2024&fechaHasta=1/19/2024&descripcionTipoBusqueda=0&descripcion=adf%20adf%20&instrumentos=2&page=0&pageSize=100&order=fechaConcesion&direccion=desc
}

anios <- function() {
  req <- prepare_api() |>
    req_url_path_append("anios")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/grandesbeneficiarios/busqueda?vpd=GE&anios=2020,2021&page=0&pageSize=100&order=ejercicio&direccion=desc
}

# Order: ejercicio, fechaConcesion
