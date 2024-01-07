sanciones <- function(...) {
  req <- prepare_api() |>
    req_url_path_append("sanciones/busqueda") |>
    req_url_query(vpd = "GE", page = 100, pageSize = 100, order="fechaConcesion", direccion = "desc")
}

#' Beneficiaries
#'
#' Look for entities that received a grant.
#' @param busqueda At least three characters of the entity you are searching.
#' @param ambitos Organization level to search for it.
#' @param ... Other queries.
#' @return A data.frame with the id, description, NIF and name of the entity.
#' @export
#' @examples
#' out <- beneficiarios("123")
beneficiarios <- function(busqueda, ambitos = "G", ...) {
  if (is.numeric(busqueda)) {
    busqueda <- as.character(busqueda)
  }
  if (!nchar(busqueda) >= 3) {
    stop("Requires at least 3 characters")
  }
  req <- prepare_api() |>
    req_url_path_append("terceros") |>
    req_url_query(ambito = ambitos, busqueda = busqueda, ...)
  # https://www.pap.hacienda.gob.es/bdnstrans/api/terceros?ambito=G&busqueda=456&vpd=GE
  resp <- req_perform(req)
  j <- resp_body_json(resp)
  rao <- do.call(rbind, lapply(j$terceros, list2DF))
  l <- strsplit(rao$descripcion, " - ", fixed = TRUE)
  rao2 <- cbind(rao, t(list2DF(l)))
  colnames(rao2)[3:4] <- c("NIF", "name")
  rownames(rao2) <- NULL
  rao2
}

# js <- jsonlite::read_json("https://www.pap.hacienda.gob.es/bdnstrans/api/regiones?vpd=GE")
