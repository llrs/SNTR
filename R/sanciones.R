# order = nivel{1,2,3}
# tipoAdministracion=O,A,C,L
# organos=4622
# actividad=344,363
# regiones=107
# instrumentos=2

#' Search sanctions
#'
#' Search organism that were sanctioned.
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
search_sanctions <- function(n = 100, ...) {

  n_pages <- ceiling(n / check_interval(n, 100, formals()$n))
  req <- prepare_api() |>
    req_url_path_append("sanciones/busqueda") |>
    req_url_query(vpd = "GE", page = 0, pageSize = 100,
                  order = "fechaConcesion", direccion = "desc")
  # page=0&pageSize=100&order=nivel1&direccion=desc
  # vpd=GE
  # tipoAdministracion=O
  # organos=4622
  # page=0
  # pageSize=100
  # order=nivel1
  # direccion=desc

  resps <- req_perform_iterative(
    req,
    next_req = iterate_with_offset("page",
                                   resp_pages = function(resp) {
                                     resp_body_json(resp)$totalPages
                                   },
    ),
    max_reqs = n_pages
  )
}

#' Beneficiaries
#'
#' Look for entities that received a grant.
#' @param busqueda At least three characters of the entity you are searching.
#' @param ambitos Organization level to search for it.
#' @param ... Other queries.
#' @return A data.frame with the id, description, NIF and name of the entity.
#' @export
#' @import httr2
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
