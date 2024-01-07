page_size <- c(100, 1000, 1000)
search_convocatorias <- function(n = 20, ...) {

  min()
  # https://www.pap.hacienda.gob.es/bdnstrans/api/convocatorias/busqueda?vpd=GE&page=0&pageSize=100&order=fechaRecepcion&direccion=desc
  req <- prepare_api() |>
    req_url_path_append("convocatorias/busqueda") |>
    req_url_query(page = "0", pageSize = "10000",
                  order = "fechaRecepcion", direccion = "desc")

  resps <- req_perform_iterative(
    req,
    next_req = iterate_with_offset("page",
                                   resp_pages = function(resp) {
                                     resp_body_json(resp)$totalPages
                                   },
    ),
    max_reqs = 20
  )

  l2 <- lapply(resps, function(x){
    r <- resp_body_json(x)
    l <- lapply(r$content, function(y){
      ly <- lengths(y)
      y[ly == 0L] <- NA
      list2DF(y)
    })
    do.call(rbind, l)
  })
  out <- do.call(rbind, l2)
}
# p <- jsonlite::read_json("https://www.pap.hacienda.gob.es/bdnstrans/api/concesiones/busqueda?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc")
# https://www.pap.hacienda.gob.es/bdnstrans/api/convocatorias?numConv=737599&vpd=GE
#' @examples
#' convocatoria(721099)
convocatoria <- function(convocatoria, ...) {
  req <- prepare_api() |>
    req_url_path_append("convocatorias") |>
    req_url_query(numConv = convocatoria)
  resp <- req_perform(req) |>
    resp_body_json()
# TODO parse the response

}
