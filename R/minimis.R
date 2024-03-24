de_minimis <- function(convocatoria = NULL, pages = NULL, order = NULL, direction = NULL) {

  order <- match.arg(order, c("convocante", "numeroConvocatoria", "reglamento", "instrumento",
                              "fechaConcesion", "fechaRegistro", "benificiario", "ayudaEquivalente"))
  direction <- match.arg(direction, c("asc", "desc"))
  req <- prepare_api() |>
    req_url_path_append("minimis/busqueda")

  page <- 0
  if (!is.null(convocatoria)) {
    resp1 <- req_url_query(req,
                           numeroConvocatoria = convocatoria, page = 0,
                           pageSize = length(convocatoria), order = "fechaConcesion",
                           direccion = "desc")
  }

    req_url_query(page = page, pageSize = 100, order = "fechaConcesion", direccion = "desc")
  # https://www.pap.hacienda.gob.es/bdnstrans/api/minimis/busqueda?vpd=GE&page=0&pageSize=100&order=fechaConcesion&direccion=desc
  # https://www.pap.hacienda.gob.es/bdnstrans/api/minimis/busqueda?vpd=GE&numeroConvocatoria=12345798&page=0&pageSize=100&order=fechaConcesion&direccion=desc
  resp1 <- req_perform(req)
  content1 <- resp_body_json(resp1)

  pages <- seq(page + 1, content1$totalPages)
  # pages <- 2:5
  reqs <- pages |> lapply(\(idx) req |> req_url_query(page = idx, .multi = "comma"))
  resps <- req_perform_sequential(reqs[1:1000], on_error = "continue")

  resps[[length(resps)+1]] <- resp1

  out <- resps_data(resps, function(resp) {
    cont <- resp_body_json(resp)
    out <- list2DF2(cont$content)
    colnames(out) <- names(cont$content[[1]])
    out
  } )

  colnames(out) <- names(content1$content[[1]])
  out
}
