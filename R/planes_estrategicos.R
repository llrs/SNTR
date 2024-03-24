#' Search strategic plans
#'
#' @param ...
#'
#' @return
#' @export
#' @references <https://www.pap.hacienda.gob.es/bdnstrans/GE/es/planesEstrategicos>
#' @examples
strategic_plans <- function(...) {
  req <- prepare_api() |>
    req_url_path_append("planesestrategicos/busqueda") |>
    req_url_query(page = 0, pageSize = 100, order = "fechaConcesion",
                  direccion = "desc")
  req |> req_perform() |> resp_body_json()
}
