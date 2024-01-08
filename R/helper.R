prepare_api <- function() {
  request("https://www.pap.hacienda.gob.es") |>
    req_url_path_append("bdnstrans/api") |>
    req_throttle(rate = 15 / 60) |> # No hard limit, just to be polite
    req_user_agent("papr (https://github.com/llrs/papr)") |>
    req_url_query(vpd = "GE")
}

#' Site configuration
#'
#' Check the website configuration.
#' @return A list with the date of the update, legal warning and allowed routes.
#' @export
#' @examples
#' out <- configuration()
#' out$ultimaActualizacion
configuration <- function() {
  out <- prepare_api() |>
    req_url_path_append("vpd/GE/configuracion") |>
    req_perform() |>
    resp_body_json()
  out$rutasPermitidas <- unlist(out$rutasPermitidas, FALSE, FALSE)
  out$
  out
}
# https://www.pap.hacienda.gob.es/bdnstrans/api/vpd/GE/configuracion
#
#
#


list2DF2 <- function(x) {
  o <- do.call(rbind, lapply(x, list2DF))
  rownames(o) <- NULL
  o
}

#' Find which ids of this levels are appropriate.
#'
#' @param organisms A data.frame from [organism()].
#' @param levels A x with the oranism recevining the grant
#' @return A subset of the organism that matches the levels
#' @export
find_org_level <- function(levels, organisms) {
  which_level <- levels
  # First level
  organisms[which(organisms$descripcion == levels[, 3]), ]
  # TODO: find all the parents of that level
  # Note that the same id might be in different orgs so prune again those
  # that do not belong
}

#' Find which ids of this levels are appropriate.
#'
#' @param organisms A data.frame from [organism()].
#' @param levels A x with the oranism recevining the grant
#' @return A subset of the organism that matches the levels
#' @export
find_region_level <- function(levels, regions) {
  which_level <- levels
  # First level
  organisms[which(regions$descripcion == levels[, 3]), ]
  # TODO: find all the parents of that level
  # Note that the same id might be in different regions so prune again those
  # that do not belong
}


check_interval <- function(value, min, max) {
  if (value < min) {
    return(min)
  }
  if (value > max) {
    return(max)
  }
  value
}
