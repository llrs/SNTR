#' Download document
#'
#' @param document Id of the document to download.
#' @param name  Path and name of the document on your system.
#'
#' @return The path of the file if it has been successfully saved.
#' @export
#' @importFrom utils download.file
#' @examples
#' documento(963765, "~/Downloads/document_963765.pdf")
documento <- function(document, name ) {
  url <- paste0("https://www.pap.hacienda.gob.es/bdnstrans/api/convocatorias/documentos?idDocumento=", documento)
  download.file(url, destfile = name)
  # resp <- prepare_api() |>
  #   req_url_path_append("convocatorias/documentos") |>
  #   req_url_query(idDocumento = 963765) |>
  #   req_perform()
  name
}
