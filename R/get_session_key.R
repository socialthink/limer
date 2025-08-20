#' Get a LimeSurvey API session key
#'
#' This function logs into the LimeSurvey API and provides an access session key.
#' @param username LimeSurvey username. Defaults to value set in \code{options()}.
#' @param password LimeSurvey password Defaults to value set in \code{options()}.
#' @return API token
#' @import httr
#' @export
#' @examples \dontrun{
#' get_session_key()
#' }

get_session_key <- function(username = getOption("lime_username"), password = getOption("lime_password")) {
  
  # API-Parameter in einem Vektor (entspricht einem JSON-Array)
  # Die Reihenfolge ist wichtig: username, password
  body.json <- list(
    method = "get_session_key",
    id = "1", # Die id kann ein beliebiger Wert sein, z.B. 1
    params = c(username, password)
  )
  
  # Sende den POST-Request an die API-Schnittstelle
  r <- httr::POST(
    getOption("lime_api"),
    httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE)
  )
  
  # Überprüfe den HTTP-Statuscode
  if (httr::status_code(r) != 200) {
    stop(paste("API-Fehler:", httr::status_code(r), httr::content(r, "text")))
  }
  
  # Parse die JSON-Antwort
  api_response <- jsonlite::fromJSON(httr::content(r, "text", encoding = "utf-8"))
  
  # Prüfe, ob die Antwort ein Fehler ist
  if (!is.null(api_response$error)) {
    stop(paste("Server-Fehler:", api_response$error))
  }
  
  # Extrahiere den Session Key
  session_key <- as.character(api_response$result)
  
  # Optional: Session Key im Cache speichern (wie in der Originalfunktion)
  if (exists("session_cache")) {
    session_cache$session_key <- session_key
  }
  
  # >>> korrekt im limer-Cache speichern
  ns <- asNamespace("limer")
  if (exists("session_cache", envir = ns, inherits = FALSE)) {
    sess_env <- get("session_cache", envir = ns)
    sess_env$session_key <- session_key
  }
  
  return(session_key)
}

# Start a new environment to hold the session key so all other functions can access it
# See http://trestletech.com/2013/04/package-wide-variablescache-in-r-package/
session_cache <- new.env(parent = emptyenv())
