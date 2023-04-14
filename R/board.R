#' Connect to the {pins} board where data will be published
#'
#' The API key for connecting to the Connect board is obtained from the environment variable
#' `***REMOVED***`.
#'
#' @param   setting   Scalar character. Either "prod" (default; for connecting to the production
#'   pins board) or "test" (for creating a local pin board, useful for debugging).
#' @return   A `pins_board` object.

get_pins_board <- function(setting = c("prod", "test")) {
  setting <- match.arg(setting)

  if (setting == "test") {
    return(pins::board_local())
  }

  api_key <- get_pins_api_key()

  pins::board_connect(
    auth = "manual",
    account = "***REMOVED***",
    server = "***REMOVED***",
    key = api_key
  )
}

get_pins_api_key <- function() {
  # TODO: alternative route for defining api key, using {keyring}
  Sys.getenv("***REMOVED***")
}
