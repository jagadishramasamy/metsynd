#' Read CSV
#'
#' @param path to filename
#'
#' @return a\code{tibble}
#' @export
#' @importFrom readr read_csv
#' @examples
#' csv = system.file("extdata","Data.csv", package="metsjuly")
#' sample_read(csv )
sample_read = function(path){
  readr::read_csv(path )
}
