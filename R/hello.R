#' @title Diagnosis of Metabolic Syndrome
#'
#' @description
#'
#'
#' @param x the name of the person to say hi to
#'
#' @return the output from code
#' @export
#' @importFrom dplyr case_when
#'
#' @examples
#' mets("Jagadish")


 mets <- function(x) {
  print(paste0("Hello ",x,", how are you?"))
}
