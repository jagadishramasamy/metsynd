#' Sample data for Analysis
#'
#' @param path path to filename
#' @description
#' A simple dataset to test and run the code
#'
#' @return a \code{tibble} which includes:
#' \describe{
#' \item{Gender}{Gender - Male or Female}
#' \item{Wasist circumference cm}{Waist circumference in cm}
#' \item{Triglycerides, mg/dL}{Triglycerides in mg/dL}
#' \item{Fasting plasma glucose, mg/dL}{Fasting plasma glucose in mg/dL}
#' \item{HDLC, mg/dL}{HDL-cholesterol in mg/dL}
#' \item{Systolic BP}{Systolic BP in mm of Hg}
#' \item{Diastolic BP}{Diastolic BP in mm of Hg}
#' }
#' @export
#'
#' @importFrom readr read_csv
#' @examples
#' csv = system.file("extdata","Data.csv", package="metsynd")
#' sample_read(csv)
sample_read = function(path){
  readr::read_csv(path)
}

