#' Sample data for Analysis
#'
#' @param path to filename
#' @description
#' A simple dataset to test and run the code
#'
#' @return a \code{data.frame} which includes:
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
#' csv = system.file("extdata","Data.csv", package="metsjuly")
#' sample_data(csv )
sample_data = function(path){
  readr::read_csv(path )
}
