% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MetabolicSyndrome.R
\name{MetabolicSyndrome}
\alias{MetabolicSyndrome}
\title{Diagnosis of Metabolic Syndrome}
\usage{
MetabolicSyndrome(x)
}
\arguments{
\item{x}{a data frame with column names as exactly specified.}
}
\value{
Yes or No
}
\description{
The modified Adult Treatment Panel -III guidelines (ATP-III) proposed by American Heart Association (AHA) and National Heart, Lung and Blood Institute (NHLBI) are used widely for the clinical diagnosis of Metabolic Syndrome. The AHA-NHLBI criteria advise using parameters such as waist circumference (WC), systolic blood pressure (SBP), diastolic blood pressure (DBP), fasting plasma glucose (FPG), triglycerides (TG) and high-density lipoprotein cholesterol (HDLC) for diagnosis of metabolic syndrome. Each parameter has to be interpreted based on the proposed cut-offs, making the diagnosis slightly complex and error-prone. This package is developed by incorporating the modified ATP-III guidelines, and it will aid in the easy and quick diagnosis of metabolic syndrome in busy healthcare settings and also for research purposes. The modified ATP-III-AHA-NHLBI criteria for the diagnosis is described by Grundy et al ., (2005) <doi:10.1161/CIRCULATIONAHA.105.169404>.
}
\examples{
MetabolicSyndrome(x)
}
