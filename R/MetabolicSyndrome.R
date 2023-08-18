#' @title Diagnosis of Metabolic Syndrome
#'
#' @description The modified Adult Treatment Panel -III guidelines (ATP-III) proposed by American Heart Association (AHA) and National Heart, Lung and Blood Institute (NHLBI) are used widely for the clinical diagnosis of Metabolic Syndrome. The AHA-NHLBI criteria advise using parameters such as waist circumference (WC), systolic blood pressure (SBP), diastolic blood pressure (DBP), fasting plasma glucose (FPG), triglycerides (TG) and high-density lipoprotein cholesterol (HDLC) for diagnosis of metabolic syndrome. Each parameter has to be interpreted based on the proposed cut-offs, making the diagnosis slightly complex and error-prone. This package is developed by incorporating the modified ATP-III guidelines, and it will aid in the easy and quick diagnosis of metabolic syndrome in busy healthcare settings and also for research purposes. The modified ATP-III-AHA-NHLBI criteria for the diagnosis is described by Grundy et al ., (2005) <doi:10.1161/CIRCULATIONAHA.105.169404>.
#'
#' @param x a data frame with column names as exactly specified.
#'
#' @return Yes or No
#' @export
#' @importFrom dplyr case_when
#'
#' @examples
#' MetabolicSyndrome(x)


MetabolicSyndrome <- function(x) {case_when((x$Gender=="Male" & x$WC >=102 & x$HDLC < 40 &
                           (x$TG  >= 150 |x$FPG >= 100 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$WC >=102 & x$TG >= 150  &
                            (x$Gender=="Male" & x$HDLC < 40|x$FPG>= 100 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$WC >=102 & x$FPG>= 100   &
                            (x$Gender=="Male" & x$HDLC < 40| x$TG >= 150 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$WC >=102 & x$SBP >= 130   &
                            (x$Gender=="Male" & x$HDLC < 40| x$TG >= 150 | x$FPG>=100 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$WC >=102 & x$DBP >= 85   &
                            (x$Gender=="Male" & x$HDLC < 40| x$TG >= 150 | x$FPG>=100 | x$SBP >= 130))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$HDLC < 40 & x$TG >= 150 &
                            (x$Gender=="Male" & x$WC >=102 |x$FPG>=100 |x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$HDLC < 40 & x$FPG>=100  &
                            (x$WC >=102 |x$TG >= 150|x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$HDLC < 40 & x$SBP >= 130  &
                            (x$Gender=="Male" & x$WC >=102 |x$TG >= 150|x$FPG>=100 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$HDLC < 40 & x$DBP >= 85  &
                            (x$Gender=="Male" & x$WC >=102 |x$TG >= 150|x$FPG>=100 |x$SBP >= 130 ))~'Yes',
                         (x$Gender=="Male"  & x$TG >= 150  &x$FPG>=100 &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$SBP >= 130 |x$DBP >= 85))~'Yes',
                         (x$Gender=="Male"  & x$TG >= 150  & x$SBP >= 130 &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$FPG>=100|x$DBP >= 85))~'Yes',
                         (x$Gender=="Male"  & x$TG >= 150  &x$DBP >= 85  &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$FPG>=100|x$SBP >= 130))~'Yes',
                         (x$Gender=="Male"  & x$FPG>=100  & x$SBP >= 130  &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$TG >= 150|x$DBP >= 85))~'Yes',
                         (x$Gender=="Male"  & x$FPG>=100  &  x$DBP >= 85 &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$TG >= 150|x$SBP >= 130))~'Yes',
                         (x$Gender=="Male"  & x$SBP >= 130 &  x$DBP >= 85 &
                            ( x$Gender=="Male" & x$WC >=102 |x$Gender=="Male" & x$HDLC < 40 |x$TG >= 150|x$FPG>=100))~'Yes',
                         (x$Gender=="Female" & x$WC >=88 & x$HDLC < 50 &
                            (x$TG >= 150 |x$FPG>= 100 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$WC >=88 & x$TG >= 150  &
                            (x$Gender=="Female" & x$HDLC < 50|x$FPG>= 100 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$WC >=88 & x$FPG>= 100   &
                            (x$Gender=="Female" & x$HDLC < 50| x$TG >= 150 | x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$WC >=88 & x$SBP >= 130   &
                            (x$Gender=="Female" & x$HDLC < 50| x$TG >= 150 | x$FPG>=100 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$WC >=88 & x$DBP >= 85   &
                            (x$Gender=="Female" & x$HDLC < 50| x$TG >= 150 | x$FPG>=100 | x$SBP >= 130))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$HDLC < 50 & x$TG >= 150 &
                            (x$Gender=="Female" & x$WC >=88 |x$FPG>=100 |x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$HDLC < 50 & x$FPG>=100  &
                            (x$WC >=88 |x$TG >= 150|x$SBP >= 130 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$HDLC < 50 & x$SBP >= 130  &
                            (x$Gender=="Female" & x$WC >=88 |x$TG >= 150|x$FPG>=100 | x$DBP >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$HDLC < 50 & x$DBP >= 85  &
                            (x$Gender=="Female" & x$WC >=88 |x$TG >= 150|x$FPG>=100 |x$SBP >= 130 ))~'Yes',
                         (x$Gender=="Female"  & x$TG >= 150  & x$FPG>=100 &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$SBP >= 130 |x$DBP >= 85))~'Yes',
                         (x$Gender=="Female"  & x$TG >= 150  & x$SBP >= 130 &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$FPG>=100|x$DBP >= 85))~'Yes',
                         (x$Gender=="Female"  & x$TG >= 150  & x$DBP >= 85  &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$FPG>=100|x$SBP >= 130))~'Yes',
                         (x$Gender=="Female"  & x$FPG>=100  & x$SBP >= 130  &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$TG >= 150|x$DBP >= 85))~'Yes',
                         (x$Gender=="Female"  & x$FPG>=100  &  x$DBP >= 85 &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$TG >= 150|x$SBP >= 130))~'Yes',
                         (x$Gender=="Female"  & x$SBP >= 130 &  x$DBP >= 85 &
                            ( x$Gender=="Female" & x$WC >=88 |x$Gender=="Female" & x$HDLC < 50 |x$TG >= 150|x$FPG>=100  ))~'Yes',
                TRUE~'No')}

