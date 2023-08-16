#' @title Diagnosis of Metabolic Syndrome
#'
#' @description The criteria used to diagnose is based on the modified Adult Treatment Panel -III guidelines (ATP-III) guidelines proposed by American Heart Association and National Heart, Lung and Blood Institute. It is comprised of multiple parameters which includes waist circumference, systolic blood pressure, diastolic blood pressure, fasting plasma glucose, triglycerides and high-density lipoprotein cholesterol. Hence, this package will be useful in the healthcare settings for easy and quick diagnosis of metabolic syndrome. The modified ATP-III criteria for the diagnosis is described by Grundy et al ., (2005) <doi:10.1161/CIRCULATIONAHA.105.169404>.
#'
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

