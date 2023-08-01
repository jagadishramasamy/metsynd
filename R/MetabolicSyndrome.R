#' @title Diagnosis of Metabolic Syndrome
#'
#' @description This package will be used to diagnose metabolic syndrome.The criteria used to diagnose is based on the National Cholesterol Education Program - Adult Treatment Panel -III guidelines (NCEP-ATP-III). It is comprised of multiple parameters which includes waist circumference, systolic blood pressure, diastolic blood pressure, fasting plasma glucose, triglycerides and high-density lipoprotein cholesterol. Hence, this package will be useful in the healthcare settings for easy and quick diagnosis of metabolic syndrome.
#'
#'
#' @param x is the data set containing the following parameters such as
#' @param Gender   as Male or Female
#' @param Waist_Circumference Waist Circumference in cm
#' @param Triglycerides Triglycerides in mg/dL
#' @param Fasting_plasma_glucose Fasting_plasma_glucose in mg/dL
#' @param HDL-cholesterol HDL-cholesterol in mg/dL
#' @param Systolic_BP Systolic BP in mm of Hg
#' @param Diastolic_BP Diastolic BP in mm of Hg
#'
#'
#' @return
#' @export
#' @importFrom dplyr case_when
#'
#' @examples
#' MetabolicSyndrome(x)


MetabolicSyndrome <- function(x) {case_when((x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`HDLC, mg/dL`< 40 &
                           (x$`Triglycerides, mg/dL` >= 150 |x$`Fasting plasma glucose, mg/dL` >= 100 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`Triglycerides, mg/dL` >= 150  &
                            (x$Gender=="Male" & x$`HDLC, mg/dL`< 40|x$`Fasting plasma glucose, mg/dL` >= 100 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`Fasting plasma glucose, mg/dL` >= 100   &
                            (x$Gender=="Male" & x$`HDLC, mg/dL`< 40| x$`Triglycerides, mg/dL` >= 150 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`Systolic BP` >= 130   &
                            (x$Gender=="Male" & x$`HDLC, mg/dL`< 40| x$`Triglycerides, mg/dL` >= 150 | x$`Fasting plasma glucose, mg/dL`>=100 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`Diastolic BP` >= 85   &
                            (x$Gender=="Male" & x$`HDLC, mg/dL`< 40| x$`Triglycerides, mg/dL` >= 150 | x$`Fasting plasma glucose, mg/dL`>=100 | x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$`HDLC, mg/dL`< 40 & x$`Triglycerides, mg/dL` >= 150 &
                            (x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$`Fasting plasma glucose, mg/dL`>=100 |x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$`HDLC, mg/dL`< 40 & x$`Fasting plasma glucose, mg/dL`>=100  &
                            (x$`Waist circumference cm`>=102 |x$`Triglycerides, mg/dL` >= 150|x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$`HDLC, mg/dL`< 40 & x$`Systolic BP` >= 130  &
                            (x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male" & x$Gender=="Male" & x$`HDLC, mg/dL`< 40 & x$`Diastolic BP` >= 85  &
                            (x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100 |x$`Systolic BP` >= 130 ))~'Yes',
                         (x$Gender=="Male"  & x$`Triglycerides, mg/dL` >= 150  &x$`Fasting plasma glucose, mg/dL`>=100 &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Systolic BP` >= 130 |x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male"  & x$`Triglycerides, mg/dL` >= 150  & x$`Systolic BP` >= 130 &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Fasting plasma glucose, mg/dL`>=100|x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male"  & x$`Triglycerides, mg/dL` >= 150  &x$`Diastolic BP` >= 85  &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Fasting plasma glucose, mg/dL`>=100|x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Male"  & x$`Fasting plasma glucose, mg/dL`>=100  & x$`Systolic BP` >= 130  &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Triglycerides, mg/dL` >= 150|x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Male"  & x$`Fasting plasma glucose, mg/dL`>=100  &  x$`Diastolic BP` >= 85 &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Triglycerides, mg/dL` >= 150|x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Male"  & x$`Systolic BP` >= 130 &  x$`Diastolic BP` >= 85 &
                            ( x$Gender=="Male" & x$`Waist circumference cm`>=102 |x$Gender=="Male" & x$`HDLC, mg/dL`< 40 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100))~'Yes',
                         (x$Gender=="Female" & x$`Waist circumference cm`>=88 & x$`HDLC, mg/dL`< 50 &
                            (x$`Triglycerides, mg/dL` >= 150 |x$`Fasting plasma glucose, mg/dL` >= 100 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$`Waist circumference cm`>=88 & x$`Triglycerides, mg/dL` >= 150  &
                            (x$Gender=="Female" & x$`HDLC, mg/dL`< 50|x$`Fasting plasma glucose, mg/dL` >= 100 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$`Waist circumference cm`>=88 & x$`Fasting plasma glucose, mg/dL` >= 100   &
                            (x$Gender=="Female" & x$`HDLC, mg/dL`< 50| x$`Triglycerides, mg/dL` >= 150 | x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$`Waist circumference cm`>=88 & x$`Systolic BP` >= 130   &
                            (x$Gender=="Female" & x$`HDLC, mg/dL`< 50| x$`Triglycerides, mg/dL` >= 150 | x$`Fasting plasma glucose, mg/dL`>=100 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$`Waist circumference cm`>=88 & x$`Diastolic BP` >= 85   &
                            (x$Gender=="Female" & x$`HDLC, mg/dL`< 50| x$`Triglycerides, mg/dL` >= 150 | x$`Fasting plasma glucose, mg/dL`>=100 | x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$`HDLC, mg/dL`< 50 & x$`Triglycerides, mg/dL` >= 150 &
                            (x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$`Fasting plasma glucose, mg/dL`>=100 |x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$`HDLC, mg/dL`< 50 & x$`Fasting plasma glucose, mg/dL`>=100  &
                            (x$`Waist circumference cm`>=88 |x$`Triglycerides, mg/dL` >= 150|x$`Systolic BP` >= 130 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$`HDLC, mg/dL`< 50 & x$`Systolic BP` >= 130  &
                            (x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100 | x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female" & x$Gender=="Female" & x$`HDLC, mg/dL`< 50 & x$`Diastolic BP` >= 85  &
                            (x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100 |x$`Systolic BP` >= 130 ))~'Yes',
                         (x$Gender=="Female"  & x$`Triglycerides, mg/dL` >= 150  &x$`Fasting plasma glucose, mg/dL`>=100 &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Systolic BP` >= 130 |x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female"  & x$`Triglycerides, mg/dL` >= 150  & x$`Systolic BP` >= 130 &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Fasting plasma glucose, mg/dL`>=100|x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female"  & x$`Triglycerides, mg/dL` >= 150  &x$`Diastolic BP` >= 85  &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Fasting plasma glucose, mg/dL`>=100|x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Female"  & x$`Fasting plasma glucose, mg/dL`>=100  & x$`Systolic BP` >= 130  &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Triglycerides, mg/dL` >= 150|x$`Diastolic BP` >= 85))~'Yes',
                         (x$Gender=="Female"  & x$`Fasting plasma glucose, mg/dL`>=100  &  x$`Diastolic BP` >= 85 &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Triglycerides, mg/dL` >= 150|x$`Systolic BP` >= 130))~'Yes',
                         (x$Gender=="Female"  & x$`Systolic BP` >= 130 &  x$`Diastolic BP` >= 85 &
                            ( x$Gender=="Female" & x$`Waist circumference cm`>=88 |x$Gender=="Female" & x$`HDLC, mg/dL`< 50 |x$`Triglycerides, mg/dL` >= 150|x$`Fasting plasma glucose, mg/dL`>=100  ))~'Yes',
                TRUE~'No')}

