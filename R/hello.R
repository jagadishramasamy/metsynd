#' @title Diagnosis of Metabolic Syndrome
#'
#' @description This will be used to diagnose Metabolic syndrome
#'
#'
#' @param x
#' x is the data
#' @return
#' @export
#' @importFrom dplyr case_when
#'
#' @examples


 mets <- function(x) {case_when((x$Gender=="Male" & x$`Waist circumference cm`>=102 & x$`HDLC, mg/dL`< 40 &
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

