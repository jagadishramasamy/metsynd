
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Metabolic Syndrome

<!-- badges: start -->
<!-- badges: end -->

This package will be used to diagnose metabolic syndrome. The criteria
used to diagnose is based on the National Cholesterol Education
Program - Adult Treatment Panel -III guidelines (NCEP-ATP-III). It is
comprised of multiple parameters which includes waist circumference,
systolic blood pressure, diastolic blood pressure, fasting plasma
glucose, triglycerides and high-density lipoprotein cholesterol. Hence,
this package will be useful in the healthcare settings for easy and
quick diagnosis of metabolic syndrome.

## Installation

You can install this package by the following function

``` r
# install.packages("MetabolicSyndrome")
```

## Example

``` r
library(MetabolicSyndrome)
```

    MetabolicSyndrome(x)

``` r
# x is the dataframe containing all the parameters required to dignose metabolic syndrome.
```

## Description of the dataframe (x)

Use the exact column names as specified below

Gender - Gender in Male or Female

WC -Waist Circumference in cm

TG - Triglycerides in mg/dL

HDLC - High Density Lipoprotein Cholesterol in mg/dL

SBP - Systolic BP in mm Hg

DBP - Diastolic BP in mm Hg

FPG - Fasting plasma glucose in mg/dL
