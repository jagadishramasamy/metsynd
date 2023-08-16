
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Metabolic Syndrome

<!-- badges: start -->
<!-- badges: end -->

The criteria used to diagnose is based on the modified Adult Treatment
Panel -III guidelines (ATP-III) guidelines proposed by American Heart
Association and National Heart, Lung and Blood Institute. It is
comprised of multiple parameters which includes waist circumference,
systolic blood pressure, diastolic blood pressure, fasting plasma
glucose, triglycerides and high-density lipoprotein cholesterol. Hence,
this package will be useful in the healthcare settings for easy and
quick diagnosis of metabolic syndrome. The modified ATP-III criteria for
the diagnosis is described by Grundy et al., (2005)
<doi:10.1161/CIRCULATIONAHA.105.169404>.

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
