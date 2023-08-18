
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Metabolic Syndrome

<!-- badges: start -->
<!-- badges: end -->

The modified Adult Treatment Panel -III guidelines (ATP-III) proposed by
American Heart Association (AHA) and National Heart, Lung and Blood
Institute (NHLBI) are used widely for the clinical diagnosis of
Metabolic Syndrome. The AHA-NHLBI criteria advise using parameters such
as waist circumference (WC), systolic blood pressure (SBP), diastolic
blood pressure (DBP), fasting plasma glucose (FPG), triglycerides (TG)
and high-density lipoprotein cholesterol (HDLC) for diagnosis of
metabolic syndrome. Each parameter has to be interpreted based on the
proposed cut-offs, making the diagnosis slightly complex and
error-prone. This package is developed by incorporating the modified
ATP-III guidelines, and it will aid in the easy and quick diagnosis of
metabolic syndrome in busy healthcare settings and also for research
purposes. The modified ATP-III-AHA-NHLBI criteria for the diagnosis is
described by Grundy et al ., (2005)
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
