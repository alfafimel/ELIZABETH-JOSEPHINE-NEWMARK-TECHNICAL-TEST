Newmark Analytics - BRAND REPUTATION DETERMINANTS
================
Elizabeth Josephine
23/03/2021

# PROBLEM DEFINITION

## **a) Specifying the Question**

Assessing factors that contribute to the brand reputation in Kenya from
the consumer’s perspective.

## **b) Defining the metrics for success**

Exploratory Data Analysis

## **c) Understanding the context**

Newmark Analytics is carrying out survey entitled: “Determinants of
brand reputation perception among consumers in Kenya.” The purpose of
this research is to assess factors that contribute to brand reputation
in Kenya from the customer’s perspective.

## **d) Recording the Experimental Design**

1.  Define the question, the metric for success, the context,
    experimental design taken.
2.  Read and explore the given dataset.
3.  Find and deal with outliers, anomalies, and missing data within the
    dataset.
4.  Perform Exploratory Data Analysis

## **e) Relevance of the data**

The data used for this project will inform the PR department on the
factors that contribute to brand reputation in Kenya from the customer’s
perspective.

# DATA ANALYSIS

## DATA SOURCING

``` r
# loading libraries
library(relaimpo)
```

    ## Loading required package: MASS

    ## Loading required package: boot

    ## Loading required package: survey

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:boot':
    ## 
    ##     aml

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

    ## Loading required package: mitools

    ## This is the global version of package relaimpo.

    ## If you are a non-US user, a version with the interesting additional metric pmvd is available

    ## from Ulrike Groempings web site at prof.beuth-hochschule.de/groemping.

``` r
library(data.table)
library(ggplot2) # Data visualization
library(ggthemes) # Plot themes
library(plotly) # Interactive data visualizations
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(dplyr) # Data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(psych) # Will be used for correlation visualization
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

    ## The following object is masked from 'package:boot':
    ## 
    ##     logit

``` r
# importing our data
# reading our data

library(readxl)
df <- read_excel("D:/submissions.xlsx")
df
```

    ## # A tibble: 167 x 112
    ##    `REPUTATION-Int~ `REPUTATION-sec~ `REPUTATION-Q1` `REPUTATION-sec~
    ##    <lgl>            <lgl>                      <dbl> <lgl>           
    ##  1 NA               NA                             2 NA              
    ##  2 NA               NA                             2 NA              
    ##  3 NA               NA                             1 NA              
    ##  4 NA               NA                             1 NA              
    ##  5 NA               NA                             1 NA              
    ##  6 NA               NA                             1 NA              
    ##  7 NA               NA                             1 NA              
    ##  8 NA               NA                             2 NA              
    ##  9 NA               NA                             2 NA              
    ## 10 NA               NA                             1 NA              
    ## # ... with 157 more rows, and 108 more variables: `REPUTATION-Q2` <dbl>,
    ## #   `REPUTATION-Q3` <lgl>, `REPUTATION-Q4` <dbl>, `REPUTATION-Q5` <chr>,
    ## #   `REPUTATION-sectionc` <lgl>, `REPUTATION-Q6` <dbl>, `REPUTATION-Q7` <dbl>,
    ## #   `REPUTATION-Q8` <dbl>, `REPUTATION-Q9` <chr>,
    ## #   `REPUTATION-untitled15` <lgl>, `REPUTATION-untitled16` <lgl>,
    ## #   `REPUTATION-untitled17` <lgl>, `REPUTATION-Q10` <dbl>,
    ## #   `REPUTATION-Q11` <chr>, `REPUTATION-untitled19` <lgl>,
    ## #   `REPUTATION-FMCGOFF1` <dbl>, `REPUTATION-FMCGOFF2` <dbl>,
    ## #   `REPUTATION-FMCGOFF3` <dbl>, `REPUTATION-FMCGINNO1` <dbl>,
    ## #   `REPUTATION-FMCGINNO2` <dbl>, `REPUTATION-FMCGCC1` <dbl>,
    ## #   `REPUTATION-FMCGCC2` <dbl>, `REPUTATION-FMCGRFS1` <dbl>,
    ## #   `REPUTATION-FMCGRFS2` <dbl>, `REPUTATION-FMCGSR1` <dbl>,
    ## #   `REPUTATION-FMCGSR2` <dbl>, `REPUTATION-FMCGSA1` <dbl>,
    ## #   `REPUTATION-FMCGTR1` <dbl>, `REPUTATION-FMCGOP1` <dbl>,
    ## #   `REPUTATION-FMCGLOY1` <dbl>, `REPUTATION-FMCGWOM1` <dbl>,
    ## #   `REPUTATION-untitled20` <lgl>, `REPUTATION-Q12` <dbl>,
    ## #   `REPUTATION-untitled211` <chr>, `REPUTATION-untitled21` <lgl>,
    ## #   `REPUTATION-FIOFF1` <dbl>, `REPUTATION-FIOFF2` <dbl>,
    ## #   `REPUTATION-FIINNO1` <dbl>, `REPUTATION-FIINNO2` <dbl>,
    ## #   `REPUTATION-FICC1` <dbl>, `REPUTATION-FICC2` <dbl>,
    ## #   `REPUTATION-FIRFS1` <dbl>, `REPUTATION-FIRFS2` <dbl>,
    ## #   `REPUTATION-FISR1` <dbl>, `REPUTATION-FISR2` <dbl>,
    ## #   `REPUTATION-FISA1` <dbl>, `REPUTATION-FITR1` <dbl>,
    ## #   `REPUTATION-FIOP1` <dbl>, `REPUTATION-FILOY1` <dbl>,
    ## #   `REPUTATION-FIWOM1` <dbl>, `REPUTATION-untitled22` <lgl>,
    ## #   `REPUTATION-Q13` <dbl>, `REPUTATION-untitled231` <chr>,
    ## #   `REPUTATION-untitled23` <lgl>, `REPUTATION-MANOFF1` <dbl>,
    ## #   `REPUTATION-MANOFF2` <dbl>, `REPUTATION-MANOFF3` <dbl>,
    ## #   `REPUTATION-MANINNO1` <dbl>, `REPUTATION-MANINNO2` <dbl>,
    ## #   `REPUTATION-MANCC1` <dbl>, `REPUTATION-MANCC2` <dbl>,
    ## #   `REPUTATION-MANRFS1` <dbl>, `REPUTATION-MANRFS2` <dbl>,
    ## #   `REPUTATION-MANSR1` <dbl>, `REPUTATION-MANSR2` <dbl>,
    ## #   `REPUTATION-MANSA1` <dbl>, `REPUTATION-MANTR1` <dbl>,
    ## #   `REPUTATION-MANOP1` <dbl>, `REPUTATION-MANLOY1` <dbl>,
    ## #   `REPUTATION-MANWOM1` <dbl>, `REPUTATION-untitled24` <lgl>,
    ## #   `REPUTATION-Q14` <dbl>, `REPUTATION-untitled251` <chr>,
    ## #   `REPUTATION-untitled25` <lgl>, `REPUTATION-AIOFF1` <dbl>,
    ## #   `REPUTATION-AIOFF2` <dbl>, `REPUTATION-AIOFF3` <dbl>,
    ## #   `REPUTATION-AIINNO1` <dbl>, `REPUTATION-AIINNO2` <dbl>,
    ## #   `REPUTATION-AICC1` <dbl>, `REPUTATION-AICC2` <dbl>,
    ## #   `REPUTATION-AIRFS1` <dbl>, `REPUTATION-AIRFS2` <dbl>,
    ## #   `REPUTATION-AISR1` <dbl>, `REPUTATION-AISR2` <dbl>,
    ## #   `REPUTATION-AISA1` <dbl>, `REPUTATION-AITR1` <dbl>,
    ## #   `REPUTATION-AIOP1` <dbl>, `REPUTATION-AILOY1` <dbl>,
    ## #   `REPUTATION-AIWOM1` <dbl>, `REPUTATION-untitled26` <lgl>,
    ## #   `REPUTATION-Q15` <dbl>, `REPUTATION-untitled271` <chr>,
    ## #   `REPUTATION-untitled27` <lgl>, `REPUTATION-POFF1` <dbl>,
    ## #   `REPUTATION-PCC1` <dbl>, `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>,
    ## #   `REPUTATION-PSA1` <dbl>, `REPUTATION-PTR1` <dbl>, ...

## DATA CHECKING

``` r
# previewing the dataset
View(df)
```

``` r
# previewing the column names
colnames(df)
```

    ##   [1] "REPUTATION-Introductoryletter"  "REPUTATION-section"            
    ##   [3] "REPUTATION-Q1"                  "REPUTATION-sectionb"           
    ##   [5] "REPUTATION-Q2"                  "REPUTATION-Q3"                 
    ##   [7] "REPUTATION-Q4"                  "REPUTATION-Q5"                 
    ##   [9] "REPUTATION-sectionc"            "REPUTATION-Q6"                 
    ##  [11] "REPUTATION-Q7"                  "REPUTATION-Q8"                 
    ##  [13] "REPUTATION-Q9"                  "REPUTATION-untitled15"         
    ##  [15] "REPUTATION-untitled16"          "REPUTATION-untitled17"         
    ##  [17] "REPUTATION-Q10"                 "REPUTATION-Q11"                
    ##  [19] "REPUTATION-untitled19"          "REPUTATION-FMCGOFF1"           
    ##  [21] "REPUTATION-FMCGOFF2"            "REPUTATION-FMCGOFF3"           
    ##  [23] "REPUTATION-FMCGINNO1"           "REPUTATION-FMCGINNO2"          
    ##  [25] "REPUTATION-FMCGCC1"             "REPUTATION-FMCGCC2"            
    ##  [27] "REPUTATION-FMCGRFS1"            "REPUTATION-FMCGRFS2"           
    ##  [29] "REPUTATION-FMCGSR1"             "REPUTATION-FMCGSR2"            
    ##  [31] "REPUTATION-FMCGSA1"             "REPUTATION-FMCGTR1"            
    ##  [33] "REPUTATION-FMCGOP1"             "REPUTATION-FMCGLOY1"           
    ##  [35] "REPUTATION-FMCGWOM1"            "REPUTATION-untitled20"         
    ##  [37] "REPUTATION-Q12"                 "REPUTATION-untitled211"        
    ##  [39] "REPUTATION-untitled21"          "REPUTATION-FIOFF1"             
    ##  [41] "REPUTATION-FIOFF2"              "REPUTATION-FIINNO1"            
    ##  [43] "REPUTATION-FIINNO2"             "REPUTATION-FICC1"              
    ##  [45] "REPUTATION-FICC2"               "REPUTATION-FIRFS1"             
    ##  [47] "REPUTATION-FIRFS2"              "REPUTATION-FISR1"              
    ##  [49] "REPUTATION-FISR2"               "REPUTATION-FISA1"              
    ##  [51] "REPUTATION-FITR1"               "REPUTATION-FIOP1"              
    ##  [53] "REPUTATION-FILOY1"              "REPUTATION-FIWOM1"             
    ##  [55] "REPUTATION-untitled22"          "REPUTATION-Q13"                
    ##  [57] "REPUTATION-untitled231"         "REPUTATION-untitled23"         
    ##  [59] "REPUTATION-MANOFF1"             "REPUTATION-MANOFF2"            
    ##  [61] "REPUTATION-MANOFF3"             "REPUTATION-MANINNO1"           
    ##  [63] "REPUTATION-MANINNO2"            "REPUTATION-MANCC1"             
    ##  [65] "REPUTATION-MANCC2"              "REPUTATION-MANRFS1"            
    ##  [67] "REPUTATION-MANRFS2"             "REPUTATION-MANSR1"             
    ##  [69] "REPUTATION-MANSR2"              "REPUTATION-MANSA1"             
    ##  [71] "REPUTATION-MANTR1"              "REPUTATION-MANOP1"             
    ##  [73] "REPUTATION-MANLOY1"             "REPUTATION-MANWOM1"            
    ##  [75] "REPUTATION-untitled24"          "REPUTATION-Q14"                
    ##  [77] "REPUTATION-untitled251"         "REPUTATION-untitled25"         
    ##  [79] "REPUTATION-AIOFF1"              "REPUTATION-AIOFF2"             
    ##  [81] "REPUTATION-AIOFF3"              "REPUTATION-AIINNO1"            
    ##  [83] "REPUTATION-AIINNO2"             "REPUTATION-AICC1"              
    ##  [85] "REPUTATION-AICC2"               "REPUTATION-AIRFS1"             
    ##  [87] "REPUTATION-AIRFS2"              "REPUTATION-AISR1"              
    ##  [89] "REPUTATION-AISR2"               "REPUTATION-AISA1"              
    ##  [91] "REPUTATION-AITR1"               "REPUTATION-AIOP1"              
    ##  [93] "REPUTATION-AILOY1"              "REPUTATION-AIWOM1"             
    ##  [95] "REPUTATION-untitled26"          "REPUTATION-Q15"                
    ##  [97] "REPUTATION-untitled271"         "REPUTATION-untitled27"         
    ##  [99] "REPUTATION-POFF1"               "REPUTATION-PCC1"               
    ## [101] "REPUTATION-PRFS1"               "REPUTATION-PSR1"               
    ## [103] "REPUTATION-PSA1"                "REPUTATION-PTR1"               
    ## [105] "REPUTATION-POP1"                "REPUTATION-PLOY1"              
    ## [107] "REPUTATION-PWOM1"               "REPUTATION-untitled38"         
    ## [109] "REPUTATION-untitled38-altitude" "REPUTATION-untitled38-accuracy"
    ## [111] "REPUTATION-untitled39"          "REPUTATION-meta-instanceID"

``` r
# previewing the dataset
class(df)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
# previewing the datatypes of the dataset
sapply(df, class)
```

    ##  REPUTATION-Introductoryletter             REPUTATION-section 
    ##                      "logical"                      "logical" 
    ##                  REPUTATION-Q1            REPUTATION-sectionb 
    ##                      "numeric"                      "logical" 
    ##                  REPUTATION-Q2                  REPUTATION-Q3 
    ##                      "numeric"                      "logical" 
    ##                  REPUTATION-Q4                  REPUTATION-Q5 
    ##                      "numeric"                    "character" 
    ##            REPUTATION-sectionc                  REPUTATION-Q6 
    ##                      "logical"                      "numeric" 
    ##                  REPUTATION-Q7                  REPUTATION-Q8 
    ##                      "numeric"                      "numeric" 
    ##                  REPUTATION-Q9          REPUTATION-untitled15 
    ##                    "character"                      "logical" 
    ##          REPUTATION-untitled16          REPUTATION-untitled17 
    ##                      "logical"                      "logical" 
    ##                 REPUTATION-Q10                 REPUTATION-Q11 
    ##                      "numeric"                    "character" 
    ##          REPUTATION-untitled19            REPUTATION-FMCGOFF1 
    ##                      "logical"                      "numeric" 
    ##            REPUTATION-FMCGOFF2            REPUTATION-FMCGOFF3 
    ##                      "numeric"                      "numeric" 
    ##           REPUTATION-FMCGINNO1           REPUTATION-FMCGINNO2 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-FMCGCC1             REPUTATION-FMCGCC2 
    ##                      "numeric"                      "numeric" 
    ##            REPUTATION-FMCGRFS1            REPUTATION-FMCGRFS2 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-FMCGSR1             REPUTATION-FMCGSR2 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-FMCGSA1             REPUTATION-FMCGTR1 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-FMCGOP1            REPUTATION-FMCGLOY1 
    ##                      "numeric"                      "numeric" 
    ##            REPUTATION-FMCGWOM1          REPUTATION-untitled20 
    ##                      "numeric"                      "logical" 
    ##                 REPUTATION-Q12         REPUTATION-untitled211 
    ##                      "numeric"                    "character" 
    ##          REPUTATION-untitled21              REPUTATION-FIOFF1 
    ##                      "logical"                      "numeric" 
    ##              REPUTATION-FIOFF2             REPUTATION-FIINNO1 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-FIINNO2               REPUTATION-FICC1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-FICC2              REPUTATION-FIRFS1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-FIRFS2               REPUTATION-FISR1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-FISR2               REPUTATION-FISA1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-FITR1               REPUTATION-FIOP1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-FILOY1              REPUTATION-FIWOM1 
    ##                      "numeric"                      "numeric" 
    ##          REPUTATION-untitled22                 REPUTATION-Q13 
    ##                      "logical"                      "numeric" 
    ##         REPUTATION-untitled231          REPUTATION-untitled23 
    ##                    "character"                      "logical" 
    ##             REPUTATION-MANOFF1             REPUTATION-MANOFF2 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-MANOFF3            REPUTATION-MANINNO1 
    ##                      "numeric"                      "numeric" 
    ##            REPUTATION-MANINNO2              REPUTATION-MANCC1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-MANCC2             REPUTATION-MANRFS1 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-MANRFS2              REPUTATION-MANSR1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-MANSR2              REPUTATION-MANSA1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-MANTR1              REPUTATION-MANOP1 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-MANLOY1             REPUTATION-MANWOM1 
    ##                      "numeric"                      "numeric" 
    ##          REPUTATION-untitled24                 REPUTATION-Q14 
    ##                      "logical"                      "numeric" 
    ##         REPUTATION-untitled251          REPUTATION-untitled25 
    ##                    "character"                      "logical" 
    ##              REPUTATION-AIOFF1              REPUTATION-AIOFF2 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-AIOFF3             REPUTATION-AIINNO1 
    ##                      "numeric"                      "numeric" 
    ##             REPUTATION-AIINNO2               REPUTATION-AICC1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-AICC2              REPUTATION-AIRFS1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-AIRFS2               REPUTATION-AISR1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-AISR2               REPUTATION-AISA1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-AITR1               REPUTATION-AIOP1 
    ##                      "numeric"                      "numeric" 
    ##              REPUTATION-AILOY1              REPUTATION-AIWOM1 
    ##                      "numeric"                      "numeric" 
    ##          REPUTATION-untitled26                 REPUTATION-Q15 
    ##                      "logical"                      "numeric" 
    ##         REPUTATION-untitled271          REPUTATION-untitled27 
    ##                    "character"                      "logical" 
    ##               REPUTATION-POFF1                REPUTATION-PCC1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-PRFS1                REPUTATION-PSR1 
    ##                      "numeric"                      "numeric" 
    ##                REPUTATION-PSA1                REPUTATION-PTR1 
    ##                      "numeric"                      "numeric" 
    ##                REPUTATION-POP1               REPUTATION-PLOY1 
    ##                      "numeric"                      "numeric" 
    ##               REPUTATION-PWOM1          REPUTATION-untitled38 
    ##                      "numeric"                    "character" 
    ## REPUTATION-untitled38-altitude REPUTATION-untitled38-accuracy 
    ##                      "numeric"                      "numeric" 
    ##          REPUTATION-untitled39     REPUTATION-meta-instanceID 
    ##                    "character"                    "character"

``` r
# previewing the head of the dataset
head(df, n = 5)
```

    ## # A tibble: 5 x 112
    ##   `REPUTATION-Int~ `REPUTATION-sec~ `REPUTATION-Q1` `REPUTATION-sec~
    ##   <lgl>            <lgl>                      <dbl> <lgl>           
    ## 1 NA               NA                             2 NA              
    ## 2 NA               NA                             2 NA              
    ## 3 NA               NA                             1 NA              
    ## 4 NA               NA                             1 NA              
    ## 5 NA               NA                             1 NA              
    ## # ... with 108 more variables: `REPUTATION-Q2` <dbl>, `REPUTATION-Q3` <lgl>,
    ## #   `REPUTATION-Q4` <dbl>, `REPUTATION-Q5` <chr>, `REPUTATION-sectionc` <lgl>,
    ## #   `REPUTATION-Q6` <dbl>, `REPUTATION-Q7` <dbl>, `REPUTATION-Q8` <dbl>,
    ## #   `REPUTATION-Q9` <chr>, `REPUTATION-untitled15` <lgl>,
    ## #   `REPUTATION-untitled16` <lgl>, `REPUTATION-untitled17` <lgl>,
    ## #   `REPUTATION-Q10` <dbl>, `REPUTATION-Q11` <chr>,
    ## #   `REPUTATION-untitled19` <lgl>, `REPUTATION-FMCGOFF1` <dbl>,
    ## #   `REPUTATION-FMCGOFF2` <dbl>, `REPUTATION-FMCGOFF3` <dbl>,
    ## #   `REPUTATION-FMCGINNO1` <dbl>, `REPUTATION-FMCGINNO2` <dbl>,
    ## #   `REPUTATION-FMCGCC1` <dbl>, `REPUTATION-FMCGCC2` <dbl>,
    ## #   `REPUTATION-FMCGRFS1` <dbl>, `REPUTATION-FMCGRFS2` <dbl>,
    ## #   `REPUTATION-FMCGSR1` <dbl>, `REPUTATION-FMCGSR2` <dbl>,
    ## #   `REPUTATION-FMCGSA1` <dbl>, `REPUTATION-FMCGTR1` <dbl>,
    ## #   `REPUTATION-FMCGOP1` <dbl>, `REPUTATION-FMCGLOY1` <dbl>,
    ## #   `REPUTATION-FMCGWOM1` <dbl>, `REPUTATION-untitled20` <lgl>,
    ## #   `REPUTATION-Q12` <dbl>, `REPUTATION-untitled211` <chr>,
    ## #   `REPUTATION-untitled21` <lgl>, `REPUTATION-FIOFF1` <dbl>,
    ## #   `REPUTATION-FIOFF2` <dbl>, `REPUTATION-FIINNO1` <dbl>,
    ## #   `REPUTATION-FIINNO2` <dbl>, `REPUTATION-FICC1` <dbl>,
    ## #   `REPUTATION-FICC2` <dbl>, `REPUTATION-FIRFS1` <dbl>,
    ## #   `REPUTATION-FIRFS2` <dbl>, `REPUTATION-FISR1` <dbl>,
    ## #   `REPUTATION-FISR2` <dbl>, `REPUTATION-FISA1` <dbl>,
    ## #   `REPUTATION-FITR1` <dbl>, `REPUTATION-FIOP1` <dbl>,
    ## #   `REPUTATION-FILOY1` <dbl>, `REPUTATION-FIWOM1` <dbl>,
    ## #   `REPUTATION-untitled22` <lgl>, `REPUTATION-Q13` <dbl>,
    ## #   `REPUTATION-untitled231` <chr>, `REPUTATION-untitled23` <lgl>,
    ## #   `REPUTATION-MANOFF1` <dbl>, `REPUTATION-MANOFF2` <dbl>,
    ## #   `REPUTATION-MANOFF3` <dbl>, `REPUTATION-MANINNO1` <dbl>,
    ## #   `REPUTATION-MANINNO2` <dbl>, `REPUTATION-MANCC1` <dbl>,
    ## #   `REPUTATION-MANCC2` <dbl>, `REPUTATION-MANRFS1` <dbl>,
    ## #   `REPUTATION-MANRFS2` <dbl>, `REPUTATION-MANSR1` <dbl>,
    ## #   `REPUTATION-MANSR2` <dbl>, `REPUTATION-MANSA1` <dbl>,
    ## #   `REPUTATION-MANTR1` <dbl>, `REPUTATION-MANOP1` <dbl>,
    ## #   `REPUTATION-MANLOY1` <dbl>, `REPUTATION-MANWOM1` <dbl>,
    ## #   `REPUTATION-untitled24` <lgl>, `REPUTATION-Q14` <dbl>,
    ## #   `REPUTATION-untitled251` <chr>, `REPUTATION-untitled25` <lgl>,
    ## #   `REPUTATION-AIOFF1` <dbl>, `REPUTATION-AIOFF2` <dbl>,
    ## #   `REPUTATION-AIOFF3` <dbl>, `REPUTATION-AIINNO1` <dbl>,
    ## #   `REPUTATION-AIINNO2` <dbl>, `REPUTATION-AICC1` <dbl>,
    ## #   `REPUTATION-AICC2` <dbl>, `REPUTATION-AIRFS1` <dbl>,
    ## #   `REPUTATION-AIRFS2` <dbl>, `REPUTATION-AISR1` <dbl>,
    ## #   `REPUTATION-AISR2` <dbl>, `REPUTATION-AISA1` <dbl>,
    ## #   `REPUTATION-AITR1` <dbl>, `REPUTATION-AIOP1` <dbl>,
    ## #   `REPUTATION-AILOY1` <dbl>, `REPUTATION-AIWOM1` <dbl>,
    ## #   `REPUTATION-untitled26` <lgl>, `REPUTATION-Q15` <dbl>,
    ## #   `REPUTATION-untitled271` <chr>, `REPUTATION-untitled27` <lgl>,
    ## #   `REPUTATION-POFF1` <dbl>, `REPUTATION-PCC1` <dbl>,
    ## #   `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>, `REPUTATION-PSA1` <dbl>,
    ## #   `REPUTATION-PTR1` <dbl>, ...

``` r
# previewing the tail of the dataset
tail(df, n = 5)
```

    ## # A tibble: 5 x 112
    ##   `REPUTATION-Int~ `REPUTATION-sec~ `REPUTATION-Q1` `REPUTATION-sec~
    ##   <lgl>            <lgl>                      <dbl> <lgl>           
    ## 1 NA               NA                             2 NA              
    ## 2 NA               NA                             2 NA              
    ## 3 NA               NA                             2 NA              
    ## 4 NA               NA                             2 NA              
    ## 5 NA               NA                             2 NA              
    ## # ... with 108 more variables: `REPUTATION-Q2` <dbl>, `REPUTATION-Q3` <lgl>,
    ## #   `REPUTATION-Q4` <dbl>, `REPUTATION-Q5` <chr>, `REPUTATION-sectionc` <lgl>,
    ## #   `REPUTATION-Q6` <dbl>, `REPUTATION-Q7` <dbl>, `REPUTATION-Q8` <dbl>,
    ## #   `REPUTATION-Q9` <chr>, `REPUTATION-untitled15` <lgl>,
    ## #   `REPUTATION-untitled16` <lgl>, `REPUTATION-untitled17` <lgl>,
    ## #   `REPUTATION-Q10` <dbl>, `REPUTATION-Q11` <chr>,
    ## #   `REPUTATION-untitled19` <lgl>, `REPUTATION-FMCGOFF1` <dbl>,
    ## #   `REPUTATION-FMCGOFF2` <dbl>, `REPUTATION-FMCGOFF3` <dbl>,
    ## #   `REPUTATION-FMCGINNO1` <dbl>, `REPUTATION-FMCGINNO2` <dbl>,
    ## #   `REPUTATION-FMCGCC1` <dbl>, `REPUTATION-FMCGCC2` <dbl>,
    ## #   `REPUTATION-FMCGRFS1` <dbl>, `REPUTATION-FMCGRFS2` <dbl>,
    ## #   `REPUTATION-FMCGSR1` <dbl>, `REPUTATION-FMCGSR2` <dbl>,
    ## #   `REPUTATION-FMCGSA1` <dbl>, `REPUTATION-FMCGTR1` <dbl>,
    ## #   `REPUTATION-FMCGOP1` <dbl>, `REPUTATION-FMCGLOY1` <dbl>,
    ## #   `REPUTATION-FMCGWOM1` <dbl>, `REPUTATION-untitled20` <lgl>,
    ## #   `REPUTATION-Q12` <dbl>, `REPUTATION-untitled211` <chr>,
    ## #   `REPUTATION-untitled21` <lgl>, `REPUTATION-FIOFF1` <dbl>,
    ## #   `REPUTATION-FIOFF2` <dbl>, `REPUTATION-FIINNO1` <dbl>,
    ## #   `REPUTATION-FIINNO2` <dbl>, `REPUTATION-FICC1` <dbl>,
    ## #   `REPUTATION-FICC2` <dbl>, `REPUTATION-FIRFS1` <dbl>,
    ## #   `REPUTATION-FIRFS2` <dbl>, `REPUTATION-FISR1` <dbl>,
    ## #   `REPUTATION-FISR2` <dbl>, `REPUTATION-FISA1` <dbl>,
    ## #   `REPUTATION-FITR1` <dbl>, `REPUTATION-FIOP1` <dbl>,
    ## #   `REPUTATION-FILOY1` <dbl>, `REPUTATION-FIWOM1` <dbl>,
    ## #   `REPUTATION-untitled22` <lgl>, `REPUTATION-Q13` <dbl>,
    ## #   `REPUTATION-untitled231` <chr>, `REPUTATION-untitled23` <lgl>,
    ## #   `REPUTATION-MANOFF1` <dbl>, `REPUTATION-MANOFF2` <dbl>,
    ## #   `REPUTATION-MANOFF3` <dbl>, `REPUTATION-MANINNO1` <dbl>,
    ## #   `REPUTATION-MANINNO2` <dbl>, `REPUTATION-MANCC1` <dbl>,
    ## #   `REPUTATION-MANCC2` <dbl>, `REPUTATION-MANRFS1` <dbl>,
    ## #   `REPUTATION-MANRFS2` <dbl>, `REPUTATION-MANSR1` <dbl>,
    ## #   `REPUTATION-MANSR2` <dbl>, `REPUTATION-MANSA1` <dbl>,
    ## #   `REPUTATION-MANTR1` <dbl>, `REPUTATION-MANOP1` <dbl>,
    ## #   `REPUTATION-MANLOY1` <dbl>, `REPUTATION-MANWOM1` <dbl>,
    ## #   `REPUTATION-untitled24` <lgl>, `REPUTATION-Q14` <dbl>,
    ## #   `REPUTATION-untitled251` <chr>, `REPUTATION-untitled25` <lgl>,
    ## #   `REPUTATION-AIOFF1` <dbl>, `REPUTATION-AIOFF2` <dbl>,
    ## #   `REPUTATION-AIOFF3` <dbl>, `REPUTATION-AIINNO1` <dbl>,
    ## #   `REPUTATION-AIINNO2` <dbl>, `REPUTATION-AICC1` <dbl>,
    ## #   `REPUTATION-AICC2` <dbl>, `REPUTATION-AIRFS1` <dbl>,
    ## #   `REPUTATION-AIRFS2` <dbl>, `REPUTATION-AISR1` <dbl>,
    ## #   `REPUTATION-AISR2` <dbl>, `REPUTATION-AISA1` <dbl>,
    ## #   `REPUTATION-AITR1` <dbl>, `REPUTATION-AIOP1` <dbl>,
    ## #   `REPUTATION-AILOY1` <dbl>, `REPUTATION-AIWOM1` <dbl>,
    ## #   `REPUTATION-untitled26` <lgl>, `REPUTATION-Q15` <dbl>,
    ## #   `REPUTATION-untitled271` <chr>, `REPUTATION-untitled27` <lgl>,
    ## #   `REPUTATION-POFF1` <dbl>, `REPUTATION-PCC1` <dbl>,
    ## #   `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>, `REPUTATION-PSA1` <dbl>,
    ## #   `REPUTATION-PTR1` <dbl>, ...

``` r
# checking the structure of the data
str(df)
```

    ## tibble [167 x 112] (S3: tbl_df/tbl/data.frame)
    ##  $ REPUTATION-Introductoryletter : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-section            : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q1                 : num [1:167] 2 2 1 1 1 1 1 2 2 1 ...
    ##  $ REPUTATION-sectionb           : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q2                 : num [1:167] 1 2 1 2 2 2 1 2 2 1 ...
    ##  $ REPUTATION-Q3                 : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q4                 : num [1:167] 2 6 7 7 6 7 1 5 1 7 ...
    ##  $ REPUTATION-Q5                 : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-sectionc           : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q6                 : num [1:167] 2 1 2 1 2 2 2 2 1 1 ...
    ##  $ REPUTATION-Q7                 : num [1:167] 3 3 3 3 2 2 2 3 4 4 ...
    ##  $ REPUTATION-Q8                 : num [1:167] 4 4 3 4 2 6 3 3 7 4 ...
    ##  $ REPUTATION-Q9                 : chr [1:167] "Accountant" "Engineer" "Admin" "Gym instructor" ...
    ##  $ REPUTATION-untitled15         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-untitled16         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-untitled17         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q10                : num [1:167] 3 1 2 2 1 3 3 3 1 1 ...
    ##  $ REPUTATION-Q11                : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-untitled19         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-FMCGOFF1           : num [1:167] 4 3 5 4 4 2 3 4 4 4 ...
    ##  $ REPUTATION-FMCGOFF2           : num [1:167] 4 4 5 4 4 2 3 4 4 4 ...
    ##  $ REPUTATION-FMCGOFF3           : num [1:167] 4 4 5 4 4 4 2 3 4 4 ...
    ##  $ REPUTATION-FMCGINNO1          : num [1:167] 4 4 4 4 4 3 4 4 4 4 ...
    ##  $ REPUTATION-FMCGINNO2          : num [1:167] 5 4 5 5 5 2 3 4 4 4 ...
    ##  $ REPUTATION-FMCGCC1            : num [1:167] 5 4 4 4 4 2 4 4 2 2 ...
    ##  $ REPUTATION-FMCGCC2            : num [1:167] 4 3 3 3 4 4 4 4 4 4 ...
    ##  $ REPUTATION-FMCGRFS1           : num [1:167] 5 5 4 4 4 4 4 4 5 5 ...
    ##  $ REPUTATION-FMCGRFS2           : num [1:167] 5 3 3 3 4 4 4 4 4 4 ...
    ##  $ REPUTATION-FMCGSR1            : num [1:167] 3 3 4 4 3 4 3 3 3 4 ...
    ##  $ REPUTATION-FMCGSR2            : num [1:167] 3 2 3 2 3 3 3 4 3 3 ...
    ##  $ REPUTATION-FMCGSA1            : num [1:167] 5 4 5 4 4 5 4 5 5 5 ...
    ##  $ REPUTATION-FMCGTR1            : num [1:167] 4 5 4 4 4 4 4 5 4 4 ...
    ##  $ REPUTATION-FMCGOP1            : num [1:167] 4 5 4 4 4 4 4 4 4 4 ...
    ##  $ REPUTATION-FMCGLOY1           : num [1:167] 4 4 4 4 4 4 5 5 4 4 ...
    ##  $ REPUTATION-FMCGWOM1           : num [1:167] 4 4 5 4 4 4 5 4 4 4 ...
    ##  $ REPUTATION-untitled20         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q12                : num [1:167] 2 3 2 2 2 4 2 1 3 2 ...
    ##  $ REPUTATION-untitled211        : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-untitled21         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-FIOFF1             : num [1:167] 4 4 5 4 4 4 4 4 5 4 ...
    ##  $ REPUTATION-FIOFF2             : num [1:167] 4 4 5 4 5 4 4 4 4 4 ...
    ##  $ REPUTATION-FIINNO1            : num [1:167] 4 4 5 4 5 4 4 4 4 4 ...
    ##  $ REPUTATION-FIINNO2            : num [1:167] 4 4 5 4 5 4 5 4 4 4 ...
    ##  $ REPUTATION-FICC1              : num [1:167] 4 3 2 4 5 4 4 4 1 4 ...
    ##  $ REPUTATION-FICC2              : num [1:167] 4 4 5 4 4 4 4 4 4 4 ...
    ##  $ REPUTATION-FIRFS1             : num [1:167] 4 2 4 4 5 4 5 4 2 4 ...
    ##  $ REPUTATION-FIRFS2             : num [1:167] 5 4 4 4 5 5 5 4 4 4 ...
    ##  $ REPUTATION-FISR1              : num [1:167] 4 2 2 4 4 3 4 4 3 4 ...
    ##  $ REPUTATION-FISR2              : num [1:167] 5 4 5 4 4 4 5 3 4 2 ...
    ##  $ REPUTATION-FISA1              : num [1:167] 5 4 5 4 4 4 5 4 4 4 ...
    ##  $ REPUTATION-FITR1              : num [1:167] 4 5 5 5 4 4 5 4 5 4 ...
    ##  $ REPUTATION-FIOP1              : num [1:167] 4 4 5 4 4 5 5 4 5 4 ...
    ##  $ REPUTATION-FILOY1             : num [1:167] 4 5 4 5 5 4 5 4 4 4 ...
    ##  $ REPUTATION-FIWOM1             : num [1:167] 4 4 4 4 4 4 5 4 4 4 ...
    ##  $ REPUTATION-untitled22         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q13                : num [1:167] 1 1 1 1 1 1 5 5 1 4 ...
    ##  $ REPUTATION-untitled231        : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-untitled23         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-MANOFF1            : num [1:167] 1 5 4 4 3 4 3 1 4 4 ...
    ##  $ REPUTATION-MANOFF2            : num [1:167] 3 3 2 4 4 2 3 1 2 4 ...
    ##  $ REPUTATION-MANOFF3            : num [1:167] 2 2 2 4 2 2 3 1 3 2 ...
    ##  $ REPUTATION-MANINNO1           : num [1:167] 3 4 4 4 4 4 3 1 4 4 ...
    ##  $ REPUTATION-MANINNO2           : num [1:167] 4 3 4 4 5 4 3 1 4 2 ...
    ##  $ REPUTATION-MANCC1             : num [1:167] 4 4 4 4 4 2 3 1 2 2 ...
    ##  $ REPUTATION-MANCC2             : num [1:167] 4 4 3 4 4 4 3 1 4 4 ...
    ##  $ REPUTATION-MANRFS1            : num [1:167] 5 4 4 4 4 4 3 1 2 2 ...
    ##  $ REPUTATION-MANRFS2            : num [1:167] 5 2 2 4 4 4 3 1 4 2 ...
    ##  $ REPUTATION-MANSR1             : num [1:167] 5 3 2 3 4 4 3 1 4 2 ...
    ##  $ REPUTATION-MANSR2             : num [1:167] 4 3 4 4 3 4 3 1 4 4 ...
    ##  $ REPUTATION-MANSA1             : num [1:167] 4 5 5 5 4 4 3 1 4 4 ...
    ##  $ REPUTATION-MANTR1             : num [1:167] 4 4 4 4 4 4 3 1 4 4 ...
    ##  $ REPUTATION-MANOP1             : num [1:167] 4 5 4 4 4 4 3 1 4 4 ...
    ##  $ REPUTATION-MANLOY1            : num [1:167] 4 5 5 4 4 4 3 1 4 2 ...
    ##  $ REPUTATION-MANWOM1            : num [1:167] 4 5 5 4 4 4 3 1 4 4 ...
    ##  $ REPUTATION-untitled24         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q14                : num [1:167] 1 1 1 4 1 1 3 3 3 2 ...
    ##  $ REPUTATION-untitled251        : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-untitled25         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-AIOFF1             : num [1:167] 4 5 4 4 4 4 4 4 5 2 ...
    ##  $ REPUTATION-AIOFF2             : num [1:167] 3 3 4 2 4 4 3 4 1 2 ...
    ##  $ REPUTATION-AIOFF3             : num [1:167] 3 4 4 4 3 2 3 4 2 2 ...
    ##  $ REPUTATION-AIINNO1            : num [1:167] 3 5 4 4 4 4 4 5 4 4 ...
    ##  $ REPUTATION-AIINNO2            : num [1:167] 3 2 4 4 4 2 4 3 4 2 ...
    ##  $ REPUTATION-AICC1              : num [1:167] 4 4 4 2 4 2 3 3 2 2 ...
    ##  $ REPUTATION-AICC2              : num [1:167] 3 3 4 4 4 4 4 3 4 2 ...
    ##  $ REPUTATION-AIRFS1             : num [1:167] 4 5 4 2 4 5 4 4 2 2 ...
    ##  $ REPUTATION-AIRFS2             : num [1:167] 3 2 4 4 3 4 4 5 5 4 ...
    ##  $ REPUTATION-AISR1              : num [1:167] 3 5 4 4 4 4 3 4 5 4 ...
    ##  $ REPUTATION-AISR2              : num [1:167] 2 3 3 4 3 3 3 3 5 4 ...
    ##  $ REPUTATION-AISA1              : num [1:167] 4 4 4 4 4 2 4 4 4 4 ...
    ##  $ REPUTATION-AITR1              : num [1:167] 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ REPUTATION-AIOP1              : num [1:167] 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ REPUTATION-AILOY1             : num [1:167] 4 4 4 4 4 4 4 4 4 2 ...
    ##  $ REPUTATION-AIWOM1             : num [1:167] 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ REPUTATION-untitled26         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-Q15                : num [1:167] 1 2 4 2 4 4 4 4 2 1 ...
    ##  $ REPUTATION-untitled271        : chr [1:167] NA NA NA NA ...
    ##  $ REPUTATION-untitled27         : logi [1:167] NA NA NA NA NA NA ...
    ##  $ REPUTATION-POFF1              : num [1:167] 3 1 1 2 4 4 4 4 2 4 ...
    ##   [list output truncated]

``` r
# checking the dimension/shape of the data
dim(df) # 167 rows and 112 columns
```

    ## [1] 167 112

## DATA CLEANING

### Missing Values

``` r
# checking for missing values
sum(is.na(df))# there are 3765 missing values in the data
```

    ## [1] 3765

``` r
#deleting columns with missing values
df1 <- df[ , colSums(is.na(df)) == 0]
sum(is.na(df1))
```

    ## [1] 0

### Duplicates

``` r
# checking for duplicates
duplicated_rows <- df1[duplicated(df1),]
duplicated_rows # there are no duplicates in the data
```

    ## # A tibble: 0 x 89
    ## # ... with 89 variables: `REPUTATION-Q1` <dbl>, `REPUTATION-Q2` <dbl>,
    ## #   `REPUTATION-Q4` <dbl>, `REPUTATION-Q6` <dbl>, `REPUTATION-Q7` <dbl>,
    ## #   `REPUTATION-Q8` <dbl>, `REPUTATION-Q9` <chr>, `REPUTATION-Q10` <dbl>,
    ## #   `REPUTATION-FMCGOFF1` <dbl>, `REPUTATION-FMCGOFF2` <dbl>,
    ## #   `REPUTATION-FMCGOFF3` <dbl>, `REPUTATION-FMCGINNO1` <dbl>,
    ## #   `REPUTATION-FMCGINNO2` <dbl>, `REPUTATION-FMCGCC1` <dbl>,
    ## #   `REPUTATION-FMCGCC2` <dbl>, `REPUTATION-FMCGRFS1` <dbl>,
    ## #   `REPUTATION-FMCGRFS2` <dbl>, `REPUTATION-FMCGSR1` <dbl>,
    ## #   `REPUTATION-FMCGSR2` <dbl>, `REPUTATION-FMCGSA1` <dbl>,
    ## #   `REPUTATION-FMCGTR1` <dbl>, `REPUTATION-FMCGOP1` <dbl>,
    ## #   `REPUTATION-FMCGLOY1` <dbl>, `REPUTATION-FMCGWOM1` <dbl>,
    ## #   `REPUTATION-Q12` <dbl>, `REPUTATION-FIOFF1` <dbl>,
    ## #   `REPUTATION-FIOFF2` <dbl>, `REPUTATION-FIINNO1` <dbl>,
    ## #   `REPUTATION-FIINNO2` <dbl>, `REPUTATION-FICC1` <dbl>,
    ## #   `REPUTATION-FICC2` <dbl>, `REPUTATION-FIRFS1` <dbl>,
    ## #   `REPUTATION-FIRFS2` <dbl>, `REPUTATION-FISR1` <dbl>,
    ## #   `REPUTATION-FISR2` <dbl>, `REPUTATION-FISA1` <dbl>,
    ## #   `REPUTATION-FITR1` <dbl>, `REPUTATION-FIOP1` <dbl>,
    ## #   `REPUTATION-FILOY1` <dbl>, `REPUTATION-FIWOM1` <dbl>,
    ## #   `REPUTATION-Q13` <dbl>, `REPUTATION-MANOFF1` <dbl>,
    ## #   `REPUTATION-MANOFF2` <dbl>, `REPUTATION-MANOFF3` <dbl>,
    ## #   `REPUTATION-MANINNO1` <dbl>, `REPUTATION-MANINNO2` <dbl>,
    ## #   `REPUTATION-MANCC1` <dbl>, `REPUTATION-MANCC2` <dbl>,
    ## #   `REPUTATION-MANRFS1` <dbl>, `REPUTATION-MANRFS2` <dbl>,
    ## #   `REPUTATION-MANSR1` <dbl>, `REPUTATION-MANSR2` <dbl>,
    ## #   `REPUTATION-MANSA1` <dbl>, `REPUTATION-MANTR1` <dbl>,
    ## #   `REPUTATION-MANOP1` <dbl>, `REPUTATION-MANLOY1` <dbl>,
    ## #   `REPUTATION-MANWOM1` <dbl>, `REPUTATION-Q14` <dbl>,
    ## #   `REPUTATION-AIOFF1` <dbl>, `REPUTATION-AIOFF2` <dbl>,
    ## #   `REPUTATION-AIOFF3` <dbl>, `REPUTATION-AIINNO1` <dbl>,
    ## #   `REPUTATION-AIINNO2` <dbl>, `REPUTATION-AICC1` <dbl>,
    ## #   `REPUTATION-AICC2` <dbl>, `REPUTATION-AIRFS1` <dbl>,
    ## #   `REPUTATION-AIRFS2` <dbl>, `REPUTATION-AISR1` <dbl>,
    ## #   `REPUTATION-AISR2` <dbl>, `REPUTATION-AISA1` <dbl>,
    ## #   `REPUTATION-AITR1` <dbl>, `REPUTATION-AIOP1` <dbl>,
    ## #   `REPUTATION-AILOY1` <dbl>, `REPUTATION-AIWOM1` <dbl>,
    ## #   `REPUTATION-Q15` <dbl>, `REPUTATION-POFF1` <dbl>, `REPUTATION-PCC1` <dbl>,
    ## #   `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>, `REPUTATION-PSA1` <dbl>,
    ## #   `REPUTATION-PTR1` <dbl>, `REPUTATION-POP1` <dbl>, `REPUTATION-PLOY1` <dbl>,
    ## #   `REPUTATION-PWOM1` <dbl>, `REPUTATION-untitled38` <chr>,
    ## #   `REPUTATION-untitled38-altitude` <dbl>,
    ## #   `REPUTATION-untitled38-accuracy` <dbl>, `REPUTATION-untitled39` <chr>,
    ## #   `REPUTATION-meta-instanceID` <chr>

``` r
# showing these unique items and assigning to a variable unique_items below
unique_items <- df1[!duplicated(df1), ]
unique_items
```

    ## # A tibble: 167 x 89
    ##    `REPUTATION-Q1` `REPUTATION-Q2` `REPUTATION-Q4` `REPUTATION-Q6`
    ##              <dbl>           <dbl>           <dbl>           <dbl>
    ##  1               2               1               2               2
    ##  2               2               2               6               1
    ##  3               1               1               7               2
    ##  4               1               2               7               1
    ##  5               1               2               6               2
    ##  6               1               2               7               2
    ##  7               1               1               1               2
    ##  8               2               2               5               2
    ##  9               2               2               1               1
    ## 10               1               1               7               1
    ## # ... with 157 more rows, and 85 more variables: `REPUTATION-Q7` <dbl>,
    ## #   `REPUTATION-Q8` <dbl>, `REPUTATION-Q9` <chr>, `REPUTATION-Q10` <dbl>,
    ## #   `REPUTATION-FMCGOFF1` <dbl>, `REPUTATION-FMCGOFF2` <dbl>,
    ## #   `REPUTATION-FMCGOFF3` <dbl>, `REPUTATION-FMCGINNO1` <dbl>,
    ## #   `REPUTATION-FMCGINNO2` <dbl>, `REPUTATION-FMCGCC1` <dbl>,
    ## #   `REPUTATION-FMCGCC2` <dbl>, `REPUTATION-FMCGRFS1` <dbl>,
    ## #   `REPUTATION-FMCGRFS2` <dbl>, `REPUTATION-FMCGSR1` <dbl>,
    ## #   `REPUTATION-FMCGSR2` <dbl>, `REPUTATION-FMCGSA1` <dbl>,
    ## #   `REPUTATION-FMCGTR1` <dbl>, `REPUTATION-FMCGOP1` <dbl>,
    ## #   `REPUTATION-FMCGLOY1` <dbl>, `REPUTATION-FMCGWOM1` <dbl>,
    ## #   `REPUTATION-Q12` <dbl>, `REPUTATION-FIOFF1` <dbl>,
    ## #   `REPUTATION-FIOFF2` <dbl>, `REPUTATION-FIINNO1` <dbl>,
    ## #   `REPUTATION-FIINNO2` <dbl>, `REPUTATION-FICC1` <dbl>,
    ## #   `REPUTATION-FICC2` <dbl>, `REPUTATION-FIRFS1` <dbl>,
    ## #   `REPUTATION-FIRFS2` <dbl>, `REPUTATION-FISR1` <dbl>,
    ## #   `REPUTATION-FISR2` <dbl>, `REPUTATION-FISA1` <dbl>,
    ## #   `REPUTATION-FITR1` <dbl>, `REPUTATION-FIOP1` <dbl>,
    ## #   `REPUTATION-FILOY1` <dbl>, `REPUTATION-FIWOM1` <dbl>,
    ## #   `REPUTATION-Q13` <dbl>, `REPUTATION-MANOFF1` <dbl>,
    ## #   `REPUTATION-MANOFF2` <dbl>, `REPUTATION-MANOFF3` <dbl>,
    ## #   `REPUTATION-MANINNO1` <dbl>, `REPUTATION-MANINNO2` <dbl>,
    ## #   `REPUTATION-MANCC1` <dbl>, `REPUTATION-MANCC2` <dbl>,
    ## #   `REPUTATION-MANRFS1` <dbl>, `REPUTATION-MANRFS2` <dbl>,
    ## #   `REPUTATION-MANSR1` <dbl>, `REPUTATION-MANSR2` <dbl>,
    ## #   `REPUTATION-MANSA1` <dbl>, `REPUTATION-MANTR1` <dbl>,
    ## #   `REPUTATION-MANOP1` <dbl>, `REPUTATION-MANLOY1` <dbl>,
    ## #   `REPUTATION-MANWOM1` <dbl>, `REPUTATION-Q14` <dbl>,
    ## #   `REPUTATION-AIOFF1` <dbl>, `REPUTATION-AIOFF2` <dbl>,
    ## #   `REPUTATION-AIOFF3` <dbl>, `REPUTATION-AIINNO1` <dbl>,
    ## #   `REPUTATION-AIINNO2` <dbl>, `REPUTATION-AICC1` <dbl>,
    ## #   `REPUTATION-AICC2` <dbl>, `REPUTATION-AIRFS1` <dbl>,
    ## #   `REPUTATION-AIRFS2` <dbl>, `REPUTATION-AISR1` <dbl>,
    ## #   `REPUTATION-AISR2` <dbl>, `REPUTATION-AISA1` <dbl>,
    ## #   `REPUTATION-AITR1` <dbl>, `REPUTATION-AIOP1` <dbl>,
    ## #   `REPUTATION-AILOY1` <dbl>, `REPUTATION-AIWOM1` <dbl>,
    ## #   `REPUTATION-Q15` <dbl>, `REPUTATION-POFF1` <dbl>, `REPUTATION-PCC1` <dbl>,
    ## #   `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>, `REPUTATION-PSA1` <dbl>,
    ## #   `REPUTATION-PTR1` <dbl>, `REPUTATION-POP1` <dbl>, `REPUTATION-PLOY1` <dbl>,
    ## #   `REPUTATION-PWOM1` <dbl>, `REPUTATION-untitled38` <chr>,
    ## #   `REPUTATION-untitled38-altitude` <dbl>,
    ## #   `REPUTATION-untitled38-accuracy` <dbl>, `REPUTATION-untitled39` <chr>,
    ## #   `REPUTATION-meta-instanceID` <chr>

``` r
#viewing the remaining columns
colnames(df1)
```

    ##  [1] "REPUTATION-Q1"                  "REPUTATION-Q2"                 
    ##  [3] "REPUTATION-Q4"                  "REPUTATION-Q6"                 
    ##  [5] "REPUTATION-Q7"                  "REPUTATION-Q8"                 
    ##  [7] "REPUTATION-Q9"                  "REPUTATION-Q10"                
    ##  [9] "REPUTATION-FMCGOFF1"            "REPUTATION-FMCGOFF2"           
    ## [11] "REPUTATION-FMCGOFF3"            "REPUTATION-FMCGINNO1"          
    ## [13] "REPUTATION-FMCGINNO2"           "REPUTATION-FMCGCC1"            
    ## [15] "REPUTATION-FMCGCC2"             "REPUTATION-FMCGRFS1"           
    ## [17] "REPUTATION-FMCGRFS2"            "REPUTATION-FMCGSR1"            
    ## [19] "REPUTATION-FMCGSR2"             "REPUTATION-FMCGSA1"            
    ## [21] "REPUTATION-FMCGTR1"             "REPUTATION-FMCGOP1"            
    ## [23] "REPUTATION-FMCGLOY1"            "REPUTATION-FMCGWOM1"           
    ## [25] "REPUTATION-Q12"                 "REPUTATION-FIOFF1"             
    ## [27] "REPUTATION-FIOFF2"              "REPUTATION-FIINNO1"            
    ## [29] "REPUTATION-FIINNO2"             "REPUTATION-FICC1"              
    ## [31] "REPUTATION-FICC2"               "REPUTATION-FIRFS1"             
    ## [33] "REPUTATION-FIRFS2"              "REPUTATION-FISR1"              
    ## [35] "REPUTATION-FISR2"               "REPUTATION-FISA1"              
    ## [37] "REPUTATION-FITR1"               "REPUTATION-FIOP1"              
    ## [39] "REPUTATION-FILOY1"              "REPUTATION-FIWOM1"             
    ## [41] "REPUTATION-Q13"                 "REPUTATION-MANOFF1"            
    ## [43] "REPUTATION-MANOFF2"             "REPUTATION-MANOFF3"            
    ## [45] "REPUTATION-MANINNO1"            "REPUTATION-MANINNO2"           
    ## [47] "REPUTATION-MANCC1"              "REPUTATION-MANCC2"             
    ## [49] "REPUTATION-MANRFS1"             "REPUTATION-MANRFS2"            
    ## [51] "REPUTATION-MANSR1"              "REPUTATION-MANSR2"             
    ## [53] "REPUTATION-MANSA1"              "REPUTATION-MANTR1"             
    ## [55] "REPUTATION-MANOP1"              "REPUTATION-MANLOY1"            
    ## [57] "REPUTATION-MANWOM1"             "REPUTATION-Q14"                
    ## [59] "REPUTATION-AIOFF1"              "REPUTATION-AIOFF2"             
    ## [61] "REPUTATION-AIOFF3"              "REPUTATION-AIINNO1"            
    ## [63] "REPUTATION-AIINNO2"             "REPUTATION-AICC1"              
    ## [65] "REPUTATION-AICC2"               "REPUTATION-AIRFS1"             
    ## [67] "REPUTATION-AIRFS2"              "REPUTATION-AISR1"              
    ## [69] "REPUTATION-AISR2"               "REPUTATION-AISA1"              
    ## [71] "REPUTATION-AITR1"               "REPUTATION-AIOP1"              
    ## [73] "REPUTATION-AILOY1"              "REPUTATION-AIWOM1"             
    ## [75] "REPUTATION-Q15"                 "REPUTATION-POFF1"              
    ## [77] "REPUTATION-PCC1"                "REPUTATION-PRFS1"              
    ## [79] "REPUTATION-PSR1"                "REPUTATION-PSA1"               
    ## [81] "REPUTATION-PTR1"                "REPUTATION-POP1"               
    ## [83] "REPUTATION-PLOY1"               "REPUTATION-PWOM1"              
    ## [85] "REPUTATION-untitled38"          "REPUTATION-untitled38-altitude"
    ## [87] "REPUTATION-untitled38-accuracy" "REPUTATION-untitled39"         
    ## [89] "REPUTATION-meta-instanceID"

``` r
View(df1)
```

``` r
# selecting the numerical data columns
df2 <- df1 %>% select_if(is.numeric)
df2
```

    ## # A tibble: 167 x 85
    ##    `REPUTATION-Q1` `REPUTATION-Q2` `REPUTATION-Q4` `REPUTATION-Q6`
    ##              <dbl>           <dbl>           <dbl>           <dbl>
    ##  1               2               1               2               2
    ##  2               2               2               6               1
    ##  3               1               1               7               2
    ##  4               1               2               7               1
    ##  5               1               2               6               2
    ##  6               1               2               7               2
    ##  7               1               1               1               2
    ##  8               2               2               5               2
    ##  9               2               2               1               1
    ## 10               1               1               7               1
    ## # ... with 157 more rows, and 81 more variables: `REPUTATION-Q7` <dbl>,
    ## #   `REPUTATION-Q8` <dbl>, `REPUTATION-Q10` <dbl>, `REPUTATION-FMCGOFF1` <dbl>,
    ## #   `REPUTATION-FMCGOFF2` <dbl>, `REPUTATION-FMCGOFF3` <dbl>,
    ## #   `REPUTATION-FMCGINNO1` <dbl>, `REPUTATION-FMCGINNO2` <dbl>,
    ## #   `REPUTATION-FMCGCC1` <dbl>, `REPUTATION-FMCGCC2` <dbl>,
    ## #   `REPUTATION-FMCGRFS1` <dbl>, `REPUTATION-FMCGRFS2` <dbl>,
    ## #   `REPUTATION-FMCGSR1` <dbl>, `REPUTATION-FMCGSR2` <dbl>,
    ## #   `REPUTATION-FMCGSA1` <dbl>, `REPUTATION-FMCGTR1` <dbl>,
    ## #   `REPUTATION-FMCGOP1` <dbl>, `REPUTATION-FMCGLOY1` <dbl>,
    ## #   `REPUTATION-FMCGWOM1` <dbl>, `REPUTATION-Q12` <dbl>,
    ## #   `REPUTATION-FIOFF1` <dbl>, `REPUTATION-FIOFF2` <dbl>,
    ## #   `REPUTATION-FIINNO1` <dbl>, `REPUTATION-FIINNO2` <dbl>,
    ## #   `REPUTATION-FICC1` <dbl>, `REPUTATION-FICC2` <dbl>,
    ## #   `REPUTATION-FIRFS1` <dbl>, `REPUTATION-FIRFS2` <dbl>,
    ## #   `REPUTATION-FISR1` <dbl>, `REPUTATION-FISR2` <dbl>,
    ## #   `REPUTATION-FISA1` <dbl>, `REPUTATION-FITR1` <dbl>,
    ## #   `REPUTATION-FIOP1` <dbl>, `REPUTATION-FILOY1` <dbl>,
    ## #   `REPUTATION-FIWOM1` <dbl>, `REPUTATION-Q13` <dbl>,
    ## #   `REPUTATION-MANOFF1` <dbl>, `REPUTATION-MANOFF2` <dbl>,
    ## #   `REPUTATION-MANOFF3` <dbl>, `REPUTATION-MANINNO1` <dbl>,
    ## #   `REPUTATION-MANINNO2` <dbl>, `REPUTATION-MANCC1` <dbl>,
    ## #   `REPUTATION-MANCC2` <dbl>, `REPUTATION-MANRFS1` <dbl>,
    ## #   `REPUTATION-MANRFS2` <dbl>, `REPUTATION-MANSR1` <dbl>,
    ## #   `REPUTATION-MANSR2` <dbl>, `REPUTATION-MANSA1` <dbl>,
    ## #   `REPUTATION-MANTR1` <dbl>, `REPUTATION-MANOP1` <dbl>,
    ## #   `REPUTATION-MANLOY1` <dbl>, `REPUTATION-MANWOM1` <dbl>,
    ## #   `REPUTATION-Q14` <dbl>, `REPUTATION-AIOFF1` <dbl>,
    ## #   `REPUTATION-AIOFF2` <dbl>, `REPUTATION-AIOFF3` <dbl>,
    ## #   `REPUTATION-AIINNO1` <dbl>, `REPUTATION-AIINNO2` <dbl>,
    ## #   `REPUTATION-AICC1` <dbl>, `REPUTATION-AICC2` <dbl>,
    ## #   `REPUTATION-AIRFS1` <dbl>, `REPUTATION-AIRFS2` <dbl>,
    ## #   `REPUTATION-AISR1` <dbl>, `REPUTATION-AISR2` <dbl>,
    ## #   `REPUTATION-AISA1` <dbl>, `REPUTATION-AITR1` <dbl>,
    ## #   `REPUTATION-AIOP1` <dbl>, `REPUTATION-AILOY1` <dbl>,
    ## #   `REPUTATION-AIWOM1` <dbl>, `REPUTATION-Q15` <dbl>,
    ## #   `REPUTATION-POFF1` <dbl>, `REPUTATION-PCC1` <dbl>,
    ## #   `REPUTATION-PRFS1` <dbl>, `REPUTATION-PSR1` <dbl>, `REPUTATION-PSA1` <dbl>,
    ## #   `REPUTATION-PTR1` <dbl>, `REPUTATION-POP1` <dbl>, `REPUTATION-PLOY1` <dbl>,
    ## #   `REPUTATION-PWOM1` <dbl>, `REPUTATION-untitled38-altitude` <dbl>,
    ## #   `REPUTATION-untitled38-accuracy` <dbl>

``` r
# visualizing any outliers in the data
par (mfrow= c ( 3, 3))
for (i in 1 : length (df2)) {
boxplot (df2[,i], main= names (df2[i]), type= "l" )
}
```

![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-8.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-9.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-18-10.png)<!-- -->
\# EXPLORATORY DATA ANALYSIS \#\# FEATURE ENGINEERING

``` r
View(df2)
```

``` r
# selecting the required columns for analysis 
# since there are several brands in the questionnaire, i will use the fast moving consumer goods as a sample for the whole data on section D to determine the factors that play a keyrole in the reputation of a brand
# selecting the required columns for analysis in fast moving consumer goods
df3 <- subset(df2, select = c("REPUTATION-Q1", "REPUTATION-Q2", "REPUTATION-Q4", "REPUTATION-Q6", "REPUTATION-Q7", "REPUTATION-Q8", "REPUTATION-Q10", "REPUTATION-FMCGOFF2", "REPUTATION-FMCGOFF3", "REPUTATION-FMCGINNO2",  "REPUTATION-FMCGRFS1",  "REPUTATION-FMCGSA1", "REPUTATION-FMCGTR1", "REPUTATION-FMCGOP1"))
# renaming columns for easy analysis
df3 <- df3 %>% rename(monthy_expenditure = "REPUTATION-Q1")
df3 <- df3 %>% rename(purchase_consideration = "REPUTATION-Q2")
df3 <- df3 %>% rename(price_variations = "REPUTATION-Q4")
df3 <- df3 %>% rename(gender = "REPUTATION-Q6")
df3 <- df3 %>% rename(education_level = "REPUTATION-Q7")
df3 <- df3 %>% rename(age_bracket = "REPUTATION-Q8")
df3 <- df3 %>% rename(conversance = "REPUTATION-Q10")
df3 <- df3 %>% rename(informative = "REPUTATION-FMCGOFF2")
df3 <- df3 %>% rename(problem_solving = "REPUTATION-FMCGOFF3")
df3 <- df3 %>% rename(pioneering_products = "REPUTATION-FMCGINNO2")
df3 <- df3 %>% rename(appealing_m_campaign = "REPUTATION-FMCGRFS1")
df3 <- df3 %>% rename(satisfaction = "REPUTATION-FMCGSA1")
df3 <- df3 %>% rename(trustworthy = "REPUTATION-FMCGTR1")
df3 <- df3 %>% rename(honest_straightforward = "REPUTATION-FMCGOP1")
```

``` r
# previewing the new dataset
View(df3)
```

## UNIVARITE ANALYSIS

### measures of central tendencies and percentiles

``` r
# descriptive statistics
# these summaries will provide us with the measures of central tendencies of the numerical columns
summary(df3)
```

    ##  monthy_expenditure purchase_consideration price_variations     gender     
    ##  Min.   :1.000      Min.   :1.000          Min.   :1.000    Min.   :1.000  
    ##  1st Qu.:1.000      1st Qu.:2.000          1st Qu.:2.000    1st Qu.:1.000  
    ##  Median :1.000      Median :2.000          Median :4.000    Median :1.000  
    ##  Mean   :1.443      Mean   :1.994          Mean   :4.216    Mean   :1.473  
    ##  3rd Qu.:2.000      3rd Qu.:2.000          3rd Qu.:6.000    3rd Qu.:2.000  
    ##  Max.   :3.000      Max.   :5.000          Max.   :9.000    Max.   :2.000  
    ##  education_level  age_bracket     conversance     informative   
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.000   1st Qu.:3.000   1st Qu.:1.000   1st Qu.:4.000  
    ##  Median :3.000   Median :4.000   Median :2.000   Median :4.000  
    ##  Mean   :2.784   Mean   :4.048   Mean   :2.222   Mean   :3.701  
    ##  3rd Qu.:3.000   3rd Qu.:5.000   3rd Qu.:3.000   3rd Qu.:4.000  
    ##  Max.   :5.000   Max.   :9.000   Max.   :5.000   Max.   :5.000  
    ##  problem_solving pioneering_products appealing_m_campaign  satisfaction 
    ##  Min.   :1.000   Min.   :2.000       Min.   :1.000        Min.   :1.00  
    ##  1st Qu.:3.000   1st Qu.:4.000       1st Qu.:4.000        1st Qu.:4.00  
    ##  Median :4.000   Median :4.000       Median :4.000        Median :4.00  
    ##  Mean   :3.347   Mean   :3.892       Mean   :4.018        Mean   :4.03  
    ##  3rd Qu.:4.000   3rd Qu.:4.000       3rd Qu.:4.000        3rd Qu.:4.00  
    ##  Max.   :5.000   Max.   :5.000       Max.   :5.000        Max.   :5.00  
    ##   trustworthy    honest_straightforward
    ##  Min.   :1.000   Min.   :1.000         
    ##  1st Qu.:4.000   1st Qu.:4.000         
    ##  Median :4.000   Median :4.000         
    ##  Mean   :4.084   Mean   :4.054         
    ##  3rd Qu.:4.000   3rd Qu.:4.000         
    ##  Max.   :5.000   Max.   :5.000

### Measures of Disprersion and all other descriptives

``` r
#descriptive statistics of the numerical columns i.e. all statistics except for the quantile ranges which have been shown in the above summary statistics
describe(df3)
```

    ##                        vars   n mean   sd median trimmed  mad min max range
    ## monthy_expenditure        1 167 1.44 0.62      1    1.35 0.00   1   3     2
    ## purchase_consideration    2 167 1.99 0.76      2    1.90 0.00   1   5     4
    ## price_variations          3 167 4.22 2.27      4    4.20 2.97   1   9     8
    ## gender                    4 167 1.47 0.50      1    1.47 0.00   1   2     1
    ## education_level           5 167 2.78 0.87      3    2.80 1.48   1   5     4
    ## age_bracket               6 167 4.05 1.79      4    3.90 1.48   1   9     8
    ## conversance               7 167 2.22 0.97      2    2.17 1.48   1   5     4
    ## informative               8 167 3.70 0.70      4    3.87 0.00   1   5     4
    ## problem_solving           9 167 3.35 0.94      4    3.42 0.00   1   5     4
    ## pioneering_products      10 167 3.89 0.62      4    3.92 0.00   2   5     3
    ## appealing_m_campaign     11 167 4.02 0.51      4    4.02 0.00   1   5     4
    ## satisfaction             12 167 4.03 0.50      4    4.03 0.00   1   5     4
    ## trustworthy              13 167 4.08 0.51      4    4.07 0.00   1   5     4
    ## honest_straightforward   14 167 4.05 0.52      4    4.03 0.00   1   5     4
    ##                         skew kurtosis   se
    ## monthy_expenditure      1.06     0.04 0.05
    ## purchase_consideration  1.94     5.92 0.06
    ## price_variations       -0.07    -1.36 0.18
    ## gender                  0.11    -2.00 0.04
    ## education_level        -0.07    -0.37 0.07
    ## age_bracket             0.57    -0.55 0.14
    ## conversance             0.35    -0.37 0.07
    ## informative            -2.05     3.82 0.05
    ## problem_solving        -0.69    -0.36 0.07
    ## pioneering_products    -0.98     2.17 0.05
    ## appealing_m_campaign   -1.33     8.54 0.04
    ## satisfaction           -1.11     8.42 0.04
    ## trustworthy            -0.95     7.90 0.04
    ## honest_straightforward -2.25    14.61 0.04

### Univariate Graphical

``` r
df3 <- as.data.frame(sapply(df3, as.numeric))
for (col in 1:ncol(df3[, 1:14])){
  hist(df3[,col], main = names(df3[col]), col = 'lightblue', xlab = names(df3[col]))
}
```

![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-4.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-5.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-6.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-7.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-8.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-9.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-10.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-11.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-12.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-13.png)<!-- -->![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-24-14.png)<!-- -->

## BIVARIATE ANALYSIS

### covariance

``` r
# covariance of variables in section a to section d
cov <- cov(df3)
round(cov, 2)
```

    ##                        monthy_expenditure purchase_consideration
    ## monthy_expenditure                   0.38                   0.10
    ## purchase_consideration               0.10                   0.58
    ## price_variations                    -0.36                  -0.29
    ## gender                              -0.01                   0.01
    ## education_level                      0.19                   0.03
    ## age_bracket                          0.45                   0.09
    ## conversance                         -0.01                   0.13
    ## informative                         -0.01                  -0.04
    ## problem_solving                      0.16                   0.01
    ## pioneering_products                  0.04                  -0.05
    ## appealing_m_campaign                 0.06                   0.00
    ## satisfaction                         0.06                   0.00
    ## trustworthy                          0.02                   0.02
    ## honest_straightforward               0.02                   0.02
    ##                        price_variations gender education_level age_bracket
    ## monthy_expenditure                -0.36  -0.01            0.19        0.45
    ## purchase_consideration            -0.29   0.01            0.03        0.09
    ## price_variations                   5.15   0.02           -0.34       -0.44
    ## gender                             0.02   0.25            0.01       -0.06
    ## education_level                   -0.34   0.01            0.75        0.57
    ## age_bracket                       -0.44  -0.06            0.57        3.20
    ## conversance                       -0.07   0.02           -0.05       -0.03
    ## informative                        0.06  -0.02            0.06       -0.06
    ## problem_solving                   -0.34  -0.01            0.15        0.29
    ## pioneering_products               -0.13   0.00            0.07       -0.06
    ## appealing_m_campaign               0.02   0.01           -0.02        0.00
    ## satisfaction                       0.02   0.03            0.02        0.09
    ## trustworthy                       -0.02   0.03           -0.01       -0.05
    ## honest_straightforward            -0.05   0.00            0.00       -0.01
    ##                        conversance informative problem_solving
    ## monthy_expenditure           -0.01       -0.01            0.16
    ## purchase_consideration        0.13       -0.04            0.01
    ## price_variations             -0.07        0.06           -0.34
    ## gender                        0.02       -0.02           -0.01
    ## education_level              -0.05        0.06            0.15
    ## age_bracket                  -0.03       -0.06            0.29
    ## conversance                   0.93       -0.09           -0.11
    ## informative                  -0.09        0.49            0.15
    ## problem_solving              -0.11        0.15            0.89
    ## pioneering_products          -0.09        0.18            0.13
    ## appealing_m_campaign          0.04        0.05            0.01
    ## satisfaction                 -0.02        0.09            0.07
    ## trustworthy                   0.00        0.06            0.05
    ## honest_straightforward       -0.04        0.11            0.02
    ##                        pioneering_products appealing_m_campaign satisfaction
    ## monthy_expenditure                    0.04                 0.06         0.06
    ## purchase_consideration               -0.05                 0.00         0.00
    ## price_variations                     -0.13                 0.02         0.02
    ## gender                                0.00                 0.01         0.03
    ## education_level                       0.07                -0.02         0.02
    ## age_bracket                          -0.06                 0.00         0.09
    ## conversance                          -0.09                 0.04        -0.02
    ## informative                           0.18                 0.05         0.09
    ## problem_solving                       0.13                 0.01         0.07
    ## pioneering_products                   0.39                 0.07         0.09
    ## appealing_m_campaign                  0.07                 0.26         0.06
    ## satisfaction                          0.09                 0.06         0.25
    ## trustworthy                           0.07                 0.02         0.11
    ## honest_straightforward                0.07                 0.02         0.10
    ##                        trustworthy honest_straightforward
    ## monthy_expenditure            0.02                   0.02
    ## purchase_consideration        0.02                   0.02
    ## price_variations             -0.02                  -0.05
    ## gender                        0.03                   0.00
    ## education_level              -0.01                   0.00
    ## age_bracket                  -0.05                  -0.01
    ## conversance                   0.00                  -0.04
    ## informative                   0.06                   0.11
    ## problem_solving               0.05                   0.02
    ## pioneering_products           0.07                   0.07
    ## appealing_m_campaign          0.02                   0.02
    ## satisfaction                  0.11                   0.10
    ## trustworthy                   0.26                   0.14
    ## honest_straightforward        0.14                   0.27

### correlation

``` r
coo <- cor(df3)
round(coo, 2)
```

    ##                        monthy_expenditure purchase_consideration
    ## monthy_expenditure                   1.00                   0.21
    ## purchase_consideration               0.21                   1.00
    ## price_variations                    -0.26                  -0.17
    ## gender                              -0.02                   0.04
    ## education_level                      0.35                   0.05
    ## age_bracket                          0.41                   0.07
    ## conversance                         -0.01                   0.17
    ## informative                         -0.01                  -0.07
    ## problem_solving                      0.27                   0.02
    ## pioneering_products                  0.09                  -0.10
    ## appealing_m_campaign                 0.19                   0.00
    ## satisfaction                         0.21                   0.00
    ## trustworthy                          0.05                   0.06
    ## honest_straightforward               0.08                   0.06
    ##                        price_variations gender education_level age_bracket
    ## monthy_expenditure                -0.26  -0.02            0.35        0.41
    ## purchase_consideration            -0.17   0.04            0.05        0.07
    ## price_variations                   1.00   0.02           -0.17       -0.11
    ## gender                             0.02   1.00            0.03       -0.07
    ## education_level                   -0.17   0.03            1.00        0.37
    ## age_bracket                       -0.11  -0.07            0.37        1.00
    ## conversance                       -0.03   0.03           -0.06       -0.02
    ## informative                        0.04  -0.06            0.09       -0.05
    ## problem_solving                   -0.16  -0.02            0.18        0.17
    ## pioneering_products               -0.09  -0.01            0.12       -0.05
    ## appealing_m_campaign               0.02   0.04           -0.05        0.00
    ## satisfaction                       0.02   0.11            0.04        0.10
    ## trustworthy                       -0.02   0.13           -0.03       -0.06
    ## honest_straightforward            -0.04  -0.01            0.00       -0.01
    ##                        conversance informative problem_solving
    ## monthy_expenditure           -0.01       -0.01            0.27
    ## purchase_consideration        0.17       -0.07            0.02
    ## price_variations             -0.03        0.04           -0.16
    ## gender                        0.03       -0.06           -0.02
    ## education_level              -0.06        0.09            0.18
    ## age_bracket                  -0.02       -0.05            0.17
    ## conversance                   1.00       -0.13           -0.12
    ## informative                  -0.13        1.00            0.22
    ## problem_solving              -0.12        0.22            1.00
    ## pioneering_products          -0.15        0.41            0.22
    ## appealing_m_campaign          0.08        0.13            0.02
    ## satisfaction                 -0.04        0.25            0.16
    ## trustworthy                   0.00        0.17            0.10
    ## honest_straightforward       -0.08        0.29            0.04
    ##                        pioneering_products appealing_m_campaign satisfaction
    ## monthy_expenditure                    0.09                 0.19         0.21
    ## purchase_consideration               -0.10                 0.00         0.00
    ## price_variations                     -0.09                 0.02         0.02
    ## gender                               -0.01                 0.04         0.11
    ## education_level                       0.12                -0.05         0.04
    ## age_bracket                          -0.05                 0.00         0.10
    ## conversance                          -0.15                 0.08        -0.04
    ## informative                           0.41                 0.13         0.25
    ## problem_solving                       0.22                 0.02         0.16
    ## pioneering_products                   1.00                 0.22         0.28
    ## appealing_m_campaign                  0.22                 1.00         0.24
    ## satisfaction                          0.28                 0.24         1.00
    ## trustworthy                           0.22                 0.09         0.44
    ## honest_straightforward                0.22                 0.09         0.39
    ##                        trustworthy honest_straightforward
    ## monthy_expenditure            0.05                   0.08
    ## purchase_consideration        0.06                   0.06
    ## price_variations             -0.02                  -0.04
    ## gender                        0.13                  -0.01
    ## education_level              -0.03                   0.00
    ## age_bracket                  -0.06                  -0.01
    ## conversance                   0.00                  -0.08
    ## informative                   0.17                   0.29
    ## problem_solving               0.10                   0.04
    ## pioneering_products           0.22                   0.22
    ## appealing_m_campaign          0.09                   0.09
    ## satisfaction                  0.44                   0.39
    ## trustworthy                   1.00                   0.53
    ## honest_straightforward        0.53                   1.00

# implementation of the solution

## principal component analysis

``` r
df4 <- prcomp(df3)
summary(df4)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5    PC6     PC7
    ## Standard deviation     2.3241 1.8094 1.09237 0.92191 0.78288 0.7761 0.74134
    ## Proportion of Variance 0.3847 0.2332 0.08498 0.06053 0.04365 0.0429 0.03914
    ## Cumulative Proportion  0.3847 0.6178 0.70283 0.76336 0.80701 0.8499 0.88905
    ##                            PC8     PC9    PC10    PC11    PC12   PC13    PC14
    ## Standard deviation     0.59632 0.55481 0.49505 0.47485 0.42099 0.3691 0.33223
    ## Proportion of Variance 0.02533 0.02192 0.01745 0.01606 0.01262 0.0097 0.00786
    ## Cumulative Proportion  0.91438 0.93630 0.95376 0.96982 0.98244 0.9921 1.00000

``` r
# Calling str() to have a look at your PCA object
str(df4)
```

    ## List of 5
    ##  $ sdev    : num [1:14] 2.324 1.809 1.092 0.922 0.783 ...
    ##  $ rotation: num [1:14, 1:14] -0.09956 -0.06472 0.94822 0.00624 -0.10793 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:14] "monthy_expenditure" "purchase_consideration" "price_variations" "gender" ...
    ##   .. ..$ : chr [1:14] "PC1" "PC2" "PC3" "PC4" ...
    ##  $ center  : Named num [1:14] 1.44 1.99 4.22 1.47 2.78 ...
    ##   ..- attr(*, "names")= chr [1:14] "monthy_expenditure" "purchase_consideration" "price_variations" "gender" ...
    ##  $ scale   : logi FALSE
    ##  $ x       : num [1:167, 1:14] -2.2 1.56 2.82 2.59 2.27 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:14] "PC1" "PC2" "PC3" "PC4" ...
    ##  - attr(*, "class")= chr "prcomp"

``` r
# Installing our ggbiplot visualisation package
# 
library(devtools)
```

    ## Loading required package: usethis

``` r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
#install_github("vqv/ggbiplot",force=TRUE)
library(ggbiplot)
```

    ## Loading required package: plyr

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:plotly':
    ## 
    ##     arrange, mutate, rename, summarise

    ## Loading required package: scales

    ## 
    ## Attaching package: 'scales'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     alpha, rescale

``` r
ggbiplot(df4)
```

![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
# Adding more detail to the plot, we provide arguments rownames as labels
# 
ggbiplot(df4, labels=rownames(df), obs.scale = 1, var.scale = 1)
```

![](ELIZABETH_JOSEPHINE_NEWMARK_TECHNICAL_TEST_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
