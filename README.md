# Research Question
2024 FIRE198 Sustainability Analytics

## Team Members

Write names of team members here.

Raphael Coronel, Taylor Woods, Brian Huang, Joseph Tang

## Research Question

Write research question here.

How does the opening/announcement of the Silver Line affect housing
costs?

## Background

Write research relevance and literature review here.

## Data

Explain data source here.

Clean data here.

``` r
#library("tidyverse")
#library("glue")

#df <- read.csv("Zhvi.csv")
```

``` r
#df <- df %>% filter(df$State == "MD" | df$State == "VA" | df$State == "DC")
#df2 <- df %>% select(-StateName,-RegionType,-SizeRank,-RegionID)
```

``` r
#write.csv(df2,file = "Zhvi_DMV2.csv")
```

## Preliminary Results

Use your final dataset to visualize how the treatment variable impacts
the outcome variable here.

First I add a column saying how many bathrooms the datapoint has has.
Only code for br5 is shown but imagine the samw thing happens fo rthe
other 4.

``` r
#br5$BedroomCnt <- rep(5,nrow(br5))
```

creating one big data frame and double checking that nothing got nipped
off and moving bedroomcnt to the front since its kinda imortant

``` r
#brAll <- rbind(br1,br2,br3,br4,br5)
#sum(nrow(br1),nrow(br2),nrow(br3),nrow(br4),nrow(br5))
#br <- brAll %>% relocate(BedroomCnt)
```

Some additional processing to get rid of horrendous date data and
replace with good year + month data 😌

``` r
#br2 <- br  %>% pivot_longer(cols = `2000-01-31`:`2024-07-31`,names_to = "Date", values_to = "Price")
#br3 <- br2 %>% mutate(Year = year(Date), Month = month(Date))
#br3$Date <- NULL
```

savin it for now

``` r
#write.csv(br3,file = "Zhvi.csv")
```

due to git file size constraints, the file is saved at:

### G:drives\2024 FIRE-SALining

Step 1. Install necessary packages.

``` r
#install.packages("tidyverse")
#install.packages("kableExtra")
```

Step 2. Declare that you will use these packages in this session.

``` r
library("tidyverse")
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("kableExtra")
```


    Attaching package: 'kableExtra'

    The following object is masked from 'package:dplyr':

        group_rows

Step 3. Upload the dataframe that you have created in Spring 2024 into
the repository.

Step 4. Open the dataframe into the RStudio Environment.

``` r
df<-read.csv("dfdmv_with_treatment.csv")
```

Step 5. Use the **head** and **kable** function showcase the first 10
rows of the dataframe to the reader.

``` r
# kable(head(df))
head(df)
```

      RegionName BedroomCnt State       City           CountyName    Price Year
    1      20002          1    DC Washington District of Columbia 96340.78 2000
    2      20002          1    DC Washington District of Columbia 96753.71 2000
    3      20002          1    DC Washington District of Columbia 97105.10 2000
    4      20002          1    DC Washington District of Columbia 97815.25 2000
    5      20002          1    DC Washington District of Columbia 98604.81 2000
    6      20002          1    DC Washington District of Columbia 99356.27 2000
      Month       date station_open
    1     1 2000-01-01            0
    2     2 2000-02-01            0
    3     3 2000-03-01            0
    4     4 2000-04-01            0
    5     5 2000-05-01            0
    6     6 2000-06-01            0

## Question 1: What is the frequency of this data frame?

Answer: once a month

## Question 2: What is the cross-sectional (geographical) unit of this data frame?

Answer: zip code

Step 6. Use the **names** function to display all the variables (column)
in the dataframe.

``` r
names(df)
```

     [1] "RegionName"   "BedroomCnt"   "State"        "City"         "CountyName"  
     [6] "Price"        "Year"         "Month"        "date"         "station_open"

## Question 3: Which column represents the treatment variable of interest?

Answer: station_open

## Question 4: Which column represents the outcome variable of interest?

Answer: Price (i wuv munni)

Step 7: Create a boxplot to visualize the distribution of the outcome
variable under treatment and no treatment.

``` r
#! eval: false
ggplot(df, aes(x=Price)) +
  geom_histogram() +
  facet_wrap(~station_open) 
```

    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-commonmark/unnamed-chunk-13-1.png)

Step 8: Fit a regression model $y=\beta_0 + \beta_1 x + \epsilon$ where
$y$ is the outcome variable and $x$ is the treatment variable. Use the
**summary** function to display the results.

``` r
model1<-lm(Price ~ station_open, data=df)

summary(model1)
```


    Call:
    lm(formula = Price ~ station_open, data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -503441 -266414  -97367  164093 2330457 

    Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    537857       2945 182.642  < 2e-16 ***
    station_open    27241       4196   6.492 8.59e-11 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 371100 on 31298 degrees of freedom
    Multiple R-squared:  0.001345,  Adjusted R-squared:  0.001313 
    F-statistic: 42.15 on 1 and 31298 DF,  p-value: 8.586e-11

## Question 5: What is the predicted value of the outcome variable when treatment=0?

Answer: 537857

## Question 6: What is predicted value of the outcome variable when treatment=1?

Answer:537857 + 27241 = 565098

basically + 30k

## Question 7: What is the equation that describes the linear regression above? Please include an explanation of the variables and subscripts.

Answer:

$$
Price = \beta_0 + \beta_1 station_open
$$

I love latex 😌 (it refuses to show up)

## Question 8: What fixed effects can be included in the regression? What does each fixed effects control for? Please include a new equation that incorporates the fixed effects.

Answer: Month, Year, CountyName, BedroomCnt, RegionName (zip code)

## Question 9: What is the impact of the treatment effect once fixed effects are included?

Answer:

``` r
model2 <-lm(Price ~ station_open + as.factor(Month) + as.factor(Year) + as.factor(CountyName) + as.factor(BedroomCnt) + as.factor(RegionName ), data = df)
```

``` r
summary(model2)
```


    Call:
    lm(formula = Price ~ station_open + as.factor(Month) + as.factor(Year) + 
        as.factor(CountyName) + as.factor(BedroomCnt) + as.factor(RegionName), 
        data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -569045  -84351  -11556   65382 1236668 

    Coefficients: (5 not defined because of singularities)
                                               Estimate Std. Error t value Pr(>|t|)
    (Intercept)                                  -18642       7893  -2.362 0.018192
    station_open                                 -52917       4499 -11.762  < 2e-16
    as.factor(Month)2                              2364       4391   0.538 0.590331
    as.factor(Month)3                              4920       4391   1.121 0.262497
    as.factor(Month)4                              7770       4390   1.770 0.076774
    as.factor(Month)5                             10460       4390   2.383 0.017183
    as.factor(Month)6                             12615       4390   2.874 0.004061
    as.factor(Month)7                             14211       4390   3.237 0.001208
    as.factor(Month)8                             15756       4445   3.545 0.000393
    as.factor(Month)9                             16972       4444   3.819 0.000134
    as.factor(Month)10                            18438       4444   4.149 3.35e-05
    as.factor(Month)11                            20252       4444   4.558 5.19e-06
    as.factor(Month)12                            22434       4443   5.049 4.47e-07
    as.factor(Year)2001                           35547       6734   5.279 1.31e-07
    as.factor(Year)2002                           79572       6699  11.878  < 2e-16
    as.factor(Year)2003                          127534       6663  19.141  < 2e-16
    as.factor(Year)2004                          190232       6649  28.609  < 2e-16
    as.factor(Year)2005                          284061       6636  42.806  < 2e-16
    as.factor(Year)2006                          330731       6622  49.947  < 2e-16
    as.factor(Year)2007                          316082       6606  47.852  < 2e-16
    as.factor(Year)2008                          282169       6607  42.709  < 2e-16
    as.factor(Year)2009                          255719       6573  38.907  < 2e-16
    as.factor(Year)2010                          260539       6548  39.786  < 2e-16
    as.factor(Year)2011                          259086       6541  39.608  < 2e-16
    as.factor(Year)2012                          273775       6533  41.906  < 2e-16
    as.factor(Year)2013                          313492       6506  48.182  < 2e-16
    as.factor(Year)2014                          350605       6515  53.819  < 2e-16
    as.factor(Year)2015                          367570       6528  56.302  < 2e-16
    as.factor(Year)2016                          381513       6500  58.697  < 2e-16
    as.factor(Year)2017                          397551       6499  61.173  < 2e-16
    as.factor(Year)2018                          417901       6499  64.304  < 2e-16
    as.factor(Year)2019                          436674       6502  67.158  < 2e-16
    as.factor(Year)2020                          468314       6499  72.061  < 2e-16
    as.factor(Year)2021                          522043       6499  80.329  < 2e-16
    as.factor(Year)2022                          568093       6506  87.315  < 2e-16
    as.factor(Year)2023                          576596       6623  87.059  < 2e-16
    as.factor(Year)2024                          599682       7614  78.764  < 2e-16
    as.factor(CountyName)District of Columbia   -173259       6459 -26.824  < 2e-16
    as.factor(CountyName)Fairfax County          -77890       7081 -11.000  < 2e-16
    as.factor(CountyName)Falls Church City       -90417       6520 -13.867  < 2e-16
    as.factor(CountyName)Loudoun County         -353552       7864 -44.958  < 2e-16
    as.factor(CountyName)Prince Georges County  -553292       6767 -81.766  < 2e-16
    as.factor(BedroomCnt)2                       207960       3001  69.294  < 2e-16
    as.factor(BedroomCnt)3                       395434       3057 129.362  < 2e-16
    as.factor(BedroomCnt)4                       513004       3200 160.307  < 2e-16
    as.factor(BedroomCnt)5                       730381       3261 224.005  < 2e-16
    as.factor(RegionName)20003                   258970       7410  34.951  < 2e-16
    as.factor(RegionName)20004                   311627       9564  32.582  < 2e-16
    as.factor(RegionName)20005                   307332       8529  36.033  < 2e-16
    as.factor(RegionName)20007                   512459       5887  87.045  < 2e-16
    as.factor(RegionName)20019                  -219757       7410 -29.656  < 2e-16
    as.factor(RegionName)20024                   132234       7896  16.746  < 2e-16
    as.factor(RegionName)20037                   542184       8297  65.345  < 2e-16
    as.factor(RegionName)20147                    81205       7474  10.866  < 2e-16
    as.factor(RegionName)20148                    76316       8070   9.457  < 2e-16
    as.factor(RegionName)20166                       NA         NA      NA       NA
    as.factor(RegionName)20170                  -288091       6685 -43.098  < 2e-16
    as.factor(RegionName)20171                  -178800       6691 -26.722  < 2e-16
    as.factor(RegionName)20190                  -163478       6322 -25.860  < 2e-16
    as.factor(RegionName)20743                    24690       7973   3.097 0.001958
    as.factor(RegionName)20774                   172114       7301  23.574  < 2e-16
    as.factor(RegionName)20785                       NA         NA      NA       NA
    as.factor(RegionName)22043                  -103221       6725 -15.349  < 2e-16
    as.factor(RegionName)22046                       NA         NA      NA       NA
    as.factor(RegionName)22102                   167950       6322  26.567  < 2e-16
    as.factor(RegionName)22182                       NA         NA      NA       NA
    as.factor(RegionName)22201                   129647       7891  16.430  < 2e-16
    as.factor(RegionName)22203                   -33821       7891  -4.286 1.82e-05
    as.factor(RegionName)22205                    27183       8150   3.335 0.000854
    as.factor(RegionName)22209                   185944       8638  21.526  < 2e-16
    as.factor(RegionName)22213                       NA         NA      NA       NA
                                                  
    (Intercept)                                *  
    station_open                               ***
    as.factor(Month)2                             
    as.factor(Month)3                             
    as.factor(Month)4                          .  
    as.factor(Month)5                          *  
    as.factor(Month)6                          ** 
    as.factor(Month)7                          ** 
    as.factor(Month)8                          ***
    as.factor(Month)9                          ***
    as.factor(Month)10                         ***
    as.factor(Month)11                         ***
    as.factor(Month)12                         ***
    as.factor(Year)2001                        ***
    as.factor(Year)2002                        ***
    as.factor(Year)2003                        ***
    as.factor(Year)2004                        ***
    as.factor(Year)2005                        ***
    as.factor(Year)2006                        ***
    as.factor(Year)2007                        ***
    as.factor(Year)2008                        ***
    as.factor(Year)2009                        ***
    as.factor(Year)2010                        ***
    as.factor(Year)2011                        ***
    as.factor(Year)2012                        ***
    as.factor(Year)2013                        ***
    as.factor(Year)2014                        ***
    as.factor(Year)2015                        ***
    as.factor(Year)2016                        ***
    as.factor(Year)2017                        ***
    as.factor(Year)2018                        ***
    as.factor(Year)2019                        ***
    as.factor(Year)2020                        ***
    as.factor(Year)2021                        ***
    as.factor(Year)2022                        ***
    as.factor(Year)2023                        ***
    as.factor(Year)2024                        ***
    as.factor(CountyName)District of Columbia  ***
    as.factor(CountyName)Fairfax County        ***
    as.factor(CountyName)Falls Church City     ***
    as.factor(CountyName)Loudoun County        ***
    as.factor(CountyName)Prince Georges County ***
    as.factor(BedroomCnt)2                     ***
    as.factor(BedroomCnt)3                     ***
    as.factor(BedroomCnt)4                     ***
    as.factor(BedroomCnt)5                     ***
    as.factor(RegionName)20003                 ***
    as.factor(RegionName)20004                 ***
    as.factor(RegionName)20005                 ***
    as.factor(RegionName)20007                 ***
    as.factor(RegionName)20019                 ***
    as.factor(RegionName)20024                 ***
    as.factor(RegionName)20037                 ***
    as.factor(RegionName)20147                 ***
    as.factor(RegionName)20148                 ***
    as.factor(RegionName)20166                    
    as.factor(RegionName)20170                 ***
    as.factor(RegionName)20171                 ***
    as.factor(RegionName)20190                 ***
    as.factor(RegionName)20743                 ** 
    as.factor(RegionName)20774                 ***
    as.factor(RegionName)20785                    
    as.factor(RegionName)22043                 ***
    as.factor(RegionName)22046                    
    as.factor(RegionName)22102                 ***
    as.factor(RegionName)22182                    
    as.factor(RegionName)22201                 ***
    as.factor(RegionName)22203                 ***
    as.factor(RegionName)22205                 ***
    as.factor(RegionName)22209                 ***
    as.factor(RegionName)22213                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 159900 on 31234 degrees of freedom
    Multiple R-squared:  0.815, Adjusted R-squared:  0.8147 
    F-statistic:  2117 on 65 and 31234 DF,  p-value: < 2.2e-16

# Questions for Week 5

## Question 10: In a difference-in-differences (DiD) model, what is the treatment GROUP?

Answer:

## Question 11: In a DiD model, what are the control groups?

Answer:

## Question 12: What is the DiD regression equation that will answer your research question?

## Question 13: Run your DiD regressions below. What are the results of the DiD regression?

## Question 14: What are the next steps of your research?

Step 9: Change the document format to gfm

Step 10: Save this document as README.qmd

Step 11: Render the document. README.md file should be created after
this process.

Step 12: Push the document back to GitHub and observe your beautiful
document in your repository!

Step 13: If your team has a complete dataframe that includes both the
treated and outcome variable, you are done with the assignment. If not,
make a research plan in Notion to collect data on the outcome and
treatment variable and combine it into one dataframe.

Step 13: If your team has a complete dataframe that includes both the
treated and outcome variable, you are done with the assignment. If not,
make a research plan in Notion to collect data on the outcome and
treatment variable and combine it into one dataframe.
