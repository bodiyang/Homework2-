Analysis of the relationship of bridge conditation and unemployed rate
======================================================================

In this analysis, I will use the bridge data and labour data of 2018
Califorinia. Firstly, I tidy up the raw bridge data and labour data,
labeling the bridge data into structure number, place code, year built,
deck condition, superstructure condition, substructure condition,
location, county code; labeling the labour data into Area Code, State,
County, Area Title, Period, Civilian\_Labor\_Force, Employed,
Unemployed\_Level, Unemployed\_Rate

Then, I merge the bridge and labour data of 2018 California together.

After the data preparation, I built up three models: 1. linear
regression of unemployed rate with the factors: bridge deck condition,
bridge superstructure and bridge substructure 2. linear regression of
unemployed number with the factors: bridge deck condition, bridge
superstructure and bridge substructure 3. linear model to predict
unemployed rate based on previous month

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

    bridge_cleaned=read.csv('https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/CA18.txt') %>% 
      select(STRUCTURE_NUMBER_008,PLACE_CODE_004,YEAR_BUILT_027,DECK_COND_058,SUPERSTRUCTURE_COND_059,SUBSTRUCTURE_COND_060,LOCATION_009, COUNTY_CODE_003) %>% 
      mutate(Duration = 2018 - YEAR_BUILT_027) %>% 
      filter(Duration < 200)

    BLS=read.csv('https://www.bls.gov/web/metro/laucntycur14.txt',skip = 6,sep = '|',header = F,colClasses = 'character')
    colnames(BLS)=c("LAUS Area Code","State","County","Area Title","Period","Civilian_Labor_Force","Employed","Unemployed_Level","Unemployed_Rate")

    BLS.CA=filter(BLS,State == '  06  ' & Period=='   Nov-19  ')
    BLS.CA$County=as.numeric(BLS.CA$County)
    BLS.CA$Civilian_Labor_Force=as.numeric(gsub(",", "", BLS.CA$Civilian_Labor_Force))
    BLS.CA$Employed=as.numeric(gsub(",", "", BLS.CA$Employed))
    BLS.CA$Unemployed_Level=as.numeric(gsub(",", "", BLS.CA$Unemployed_Level))
    BLS.CA$Unemployed_Rate=as.numeric(BLS.CA$Unemployed_Rate)

    data.final=left_join(bridge_cleaned,BLS.CA,by = c("COUNTY_CODE_003" = "County"))

Linear model for unemployed rate with the factors: bridge deck condition, bridge superstructure and bridge substructure
-----------------------------------------------------------------------------------------------------------------------

We can build up a model of multivariate linear regression of the
unemployed rate and the bridge deck condition, superstructure &
substructure.

    lmUnemployedRate = lm(Unemployed_Rate~DECK_COND_058 + SUPERSTRUCTURE_COND_059 + SUBSTRUCTURE_COND_060 , data=data.final)

    summary(lmUnemployedRate)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Rate ~ DECK_COND_058 + SUPERSTRUCTURE_COND_059 + 
    ##     SUBSTRUCTURE_COND_060, data = data.final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5296 -1.4009 -0.5189  0.4043 17.1714 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)               4.10000    2.64417   1.551    0.121
    ## DECK_COND_0582            1.34657    4.33469   0.311    0.756
    ## DECK_COND_0583            0.73518    3.94285   0.186    0.852
    ## DECK_COND_0584            1.49750    3.89359   0.385    0.701
    ## DECK_COND_0585            1.10679    3.89522   0.284    0.776
    ## DECK_COND_0586            1.17930    3.89501   0.303    0.762
    ## DECK_COND_0587            1.30150    3.89478   0.334    0.738
    ## DECK_COND_0588            1.02813    3.89513   0.264    0.792
    ## DECK_COND_0589            1.09791    4.06350   0.270    0.787
    ## DECK_COND_058N            0.48770    3.89465   0.125    0.900
    ## SUPERSTRUCTURE_COND_0592 -2.13581    2.85892  -0.747    0.455
    ## SUPERSTRUCTURE_COND_0593 -0.67184    1.98510  -0.338    0.735
    ## SUPERSTRUCTURE_COND_0594 -0.39778    1.87536  -0.212    0.832
    ## SUPERSTRUCTURE_COND_0595 -0.55918    1.87266  -0.299    0.765
    ## SUPERSTRUCTURE_COND_0596 -0.48978    1.87209  -0.262    0.794
    ## SUPERSTRUCTURE_COND_0597 -0.54042    1.87228  -0.289    0.773
    ## SUPERSTRUCTURE_COND_0598 -0.59892    1.87301  -0.320    0.749
    ## SUPERSTRUCTURE_COND_0599  0.27688    2.17491   0.127    0.899
    ## SUPERSTRUCTURE_COND_059N       NA         NA      NA       NA
    ## SUBSTRUCTURE_COND_0602   -0.40197    2.18056  -0.184    0.854
    ## SUBSTRUCTURE_COND_0603    0.79380    2.21908   0.358    0.721
    ## SUBSTRUCTURE_COND_0604    0.32587    2.16595   0.150    0.880
    ## SUBSTRUCTURE_COND_0605   -0.05860    2.16212  -0.027    0.978
    ## SUBSTRUCTURE_COND_0606   -0.03266    2.16300  -0.015    0.988
    ## SUBSTRUCTURE_COND_0607   -0.56015    2.16174  -0.259    0.796
    ## SUBSTRUCTURE_COND_0608   -0.76539    2.16250  -0.354    0.723
    ## SUBSTRUCTURE_COND_0609   -1.11171    2.61664  -0.425    0.671
    ## SUBSTRUCTURE_COND_060N   -0.26881    2.85948  -0.094    0.925
    ## 
    ## Residual standard error: 2.644 on 25708 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.01161,    Adjusted R-squared:  0.01061 
    ## F-statistic: 11.61 on 26 and 25708 DF,  p-value: < 2.2e-16

From the result of the regression, we can see the model of the
unemployed rate and the bridge deck condition, superstructure &
substructure can be written as: Unemployedment Rate = 4.10 + (1.38 \*
deck condition) + (-0.64 \* superstructure condition) + (-0.23 \*
substructure condition)

Therefore, once we have the index of deck condition, superstructure
condition and substructure condion of a bridge, we would be able to
predict the unemployment number in this state

Linear model for unemployed number with the factors: bridge deck condition, bridge superstructure and bridge substructure
-------------------------------------------------------------------------------------------------------------------------

We can build up a model of multivariate linear regression of the
unemployed number and the bridge deck condition, superstructure &
substructure.

    lmUnemployed = lm(Unemployed_Level~DECK_COND_058 + SUPERSTRUCTURE_COND_059 + SUBSTRUCTURE_COND_060 , data=data.final)

    summary(lmUnemployed)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ DECK_COND_058 + SUPERSTRUCTURE_COND_059 + 
    ##     SUBSTRUCTURE_COND_060, data = data.final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -129751  -41551  -25217   -5135  222173 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                   293      70174   0.004    0.997
    ## DECK_COND_0582             134827     115040   1.172    0.241
    ## DECK_COND_0583              39412     104641   0.377    0.706
    ## DECK_COND_0584              35506     103333   0.344    0.731
    ## DECK_COND_0585              31485     103376   0.305    0.761
    ## DECK_COND_0586              45197     103371   0.437    0.662
    ## DECK_COND_0587              30226     103365   0.292    0.770
    ## DECK_COND_0588              30913     103374   0.299    0.765
    ## DECK_COND_0589              97483     107842   0.904    0.366
    ## DECK_COND_058N              19171     103361   0.185    0.853
    ## SUPERSTRUCTURE_COND_0592   -47887      75874  -0.631    0.528
    ## SUPERSTRUCTURE_COND_0593   -28046      52683  -0.532    0.594
    ## SUPERSTRUCTURE_COND_0594   -16733      49771  -0.336    0.737
    ## SUPERSTRUCTURE_COND_0595   -13250      49699  -0.267    0.790
    ## SUPERSTRUCTURE_COND_0596     1877      49684   0.038    0.970
    ## SUPERSTRUCTURE_COND_0597     2671      49689   0.054    0.957
    ## SUPERSTRUCTURE_COND_0598     9576      49708   0.193    0.847
    ## SUPERSTRUCTURE_COND_0599    -2356      57721  -0.041    0.967
    ## SUPERSTRUCTURE_COND_059N       NA         NA      NA       NA
    ## SUBSTRUCTURE_COND_0602     -14593      57870  -0.252    0.801
    ## SUBSTRUCTURE_COND_0603      -8051      58893  -0.137    0.891
    ## SUBSTRUCTURE_COND_0604     -11562      57483  -0.201    0.841
    ## SUBSTRUCTURE_COND_0605      -7303      57381  -0.127    0.899
    ## SUBSTRUCTURE_COND_0606      32223      57404   0.561    0.575
    ## SUBSTRUCTURE_COND_0607      16822      57371   0.293    0.769
    ## SUBSTRUCTURE_COND_0608      11546      57391   0.201    0.841
    ## SUBSTRUCTURE_COND_0609     -55091      69444  -0.793    0.428
    ## SUBSTRUCTURE_COND_060N      33050      75889   0.436    0.663
    ## 
    ## Residual standard error: 70170 on 25708 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.03355,    Adjusted R-squared:  0.03257 
    ## F-statistic: 34.33 on 26 and 25708 DF,  p-value: < 2.2e-16

From the result of the regression, we can see the model of the
unemployed rate and the bridge deck condition, superstructure &
substructure can be written as: Unemployedment Number = 293 + (51580 \*
deck condition) + (-11767 \* superstructure condition) + (-379 \*
substructure condition);

Therefore, once we have the index of deck condition, superstructure
condition and substructure condion of a bridge, we would be able to
predict the unemployment number in this state

Linear model for the prediction of next month’s unemployed rate/number: bridge deck condition, bridge superstructure & bridge substructure and time factor
----------------------------------------------------------------------------------------------------------------------------------------------------------

Compared with the previous two models, we need to take the time into
consideration.

So to speak, we can build up a model of multivariate linear regression
of the unemployed number and the bridge deck condition, superstructure &
substructure and also the time components. In the model I added
Year\_Built as a time factor: Unemployed\_Level~DECK\_COND\_058 +
SUPERSTRUCTURE\_COND\_059 + SUBSTRUCTURE\_COND\_060 + YEAR\_BUILT\_027

    lmUnemployed = lm(Unemployed_Level~DECK_COND_058 + SUPERSTRUCTURE_COND_059 + SUBSTRUCTURE_COND_060 + YEAR_BUILT_027, data=data.final)

    summary(lmUnemployed)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ DECK_COND_058 + SUPERSTRUCTURE_COND_059 + 
    ##     SUBSTRUCTURE_COND_060 + YEAR_BUILT_027, data = data.final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -129289  -40754  -24133   -3909  219612 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              389989.13   80724.42   4.831 1.37e-06 ***
    ## DECK_COND_0582           139124.80  114832.25   1.212    0.226    
    ## DECK_COND_0583            37503.49  104451.32   0.359    0.720    
    ## DECK_COND_0584            36338.58  103146.12   0.352    0.725    
    ## DECK_COND_0585            33175.48  103189.39   0.322    0.748    
    ## DECK_COND_0586            46136.67  103183.66   0.447    0.655    
    ## DECK_COND_0587            30849.26  103177.62   0.299    0.765    
    ## DECK_COND_0588            30414.39  103186.98   0.295    0.768    
    ## DECK_COND_0589            92537.27  107648.34   0.860    0.390    
    ## DECK_COND_058N            18879.30  103174.07   0.183    0.855    
    ## SUPERSTRUCTURE_COND_0592 -44667.00   75737.19  -0.590    0.555    
    ## SUPERSTRUCTURE_COND_0593 -24154.59   52589.31  -0.459    0.646    
    ## SUPERSTRUCTURE_COND_0594 -11704.56   49683.39  -0.236    0.814    
    ## SUPERSTRUCTURE_COND_0595  -8327.72   49611.73  -0.168    0.867    
    ## SUPERSTRUCTURE_COND_0596   7370.65   49597.20   0.149    0.882    
    ## SUPERSTRUCTURE_COND_0597   8911.07   49603.21   0.180    0.857    
    ## SUPERSTRUCTURE_COND_0598  16601.81   49623.80   0.335    0.738    
    ## SUPERSTRUCTURE_COND_0599   4415.11   57620.34   0.077    0.939    
    ## SUPERSTRUCTURE_COND_059N        NA         NA      NA       NA    
    ## SUBSTRUCTURE_COND_0602   -22168.38   57770.98  -0.384    0.701    
    ## SUBSTRUCTURE_COND_0603   -14038.13   58789.51  -0.239    0.811    
    ## SUBSTRUCTURE_COND_0604   -17714.49   57382.15  -0.309    0.758    
    ## SUBSTRUCTURE_COND_0605   -11975.25   57279.41  -0.209    0.834    
    ## SUBSTRUCTURE_COND_0606    27864.43   57302.22   0.486    0.627    
    ## SUBSTRUCTURE_COND_0607    14593.86   57267.80   0.255    0.799    
    ## SUBSTRUCTURE_COND_0608    10450.19   57287.56   0.182    0.855    
    ## SUBSTRUCTURE_COND_0609   -47697.77   69322.23  -0.688    0.491    
    ## SUBSTRUCTURE_COND_060N    37694.36   75752.74   0.498    0.619    
    ## YEAR_BUILT_027             -200.15      20.61  -9.713  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 70050 on 25707 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.03708,    Adjusted R-squared:  0.03607 
    ## F-statistic: 36.67 on 27 and 25707 DF,  p-value: < 2.2e-16

From the result of the regression, we can see the model of the
unemployed rate and the bridge deck condition, superstructure &
substructure can be written as: Unemployedment Number = 389989 + (51661
\* deck condition) + (-6444 \* superstructure condition) + (-2554 \*
substructure condition) + (-200 \* year built);

Therefore, once we have the index of deck condition, superstructure
condition and substructure condion of a bridge, we would be able to
predict the unemployment number in this state
