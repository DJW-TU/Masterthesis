R Notebook
================
Dominik Wulf
08 September, 2022

``` r
#load packages
library("exuber")
library("dplyr")
```

    ## Warning: Paket 'dplyr' wurde unter R Version 4.2.1 erstellt

    ## 
    ## Attache Paket: 'dplyr'

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("ggplot2")
library("tidyr")
library("plotly")
```

    ## 
    ## Attache Paket: 'plotly'

    ## Das folgende Objekt ist maskiert 'package:ggplot2':
    ## 
    ##     last_plot

    ## Das folgende Objekt ist maskiert 'package:stats':
    ## 
    ##     filter

    ## Das folgende Objekt ist maskiert 'package:graphics':
    ## 
    ##     layout

``` r
library("readr")
library("readxl")
```

    ## Warning: Paket 'readxl' wurde unter R Version 4.2.1 erstellt

``` r
library("lubridate")
```

    ## 
    ## Attache Paket: 'lubridate'

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
Art_data <- data.frame(read_csv("../Data/Art/220711_art.csv"))
```

    ## Rows: 1562 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): Number of sales, Sales USD
    ## dttm (1): DateTime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Collectibles
Collectibles_data <- data.frame(read_csv("../Data/Collectibles/220711_collectibles.csv")) 
```

    ## Rows: 1845 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): Number of sales, Sales USD
    ## dttm (1): DateTime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Game
Gaming_data <- data.frame(read_csv("../Data/Gaming/220711_game.csv"))
```

    ## Rows: 1660 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): Number of sales, Sales USD
    ## dttm (1): DateTime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Metaverse
Metaverse_data <- data.frame(read_csv("../Data/Metaverse/220711_Metaverse.csv"))
```

    ## Rows: 1747 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): Number of sales, Sales USD
    ## dttm (1): DateTime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Utility
Utility_data <- data.frame(read_csv("../Data/Utility/220711_utility.csv"))
```

    ## Rows: 1537 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): Number of sales, Sales USD
    ## dttm (1): DateTime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#calculate data --> avg.USD
get_avg_usd <- function(df) {
  df$avg_usd <- ifelse(df[,2]==0, 0, round(df[,3]/df[,2],2))
  return(df)
}

#check dataframe for null values
get_last_zero <- function(df) {
  max_date <- subset(df,avg_usd == 0) %>% tail(n=1)
  if(nrow(max_date) == 0){
    return(NULL)
  } else {
    return(max_date$DateTime[1])
  }
}
```

``` r
lag_t <- 1
significance_lvl <- 95
names <- list("Art","Collectible","Game","Metaverse","Utility")
data <- list(Art_data,Collectibles_data,Gaming_data,Metaverse_data,Utility_data)
```

``` r
data <- lapply(data,get_avg_usd)
date_list <- lapply(data,get_last_zero)
```

``` r
#subset Data to have non null values in dataset
i <- 1
data_sub <- list()

for (x in data){
  
  if(!is.null(date_list[[i]])){
    x <- subset(x, DateTime >= date_list[[i]])
    data_sub[[i]] <- x
    } else{
    data_sub[[i]] <- x
  }
  i <- i + 1
}
```

``` r
#subset data to only carry date and avg_usd column
i <- 1
for (x in data_sub){
  x <- x[-c(2,3)]
  data_sub[[i]] <- x
  i <- i + 1
  }
```

``` r
# get time series info
starting <- list()
ending <- list()

for (x in data_sub){
    s <- min(x$DateTime)
    e <- max(x$DateTime)
    s <- format(s,format="%Y-%m-%d")
    e <- format(e,format="%Y-%m-%d")
    starting <- append(starting,s)
    ending <- append(ending,e)
}

time_frames <- data.frame(unlist(starting), unlist(ending))
```

``` r
#gsadf tests on data with time stamps and summaries
test_results <- lapply(data_sub, radf, lag = lag_t)
```

    ## Using `DateTime` as index variable.
    ## Using `DateTime` as index variable.
    ## Using `DateTime` as index variable.
    ## Using `DateTime` as index variable.
    ## Using `DateTime` as index variable.

``` r
test_summary <- lapply(test_results, summary)
```

    ## Using `exuberdata::radf_crit2` for `cv`.

    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.

``` r
test_datestamps <- lapply(test_results,datestamp,sig_lvl = significance_lvl, lag = lag_t, nonrejected = TRUE)
```

    ## Using `exuberdata::radf_crit2` for `cv`.

    ## Warning: Argument 'min_duration' excludes all explosive periods

    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.

    ## Warning: Argument 'min_duration' excludes all explosive periods

    ## Using `exuberdata::radf_crit2` for `cv`.

``` r
#apply BSADF test on time series
lapply(test_results,autoplot,sig_lvl = significance_lvl, option = "gsadf", nonrejected = TRUE)
```

    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.
    ## Using `exuberdata::radf_crit2` for `cv`.

    ## [[1]]

![](Agg_tests_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## 
    ## [[2]]

![](Agg_tests_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

    ## 
    ## [[3]]

![](Agg_tests_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ## 
    ## [[4]]

![](Agg_tests_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

    ## 
    ## [[5]]

![](Agg_tests_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r
# get number of rows of dataframes to extract critical values from exuber package
n_rows <- lapply(data_sub,nrow)
```

``` r
# get critical value sequences for data from exuber package

cvs <- list()
for (x in n_rows) {
  if (x<600){
    y <- radf_crit[x-lag_t]
    cvs <- append(cvs,y)
    } else{
    y <-exuberdata::radf_crit2[x-lag_t]
    cvs <- append(cvs,y)
  }
}
```

``` r
#calculate minimal window to create date sequence in next step
get_w_m <- function(obs){
  r_0 <- 0.01 + (1.8/sqrt(obs))
  length <- r_0*obs
  return(round(length))
}
```

``` r
# create datesequence for graphs
date_seq_list <- list()
# Subtract minimum window length from start date and create data sequence 
for (x in 1:length(data_sub)) {
  len <- as.difftime(n_rows[[x]], unit="days") - get_w_m(n_rows[[x]]) - lag_t
  date <- tail(data_sub[[x]],n=1)$DateTime - (len - 1)
  date_seq <- seq(date, by = "day", length.out = len)
  date_seq <- list(format(date_seq,foramt = "%Y-%m-%d"))
  date_seq_list <- append(date_seq_list,date_seq)
}
```

``` r
#create dfs for plots
plot_dfs <- list()

for (x in 1:length(data_sub)){
  y <- cbind(date_seq_list[[x]],test_results[[x]]$bsadf,cvs[[x]]$bsadf_cv)
  y <- as.data.frame(y)
  plot_dfs <- append(plot_dfs,list(y))
}
```

    ## Warning in cbind(date_seq_list[[x]], test_results[[x]]$bsadf, cvs[[x]]
    ## $bsadf_cv): number of rows of result is not a multiple of vector length (arg 1)

    ## Warning in cbind(date_seq_list[[x]], test_results[[x]]$bsadf, cvs[[x]]
    ## $bsadf_cv): number of rows of result is not a multiple of vector length (arg 1)

``` r
for (x in 1:length(plot_dfs)) {
  plot_dfs[[x]]$date <- as.Date(plot_dfs[[x]]$V1)
  plot_dfs[[x]] <- subset(plot_dfs[[x]],select = -c(1))
  plot_dfs[[x]][,1:4] <- sapply(plot_dfs[[x]][,1:4], as.numeric)
  colnames(plot_dfs[[x]]) <- c('bsadf','low','middle','high','date')
}
```

``` r
# function to get data into form for plot, standard significance level to 95
plot_prep <- function(n,sig=95) {
  
  x <- 3
  
  if(sig == 90){
    x <- 2
  }else if (sig == 95){
    x <- 3
  }else {
    x <- 4
  }
  
  variable1 <- rep(c("bsadf"),times=nrow(plot_dfs[[n]][1]))
  bsadf_data <- cbind(plot_dfs[[n]][5],plot_dfs[[n]][1],variable1)
  colnames(bsadf_data) <- c('date','value','variable')
  
  variable2 <- rep(c(paste(as.character(sig),"%",sep="")),times=nrow(plot_dfs[[n]][x]))
  cv_data <-cbind(plot_dfs[[n]][5],plot_dfs[[n]][x],variable2)
  colnames(cv_data) <- c('date','value','variable')
  data <- rbind(bsadf_data, cv_data)
  return(data)
}
```

``` r
# apply plot preparation function of previous step to plot dfs
plot_list <- list()
for (x in 1:length(plot_dfs)){
  data <- plot_prep(x,sig = significance_lvl)
  plot_list <- append(plot_list,list(data))
}
```

``` r
line_width <- 0.3

for (x in 1:length(plot_list)){
  start_date <- test_datestamps[[x]]$avg_usd$Start
  end_date <- test_datestamps[[x]]$avg_usd$End
  recession <- data.frame(date_start= as_date(start_date),
                        date_end = as_date(end_date))
  
  
  if (nrow(recession)>0){
  plt <- ggplot()+ 
            geom_line(data = plot_list[[x]], aes(x = date, y = value, color = variable, linetype = variable), size = line_width) +
            ylim(min(plot_list[[x]]$value)-1,max(plot_list[[x]]$value)+1)+
            labs(title=names[x]) +  # title and caption
            scale_colour_manual(label=c('cv','ts'),
                             values = c("#6b2d13", "#135c6b")) +
            scale_linetype_manual(values = c("dotdash","solid")) + 
            guides(linetype = "none") +
            theme(
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.line.x.bottom = element_line(color = 'grey'),
              axis.line.y.left   = element_line(color = 'grey'),
              panel.grid = element_line(color = "#FDEDEC",
                                        size = 0.5,
                                        linetype = 2),
              panel.background = element_rect(fill = "white"),
              legend.title = element_blank()
              )+
            geom_rect(data = recession, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
                    fill = "#6b1713", alpha= 0.3,show.legend = FALSE)
    print(plt)
    ggsave(plt, path = "../Figs", file=paste0("bsadf_", names[x],".png"), width = 14, height = 10, units = "cm")}
  else{
  plt <- ggplot()+ 
            geom_line(data = plot_list[[x]], aes(x = date, y = value, color = variable, linetype = variable), size = line_width) +
            ylim(min(plot_list[[x]]$value)-1,max(plot_list[[x]]$value)+1)+
            labs(title=names[x]) +  # title and caption
            scale_colour_manual(label=c('cv','ts'),
                             values = c("#6b2d13", "#135c6b")) +
            scale_linetype_manual(values = c("dotdash","solid")) + 
            guides(linetype = "none") +
            theme(
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.line.x.bottom = element_line(color = 'grey'),
              axis.line.y.left   = element_line(color = 'grey'),
              panel.grid = element_line(color = "#FDEDEC",
                                        size = 0.5,
                                        linetype = 2),
              panel.background = element_rect(fill = "white"),
              legend.title = element_blank()
              )
    print(plt)
    ggsave(plt, path = "../Figs", file=paste0("bsadf_", names[x],".png"), width = 14, height = 10, units = "cm")}
  }
```

![](Agg_tests_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](Agg_tests_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](Agg_tests_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](Agg_tests_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->![](Agg_tests_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->

``` r
print(test_summary)
```

    ## [[1]]
    ## 
    ## ── Summary (minw = 78, lag = 1) ─────────────────── Monte Carlo (nrep = 2000) ──
    ## 
    ## avg_usd :
    ## # A tibble: 3 × 5
    ##   stat   tstat   `90`   `95`  `99`
    ##   <fct>  <dbl>  <dbl>  <dbl> <dbl>
    ## 1 adf   -7.64  -0.481 -0.122 0.610
    ## 2 sadf   0.291  1.33   1.60  2.10 
    ## 3 gsadf  0.334  2.16   2.35  2.78 
    ## 
    ## 
    ## [[2]]
    ## 
    ## ── Summary (minw = 91, lag = 1) ─────────────────── Monte Carlo (nrep = 2000) ──
    ## 
    ## avg_usd :
    ## # A tibble: 3 × 5
    ##   stat  tstat   `90`   `95`  `99`
    ##   <fct> <dbl>  <dbl>  <dbl> <dbl>
    ## 1 adf   -8.90 -0.397 0.0233 0.780
    ## 2 sadf   7.46  1.34  1.64   2.13 
    ## 3 gsadf  7.46  2.22  2.40   2.89 
    ## 
    ## 
    ## [[3]]
    ## 
    ## ── Summary (minw = 89, lag = 1) ─────────────────── Monte Carlo (nrep = 2000) ──
    ## 
    ## avg_usd :
    ## # A tibble: 3 × 5
    ##   stat   tstat   `90`        `95`  `99`
    ##   <fct>  <dbl>  <dbl>       <dbl> <dbl>
    ## 1 adf   -18.7  -0.363 -0.00000685 0.767
    ## 2 sadf    3.54  1.34   1.62       2.12 
    ## 3 gsadf   4.37  2.22   2.40       2.86 
    ## 
    ## 
    ## [[4]]
    ## 
    ## ── Summary (minw = 76, lag = 1) ─────────────────── Monte Carlo (nrep = 2000) ──
    ## 
    ## avg_usd :
    ## # A tibble: 3 × 5
    ##   stat  tstat   `90`    `95`  `99`
    ##   <fct> <dbl>  <dbl>   <dbl> <dbl>
    ## 1 adf   -7.07 -0.420 -0.0905 0.544
    ## 2 sadf  -3.31  1.33   1.59   2.10 
    ## 3 gsadf -1.02  2.16   2.34   2.77 
    ## 
    ## 
    ## [[5]]
    ## 
    ## ── Summary (minw = 73, lag = 1) ─────────────────── Monte Carlo (nrep = 2000) ──
    ## 
    ## avg_usd :
    ## # A tibble: 3 × 5
    ##   stat   tstat   `90`    `95`  `99`
    ##   <fct>  <dbl>  <dbl>   <dbl> <dbl>
    ## 1 adf   -19.2  -0.440 -0.0884 0.610
    ## 2 sadf   -6.02  1.33   1.59   2.10 
    ## 3 gsadf   1.57  2.15   2.34   2.80

``` r
for (x in 1:length(test_datestamps)){
  print(names[x])
  print(data.frame(test_datestamps[[x]]))
}
```

    ## [[1]]
    ## [1] "Art"
    ## 
    ## Dataframe mit 0 Spalten und 0 Zeilen
    ## [[1]]
    ## [1] "Collectible"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2020-09-14   2020-09-15  2020-09-16                2       negative
    ## 2    2020-09-17   2020-09-17  2020-09-18                1       positive
    ## 3    2020-09-24   2020-09-24  2020-09-25                1       positive
    ## 4    2020-09-28   2020-09-28  2020-09-29                1       positive
    ## 5    2021-02-02   2021-02-04  2021-02-06                4       positive
    ## 6    2021-02-10   2021-02-10  2021-02-11                1       positive
    ## 7    2021-02-19   2021-02-22  2021-02-24                5       positive
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE               FALSE
    ## 2           FALSE               FALSE
    ## 3           FALSE               FALSE
    ## 4           FALSE               FALSE
    ## 5           FALSE               FALSE
    ## 6           FALSE               FALSE
    ## 7           FALSE               FALSE
    ## [[1]]
    ## [1] "Game"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2018-07-28   2018-07-28  2018-07-29                1       positive
    ## 2    2021-02-08   2021-02-09  2021-02-10                2       negative
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE               FALSE
    ## 2           FALSE               FALSE
    ## [[1]]
    ## [1] "Metaverse"
    ## 
    ## Dataframe mit 0 Spalten und 0 Zeilen
    ## [[1]]
    ## [1] "Utility"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2021-01-05   2021-01-05  2021-01-06                1       positive
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE                TRUE

``` r
#function to create result table
rslt <- function(list,tf,nombres) {
  
  len_df <- length(list)
  df_return <- list[[1]]$avg_usd[3,]

  for (x in 2:len_df){
    v1 <- list[[x]]$avg_usd[3,]
    df_return <- rbind(df_return,v1)
  }
  df_return <- cbind(unlist(nombres),tf,df_return)
  return(df_return)
}
```

``` r
result_table <- rslt(test_summary,time_frames,names) 
print(result_table)
```

    ##   unlist(nombres) unlist.starting. unlist.ending.  stat      tstat       90
    ## 1             Art       2018-12-11     2022-07-11 gsadf  0.3340116 2.163665
    ## 2     Collectible       2017-11-18     2022-07-11 gsadf  7.4628612 2.217438
    ## 3            Game       2018-01-07     2022-07-11 gsadf  4.3739536 2.217188
    ## 4       Metaverse       2019-02-09     2022-07-11 gsadf -1.0185364 2.156790
    ## 5         Utility       2019-04-09     2022-07-11 gsadf  1.5694687 2.149331
    ##         95       99
    ## 1 2.352523 2.775672
    ## 2 2.404235 2.893892
    ## 3 2.404121 2.861435
    ## 4 2.337234 2.765171
    ## 5 2.337234 2.798748

``` r
for (x in 1:length(test_datestamps)){
  print(names[x])
  print(data.frame(test_datestamps[[x]]))
}
```

    ## [[1]]
    ## [1] "Art"
    ## 
    ## Dataframe mit 0 Spalten und 0 Zeilen
    ## [[1]]
    ## [1] "Collectible"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2020-09-14   2020-09-15  2020-09-16                2       negative
    ## 2    2020-09-17   2020-09-17  2020-09-18                1       positive
    ## 3    2020-09-24   2020-09-24  2020-09-25                1       positive
    ## 4    2020-09-28   2020-09-28  2020-09-29                1       positive
    ## 5    2021-02-02   2021-02-04  2021-02-06                4       positive
    ## 6    2021-02-10   2021-02-10  2021-02-11                1       positive
    ## 7    2021-02-19   2021-02-22  2021-02-24                5       positive
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE               FALSE
    ## 2           FALSE               FALSE
    ## 3           FALSE               FALSE
    ## 4           FALSE               FALSE
    ## 5           FALSE               FALSE
    ## 6           FALSE               FALSE
    ## 7           FALSE               FALSE
    ## [[1]]
    ## [1] "Game"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2018-07-28   2018-07-28  2018-07-29                1       positive
    ## 2    2021-02-08   2021-02-09  2021-02-10                2       negative
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE               FALSE
    ## 2           FALSE               FALSE
    ## [[1]]
    ## [1] "Metaverse"
    ## 
    ## Dataframe mit 0 Spalten und 0 Zeilen
    ## [[1]]
    ## [1] "Utility"
    ## 
    ##   avg_usd.Start avg_usd.Peak avg_usd.End avg_usd.Duration avg_usd.Signal
    ## 1    2021-01-05   2021-01-05  2021-01-06                1       positive
    ##   avg_usd.Ongoing avg_usd.Nonrejected
    ## 1           FALSE                TRUE
