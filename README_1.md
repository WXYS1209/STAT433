**STAT-433 HW1**
================
Xiaoyang Wang
2022-09-26

``` r
library(dplyr)
library(nycflights13)
library(ggplot2)
```

# 1 How many flights have a missing dep_time? What other variables are missing? What might these rows represent?

``` r
mdt = flights %>% filter(is.na(dep_time)) %>% summarise(nn=n())
```

8255 flights have a missing dep_time.

``` r
flights %>% filter(is.na(dep_time)) %>% is.na() %>% apply(2, sum)
```

    ##           year          month            day       dep_time sched_dep_time 
    ##              0              0              0           8255              0 
    ##      dep_delay       arr_time sched_arr_time      arr_delay        carrier 
    ##           8255           8255              0           8255              0 
    ##         flight        tailnum         origin           dest       air_time 
    ##              0           2512              0              0           8255 
    ##       distance           hour         minute      time_hour 
    ##              0              0              0              0

It is clearly that **dep_delay**, **arr_time**, **arr_delay**,
**tailnum**, **air_time** are missing. These rows may represent canceled
flights.

# 2 Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.

After converting, some values of the two new variables(**new_dep_time**
and **new_sched_dep_time**) are as follows:

``` r
flights %>% 
  mutate(new_dep_time = dep_time %% 100 + dep_time %/% 100 * 60,
         new_sched_dep_time = sched_dep_time %% 100 + sched_dep_time %/% 100 * 60) %>% 
  summarise(new_dep_time, new_sched_dep_time) %>% head()
```

    ## # A tibble: 6 × 2
    ##   new_dep_time new_sched_dep_time
    ##          <dbl>              <dbl>
    ## 1          317                315
    ## 2          333                329
    ## 3          342                340
    ## 4          344                345
    ## 5          354                360
    ## 6          354                358

# 3 Look at the number of canceled flights per day. Is there a pattern? Is the proportion of canceled flights related to the average delay? Use multiple dyplr operations, all on one line, concluding with ggplot(aes(x= ,y=)) + geom_point()

We define **is.na(dep_delay)** as canceled filghts.

``` r
library(patchwork)

p1 = flights %>%
  mutate(date = lubridate::make_datetime(year, month, day)) %>%
  group_by(date) %>%
  summarise(canceled = sum(is.na(dep_delay))) %>% 
  ggplot(aes(x = date, y = canceled)) + 
  geom_line() + 
  xlab("Date") + 
  ylab('Canceled Flights')

p2 = flights %>%
  mutate(date = lubridate::make_datetime(year, month, day)) %>%
  group_by(date) %>%
  summarise(canceled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = canceled / n, y = mean_dep_delay)) + 
  geom_point() + 
  xlab("Proportion of Canceled Flights") + 
  ylab('Average Delay (minutes)')

p1+p2
```

![](README_1_files/figure-gfm/Problem%203-1.png)<!-- -->

From the left plot, we can see that there might not exist any pattern
between the number of canceled flights and date. From the right plot, we
can see that there is no obvious relationship between those two
variables. However, as the proportion of canceled flights increases, the
proportion of longer average delay increases generally.
