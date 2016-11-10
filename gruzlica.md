# Gruźlica
Michał Buszkiewicz  
10 listopada 2016  



Wczytywanie biblioteki EDAWR zawierającej zbiór "tb". Pomijanie pomiarów zawierających brakujące dane.

```r
library(EDAWR)
tb_data <- tb[complete.cases(tb), ]
```

Krótkie podsumowanie danych:

```r
knitr::kable(summary(tb_data))
```

       country               year          sex                child             adult           elderly       
---  -----------------  -------------  -----------------  ----------------  ---------------  -----------------
     Length:3380        Min.   :1995   Length:3380        Min.   :    0.0   Min.   :     0   Min.   :     0.0 
     Class :character   1st Qu.:2000   Class :character   1st Qu.:   25.0   1st Qu.:  1130   1st Qu.:    85.0 
     Mode  :character   Median :2005   Mode  :character   Median :   75.0   Median :  2593   Median :   230.0 
     NA                 Mean   :2004   NA                 Mean   :  472.6   Mean   : 10883   Mean   :  1255.5 
     NA                 3rd Qu.:2009   NA                 3rd Qu.:  261.2   3rd Qu.:  6712   3rd Qu.:   642.8 
     NA                 Max.   :2013   NA                 Max.   :25661.0   Max.   :731540   Max.   :125991.0 

Liczba zachorowań z podziałem na płeć:

```r
by_sex <- group_by(tb_data, sex) %>% summarize(sum(child), sum(adult), sum(elderly))
knitr::kable(by_sex)
```



sex       sum(child)   sum(adult)   sum(elderly)
-------  -----------  -----------  -------------
female        827908     13481615        1301076
male          769649     23304306        2942501

Wykres liniowy sumarycznej liczby zachorowań z podziałem na grupę wiekową w kolejnych latach:

```r
sums_by_year = function(dataset, country_choice = NA) {
  filtered_dataset = if (is.na(country_choice))
                       dataset
                     else
                       dataset %>% filter(country == country_choice)
  result = filtered_dataset %>%
           group_by(year) %>%
           summarize(
             child = sum(child),
             adult = sum(adult),
             elderly = sum(elderly)
           )
}

plot_tb_data = function(dataset, country = NA) {
  y_max = (sums_by_year(dataset) %>%
             mutate(year, all = (child + adult + elderly)) %>%
             slice(which.max(all)))$all
  title = if (is.na(country)) "Zachorowania na gruźlicę w grupach wiekowych" else country
  grouped_data = sums_by_year(dataset, country)
  print(length(grouped_data$year))
  matplot(x = grouped_data$year, y = grouped_data %>% select(-year),
          type = c("b"), pch = 1,
          main = title, ylab = "Liczba zachorowań", xlab = "Rok badania",
          ylim = c(0, y_max))
  legend("topleft", legend=c("Dzieci", "Dorośli", "Starsi"), col = 1:3, pch = 1)
}

plot_tb_data(tb_data)
```

```
## [1] 19
```

<img src="gruzlica_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Analogiczne do powyższego wykresy dla poszczególnych krajów:

```r
countries = unique(tb_data$country)
for (i in countries) {
  plot_tb_data(tb_data, i)
}
```

<img src="gruzlica_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-2.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-3.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-4.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-5.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-6.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-7.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-8.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-9.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-10.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-11.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-12.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-13.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-14.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-15.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-16.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-17.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-18.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-19.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-20.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-21.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-22.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-23.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-24.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-25.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-26.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-27.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-28.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-29.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-30.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-31.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-32.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-33.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-34.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-35.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-36.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-37.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-38.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-39.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-40.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-41.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-42.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-43.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-44.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-45.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-46.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-47.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-48.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-49.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-50.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-51.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-52.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-53.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-54.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-55.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-56.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-57.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-58.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-59.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-60.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-61.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-62.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-63.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-64.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-65.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-66.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-67.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-68.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-69.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-70.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-71.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-72.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-73.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-74.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-75.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-76.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-77.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-78.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-79.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-80.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-81.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-82.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-83.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-84.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-85.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-86.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-87.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-88.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-89.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-90.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-91.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-92.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-93.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-94.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-95.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-96.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-97.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-98.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-99.png" style="display: block; margin: auto;" /><img src="gruzlica_files/figure-html/unnamed-chunk-5-100.png" style="display: block; margin: auto;" />
