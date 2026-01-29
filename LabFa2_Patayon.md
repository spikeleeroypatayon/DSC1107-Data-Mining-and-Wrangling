LabFa2_Patayon
================
PATAYON, SPIKE LEE-ROY V
2026-01-29

# Laboratory FA2 - Data Mining and Wrangling

**Setup**

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data("starwars", package = "dplyr")
```

## **Q1 — Dataset Inspection and Missing Values (5 points)**

1.  Display the structure of the dataset using `glimpse(starwars)`.

    ``` r
    glimpse(starwars)
    ```

        ## Rows: 87
        ## Columns: 14
        ## $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
        ## $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
        ## $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
        ## $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
        ## $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
        ## $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
        ## $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
        ## $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
        ## $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
        ## $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
        ## $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
        ## $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
        ## $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
        ## $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

    ``` r
    head(starwars)
    ```

        ## # A tibble: 6 × 14
        ##   name      height  mass hair_color skin_color eye_color birth_year sex   gender
        ##   <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
        ## 1 Luke Sky…    172    77 blond      fair       blue            19   male  mascu…
        ## 2 C-3PO        167    75 <NA>       gold       yellow         112   none  mascu…
        ## 3 R2-D2         96    32 <NA>       white, bl… red             33   none  mascu…
        ## 4 Darth Va…    202   136 none       white      yellow          41.9 male  mascu…
        ## 5 Leia Org…    150    49 brown      light      brown           19   fema… femin…
        ## 6 Owen Lars    178   120 brown, gr… light      blue            52   male  mascu…
        ## # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
        ## #   vehicles <list>, starships <list>

2.  Identify:

    - number of observations: **87**

    - number of variables: **14**

3.  Compute the number of missing values in `height`, `mass`, and
    `homeworld`.

``` r
starwars %>%
  summarise(
    missing_height = sum(is.na(height)),
    missing_mass = sum(is.na(mass)),
    missing_homeworld = sum(is.na(homeworld))
  )
```

    ## # A tibble: 1 × 3
    ##   missing_height missing_mass missing_homeworld
    ##            <int>        <int>             <int>
    ## 1              6           28                10

## **Q2 — Create a Wide Summary Table (`pivot_wider`) (7 points)**

1.  Filter the dataset to include only characters with **non-missing**
    `species`.

2.  Group the data by `species` and `gender`.

3.  Compute the **mean height** for each group.

4.  Use `pivot_wider()` so that:

    - rows represent `species`

    - columns represent `gender`

    - values are mean height

Display the resulting table.

``` r
starwars_wide_summary <- starwars %>%
  filter(!is.na(species)) %>%
  group_by(species, gender) %>%
  summarise(mean_height = mean(height, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = gender, 
    values_from = mean_height
  )
print(starwars_wide_summary)
```

    ## # A tibble: 37 × 3
    ##    species   masculine feminine
    ##    <chr>         <dbl>    <dbl>
    ##  1 Aleena          79        NA
    ##  2 Besalisk       198        NA
    ##  3 Cerean         198        NA
    ##  4 Chagrian       196        NA
    ##  5 Clawdite        NA       168
    ##  6 Droid          140        96
    ##  7 Dug            112        NA
    ##  8 Ewok            88        NA
    ##  9 Geonosian      183        NA
    ## 10 Gungan         209.       NA
    ## # ℹ 27 more rows

## **Q3 — Convert Wide Table Back to Long Format (`pivot_longer`) (6 points)**

Using the table created in **Question 2**:

1.  Apply `pivot_longer()` to convert the gender columns back into:

    - a column named `gender`

    - a column named `mean_height`

2.  Remove rows with missing `mean_height`.

Display the resulting tidy dataset.

``` r
starwars_long_format <- starwars_wide_summary %>%
  pivot_longer(
    cols = -species,
    names_to = "gender",
    values_to = "mean_height"
  ) %>%
  filter(!is.na(mean_height))

print(starwars_long_format)
```

    ## # A tibble: 41 × 3
    ##    species   gender    mean_height
    ##    <chr>     <chr>           <dbl>
    ##  1 Aleena    masculine          79
    ##  2 Besalisk  masculine         198
    ##  3 Cerean    masculine         198
    ##  4 Chagrian  masculine         196
    ##  5 Clawdite  feminine          168
    ##  6 Droid     masculine         140
    ##  7 Droid     feminine           96
    ##  8 Dug       masculine         112
    ##  9 Ewok      masculine          88
    ## 10 Geonosian masculine         183
    ## # ℹ 31 more rows

## **Q4 — Create New Variables and Handle Missing Data (6 points)**

1.  From the original dataset, create a new variable:

    ​![](images/clipboard-4101865866.png)

2.  Categorize height into:

    - `short` (\<170 cm)

    - `average` (170–189 cm)

    - `tall` (≥190 cm)

3.  Replace missing `homeworld` values with `"Unknown"`.

Show `glimpse()` of the resulting dataset.

``` r
starwars_updated <- starwars %>%
  mutate(
    BMI = mass/(height/100)^2,
    height_cat = case_when(
      height < 170 ~ "Short",
      height >= 170 & height < 190 ~ "Average",
      height >= 190 ~ "Tall",
      TRUE ~ NA_character_
    ),
    homeworld = replace_na(homeworld, "Unknown")
  )

glimpse(starwars_updated)
```

    ## Rows: 87
    ## Columns: 16
    ## $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    ## $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    ## $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    ## $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    ## $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    ## $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    ## $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    ## $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    ## $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    ## $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    ## $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    ## $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    ## $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    ## $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…
    ## $ BMI        <dbl> 26.02758, 26.89232, 34.72222, 33.33007, 21.77778, 37.87401,…
    ## $ height_cat <chr> "Average", "Short", "Short", "Tall", "Short", "Average", "S…

``` r
starwars_updated
```

    ## # A tibble: 87 × 16
    ##    name     height  mass hair_color skin_color eye_color birth_year sex   gender
    ##    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
    ##  1 Luke Sk…    172    77 blond      fair       blue            19   male  mascu…
    ##  2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu…
    ##  3 R2-D2        96    32 <NA>       white, bl… red             33   none  mascu…
    ##  4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
    ##  5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
    ##  6 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
    ##  7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
    ##  8 R5-D4        97    32 <NA>       white, red red             NA   none  mascu…
    ##  9 Biggs D…    183    84 black      light      brown           24   male  mascu…
    ## 10 Obi-Wan…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
    ## # ℹ 77 more rows
    ## # ℹ 7 more variables: homeworld <chr>, species <chr>, films <list>,
    ## #   vehicles <list>, starships <list>, BMI <dbl>, height_cat <chr>

## **Q5 — Unnesting and Interpretation (6 points)**

1.  Select `name` and `films`.

2.  Convert the `films` list-column into long format.

3.  Count the number of film appearances per character.

4.  Display the **top 8 characters** with the most appearances.

5.  In **2–3 sentences**, explain why this data must be converted to
    long format.

``` r
top_characters <- starwars %>%
  select(name, films) %>%
  unnest(films) %>%
  count(name, sort = TRUE) %>%
  slice_head(n = 8)

print(top_characters)
```

    ## # A tibble: 8 × 2
    ##   name               n
    ##   <chr>          <int>
    ## 1 R2-D2              7
    ## 2 C-3PO              6
    ## 3 Obi-Wan Kenobi     6
    ## 4 Chewbacca          5
    ## 5 Leia Organa        5
    ## 6 Luke Skywalker     5
    ## 7 Palpatine          5
    ## 8 Yoda               5

### Why convert to long format?

The `films` column is initially a **nested list**, where multiple film
titles are stored inside a single cell for each character. We must
convert this to a **long format** (one row per film per character)
because standard R functions like `count()` or `group_by()` cannot see
inside list-structures; unnesting treats each appearance as an
individual observation that can be mathematically summed.
