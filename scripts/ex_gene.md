Gene Expression
================
ZDR
2018-09-03

-   [Setup](#setup)
-   [J-L Application](#j-l-application)

Setup
-----

<!-- -------------------------------------------------- -->
The following is a look at the UCI machine learning repository [https://archive.ics.uci.edu/ml/datasets/gene+expression+cancer+RNA-Seq](gene%20expression) database. These data have dimensionality on the order of `20,000`. We will apply randomized dimension reduction -- informed by the Johnson-Lindenstrauss lemma -- to make subsequent analysis more tractable.

First we need to read and wrangle the data. Let's standardize every variable before further analysis -- off-center data will play poorly with projection, and high-value data could lead to overflow.

``` r
df_raw <-
    file_data %>%
    read_csv()
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   X1 = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
mat_data <-
    df_raw %>%
    select(-X1) %>%
    as.matrix(.) %>%
    row_scale(.)
```

Let's check out the raw size of the original data.

``` r
dim(mat_data)
```

    ## [1]   801 20531

We have woefully few observations (801) for the large number of variables (20531) in these data. Let's see if we find a reasonable dimension reduction.

J-L Application
---------------

<!-- -------------------------------------------------- -->
First, let's carry out a Johnson-Lindenstrauss informed projection. A high-dimensional gaussian is nearly orthogonal, so we need only draw from a multivariate gaussian of the appropriate size and normalize.

``` r
## Make reproducible
set.seed(101)
## Calculate dimension
C <- 8                             # Over-samping factor
n <- dim(mat_data)[1]              # Observations
d <- dim(mat_data)[2]              # Dimensionality

k <- C * ceiling(log(n) / eps ^ 2) # J-L dimension
## Random `projection`
P <-
    map_dfc(1:k, function(a) {x = rnorm(d); x / norm(x, type = "2")}) %>%
    as.matrix(.)
## Project the data
mat_proj <-
    mat_data %*% P
## Match the original average distance; Subsample for speed
Ind <- sample(x = 1:n, size = 500)

D_sub_orig <-
    mat_data[Ind, ] %>%
    dist()

D_sub_fix <-
    mat_proj[Ind, ] %>%
    dist()

factor <-
    mean(D_sub_orig, na.rm = TRUE) / mean(D_sub_fix, na.rm = TRUE)

mat_proj <-
    mat_proj * factor
```

Randomly sub-select the distances and check they are close.

``` r
## Re-compute distances with proper scaling
Ind <- sample(x = 1:n, size = 500) # New draw

D_sub_orig <-
    mat_data[Ind, ] %>%
    dist()

D_sub_proj <-
    mat_proj[Ind, ] %>%
    dist()

## Compute quantiles of discrepancy
R_diff <- (D_sub_proj - D_sub_orig) / D_sub_orig
qt <- quantile(R_diff)
qt
```

    ##            0%           25%           50%           75%          100% 
    ## -4.137900e-02 -6.577605e-03 -1.702939e-05  6.694303e-03  4.333542e-02

Our tolerance was 0.1, and the extremes of the distance discrepancy were -0.041379 and 0.0433354. In this case, we found the projection in one shot; in practice, we might have to re-draw until an acceptable projection is found. J-L guarantees we can do this in polynomial time.
