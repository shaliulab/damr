# `damr` [![Travis-CI Build Status](https://travis-ci.org/rethomics/damr.svg?branch=master)](https://travis-ci.org/rethomics/damr)[![Coverage Status](https://img.shields.io/codecov/c/github/rethomics/damr/master.svg)](https://codecov.io/github/damr/behavr?branch=master)

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/hms?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/hms)  -->

<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/hms/master.svg)](https://codecov.io/github/tidyverse/hms?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hms)](https://cran.r-project.org/package=hms) -->

`damr` is part of the [rethomics framework](todo.html).
This README is a short explanation of the basics of `damr`.
A [comprehensive documentation](todo.html) of rethomics is also available.

## Drosophila Activity Monitor
todo, say something about what DAMS is
 


## Installation


```r
library(devtools)
install_github("rethomics/behavr")
install_github("rethomics/damr")
```

## A closer look at a query

When running experiments with DAM2, you will end up with a result directory (folder) that contains one file per monitor. Each file stores the data for up to 32 animals at once.
Each animal may have a different set of metadata (e.g. sex, genotype, treatment,...).

In the [rethomics framework](todo.html), the priviledged way to load data is to write a query.
That is, a table where each row is an animal, and each condition (metadata) is a column.
Read more about queries [here](todo2.html)

In practice, you would write your query in excel or similar software, save it as a CSV, and then load it with `read.csv()`, or `fread` (in the `data.table` package).

`damr` comes with a couple of toy queries and DAM2 files we can play with.


```r
library(behavr)
data("two_files_query")
print(two_files_query)
```

```
##        file      start_datetime stop_datetime region_id condition genotype
## 1  M064.txt 2017-07-01 10:00:00    2017-07-07         1         a        A
## 2  M064.txt 2017-07-01 10:00:00    2017-07-07         2         a        A
## 3  M064.txt 2017-07-01 10:00:00    2017-07-07         3         a        B
## 4  M064.txt 2017-07-01 10:00:00    2017-07-07         4         a        B
## 5  M064.txt 2017-07-01 10:00:00    2017-07-07         5         a        A
## 6  M064.txt 2017-07-01 10:00:00    2017-07-07         6         a        A
## 7  M064.txt 2017-07-01 10:00:00    2017-07-07         7         a        B
## 8  M064.txt 2017-07-01 10:00:00    2017-07-07         8         a        B
## 9  M064.txt 2017-07-01 10:00:00    2017-07-07         9         a        A
## 10 M064.txt 2017-07-01 10:00:00    2017-07-07        10         a        A
## 11 M064.txt 2017-07-01 10:00:00    2017-07-07        11         a        B
## 12 M064.txt 2017-07-01 10:00:00    2017-07-07        12         a        B
## 13 M064.txt 2017-07-01 10:00:00    2017-07-07        13         a        A
## 14 M064.txt 2017-07-01 10:00:00    2017-07-07        14         a        A
## 15 M064.txt 2017-07-01 10:00:00    2017-07-07        15         a        B
## 16 M064.txt 2017-07-01 10:00:00    2017-07-07        16         a        B
## 17 M064.txt 2017-07-01 10:00:00    2017-07-07        17         b        A
## 18 M064.txt 2017-07-01 10:00:00    2017-07-07        18         b        A
## 19 M064.txt 2017-07-01 10:00:00    2017-07-07        19         b        B
## 20 M064.txt 2017-07-01 10:00:00    2017-07-07        20         b        B
## 21 M064.txt 2017-07-01 10:00:00    2017-07-07        21         b        A
## 22 M064.txt 2017-07-01 10:00:00    2017-07-07        22         b        A
## 23 M064.txt 2017-07-01 10:00:00    2017-07-07        23         b        B
## 24 M064.txt 2017-07-01 10:00:00    2017-07-07        24         b        B
## 25 M064.txt 2017-07-01 10:00:00    2017-07-07        25         b        A
## 26 M064.txt 2017-07-01 10:00:00    2017-07-07        26         b        A
## 27 M064.txt 2017-07-01 10:00:00    2017-07-07        27         b        B
## 28 M064.txt 2017-07-01 10:00:00    2017-07-07        28         b        B
## 29 M064.txt 2017-07-01 10:00:00    2017-07-07        29         b        A
## 30 M064.txt 2017-07-01 10:00:00    2017-07-07        30         b        A
## 31 M064.txt 2017-07-01 10:00:00    2017-07-07        31         b        B
## 32 M064.txt 2017-07-01 10:00:00    2017-07-07        32         b        B
## 33 M014.txt 2017-07-01 10:00:00    2017-07-07         1         a        A
## 34 M014.txt 2017-07-01 10:00:00    2017-07-07         2         a        A
## 35 M014.txt 2017-07-01 10:00:00    2017-07-07         3         a        B
## 36 M014.txt 2017-07-01 10:00:00    2017-07-07         4         a        B
## 37 M014.txt 2017-07-01 10:00:00    2017-07-07         5         a        A
## 38 M014.txt 2017-07-01 10:00:00    2017-07-07         6         a        A
## 39 M014.txt 2017-07-01 10:00:00    2017-07-07         7         a        B
## 40 M014.txt 2017-07-01 10:00:00    2017-07-07         8         a        B
## 41 M014.txt 2017-07-01 10:00:00    2017-07-07         9         a        A
## 42 M014.txt 2017-07-01 10:00:00    2017-07-07        10         a        A
## 43 M014.txt 2017-07-01 10:00:00    2017-07-07        11         a        B
## 44 M014.txt 2017-07-01 10:00:00    2017-07-07        12         a        B
## 45 M014.txt 2017-07-01 10:00:00    2017-07-07        13         a        A
## 46 M014.txt 2017-07-01 10:00:00    2017-07-07        14         a        A
## 47 M014.txt 2017-07-01 10:00:00    2017-07-07        15         a        B
## 48 M014.txt 2017-07-01 10:00:00    2017-07-07        16         a        B
## 49 M014.txt 2017-07-01 10:00:00    2017-07-07        17         b        A
## 50 M014.txt 2017-07-01 10:00:00    2017-07-07        18         b        A
## 51 M014.txt 2017-07-01 10:00:00    2017-07-07        19         b        B
## 52 M014.txt 2017-07-01 10:00:00    2017-07-07        20         b        B
## 53 M014.txt 2017-07-01 10:00:00    2017-07-07        21         b        A
## 54 M014.txt 2017-07-01 10:00:00    2017-07-07        22         b        A
## 55 M014.txt 2017-07-01 10:00:00    2017-07-07        23         b        B
## 56 M014.txt 2017-07-01 10:00:00    2017-07-07        24         b        B
## 57 M014.txt 2017-07-01 10:00:00    2017-07-07        25         b        A
## 58 M014.txt 2017-07-01 10:00:00    2017-07-07        26         b        A
## 59 M014.txt 2017-07-01 10:00:00    2017-07-07        27         b        B
## 60 M014.txt 2017-07-01 10:00:00    2017-07-07        28         b        B
## 61 M014.txt 2017-07-01 10:00:00    2017-07-07        29         b        A
## 62 M014.txt 2017-07-01 10:00:00    2017-07-07        30         b        A
## 63 M014.txt 2017-07-01 10:00:00    2017-07-07        31         b        B
## 64 M014.txt 2017-07-01 10:00:00    2017-07-07        32         b        B
```

Lets stop for a second and look at how our example query is formated.
`two_files_query` is a query with 64 animals in two files (`M064.txt` and `M014.txt`).
As you can see, each animal is defined by:

* a `file` which is the name of the file (monitor) this animal was recorded in
* a `start_datetime`, when your experiment starts (t0 and ZT0)
* a `stop_time`, when your experiment stops
* a `region_id`, in which channel your animal is (1:32)
* condition, genotype, ... that are biologically relevant **meta variables**

Importanlty, the name of the first four columns is *decided for you*, but the next (condition, genotype,...) are not. In fact, *you can define yourself any number and type* (numbers, dates,...) of meta variables!
In fact, in the [rethomics framework](todo.html), you will probably analyse hundreads of animals, from several replicates, in one go from the same query.


```r
library(damr)
data("two_files_query")
print(two_files_query)
```

```
##        file      start_datetime stop_datetime region_id condition genotype
## 1  M064.txt 2017-07-01 10:00:00    2017-07-07         1         a        A
## 2  M064.txt 2017-07-01 10:00:00    2017-07-07         2         a        A
## 3  M064.txt 2017-07-01 10:00:00    2017-07-07         3         a        B
## 4  M064.txt 2017-07-01 10:00:00    2017-07-07         4         a        B
## 5  M064.txt 2017-07-01 10:00:00    2017-07-07         5         a        A
## 6  M064.txt 2017-07-01 10:00:00    2017-07-07         6         a        A
## 7  M064.txt 2017-07-01 10:00:00    2017-07-07         7         a        B
## 8  M064.txt 2017-07-01 10:00:00    2017-07-07         8         a        B
## 9  M064.txt 2017-07-01 10:00:00    2017-07-07         9         a        A
## 10 M064.txt 2017-07-01 10:00:00    2017-07-07        10         a        A
## 11 M064.txt 2017-07-01 10:00:00    2017-07-07        11         a        B
## 12 M064.txt 2017-07-01 10:00:00    2017-07-07        12         a        B
## 13 M064.txt 2017-07-01 10:00:00    2017-07-07        13         a        A
## 14 M064.txt 2017-07-01 10:00:00    2017-07-07        14         a        A
## 15 M064.txt 2017-07-01 10:00:00    2017-07-07        15         a        B
## 16 M064.txt 2017-07-01 10:00:00    2017-07-07        16         a        B
## 17 M064.txt 2017-07-01 10:00:00    2017-07-07        17         b        A
## 18 M064.txt 2017-07-01 10:00:00    2017-07-07        18         b        A
## 19 M064.txt 2017-07-01 10:00:00    2017-07-07        19         b        B
## 20 M064.txt 2017-07-01 10:00:00    2017-07-07        20         b        B
## 21 M064.txt 2017-07-01 10:00:00    2017-07-07        21         b        A
## 22 M064.txt 2017-07-01 10:00:00    2017-07-07        22         b        A
## 23 M064.txt 2017-07-01 10:00:00    2017-07-07        23         b        B
## 24 M064.txt 2017-07-01 10:00:00    2017-07-07        24         b        B
## 25 M064.txt 2017-07-01 10:00:00    2017-07-07        25         b        A
## 26 M064.txt 2017-07-01 10:00:00    2017-07-07        26         b        A
## 27 M064.txt 2017-07-01 10:00:00    2017-07-07        27         b        B
## 28 M064.txt 2017-07-01 10:00:00    2017-07-07        28         b        B
## 29 M064.txt 2017-07-01 10:00:00    2017-07-07        29         b        A
## 30 M064.txt 2017-07-01 10:00:00    2017-07-07        30         b        A
## 31 M064.txt 2017-07-01 10:00:00    2017-07-07        31         b        B
## 32 M064.txt 2017-07-01 10:00:00    2017-07-07        32         b        B
## 33 M014.txt 2017-07-01 10:00:00    2017-07-07         1         a        A
## 34 M014.txt 2017-07-01 10:00:00    2017-07-07         2         a        A
## 35 M014.txt 2017-07-01 10:00:00    2017-07-07         3         a        B
## 36 M014.txt 2017-07-01 10:00:00    2017-07-07         4         a        B
## 37 M014.txt 2017-07-01 10:00:00    2017-07-07         5         a        A
## 38 M014.txt 2017-07-01 10:00:00    2017-07-07         6         a        A
## 39 M014.txt 2017-07-01 10:00:00    2017-07-07         7         a        B
## 40 M014.txt 2017-07-01 10:00:00    2017-07-07         8         a        B
## 41 M014.txt 2017-07-01 10:00:00    2017-07-07         9         a        A
## 42 M014.txt 2017-07-01 10:00:00    2017-07-07        10         a        A
## 43 M014.txt 2017-07-01 10:00:00    2017-07-07        11         a        B
## 44 M014.txt 2017-07-01 10:00:00    2017-07-07        12         a        B
## 45 M014.txt 2017-07-01 10:00:00    2017-07-07        13         a        A
## 46 M014.txt 2017-07-01 10:00:00    2017-07-07        14         a        A
## 47 M014.txt 2017-07-01 10:00:00    2017-07-07        15         a        B
## 48 M014.txt 2017-07-01 10:00:00    2017-07-07        16         a        B
## 49 M014.txt 2017-07-01 10:00:00    2017-07-07        17         b        A
## 50 M014.txt 2017-07-01 10:00:00    2017-07-07        18         b        A
## 51 M014.txt 2017-07-01 10:00:00    2017-07-07        19         b        B
## 52 M014.txt 2017-07-01 10:00:00    2017-07-07        20         b        B
## 53 M014.txt 2017-07-01 10:00:00    2017-07-07        21         b        A
## 54 M014.txt 2017-07-01 10:00:00    2017-07-07        22         b        A
## 55 M014.txt 2017-07-01 10:00:00    2017-07-07        23         b        B
## 56 M014.txt 2017-07-01 10:00:00    2017-07-07        24         b        B
## 57 M014.txt 2017-07-01 10:00:00    2017-07-07        25         b        A
## 58 M014.txt 2017-07-01 10:00:00    2017-07-07        26         b        A
## 59 M014.txt 2017-07-01 10:00:00    2017-07-07        27         b        B
## 60 M014.txt 2017-07-01 10:00:00    2017-07-07        28         b        B
## 61 M014.txt 2017-07-01 10:00:00    2017-07-07        29         b        A
## 62 M014.txt 2017-07-01 10:00:00    2017-07-07        30         b        A
## 63 M014.txt 2017-07-01 10:00:00    2017-07-07        31         b        B
## 64 M014.txt 2017-07-01 10:00:00    2017-07-07        32         b        B
```


## Importing DAM2 data into R

In real life, you will have your data in a folder (i.e. directory) such as `C:/A/random/folder`.
But this would be different for each user.
So that the example works for everyone, we can use `damr_example_dir()` 
to locate the folder where I put some data for you.


```r
result_dir <- damr_example_dir()
print(result_dir)
```

```
## [1] "/home/travis/R/Library/damr/extdata"
```

Then, the magic happens when we use the `query_dam2()` function:


```r
dt <- query_dam2(result_dir, two_files_query)
```

```
## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed

## Warning in read_tokens_(data, tokenizer, col_specs, col_names, locale_, :
## length of NULL cannot be changed
```

```r
print(dt)
```

```
## 
##  ==== METADATA ====
## 
##                                  id region_id                experiment_id
##                              <char>     <int>                       <char>
##  1: 01|2017-07-01 10:00:00|M014.txt         1 2017-07-01 10:00:00|M014.txt
##  2: 01|2017-07-01 10:00:00|M064.txt         1 2017-07-01 10:00:00|M064.txt
##  3: 02|2017-07-01 10:00:00|M014.txt         2 2017-07-01 10:00:00|M014.txt
##  4: 02|2017-07-01 10:00:00|M064.txt         2 2017-07-01 10:00:00|M064.txt
##  5: 03|2017-07-01 10:00:00|M014.txt         3 2017-07-01 10:00:00|M014.txt
##  6: 03|2017-07-01 10:00:00|M064.txt         3 2017-07-01 10:00:00|M064.txt
##  7: 04|2017-07-01 10:00:00|M014.txt         4 2017-07-01 10:00:00|M014.txt
##  8: 04|2017-07-01 10:00:00|M064.txt         4 2017-07-01 10:00:00|M064.txt
##  9: 05|2017-07-01 10:00:00|M014.txt         5 2017-07-01 10:00:00|M014.txt
## 10: 05|2017-07-01 10:00:00|M064.txt         5 2017-07-01 10:00:00|M064.txt
## 11: 06|2017-07-01 10:00:00|M014.txt         6 2017-07-01 10:00:00|M014.txt
## 12: 06|2017-07-01 10:00:00|M064.txt         6 2017-07-01 10:00:00|M064.txt
## 13: 07|2017-07-01 10:00:00|M014.txt         7 2017-07-01 10:00:00|M014.txt
## 14: 07|2017-07-01 10:00:00|M064.txt         7 2017-07-01 10:00:00|M064.txt
## 15: 08|2017-07-01 10:00:00|M014.txt         8 2017-07-01 10:00:00|M014.txt
## 16: 08|2017-07-01 10:00:00|M064.txt         8 2017-07-01 10:00:00|M064.txt
## 17: 09|2017-07-01 10:00:00|M014.txt         9 2017-07-01 10:00:00|M014.txt
## 18: 09|2017-07-01 10:00:00|M064.txt         9 2017-07-01 10:00:00|M064.txt
## 19: 10|2017-07-01 10:00:00|M014.txt        10 2017-07-01 10:00:00|M014.txt
## 20: 10|2017-07-01 10:00:00|M064.txt        10 2017-07-01 10:00:00|M064.txt
## 21: 11|2017-07-01 10:00:00|M014.txt        11 2017-07-01 10:00:00|M014.txt
## 22: 11|2017-07-01 10:00:00|M064.txt        11 2017-07-01 10:00:00|M064.txt
## 23: 12|2017-07-01 10:00:00|M014.txt        12 2017-07-01 10:00:00|M014.txt
## 24: 12|2017-07-01 10:00:00|M064.txt        12 2017-07-01 10:00:00|M064.txt
## 25: 13|2017-07-01 10:00:00|M014.txt        13 2017-07-01 10:00:00|M014.txt
## 26: 13|2017-07-01 10:00:00|M064.txt        13 2017-07-01 10:00:00|M064.txt
## 27: 14|2017-07-01 10:00:00|M014.txt        14 2017-07-01 10:00:00|M014.txt
## 28: 14|2017-07-01 10:00:00|M064.txt        14 2017-07-01 10:00:00|M064.txt
## 29: 15|2017-07-01 10:00:00|M014.txt        15 2017-07-01 10:00:00|M014.txt
## 30: 15|2017-07-01 10:00:00|M064.txt        15 2017-07-01 10:00:00|M064.txt
## 31: 16|2017-07-01 10:00:00|M014.txt        16 2017-07-01 10:00:00|M014.txt
## 32: 16|2017-07-01 10:00:00|M064.txt        16 2017-07-01 10:00:00|M064.txt
## 33: 17|2017-07-01 10:00:00|M014.txt        17 2017-07-01 10:00:00|M014.txt
## 34: 17|2017-07-01 10:00:00|M064.txt        17 2017-07-01 10:00:00|M064.txt
## 35: 18|2017-07-01 10:00:00|M014.txt        18 2017-07-01 10:00:00|M014.txt
## 36: 18|2017-07-01 10:00:00|M064.txt        18 2017-07-01 10:00:00|M064.txt
## 37: 19|2017-07-01 10:00:00|M014.txt        19 2017-07-01 10:00:00|M014.txt
## 38: 19|2017-07-01 10:00:00|M064.txt        19 2017-07-01 10:00:00|M064.txt
## 39: 20|2017-07-01 10:00:00|M014.txt        20 2017-07-01 10:00:00|M014.txt
## 40: 20|2017-07-01 10:00:00|M064.txt        20 2017-07-01 10:00:00|M064.txt
## 41: 21|2017-07-01 10:00:00|M014.txt        21 2017-07-01 10:00:00|M014.txt
## 42: 21|2017-07-01 10:00:00|M064.txt        21 2017-07-01 10:00:00|M064.txt
## 43: 22|2017-07-01 10:00:00|M014.txt        22 2017-07-01 10:00:00|M014.txt
## 44: 22|2017-07-01 10:00:00|M064.txt        22 2017-07-01 10:00:00|M064.txt
## 45: 23|2017-07-01 10:00:00|M014.txt        23 2017-07-01 10:00:00|M014.txt
## 46: 23|2017-07-01 10:00:00|M064.txt        23 2017-07-01 10:00:00|M064.txt
## 47: 24|2017-07-01 10:00:00|M014.txt        24 2017-07-01 10:00:00|M014.txt
## 48: 24|2017-07-01 10:00:00|M064.txt        24 2017-07-01 10:00:00|M064.txt
## 49: 25|2017-07-01 10:00:00|M014.txt        25 2017-07-01 10:00:00|M014.txt
## 50: 25|2017-07-01 10:00:00|M064.txt        25 2017-07-01 10:00:00|M064.txt
## 51: 26|2017-07-01 10:00:00|M014.txt        26 2017-07-01 10:00:00|M014.txt
## 52: 26|2017-07-01 10:00:00|M064.txt        26 2017-07-01 10:00:00|M064.txt
## 53: 27|2017-07-01 10:00:00|M014.txt        27 2017-07-01 10:00:00|M014.txt
## 54: 27|2017-07-01 10:00:00|M064.txt        27 2017-07-01 10:00:00|M064.txt
## 55: 28|2017-07-01 10:00:00|M014.txt        28 2017-07-01 10:00:00|M014.txt
## 56: 28|2017-07-01 10:00:00|M064.txt        28 2017-07-01 10:00:00|M064.txt
## 57: 29|2017-07-01 10:00:00|M014.txt        29 2017-07-01 10:00:00|M014.txt
## 58: 29|2017-07-01 10:00:00|M064.txt        29 2017-07-01 10:00:00|M064.txt
## 59: 30|2017-07-01 10:00:00|M014.txt        30 2017-07-01 10:00:00|M014.txt
## 60: 30|2017-07-01 10:00:00|M064.txt        30 2017-07-01 10:00:00|M064.txt
## 61: 31|2017-07-01 10:00:00|M014.txt        31 2017-07-01 10:00:00|M014.txt
## 62: 31|2017-07-01 10:00:00|M064.txt        31 2017-07-01 10:00:00|M064.txt
## 63: 32|2017-07-01 10:00:00|M014.txt        32 2017-07-01 10:00:00|M014.txt
## 64: 32|2017-07-01 10:00:00|M064.txt        32 2017-07-01 10:00:00|M064.txt
##                                  id region_id                experiment_id
##          start_datetime     file stop_datetime condition genotype
##                  <POSc>   <char>        <fctr>    <fctr>   <fctr>
##  1: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
##  2: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
##  3: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
##  4: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
##  5: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
##  6: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
##  7: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
##  8: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
##  9: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 10: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 11: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 12: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 13: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 14: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 15: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 16: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 17: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 18: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 19: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 20: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 21: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 22: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 23: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 24: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 25: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 26: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 27: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 28: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 29: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 30: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 31: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 32: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 33: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 34: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 35: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 36: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 37: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 38: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 39: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 40: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 41: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 42: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 43: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 44: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 45: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 46: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 47: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 48: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 49: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 50: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 51: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 52: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 53: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 54: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 55: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 56: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 57: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 58: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 59: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 60: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 61: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 62: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 63: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 64: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
##          start_datetime     file stop_datetime condition genotype
## 
##  ====== DATA ======
## 
##                                      id        t activity
##                                  <char>    <hms>    <int>
##      1: 01|2017-07-01 10:00:00|M014.txt 00:00:00        2
##      2: 01|2017-07-01 10:00:00|M014.txt 00:01:00        3
##      3: 01|2017-07-01 10:00:00|M014.txt 00:02:00        1
##      4: 01|2017-07-01 10:00:00|M014.txt 00:03:00        2
##     ---                                                  
## 146428: 32|2017-07-01 10:00:00|M064.txt 38:01:00        0
## 146429: 32|2017-07-01 10:00:00|M064.txt 38:02:00        0
## 146430: 32|2017-07-01 10:00:00|M064.txt 38:03:00        0
## 146431: 32|2017-07-01 10:00:00|M064.txt 38:04:00        0
## 146432: 32|2017-07-01 10:00:00|M064.txt 38:05:00        2
```
`dt`, is a [behavr table](https://github.com/rethomics/behavr).

We can have a look at the metadata:


```r
dt[meta=T]
```

```
##                                  id region_id                experiment_id
##  1: 01|2017-07-01 10:00:00|M014.txt         1 2017-07-01 10:00:00|M014.txt
##  2: 01|2017-07-01 10:00:00|M064.txt         1 2017-07-01 10:00:00|M064.txt
##  3: 02|2017-07-01 10:00:00|M014.txt         2 2017-07-01 10:00:00|M014.txt
##  4: 02|2017-07-01 10:00:00|M064.txt         2 2017-07-01 10:00:00|M064.txt
##  5: 03|2017-07-01 10:00:00|M014.txt         3 2017-07-01 10:00:00|M014.txt
##  6: 03|2017-07-01 10:00:00|M064.txt         3 2017-07-01 10:00:00|M064.txt
##  7: 04|2017-07-01 10:00:00|M014.txt         4 2017-07-01 10:00:00|M014.txt
##  8: 04|2017-07-01 10:00:00|M064.txt         4 2017-07-01 10:00:00|M064.txt
##  9: 05|2017-07-01 10:00:00|M014.txt         5 2017-07-01 10:00:00|M014.txt
## 10: 05|2017-07-01 10:00:00|M064.txt         5 2017-07-01 10:00:00|M064.txt
## 11: 06|2017-07-01 10:00:00|M014.txt         6 2017-07-01 10:00:00|M014.txt
## 12: 06|2017-07-01 10:00:00|M064.txt         6 2017-07-01 10:00:00|M064.txt
## 13: 07|2017-07-01 10:00:00|M014.txt         7 2017-07-01 10:00:00|M014.txt
## 14: 07|2017-07-01 10:00:00|M064.txt         7 2017-07-01 10:00:00|M064.txt
## 15: 08|2017-07-01 10:00:00|M014.txt         8 2017-07-01 10:00:00|M014.txt
## 16: 08|2017-07-01 10:00:00|M064.txt         8 2017-07-01 10:00:00|M064.txt
## 17: 09|2017-07-01 10:00:00|M014.txt         9 2017-07-01 10:00:00|M014.txt
## 18: 09|2017-07-01 10:00:00|M064.txt         9 2017-07-01 10:00:00|M064.txt
## 19: 10|2017-07-01 10:00:00|M014.txt        10 2017-07-01 10:00:00|M014.txt
## 20: 10|2017-07-01 10:00:00|M064.txt        10 2017-07-01 10:00:00|M064.txt
## 21: 11|2017-07-01 10:00:00|M014.txt        11 2017-07-01 10:00:00|M014.txt
## 22: 11|2017-07-01 10:00:00|M064.txt        11 2017-07-01 10:00:00|M064.txt
## 23: 12|2017-07-01 10:00:00|M014.txt        12 2017-07-01 10:00:00|M014.txt
## 24: 12|2017-07-01 10:00:00|M064.txt        12 2017-07-01 10:00:00|M064.txt
## 25: 13|2017-07-01 10:00:00|M014.txt        13 2017-07-01 10:00:00|M014.txt
## 26: 13|2017-07-01 10:00:00|M064.txt        13 2017-07-01 10:00:00|M064.txt
## 27: 14|2017-07-01 10:00:00|M014.txt        14 2017-07-01 10:00:00|M014.txt
## 28: 14|2017-07-01 10:00:00|M064.txt        14 2017-07-01 10:00:00|M064.txt
## 29: 15|2017-07-01 10:00:00|M014.txt        15 2017-07-01 10:00:00|M014.txt
## 30: 15|2017-07-01 10:00:00|M064.txt        15 2017-07-01 10:00:00|M064.txt
## 31: 16|2017-07-01 10:00:00|M014.txt        16 2017-07-01 10:00:00|M014.txt
## 32: 16|2017-07-01 10:00:00|M064.txt        16 2017-07-01 10:00:00|M064.txt
## 33: 17|2017-07-01 10:00:00|M014.txt        17 2017-07-01 10:00:00|M014.txt
## 34: 17|2017-07-01 10:00:00|M064.txt        17 2017-07-01 10:00:00|M064.txt
## 35: 18|2017-07-01 10:00:00|M014.txt        18 2017-07-01 10:00:00|M014.txt
## 36: 18|2017-07-01 10:00:00|M064.txt        18 2017-07-01 10:00:00|M064.txt
## 37: 19|2017-07-01 10:00:00|M014.txt        19 2017-07-01 10:00:00|M014.txt
## 38: 19|2017-07-01 10:00:00|M064.txt        19 2017-07-01 10:00:00|M064.txt
## 39: 20|2017-07-01 10:00:00|M014.txt        20 2017-07-01 10:00:00|M014.txt
## 40: 20|2017-07-01 10:00:00|M064.txt        20 2017-07-01 10:00:00|M064.txt
## 41: 21|2017-07-01 10:00:00|M014.txt        21 2017-07-01 10:00:00|M014.txt
## 42: 21|2017-07-01 10:00:00|M064.txt        21 2017-07-01 10:00:00|M064.txt
## 43: 22|2017-07-01 10:00:00|M014.txt        22 2017-07-01 10:00:00|M014.txt
## 44: 22|2017-07-01 10:00:00|M064.txt        22 2017-07-01 10:00:00|M064.txt
## 45: 23|2017-07-01 10:00:00|M014.txt        23 2017-07-01 10:00:00|M014.txt
## 46: 23|2017-07-01 10:00:00|M064.txt        23 2017-07-01 10:00:00|M064.txt
## 47: 24|2017-07-01 10:00:00|M014.txt        24 2017-07-01 10:00:00|M014.txt
## 48: 24|2017-07-01 10:00:00|M064.txt        24 2017-07-01 10:00:00|M064.txt
## 49: 25|2017-07-01 10:00:00|M014.txt        25 2017-07-01 10:00:00|M014.txt
## 50: 25|2017-07-01 10:00:00|M064.txt        25 2017-07-01 10:00:00|M064.txt
## 51: 26|2017-07-01 10:00:00|M014.txt        26 2017-07-01 10:00:00|M014.txt
## 52: 26|2017-07-01 10:00:00|M064.txt        26 2017-07-01 10:00:00|M064.txt
## 53: 27|2017-07-01 10:00:00|M014.txt        27 2017-07-01 10:00:00|M014.txt
## 54: 27|2017-07-01 10:00:00|M064.txt        27 2017-07-01 10:00:00|M064.txt
## 55: 28|2017-07-01 10:00:00|M014.txt        28 2017-07-01 10:00:00|M014.txt
## 56: 28|2017-07-01 10:00:00|M064.txt        28 2017-07-01 10:00:00|M064.txt
## 57: 29|2017-07-01 10:00:00|M014.txt        29 2017-07-01 10:00:00|M014.txt
## 58: 29|2017-07-01 10:00:00|M064.txt        29 2017-07-01 10:00:00|M064.txt
## 59: 30|2017-07-01 10:00:00|M014.txt        30 2017-07-01 10:00:00|M014.txt
## 60: 30|2017-07-01 10:00:00|M064.txt        30 2017-07-01 10:00:00|M064.txt
## 61: 31|2017-07-01 10:00:00|M014.txt        31 2017-07-01 10:00:00|M014.txt
## 62: 31|2017-07-01 10:00:00|M064.txt        31 2017-07-01 10:00:00|M064.txt
## 63: 32|2017-07-01 10:00:00|M014.txt        32 2017-07-01 10:00:00|M014.txt
## 64: 32|2017-07-01 10:00:00|M064.txt        32 2017-07-01 10:00:00|M064.txt
##                                  id region_id                experiment_id
##          start_datetime     file stop_datetime condition genotype
##  1: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
##  2: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
##  3: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
##  4: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
##  5: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
##  6: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
##  7: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
##  8: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
##  9: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 10: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 11: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 12: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 13: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 14: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 15: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 16: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 17: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 18: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 19: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 20: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 21: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 22: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 23: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 24: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 25: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 26: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 27: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        A
## 28: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        A
## 29: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 30: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 31: 2017-07-01 10:00:00 M014.txt    2017-07-07         a        B
## 32: 2017-07-01 10:00:00 M064.txt    2017-07-07         a        B
## 33: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 34: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 35: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 36: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 37: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 38: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 39: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 40: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 41: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 42: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 43: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 44: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 45: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 46: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 47: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 48: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 49: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 50: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 51: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 52: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 53: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 54: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 55: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 56: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 57: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 58: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 59: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        A
## 60: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        A
## 61: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 62: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
## 63: 2017-07-01 10:00:00 M014.txt    2017-07-07         b        B
## 64: 2017-07-01 10:00:00 M064.txt    2017-07-07         b        B
##          start_datetime     file stop_datetime condition genotype
```



## Going further

* [behavr](https://github.com/rethomics/behavr) -- to manipulate the data (create new variable/meta-variables)
* [ggetho](https://github.com/rethomics/ggetho) -- to plot visualise the data
* [sleepr](https://github.com/rethomics/sleepr) -- to perform sleep and circadian rythm analysis

