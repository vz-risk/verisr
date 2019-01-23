verisr
======

This package is to support data analysis within the VERIS framework (<http://veriscommunity.net>). It is intended to work directly with raw JSON and can be used against the VERIS Community Database (VCDB) found at (<http://veriscommunity.net/doku.php?id=public>) and (<https://github.com/vz-risk/VCDB>).

This package has two purposes. First is to convert one or more directories of VERIS (JSON) files into a usable object (in this version it is currently a data.table, but I hope to move to a dplyr object). Second, it offers a set of convenience functions for doing basic information retrieval from the object.

Install it from straight from github:

``` {.r}
# install devtools from https://github.com/hadley/devtools
devtools::install_github("vz-risk/verisr")
```

To begin, load the package and point it at a directory of JSON files storing VERIS data.

``` {.r}
library(verisr)
data('industry2', package='verisr')
vcdb.dir <- "../VCDB/data/json/"
# may optionally load a custom json schema file.
if (interactive()) { # show progress bar if the session is interactive
  vcdb <- json2veris(vcdb.dir, schema="../VCDB/vcdb-merged.json", progressbar=TRUE)
} else {
  vcdb <- json2veris(vcdb.dir, schema="../VCDB/vcdb-merged.json")  
}
```

You can also use a vector of directory names to load files from multiple sources

``` {.r}
library(verisr)
data('industry2', package='verisr')
data_dirs <- c("../VCDB/data/json", "private_data")
veris <- json2veris(data_dirs, schema="path/to/schema.json")
```

What json2veris() returns is a plain data.table object, which enables you (the developer) to work directly with the data.

``` {.r}
class(vcdb)
```

    ## [1] "verisr"     "data.table" "data.frame"

``` {.r}
dim(vcdb)
```

    ## [1] 4313 1705

There are several convenience functions to get a feel for what's in the current verisr object.

``` {.r}
summary(vcdb)
```

    ## 4313 incidents in this object.

    ##       actor                action            asset     
    ##  External:2300   Environmental:   7   Kiosk/Term: 100  
    ##  Internal:1756   Error        :1176   Media     :1185  
    ##  Partner : 184   Hacking      :1353   Network   : 126  
    ##  Unknown : 150   Malware      : 353   Person    : 325  
    ##                  Misuse       : 747   Server    :1969  
    ##                  Physical     : 813   Unknown   : 375  
    ##                  Social       : 303   User Dev  : 930  
    ##                  Unknown      : 167                    
    ##                                                        
    ##            attribute   
    ##  Availability   :1384  
    ##  Confidentiality:3901  
    ##  Integrity      : 974  
    ##                        
    ##                        
    ##                        
    ##                        
    ##                        
    ## 

``` {.r}
library(ggplot2)
plot(vcdb)
```

![plot of chunk basic-plot](./README_files/figure-markdown_github/basic-plot.png)

Let's look for a specific variable by getting the data aggregated on a VERIS enumeration. In this case the variety of external actor.

``` {.r}
ext.variety <- getenum(vcdb, "actor.external.variety")
print(ext.variety)
```

    ##                 enum    x    n      freq
    ##  1:     Acquaintance    2 2300 0.0008696
    ##  2:         Activist  393 2300 0.1708696
    ##  3:          Auditor    0 2300 0.0000000
    ##  4:       Competitor    8 2300 0.0034783
    ##  5:         Customer    6 2300 0.0026087
    ##  6:    Force majeure   16 2300 0.0069565
    ##  7:  Former employee   28 2300 0.0121739
    ##  8:     Nation-state   25 2300 0.0108696
    ##  9:  Organized crime  114 2300 0.0495652
    ## 10:            Other   23 2300 0.0100000
    ## 11: State-affiliated  197 2300 0.0856522
    ## 12:        Terrorist    3 2300 0.0013043
    ## 13:     Unaffiliated  161 2300 0.0700000
    ## 14:          Unknown 1402 2300 0.6095652

You can see this returns the enumeration (enum), the count of that enumeration (x), the sample size (n) of the enumeration class (external actor in this case) and the frequency (freq = x/n). From that, you could create a barplot with ggplot:

``` {.r}
gg <- ggplot(ext.variety, aes(x=enum, y=x))
gg <- gg + geom_bar(stat="identity", fill="steelblue")
gg <- gg + coord_flip() + theme_bw()
print(gg)
```

![plot of chunk basic-ggplot](./README_files/figure-markdown_github/basic-ggplot.png)

or use a built-in function to do the same thing (but a little prettier).

``` {.r}
print(simplebar(ext.variety, "Variety of Hacking Actions"))
```

![plot of chunk internal-plot](./README_files/figure-markdown_github/internal-plot.png)

Filters have changed
====================

The way filters are handled are different. The old function of getfilter() has been removed, it would just return a vector of logicals the same length as the verisr object which would indicate which records to use. Since you have the data (the verisr object is just a data.table) and all the enumerations are logical values, it should be trivial to create a filter. For example, to filter on all the incidents with confirmed data loss, and then further filter for hacking vector of web appliation...

``` {.r}
# see the docs on data.table for getting columns like this
ddfilter <- vcdb[["attribute.confidentiality.data_disclosure.Yes"]]
webfilter <- vcdb[["action.hacking.vector.Web application"]]
# now we can combine with | or & ("or" and "and" respectively)
# to filter incidents with confirmed data loss and web vector:
ddweb <- ddfilter & webfilter
```

Since these are just logical vectors now, we can use sum() to see how many matches.

``` {.r}
cat("Confirmed data loss events:", sum(ddfilter), "\n")
```

    ## Confirmed data loss events: 2517

``` {.r}
cat("Hacking vector of web apps:", sum(webfilter), "\n")
```

    ## Hacking vector of web apps: 692

``` {.r}
cat("Both data loss and web app:", sum(ddweb), "\n")
```

    ## Both data loss and web app: 363

Special names added to verisr object
====================================

Most of the names to query are obvious from the schema. Things like "actor.external.motive" for example is relatively intuitive. But when the verisr object is created there are several more fields dervied from the data to make queries easier. Those are:

-   *actor* will return top level actor categories
-   *action* will return top level action categories
-   *asset.variety* will return top level asset categories
-   *attribute* will return top level asset categories
-   *victim.industry2* will return the first 2 digits of the NAICS code
-   *victim.industry3* same, first 3 digits
-   *victim.orgsize* returns "Large" and "Small" enumerations
-   *pattern* returns the patterns (see DBIR 2014) each line is classified as

If you come across any more that you'd like added, please reach out.

Querying Multiple Enumerations
==============================

One rather fun feature of the lastest version is the ability to query for an enumeration as it relates to one or more other enumerations. For example, if you wanted to create a A2 grid, which compares the action categories to the asset categories, it's a single query:

``` {.r}
a2 <- getenum(vcdb, c("action", "asset.variety"))
head(a2)
```

    ##        enum  enum1    x    n     freq
    ## 1:  Malware Server  241 4313 0.055878
    ## 2:  Hacking Server 1132 4313 0.262462
    ## 3:   Social Server  225 4313 0.052168
    ## 4: Physical Server   42 4313 0.009738
    ## 5:   Misuse Server  457 4313 0.105959
    ## 6:    Error Server  271 4313 0.062833

In previous versions there was a `getenum` and `getenumby` function for one enumeration or multiple enumerations respectively. However, as of version 2.1, `getenumby` is an alias to `getenum` and both calls have the same functionality.

And we can now just visualize that with ggplot in a nice 2x2 grid

![plot of chunk a2grid](./README_files/figure-markdown_github/a2grid.png)

    ##    user  system elapsed 
    ##  26.916   0.974  29.340
