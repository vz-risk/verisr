verisr
========================================================

This package is to support data analysis within the VERIS framework (http://veriscommunity.net).  It is intended to work directly with raw JSON and can be used against the VERIS Community Database (VCDB) found at (http://veriscommunity.net/doku.php?id=public) and (https://github.com/vz-risk/VCDB).

Install it from straight from github:


```r
library("devtools")
install_github("verisr", "vz-risk")
```


To begin, load the package and point it at a directory of JSON files storing VERIS data.


```r
library(verisr)
vcdb.dir <- "../VCDB/incidents"
vcdb <- json2veris(vcdb.dir)
```


Now that we have this, we can get a quick view of what's in the data:


```r
summary(vcdb)
```

```
## 1737 incidents in this object.
## 
##  ext  int  prt 
## 1024  556  104 
## 
## mal hak soc mis err phy env 
##  49 466  33 225 413 520   1
```


Let's look for a specific variable:


```r
ext.variety <- getenum(vcdb, "actor.external.variety")
print(ext.variety)
```

```
##                enum   x
## 2        Competitor   1
## 3          Customer   1
## 9  State-affiliated   2
## 4     Force majeure   3
## 6      Nation-state   3
## 8             Other   9
## 5   Former employee  11
## 7   Organized crime  21
## 1          Activist 134
## 10     Unaffiliated 220
## 11          Unknown 619
```


And we could create a barplot with ggplot:


```r
library(ggplot2)
gg <- ggplot(ext.variety, aes(x = enum, y = x))
gg <- gg + geom_bar(stat = "identity", fill = "steelblue")
gg <- gg + coord_flip() + theme_bw()
print(gg)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


In progress: searching by two enumerations:


```r
hacking.actor <- getenumby(vcdb, "action.hacking.vector", "actor.external.variety")
gg <- ggplot(hacking.actor, aes(x = enum, y = x))
gg <- gg + geom_bar(stat = "identity", fill = "steelblue")
gg <- gg + facet_wrap(~primary, ncol = 2)
gg <- gg + ylab("Incidents") + xlab("Hacking Vector")
gg <- gg + ggtitle("External Actors by Hacking Vector")
gg <- gg + coord_flip() + theme_bw()
print(gg)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

