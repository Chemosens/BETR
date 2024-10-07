# BETR
## Description
This package aims to work with intensities and BET.  

## Installation
```R
library(devtools)
install_github("https://github.com/ChemoSens/BETR")
```
## Example of usage

### Data example
The package contains an example presented in Martin et al. (2024) on astringency.
```R
data(astringency)
head(astringency)
```
### Calculating BET
```R
res_bet=bet(df=astringency)
```
### Calculating IBT
```R
res_ibt=ibt(df=astringency)
```

### Calculating the difference between BET and IBT (DRI)
```R
res_dri=abs(res_bet-res_ibt)
```
### Getting an individual graph
```R
graph(df=astringency,subject="S002")
````

### Getting all individual graphs ordered by DRI
```R
res_dri_ordered=res_dri[order(res_dri)]
```
```R
p=list()
for(subject in names(res_dri_ordered))
{
  p[[subject]]=graph(df=astringency,subject=subject)$p
}
library(gridExtra)
grid.arrange(grobs=p[1:12],nrow=3)
```

