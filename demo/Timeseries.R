library(googlesheets4)
library(tidyverse)
library(cowplot)

formA.hist <- read_sheet("https://docs.google.com/spreadsheets/d/1svdCQI9Jb3MpIObGWExg942MUsBB4uRquhD1FNTkChM/edit#gid=688471781",1)
View(formA.hist)

## Need to fix baseline timestamp

min(formA.hist$timestamp,na.rm=TRUE) -> first.post
base.timestamp <- as.POSIXlt(first.post)
## Put at the top of the hours.
base.timestamp$min <-0
base.timestamp$sec <-0
base.timestamp <- as.POSIXct(base.timestamp)

formA.hist[formA.hist$context=="*Baseline*","timestamp"] <- as.POSIXct(base.timestamp)

filter(formA.hist, uid=="011c1") %>% ggplot(aes(x=timestamp,y=Physics_EAP)) + geom_line() + geom_text(aes(label=context))

fiveuid <- sample(unique(formA.hist$uid),5)
filter(formA.hist, uid %in% fiveuid) %>% ggplot(aes(x=timestamp,y=Physics_EAP)) + geom_line() +
  facet_grid(rows=vars(uid))


## ndiff differences the series, leaving the first (baseline) value as NA.
## Thus is is a candidate aux function for mutate.
ndiff <- function (x) {
  res <- c(NA,diff(x))
  names(res) <- names(x)
  res
}
Phys1 <- filter(formA.hist,uid=="011c1") %>% pull(Physics_EAP)
names(Phys1) <- filter(formA.hist,uid=="011c1") %>% pull(context)

## Plots versus timestamps.  Hard to see what is going on.
filter(formA.hist, uid=="011c1") %>% ggplot(aes(x=timestamp,y=ndiff(Physics_EAP))) + geom_line()
filter(formA.hist, uid %in% fiveuid) %>% ggplot(aes(x=timestamp,y=Physics_EAP)) + geom_line() +
  facet_grid(rows=vars(uid))
filter(formA.hist, uid %in% fiveuid) %>% ggplot(aes(x=timestamp,y=ndiff(Physics_EAP))) + geom_line() +
  facet_grid(rows=vars(uid))

## Working with discrete time.
## 1) Need to use geom_col() not geom_bar()
## 2) fct_reorder(context,timestamp) makes sure levels are in time order, not alphabetical!!
## 3) scale_fill_gradient2(low="red",mid="yellow",high="green") gives traffic light coloring.
## 4) theme(axis.text.y=element_blank()) suppresses the labels in the second plot.
## 5) cowplot::plot_grid() plots side by side.

c011c1.eap <- filter(formA.hist, uid=="011c1") %>% ggplot(aes(x=fct_reorder(context,timestamp),y=Physics_EAP)) +
    geom_col(aes(fill=..y..))+scale_fill_gradient2(low="red",mid="yellow",high="green",midpoint=0)+
    coord_flip(ylim=c(-1,1))+theme_light()+labs(x="",y="EAP[Physics]")
c011c1.diff <- filter(formA.hist, uid=="011c1") %>% ggplot(aes(x=fct_reorder(context,timestamp),y=ndiff(Physics_EAP))) +
  geom_col(aes(fill=..y..))+scale_fill_gradient2(low="red",mid="yellow",high="green",midpoint=0)+
  coord_flip(ylim=c(-1,1))+theme_light()+labs(x="",y="\u0394EAP[Physics]") + theme(axis.text.y=element_blank())

plot_grid(c011c1.eap,c011c1.diff)

## Remaining Issues:
# 1) Probably want 3 plots:  level names and trophies, EAP, Delta EAP
#### Need to merge trophy data in with levels.
# 2) There could be more than one level with the same context.  Need to fix.
####  Maybe plotting by order number will fix this.
# 3) Need to figure out how to add "outer" labels to multiplot Grid.


EAPBal(filter(formA.hist,uid=="011c1") %>% pull("Physics_EAP"),
       filter(formA.hist,uid=="011c1") %>% pull("context"),
       varname="Physics")
