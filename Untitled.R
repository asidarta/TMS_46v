rm(list = ls())
options(width=200)

require('ggplot2')
require('plyr')
require('Rmisc')
require('R.matlab')

my.df <- read.table(file="/Users/ae2012/Desktop/tms.txt",sep=",",header=TRUE)

pre <- summarySE(my.df, "pre",  c("lag","group"))
pre$task <- "before"
colnames(pre)[4] <- "avg"
post<- summarySE(my.df, "post", c("lag","group"))
post$task <- "post"
colnames(post)[4] <- "avg"

to.plot <- rbind(pre,post)
to.plot$lag <- as.factor(to.plot$lag)

ggplot(to.plot, aes(x=task, y=avg,
                    ymin=avg-se,
                    ymax=avg+se,
                    col=lag,
                    group=lag)) + 
  geom_point() + geom_line() + 
  geom_errorbar(width=0.05) +
  ylim(0,0.5) +
  ylab("Hit - False Alarm") + theme_bw() +
  facet_grid(.~group)
 
# Reshape the dataframe
pre <- my.df[,c(1:3,5)]
pre$time <- "pre"
colnames(pre)[3] <- "score"
post<- my.df[,c(1:2,4:5)]
post$time <- "post"
colnames(post)[3] <- "score"
new.df <- rbind(pre,post)
new.df$lag<-as.factor(new.df$lag)

combined <- ddply(new.df, c("subj.id","group","time"), summarize, score=mean(score))
summarySE(combined, measurevar = "score", groupvars = c("group","time"))

t.test( subset(my.df.combined, group=="treatment")$pre,
        subset(my.df.combined, group=="treatment")$post )

require("ez")

ezANOVA( data=combined,
         dv=score,
         wid=subj.id,
         within=time,
         between=group)

pairwise.t.test(combined$score, c(combined$group,combined$time), paired=FALSE)
