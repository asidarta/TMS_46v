

######### This file directly reads textfile from the experiment ############
rm(list = ls())
options(width=200)

require('ggplot2')
require('plyr')
require('Rmisc')
require("reshape")

getwd()  # Read the current working directory
dirpath <- "/Users/ae2012/Documents/R/TMS_data/"   # home directory in linux is shown as ~
setwd(dirpath) # Set the working directory for the analyses


## Easier to select which test version to use. This also determines the filename to save.
## To source a script means a command to load whatever functions contained in that R script.
myTest_version_string = "tms_param"   
#source("test_param.R")
source(paste(myTest_version_string,".R",sep=""))


allsubjects <- NULL
anchors <- 1: nANCHOR


###### ACCESS INDIVIDUAL DATA FROM EACH SUBJECT WITH FOR LOOP #######

for (i in 1 : nsubj ) {

  subjtab <- NULL
  
  for (mytask in tasks) {
  #print(mytask)
  
     for (block.num in 1 : nBLOCK.WM) {
       
         subjname <- s.names[i]
         my.file <- paste(substr(mytask,0,3), "_", subjname, block.num,".txt", sep="")
         my.file <- paste(dirpath,subjname,"_data/",my.file, sep="")

         # [Updated Feb 2018] I add error handler if Visual condition files aren't found.
         if (file.exists(my.file)) {
            tab <- read.table(file=my.file, sep=" ")
            # Remove NA columns; leaving only 8 usable columns!
            tab <- tab[,colSums(is.na(tab))==0]
            # Remove DELAY column, I don't need that!
            if (ncol(tab) > 7) { tab <- tab[,1:7] }
            # Then, give column names
            names(tab) <- c('trial',c(sprintf("anchor%i",anchors)),'probe','answer','RT') 
            ## Remove "ANSWER:" and get the Yes/No response...
            tab$answer <- sapply(tab[, nANCHOR + 3],
                                 function(x) {
                                   as.numeric(strsplit(as.character(x),":")[[1]][2])   # This is a list operation
                                 })
         }
         
         else {
            print(paste(subjname,"- File doesn't exist! NA will be used."))
            tab <- tab[FALSE,]  # Well this won't work if no prior tab df exists!
            tab[1,] <- c(rep(NA,11))
            tab$answer <- NA
         }
         tab$block   <- block.num
         tab$subj.id <- subjname
         tab$group   <- s.group[i]   # Control versus Treatment???
         subjtab <- rbind(subjtab, tab);   # Nice function to append data frame. Append means adding rows.
     }
    
  }
  # Without doing this, function in ddply cannot distinguish the same trial number but of multiple blocks. 
  #subjtab$trial <- subjtab$block*24 + subjtab$trial.in.block
  subjtab$trial = 1: length(subjtab$subj.id==subjname)
  allsubjects <- rbind(allsubjects, subjtab)
  
}

#allsubjects <- subset( allsubjects, block%in%c(2,3,4,5,6))

## Save the WM raw data as a textfile....
#write.csv(allsubjects, "all_wm.csv")

# Get the number of subjects in each group.....
sprintf("Lets see how many subjects in each group.....")
ddply(allsubjects, "group", summarize, "count"=length(unique(subj.id)))



## Make a column that represents for a given trial, whether the probe was one of the anchors or not
get.lag <- function(row) {
  ## Takes as input a row, and outputs a number that tells us which anchor was equal to the probe,
  ## or, if the probe was not among the anchors, then it returns NA.
  #index <- which(row$probe == row[,c('anchor1','anchor2','anchor3','anchor4','anchor5','anchor6')])
  index <- which(row$probe == row[,c(sprintf("anchor%i",anchors))])
  if (length(index)==0) { return (NA) } else { return (index-nANCHOR-1) }
  ## Ananda update: chronological order uses negative value, lag -6 means 6 directions ago from the probe.
}



allsubjects$lag <- NA
## Go and check each row of our allsubjects data.
for (i in 1:nrow(allsubjects)) {
  allsubjects[i,]$lag <- get.lag(allsubjects[i,]);   ## Call the 'get.lag' function
}



# Now, I create 2 additional columns for correct answer and subject score!
allsubjects$lag <- as.factor(allsubjects$lag)
allsubjects$correct.answer <- ifelse( is.na(allsubjects$lag), 0, 1)
allsubjects$score <- ifelse( allsubjects$answer==allsubjects$correct.answer, 1, 0)

# Use 'str' to see the content of the data frame showing columns and the data type.
str(allsubjects)



## For each of the anchors, compute the distance to the probe.
## NOTE: we invert the order of the anchors here. So dist1 refers to the distance
## to the probe of the *most recent* anchor (i.e. anchor 6), not the first anchor.
#for (i in 1:nANCHOR) {
#  allsubjects[,sprintf("dist%i",i)] <- abs(allsubjects$probe - allsubjects[,sprintf("anchor%i",nANCHOR+1-i)])
#}

# Make these distance as a string of text
#allsubjects$dist.combo <- paste(allsubjects$dist1,allsubjects$dist2,allsubjects$dist3,sep=",")
#allsubjects$dist.combo <- as.factor(allsubjects$dist.combo)

allsubjects$anchor1.2 <- abs(allsubjects$anchor1 - allsubjects$anchor2)
allsubjects$anchor3.2 <- abs(allsubjects$anchor3 - allsubjects$anchor2)
allsubjects$sum.diff.anchor <- allsubjects$anchor1.2 #+ allsubjects$anchor3.2



# Decide for each trial whether it is probe.outside or probe.inside. and then check whether prop yes is different.
get.outside <- function(row) {
   ## Takes as input a row, and outputs a number that tells us when probe larger than maximum/minimum anchor
   #index <- which(row$probe > row[,c('anchor1','anchor2','anchor3','anchor4')])
   indexmax <- which(row$probe > max(row[,c(sprintf("anchor%i",anchors))]))
   indexmin <- which(row$probe < min(row[,c(sprintf("anchor%i",anchors))]))
   if (length(indexmax)>0 | (length(indexmin)>0)) { return (1) } else { return (NA) }
}

allsubjects$outside <- NA

for (i in 1:nrow(allsubjects)) {
   allsubjects[i,]$outside <- get.outside(allsubjects[i,])
}





######## LET'S QUANTIFY THE MEMORY PERFORMANCE ###########

# We use ddply here as a way to perform an operation for a subset of a data frame, as defined by the
# grouping variables. Meaning, the following function(xx) will be performed for each unique subject,
# block, lag, and group.
prop.yes <- ddply ( allsubjects, 
                    c("subj.id", "block", "lag", "group"),  # Note the grouping variables
                    function(xx) {
                           prop = sum(xx$answer)/length(xx$answer)
                           n = nrow(xx)
                           if ((sum(xx$answer) == length(xx$answer)) | (sum(xx$answer) == 0)) {
                              corrected = (sum(xx$answer) + 0.5)/(length(xx$answer)+1)
                           } 
                           else { corrected = prop }
                        return (c("uncorr" = prop,
                                  "prop.yes"= prop,    # Corrected or uncorrected ??????
                                  "n" = n))
                    })


# NOTE: prop.yes contains both proportion of YES responses in trials when probe = anchor and probe = lures.
# We only want the Hit rate, that is, when probe = anchor. Next, we have to select just the lure trials 
# which we need to extract false alarm to compute hit minus false alarm.
#
no.lag <- subset(prop.yes, is.na(lag))

false.alarms <- data.frame(subj.id     = no.lag$subj.id,
                           block       = no.lag$block,
                           false.alarm = no.lag$prop.yes,    # Here, FA is based on corrected values
                           true.neg    = 1-no.lag$prop.yes)  # Specificity, true negative rate

# Merge two data frames!
hit.fa <- merge(subset(prop.yes,!is.na(lag)), false.alarms)
#hit.fa <- merge(subset(prop.yes,n!=12), false.alarms)


## Now compute WM performance;  (Revised Feb 2018 for visual WM with no data, NA).
## Which one is the best performance measure for WM?
hit.fa$hit.min.fa <- hit.fa$prop.yes- hit.fa$false.alarm                   # (1) Hit-FA rate, corrected
hit.fa$dprime <- qnorm(hit.fa$prop.yes) - qnorm(hit.fa$false.alarm)        # (2) dprime, corrected
hit.fa$beta <- exp(-qnorm(hit.fa$prop.yes)*qnorm(hit.fa$prop.yes)/2 +      # (3) beta, corrected
                    qnorm(hit.fa$false.alarm)*qnorm(hit.fa$false.alarm)/2)



### Lastly, we compute the PRE and POST TMS average WM scores for each subject
pre.post <- ddply(hit.fa, c("subj.id","lag","group"), function(xx){
                  pre  <- subset(xx, block <=3 )
                  post <- subset(xx, block > 3 )
                  return(data.frame( "pre.wm"  = mean(pre$hit.min.fa, na.rm=TRUE),
                                     "post.wm" = mean(post$hit.min.fa, na.rm=TRUE),
                                     "pre.hit" = mean(pre$prop.yes, na.rm=TRUE),
                                     "post.hit"= mean(post$prop.yes, na.rm=TRUE),
                                     "pre.fa"  = mean(pre$false.alarm, na.rm=TRUE),
                                     "post.fa" = mean(post$false.alarm, na.rm=TRUE) ))
} )

pre.post








# Here, we see whether corect responses are higher if the sum of angular differences.....
new.df <- summarySE(pre.post, "pre.wm", c("subj.id","sum.diff.anchor","group"), na.rm=TRUE)
summarySE(pre.post, "pre.wm", c("sum.diff.anchor"), na.rm=TRUE)

ggplot(new.df, aes(x=sum.diff.anchor, y=pre.wm, 
                   col=subj.id)) + geom_point() + facet_grid(.~group)



### Let's melt the data frame for plotting purposes. After melting, we can plot a graph based on
### a grouping variable, for example, based on pre and post.
pre.post.melt <- melt(pre.post, id.vars = c("subj.id","lag","group"), 
                      measure.vars = c("pre.wm", "post.wm","pre.hit", 
                                       "post.hit","pre.fa", "post.fa"))
colnames(pre.post.melt)[4] = "time"
colnames(pre.post.melt)[5] = "score"



# (1) Compute hit minus false alarm of GROUP DATA!
grp.lag.hit.fa <- ddply(pre.post.melt, c("lag","group","time"), 
                function(xx){
                     mean = mean(xx$score)
                     stde = sd(xx$score)/sqrt(length(xx$score))
                     new.var = as.character(xx$time)
                     time = unlist(strsplit(new.var, split="\\."))[1]
                     var  = unlist(strsplit(new.var, split="\\."))[2]
                     return(data.frame("time"=time,"n"=nrow(xx),
                                       "param"=var,"mean"=mean,"se"=stde))
})

# Plot the graph for each score......
ggplot( grp.lag.hit.fa, aes(x=time, y=mean,
                        ymax=mean+se, 
                        ymin=mean-se,
                        group=lag,
                        col=lag) ) + 
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  facet_grid(param~group,scales="free_y") +
  theme_bw()


# (2) What if we disregard the lags!?

# Silly, this is to split the string in the 'time' column.
split.str <- function(xx,n){
    if (n<=2){var = unlist(strsplit(xx, split="\\."))[n]
    }
}
       
# Average the two lags into one.....
grp.hit.fa <- summarySE(pre.post.melt, "score", c("subj.id","group","time"))

t.test(subset(grp.hit.fa, group=="sham" & time=="post.wm")$score,
       subset(grp.hit.fa, group=="right" & time=="post.wm")$score)


# Then get the average across subjects.....
grp.hit.fa <- summarySE(grp.hit.fa, "score", c("group","time"))
grp.hit.fa$param <- apply(as.matrix(grp.hit.fa$time), 1, split.str, 2)
grp.hit.fa$param <- factor(grp.hit.fa$param, level=c("wm","hit","fa"))
grp.hit.fa$time  <- apply(as.matrix(grp.hit.fa$time), 1, split.str, 1)
grp.hit.fa$time  <- factor(grp.hit.fa$time, level=c("pre","post"))

ggplot( grp.hit.fa, aes(x=time, y=score,
                        ymax=score+se, 
                        ymin=score-se, group = group) ) + 
  geom_point() + 
  geom_line() +
  geom_errorbar(width=0) +
  facet_grid(param~group,scales="free_y") +
  theme_bw()




## Showing points in the plot?
pre.post.melt.wm <- subset(pre.post.melt, time%in%c("pre.wm","post.wm"))
pre.post.melt.wm <- ddply(pre.post.melt.wm, c("subj.id","group","time"), summarize, score=mean(score))
pre.post.melt.wm$time <- ifelse(pre.post.melt.wm$time=="pre.wm","pre","post") 
pre.post.melt.wm$time <- factor(pre.post.melt.wm$time, level=c("pre","post"))

ggplot() + #geom_bar(data=subset(grp.hit.fa, param=="wm"  ),
           #         aes(x=time, y=score), stat="identity", width=0.3, alpha=0.4 ) + 
  #geom_errorbar( data=subset(grp.hit.fa, param=="wm" ),
   #              aes(x=time, y=score, ymax=score+se, ymin=score-se), width=0 ) +
  geom_point( data=pre.post.melt.wm, aes(x=time, y=score, group=subj.id)) + 
  geom_line( data=pre.post.melt.wm, aes(x=time, y=score, group=subj.id)) +
  facet_grid(.~group) +
  ylim(-0.1,0.5) +
  theme_bw()







### This is if we want to show individual data point...
grp.hit.fa <- subset(grp.hit.fa, param=="wm")
ggplot( grp.hit.fa, aes(x=time, y=mean,
                        ymax=mean+se, 
                        ymin=mean-se, group = subj.id) ) + 
  geom_point()+geom_line() +
  facet_grid(param~group,scales="free_y") +
  theme_bw()


#### Plot it by lag to see if individual trend #####
ggplot( subset(pre.post.melt, time %in% c("pre.wm","post.wm")), 
        aes(x=time, y=score, group=subj.id, col=subj.id)) +
  geom_point() + geom_line() +
  facet_grid(lag~group,scales="free_y") +
  theme_bw() + ylim(-0.2,0.5)




# (3) Categorizing responder and non-responder.
new.df <- ddply( pre.post, .(subj.id,group), summarize, "pre.wm"=mean(pre.wm),
                 "post.wm"=mean(post.wm)
)

new.df$status <- ifelse(new.df$pre.wm < new.df$post.wm, "improve", "worsen")

new.df.melt <- melt(new.df, id.vars = c("subj.id","group","status"), 
                    measure.vars = c("pre.wm", "post.wm"))

new.df.melt$status <- as.factor(new.df.melt$status)
colnames(new.df.melt)[4] <- "time"

ezANOVA( data=new.df.melt,
         dv=value,
         wid=subj.id,
         within=time,
         between=c(group,status) )

avg.df <- summarySE(new.df.melt, "value", c("group","status","time"))

ggplot( new.df.melt, aes(x=time, y=value, group=subj.id)) + #, ymax=value+se, ymin=value-se ) ) + 
  geom_point() + geom_line() +
  #geom_errorbar(width=0) +
  facet_grid(status~group,scales="free_y") +
  theme_bw() + ylim(-0.2,0.5)






# (4) Perform ANOVA using "ez" library. This is a straightforward way of doing ANOVA instead of using
# the usual R function. You just have to define the necessary between-, within-subject variables.
require("ez")

ezANOVA( data=subset(pre.post.melt, pre.post.melt$time %in% c("pre.wm","post.wm")),
         dv=score,
         wid=subj.id,
         within=time,
         between=group )
        
### Using linear model.....


sprintf("Comparing PRE and POST for tms group")
new.df <- subset(pre.post.melt, pre.post.melt$group=="tms") 
t.test(subset(new.df, new.df$time=="pre.wm")$score,
       subset(new.df, new.df$time=="post.wm")$score)

sprintf("Comparing POST WM between sham and tms")
new.df <- subset(pre.post.melt, pre.post.melt$time=="post.wm") 
t.test(subset(new.df, new.df$group=="sham")$score,
       subset(new.df, new.df$group=="tms")$score)




if(0){
# (5) Try more detailed analysis. Analyze per block??
pre.post.blk <- ddply(hit.fa, c("subj.id","lag","group","block"), function(xx){
  return(data.frame( "wm"  = mean(xx$hit.min.fa, na.rm=TRUE),
                     "hit" = mean(xx$prop.yes, na.rm=TRUE),
                     "fa" = mean(xx$false.alarm, na.rm=TRUE) ))
} )


##### Let's melt the data frame
pre.post.blk.melt <- melt(pre.post.blk, id.vars = c("subj.id","lag","group","block"), 
                      measure.vars = c("wm", "hit","fa"))
colnames(pre.post.blk.melt)[5] = "time"
colnames(pre.post.blk.melt)[6] = "score"

grp.blk.hit.fa <- ddply(pre.post.blk.melt, c("group","lag","time","block"), 
                    function(xx){
                      mean = mean(xx$score)
                      stde = sd(xx$score)/sqrt(length(xx$score))
                      return(data.frame("n"=nrow(xx),"mean"=mean,"se"=stde))
                    })

# Plot the graph for each score......
ggplot( grp.blk.hit.fa, aes(x=block, y=mean,
                            ymax=mean+se, 
                            ymin=mean-se,
                            group=group,
                            col=as.factor(group)) ) + 
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  facet_grid(time~lag,scales="free_y") +
  theme_bw()


# Plot the graph for each score for EACH subject
ggplot( grp.blk.hit.fa, aes(x=block, y=mean,
                            ymax=mean+se, 
                            ymin=mean-se,
                            group=group,
                            col=as.factor(group)) ) + 
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  facet_grid(time~lag,scales="free_y") +
  theme_bw()
}



#(6) What about testing by finding the difference-score?
diff.score <- ddply(hit.fa, c("subj.id","lag","group"), function(xx){
  pre  <- subset(xx, block<=3)
  post <- subset(xx, block >3)
  return(data.frame( "post.wm"= mean(post$hit.min.fa, na.rm=TRUE),
                     "wm"= (mean(post$hit.min.fa, na.rm=TRUE)-mean(pre$hit.min.fa, na.rm=TRUE)),
                     "hit"= mean(post$prop.yes, na.rm=TRUE)-mean(pre$prop.yes, na.rm=TRUE),
                     "fa"= mean(post$false.alarm, na.rm=TRUE)-mean(pre$false.alarm, na.rm=TRUE) ))
} )

diff.score <- melt(diff.score, id.vars = c("subj.id","lag","group"),
                   measure.vars = c("wm","hit","fa"))
colnames(diff.score)[4] = "variable"
colnames(diff.score)[5] = "score"



#grp.diff <- ddply(diff.score, c("group","variable"), 
#                        function(xx){
#                          mean = mean(xx$score)
#                          stde = sd(xx$score)/sqrt(length(xx$score))
#                          return(data.frame("n"=nrow(xx),"score"=mean,"se"=stde))
#                        })
grp.diff <- summarySE(diff.score, "score", c("subj.id","group","variable"))

x <- subset(grp.diff, variable=="wm" & group=="tms")
plot(1:nrow(x), x$score)
x[10,5]<-NA

## Look is there any outlier?
x$score
mean(x$score,na.rm=T)+IQR(x$score)
mean(x$score,na.rm=T)-IQR(x$score)
scale(x$score)





# Plot the graph for each score. Position dodge is to make the bars side-by-side.
# Note: What is width argument in position_dodge()?
grp.diff <- summarySE(diff.score, "score", c("group","variable"))
ggplot( grp.diff, aes(x=group,y=score,ymax=score+se,ymin=score-se,fill=group )) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9))  +
  geom_point(aes(y=score), position=position_dodge(width=0.9)) +
  geom_errorbar(stat="identity", width=0,position=position_dodge(width=0.9)) +
  facet_grid(.~variable,scales="free_y") +
  theme_bw()

diff.score.avg <- ddply(diff.score, c("subj.id","group","variable"), 
                                 function(xx){
                                   mean = mean(xx$score)
                                   stde = sd(xx$score)/sqrt(length(xx$score))
                                   return(data.frame("n"=nrow(xx),"mean"=mean,"se"=stde))
                                 }) 
diff.score.avg <- subset(diff.score.avg, diff.score.avg$variable=="wm")
t.test(subset(diff.score.avg, diff.score.avg$group=="tms")$mean, 
       subset(diff.score.avg, diff.score.avg$group=="sham")$mean,
       alternative = c("less"))


####


#(7) See whether different angular distance contributes to WM Score!
prop.yes.dist <- ddply ( subset(allsubjects, block<=3),
                    c("subj.id", "block", "group", "dist.combo"),  # Note the grouping variables
                    function(xx) {
                      prop = sum(xx$answer)/length(xx$answer)
                      n = nrow(xx)
                      if ((sum(xx$answer) == length(xx$answer)) | (sum(xx$answer) == 0)) {
                        corrected = (sum(xx$answer) + 0.5)/(length(xx$answer)+1)
                      } 
                      else { corrected = prop }
                      return (c("uncorr" = prop,
                                "prop.yes"= corrected,    # Corrected or uncorrected ??????
                                "n" = n))
                    })

prop.yes.dist     <- summarySE(prop.yes.dist, "prop.yes", c("subj.id", "group", "dist.combo"), na.rm=T)
prop.yes.dist.avg <- summarySE(prop.yes.dist, "prop.yes", c( "dist.combo"), na.rm=T)



ggplot(prop.yes.dist.avg, aes(y=dist.combo, x=prop.yes, 
                              xmin=prop.yes-se, xmax=prop.yes+se )) +
         geom_point() +
         geom_errorbar(width=0)


#############

save (list = c("diff.score.avg","new.df.melt"), 
      file = paste("wm.RDATA",sep=""))




##############

# Different angular spans.

prop.yes.ang.diff <- ddply ( allsubjects, 
                    c("subj.id", "block","lag", "group", "sum.diff.anchor"),  # Note the grouping variables
                    function(xx) {
                      prop = sum(xx$answer)/length(xx$answer)
                      n = nrow(xx)
                      if ((sum(xx$answer) == length(xx$answer)) | (sum(xx$answer) == 0)) {
                        corrected = (sum(xx$answer) + 0.5)/(length(xx$answer)+1)
                      } 
                      else { corrected = prop }
                      return (c("uncorr" = prop,
                                "prop.yes"= prop,    # Corrected or uncorrected ??????
                                "n" = n))
                    })

no.lag.ang.diff <- subset(prop.yes.ang.diff, is.na(lag))

false.alarms.ang.diff <- data.frame(subj.id     = no.lag$subj.id,
                                    block       = no.lag$block,
                                    false.alarm = no.lag$prop.yes,    # Here, FA is based on corrected values
                                    true.neg    = 1-no.lag$prop.yes)  # Specificity, true negative rate

# Merge two data frames!
hit.fa.ang.diff <- merge(subset(prop.yes.ang.diff,!is.na(lag)), false.alarms)

hit.fa.ang.diff <- ddply(hit.fa.ang.diff, c("subj.id","group","block","sum.diff.anchor"), summarize,
                      prop.yes = mean(prop.yes), 
                      false.alarm = mean(false.alarm))

avg.hit.fa.ang.diff <- summarySE(hit.fa.ang.diff, "prop.yes", c("sum.diff.anchor","group"))

ggplot(hit.fa.ang.diff, aes(x=sum.diff.anchor,y=prop.yes)) + 
  geom_point() + 
  facet_grid(.~group)

ggplot(avg.hit.fa.ang.diff, aes(x=sum.diff.anchor,y=prop.yes,ymax=prop.yes+se,ymin=prop.yes-se)) + 
  geom_point() + 
  geom_errorbar(width=0) +
  facet_grid(.~group)


##########

pre.post.ang.diff <- ddply(hit.fa.ang.diff, c("subj.id","block","group","sum.diff.anchor"), 
                           function(xx){
    pre  <- subset(xx, block <=3 )
    post <- subset(xx, block > 3 )
    return(data.frame("pre.hit" = mean(pre$prop.yes, na.rm=TRUE),
                      "post.hit"= mean(post$prop.yes, na.rm=TRUE),
                      "pre.fa"  = mean(pre$false.alarm, na.rm=TRUE),
                      "post.fa" = mean(post$false.alarm, na.rm=TRUE) ))
} )

temp.df <- melt(pre.post.ang.diff, id.vars = c("subj.id","group","sum.diff.anchor"), 
                 measure.vars = c("pre.hit","post.hit","pre.fa", "post.fa"))

summarydf <- summarySE(temp.df, "value",c("variable","group","sum.diff.anchor"), na.rm=TRUE)

ggplot(summarydf, aes(x=sum.diff.anchor,y=value,ymax=value+se,ymin=value-se)) + 
  geom_point() +
  geom_errorbar(width=0) + 
  facet_grid(group~variable)
