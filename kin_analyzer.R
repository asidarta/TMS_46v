
rm(list = ls())

options(width=200)
#library(corrplot)
require('ggplot2')
require('plyr')
require("Rmisc")
require("gridExtra")


getwd()
options(width=200)


dirpath <- "~/Documents/R/TMS_data/"   # home directory in linux is shown as ~
setwd(dirpath)

## Easier to select which test version to use. This also determines filename to save.
myTest_version_string = "tms_param"   
source(paste(myTest_version_string,".R",sep=""))

allsubjects <- NULL
anchors <- 1: nANCHOR

for ( i in 1 : nsubj ) {
  subjtab <- NULL
  for ( block.num in 1 : nBLOCK.MOTOR ) {
     subjname <- s.names[i]
     group <- s.group[i]
     my.file <- paste("motorLog_", subjname, '_', block.num,".txt", sep="")
     my.file <- paste(dirpath,subjname,"_data/",my.file, sep="")
     # Load textfile as table, very easy in R. Note the column separator!
     tab <- read.table(file=my.file, sep=",", header=TRUE)
     tab$subj.id <- subjname
     tab$group   <- group
     tab$block   <- block.num
     tab$abs.PD <- abs(tab$angle_maxv_shift)
     tab$success <- ifelse(tab$boom,"success","failure")
     nrow(tab)
     # Nice function to append (COMBINE) several data frames together.
     subjtab <- rbind(subjtab, tab)
  }
  subjtab$newTrial = 1:nrow(subjtab)
  allsubjects <- rbind(allsubjects, subjtab)
}


## Let's save all data into a textfile...
#write.csv(allsubjects, "all_kin.csv")



# (1) Individual subject's kinematic performance with explosion shown....
require(gridExtra)
if(0) {
  for (current_id in unique(allsubjects$subj.id)) {
    mySubset <- subset(allsubjects, subj.id==current_id)
    axismin  <- -40; axismax  <- 40
    # Use angular deviation (not PDy!) w.r.t to target center
    plot1 <- ggplot(mySubset, aes(x=newTrial, y=angle_maxv_shift, shape=task)) + 
      geom_point(colour="grey50", size = 2) +
      geom_point(stroke=1, aes(colour=success)) +
      scale_colour_manual(values = c("failure"="black","success"="green"), guide = FALSE) +
      ylim(axismin, axismax) + 
      labs(x="Trial (Block 1 to 5)", y="Signed Error (degree)")
    plot2 <- ggplot(mySubset, aes(x=newTrial, y=abs(angle_maxv_shift), shape=task)) + 
      geom_point(colour="grey50", size = 2) +
      geom_point(stroke=1, aes(colour=success)) +
      scale_colour_manual(values = c("failure"="black","success"="green"), guide = FALSE) +
      ylim(-5, axismax) + 
      labs(x="Trial (Block 1 to 5)", y="Signed Error (degree)") +
      geom_smooth(colour="grey60", method = "lm")
      
    grid.arrange(plot1, plot2, nrow=2, ncol=1)   
    ggsave( sprintf('kinematic_%s.pdf',current_id), arrangeGrob(plot1, plot2, nrow=2, ncol=1), 
            width=10, height=10, units="in", dpi=500)
  }
}




# (2) During random exploration (pre_train only)
my.df <- ddply( subset(allsubjects, task=="pre_train"), "subj.id", function(xx){
  xx$newTrial = 1:nrow(xx)
  xx$angle_maxv_shift = abs(xx$angle_maxv_shift)
  return(xx) }
)

grp.avg <- ddply( my.df, "group", summarize, 
                  "mean"=mean(abs.PD),
                  "mean.se"=sd(abs.PD)/sqrt(length(unique(subj.id))),
                  "var"=sd(angle_maxv_shift)
                  )




# (3) Group average for the individual trial....
# Exclude the pre-train block...
my.df <- ddply( subset(allsubjects, task!="pre_train"), "subj.id", function(xx){
                      xx$newTrial = 1:nrow(xx)
                      xx$angle_maxv_shift = abs(xx$angle_maxv_shift)
                      return(xx) }
)

error.grp <- summarySE( my.df, "abs.PD", c("newTrial","task","group"), na.rm = TRUE)

ggplot(error.grp, aes( x=newTrial,y=abs.PD,
                       ymin=abs.PD-se,
                       ymax=abs.PD+se,
                       color=task) ) +
  scale_color_manual(values=c("training"="blue","motor_post"="red")) +
  scale_fill_manual (values=c("training"="blue","motor_post"="red")) +
  geom_point() + geom_errorbar(width=0) +
  geom_smooth(method="lm") +
  #geom_ribbon( aes(fill=task), alpha=0.4 ) +
  ylim(0,20) + 
  ggtitle("Accuracy performance across all subjects") +
  xlab("Trial number") + ylab("Mean Absolute Deviation (degree)") +
  facet_grid(group~.)



# The version with binning (n = 5).
bin.size = 5
allsubjects.bin <- subset( allsubjects, task != "pre_train")   
allsubjects.bin <- ddply( allsubjects.bin, c('subj.id',"task","group"), function(xx) {
  end = nrow(xx)/bin.size
  task=NULL;  PD.bin=NULL;  var.bin=NULL; num=NULL;  reward.bin=NULL
  for (i in 1:end ) {
    start = (i-1)*bin.size+1
    myrange = range( xx[start:(start+bin.size), "angle_maxv_shift"] )
    # note the notation of xx[,]
    PD.bin = c(PD.bin, mean(xx[start:(start+bin.size), "abs.PD"], na.rm=TRUE))
    var.bin = c(var.bin, myrange[2]-myrange[1])
    reward.bin = c(reward.bin, mean(xx[start:(start+bin.size), "boom"], na.rm=TRUE))
  }
  return(data.frame("abs.PD.bin" = PD.bin,
                    "var.bin" = var.bin,                                              
                    "reward.bin"=reward.bin ))
}
)

allsubjects.bin <- ddply(allsubjects.bin, c("subj.id","group"), function(xx) {
  return(data.frame(xx,"trial" = 1:nrow(xx)))
})

allsubjects.bin$task = ifelse(allsubjects.bin$task=="training","learning",as.character(allsubjects.bin$task))
allsubjects.bin$task = ifelse(allsubjects.bin$task=="pre_train","explore",as.character(allsubjects.bin$task))


error.grp.bin <- summarySE( allsubjects.bin, "abs.PD.bin", c( "task","trial","group" ), na.rm = TRUE)


ggplot(error.grp.bin, aes( x=trial, y=abs.PD.bin,
                           ymin=abs.PD.bin-se,
                           ymax=abs.PD.bin+se)) +
  scale_color_manual(values=c("pre_train"="black","learning"="blue","motor_post"="red")) +
  scale_fill_manual (values=c("pre_train"="black","learning"="blue","motor_post"="red")) +
  # Note I'm using geom_ribbon. 
  geom_line( size=0.2, aes(y=abs.PD.bin, color=task) ) + 
  geom_ribbon( aes(fill=task), alpha=0.4 ) +
  geom_smooth( method = "lm", aes(color=task) ) +
  #geom_errorbar(width=0) + 
  ylim(0,12) + 
  ggtitle("Accuracy performance across all subjects") +
  xlab("Trial number (bin=5)") + ylab("Mean Absolute Deviation (degree, binned)") + 
  facet_grid(.~group, scales="free") +
  theme_bw()










# (4) Total reinforcement.....
reward <- summarySE(my.df, "boom", c("subj.id","group"))
#reward <- summarySE(reward, "boom", c("group"))
t.test(subset(reward,group=="right" )$boom,
       subset(reward,group=="sham")$boom)



# (4) Average accuracy motor post
post.grp <- subset(allsubjects, task=="motor_post")
#post.grp <- summarySE( post.grp, "abs.PD", c("task","group"), na.rm = TRUE)
t.test( subset(post.grp,group=="right" )$abs.PD,
        subset(post.grp,group=="sham")$abs.PD )

average.post <- summarySE(post.grp, "abs.PD","group")
ggplot(average.post, aes(x=group, y=abs.PD, ymax=abs.PD+se, ymin=abs.PD-se)) + 
  geom_bar(stat="identity") + geom_errorbar(width=0) +
  theme_bw() +
  coord_cartesian(ylim=c(3,7))


post.grp.avg <- summarySE(post.grp, "abs.PD",c("subj.id","group"))

load("wm.Rdata")

# See whether we can find correlation between reduction in WM and learning!
to.plot <- join(post.grp.avg[,c(1,2,4)], diff.score.avg[,c(1,2,5)])
to.plot <- join(to.plot, reward[,c(1,2,4)])

ggplot(to.plot, aes(x=mean, y=abs.PD, col=group)) +
  geom_smooth(method="lm") +
  geom_point() + 
  theme_bw() + xlab("Difference in WM Score")







# (5) Group average for change in movement direction, exploration vs exploitation
d.m <- ddply( allsubjects,  c('subj.id','task',"group"),
              function(xx) {
                xx <- xx[ order(xx$newTrial), ]  # ensuring the order
                abs.dm<-NULL; delta.m<-NULL; next.boom<-NULL
                for (i in 1:(nrow(xx)-1)) {
                  # Accessing each trial; keep adding element to variable "delta.m"
                  delta.m  <- c(delta.m,    xx[i+1,"angle_maxv_shift"]-xx[i,"angle_maxv_shift"])
                  abs.dm   <- c(abs.dm, abs(xx[i+1,"angle_maxv_shift"]-xx[i,"angle_maxv_shift"]))
                  next.boom<- c(next.boom, xx[i+1,"boom"])
                }
                delta.m  <- c(delta.m, NA); abs.dm <- c(abs.dm, NA)
                return(data.frame(trial=xx$newTrial,
                                  block=xx$block,
                                  session=unique(xx$session),
                                  delta.m=delta.m,
                                  abs.dm = abs.dm,
                                  next.boom = c(next.boom,0),
                                  success=xx$success )) 
              }
)


# Create a new data frame that summarizes mean, sd, etc. for the different phases!
avg.trial.trial <- ddply( d.m, c("task","success","group","block"), 
                          function(xx){
                            n.subj = length(unique(d.m$subj.id))
                            c( avg = mean(xx$abs.dm, na.rm = TRUE),
                               N  = length(xx$abs.dm),
                               n.subj  = n.subj,
                               se = sd(xx$abs.dm, na.rm = TRUE)/sqrt(n.subj)) # Note!
                          })

# Create clearer naming to indicate reward status for the training phase.
avg.trial.trial$task.block = ifelse (avg.trial.trial$task=="training",
                                      sprintf("%s_%s", avg.trial.trial$task, avg.trial.trial$block), 
                                      as.character(avg.trial.trial$task))
avg.trial.trial$task.block <- as.factor(avg.trial.trial$task.block)

# Reorder the levels according to the following task phase....
task = c("pre_train", "training_1","training_2","training_3","training_4","motor_post")
avg.trial.trial$task.block <- factor(avg.trial.trial$task.block, levels = task)

ggplot(avg.trial.trial, aes(x=task.block, 
                            y=avg, 
                            ymin=avg-se,
                            ymax=avg+se,
                            group=success)) + 
  geom_point() + geom_line() +
  geom_errorbar(width=0) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Average magnitude of change in movement direction") +
  ylab("Angle (degree)") + xlab("") +
  facet_grid(.~group) +
  theme_bw() + theme( 
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
  )


##### Reward difference between block 1 and 4.
reward.blk <- summarySE(my.df, "boom", c("subj.id","group","block"))
reward.blk$boom <- reward.blk$boom * 200  # 200 is total number of training trials....
reward.blk.avg <- summarySE(reward.blk, "boom", c("group","block"))
reward.blk.avg <- subset(reward.blk.avg, reward.blk.avg$block<5)

reward.diff <- ddply( reward.blk, c("subj.id","group"), function(xx){
  early = subset(xx, block==1)
  late  = subset(xx, block==4)
  c("diff"=late$boom - early$boom) }
)

t <- t.test(subset(reward.diff,group=="right" )$diff,
            subset(reward.diff,group=="sham")$diff,
            alternative = c("less"))


sprintf("Reward increment between Sham vs TMS group, p = %f",t$p.value)

average.boom <- subset(reward.blk.avg, block %in% c(1,4))

ggplot(average.boom, aes(x=block, y=boom, ymax=boom+se, ymin=boom-se)) + 
  geom_bar(stat="identity") + geom_errorbar(width=0) +
  theme_bw() +
  coord_cartesian(ylim=c(40,100)) +
  facet_grid(.~group)


require(ez)

ezANOVA( data=subset(reward.blk,block %in% c(1,4)),
         dv=boom,
         wid=subj.id,
         within=block,
         between=group )








# (6) Get motor performance index; list down all possible kinematics/motor performance measures
#     NOTE = Here, abs.PD is the absolute of angle_maxv_shift.
#     It is good to include direction as one of the grouping variable too!
#     NOTE (Feb 2018): How do you compute the slope?

motor.score <- ddply( allsubjects, c("subj.id","task"), 
                      function(xx) {
                        # To disregard the last few movements during post?
                        if (unique(xx$task=="motor_post")){
                          xx <- subset(xx, Trial_block<=15)
                        }
                        # To model the learning slope?
                        eq.abs  <- lm(abs.PD ~ trial, xx )
                        eq.bias <- lm(angle_maxv_shift ~ trial, xx )
                        c("lag" = unique(xx$lag),
                          "n.boom"     = sum(xx$boom),        # total rewarded trials; use sum()!!!
                          "mean.PD"    = mean(xx$angle_maxv_shift),      # bias measure
                          "mean.absPD" = mean(xx$abs.PD),                # mean abs Error
                          "var.PD"     = sd(xx$angle_maxv_shift),        # trial movement var (signed)
                          "mean.abs.dm"= mean(xx$abs.dm, na.rm = TRUE),  # mean abs change in direction
                          "var.dm"     = sd(xx$delta.m, na.rm = TRUE),   # var in change in direction
                          "slope.abs"  = coef(eq.abs)[["trial"]] )       # slope of abs Error (whole)
                      } )



# It's also nice to focus more on training blocks only! In particular, we can quantify the
# slope of absPD, increment in boom, and change in var between block 1 and 4.
train.score <- ddply( subset(allsubjects, task=="training"), c("subj.id"), 
                      function(xx) { 
                        # Be careful, we don't want "block" column but "block2" column................
                        early <- subset(xx, (block2==1))
                        #late  <- subset(xx, (block2==4 & Trial_block>=41))
                        #early <- subset(xx, (block2 %in% c(1,2)))
                        late  <- subset(xx, (block2 %in% c(3,4)))
                        #mysign <- sign(mean(pre$error))
                        #mysign <- sign(abs(mean(pre$error))-abs(mean(post$error))) 
                        early.succ <- subset(early, boom==1)                     
                        late.succ  <- subset(late, boom==1)
                        early.fail <- subset(early, boom==0)                      
                        late.fail  <- subset(late, boom==0) 
                        succ  <- subset(xx, boom==1)
                        fail  <- subset(xx, boom==0)
                        c( "lag" = unique(xx$lag),
                           "succ.dm"    = mean(succ$abs.dm, na.rm = TRUE),
                           "fail.dm"    = mean(fail$abs.dm, na.rm = TRUE),
                           "early.boom" = sum(early$boom),
                           "early.succ" = mean(early.succ$abs.dm, na.rm = TRUE),
                           "early.fail" = mean(early.fail$abs.dm, na.rm = TRUE),
                           "n.boom"     = sum(xx$boom),
                           "late.boom"  = sum(late$boom),
                           "late.succ"  = mean(late.succ$abs.dm, na.rm = TRUE),
                           "late.fail"  = mean(late.fail$abs.dm, na.rm = TRUE),
                           "m.abs"      = coef( lm(abs.PD ~ trial, xx ))[["trial"]],
                           "delta.boom" = sum(late$boom) - sum(early$boom),  # use sum()
                           "delta.var"  = var(late$angle_maxv_shift) - var(early$angle_maxv_shift),
                           "delta.bias" = mean(late$angle_maxv_shift) - mean(early$angle_maxv_shift),
                           "delta.abs"  = mean(early$abs.PD) - mean(late$abs.PD),
                           "delta.succ" = mean(early.succ$abs.dm, na.rm = TRUE) -
                             mean(late.succ$abs.dm, na.rm = TRUE),
                           "delta.fail" = mean(early.fail$abs.dm, na.rm = TRUE) -
                             mean(late.fail$abs.dm, na.rm = TRUE),
                           "early.absPD"= mean(early$abs.PD),#var(early$angle_maxv_shift),
                           "late.absPD" = mean(late$abs.PD),
                           "m.early.abs" = coef( lm(abs.PD ~ trial, early ))[["trial"]],
                           "m.late.abs"  = coef( lm(abs.PD ~ trial, late  ))[["trial"]]
                        )
                      } )
train.score$m.abs*1000


# David suggested we combined 2 sessions as one long vector for each subject.
two.session <- ddply( subset(allsubjects, task=="training"), c("subj.id"), 
                      function(xx) { 
                        # Extract df for only session 1 and 2, then do row bind to concatenate them
                        session1 <- subset(xx, session==1)
                        session2 <- subset(xx, session==2)
                        one.vector <- rbind(session1,session2)
                        one.vector$trial <- 1:nrow(one.vector)
                        one.vector$color <- ifelse((one.vector$trial<=15|one.vector$trial>=nrow(one.vector)-20) ,
                                                   "red", "green") 
                        early <- subset(xx, (block2==1 & Trial_block<=20))
                        late  <- subset(xx, (block2==4 & Trial_block>=41))
                        ggplot(one.vector, aes(x=trial, y=abs.PD, color=as.factor(color) )) + geom_point()
                        #mysign <- sign(mean(pre$error))
                        #mysign <- sign(abs(mean(pre$error))-abs(mean(post$error))) 
                        c( "m.abs"  = coef( lm(abs.PD ~ trial, one.vector ))[["trial"]],
                           "m.bias" = coef( lm(angle_maxv_shift ~ trial, one.vector))[["trial"]],
                           "delta.abs" = mean(subset(one.vector, trial<=15)$abs.PD) -
                             mean(subset(one.vector, trial>=nrow(one.vector)-40)$abs.PD)
                        )
                      } )



# (Nov 13) The ratio between exploration & exploitation for each subject during training! 
reinf_index <- ddply( subset(motor.score, task=="training"), c("subj.id"), 
                      function(xx) { 
                        num   = subset(xx, boom==0)
                        denom = subset(xx, boom==1)
                        c( "ratio.var.dm" = num$var.dm/denom$var.dm,
                           "ratio.abs.PD" = num$mean.absPD/denom$mean.absPD) 
                      } )

train.score <- merge(train.score, reinf_index)

# (Update: Jan 19, 2018) I also added the difference in accuracy POST and PRE_TRAIN. 
post.score <- ddply( subset(allsubjects, task!="training"), c("subj.id"), 
                     function(xx) { 
                       pre  = subset(xx, task=="pre_train")
                       post = subset(xx, task=="motor_post" & Trial_block<=25)
                       shift = subset(xx, task=="motor_post")$amount_shifted_deg
                       c( "lag" = unique(xx$lag),
                          "test.delta.abs"  = mean( abs(post$angle_maxv_shift - mean(shift)) ) - 
                            mean(post$abs.PD,na.rm = TRUE), 
                          "test.delta.bias" = (mean(pre$angle_maxv_shift,na.rm = TRUE) - mean(shift)) -
                            mean(post$angle_maxv_shift,na.rm = TRUE),
                          "test.delta.var"  = var(post$angle_maxv_shift,na.rm = TRUE) - 
                            var(pre$angle_maxv_shift,na.rm = TRUE),
                          "test.delta.rng"  = (range(post$angle_maxv_shift)[2]-range(post$angle_maxv_shift)[1])-  
                            (range(pre$angle_maxv_shift)[2]-range(pre$angle_maxv_shift)[1])
                       )
                     } )



# More elaborate plotting based on movement (target) direction (Feb 2018)

###### Incremental reward is correlated with improvement in accuracy??
my.df <- merge(post.score, train.score)
ggplot(my.df, aes(x=test.delta.abs, y=delta.boom)) + geom_point() + 
  geom_smooth(method="lm", colour="red") +
  facet_grid(.~session)
ggplot(my.df, aes(x=test.delta.bias, y=delta.boom)) + geom_point() + 
  geom_smooth(method="lm", colour="red") +
  facet_grid(.~session)
ggplot(my.df, aes(x=-test.delta.var, y=delta.boom)) + geom_point() + 
  geom_smooth(method="lm", colour="red") +
  facet_grid(.~session)


###### Total reward is correlated with improvement in accuracy??
post.score  = subset(motor.score, task=="motor_post")[,-c(2,3,6)]
train.df = subset(train.score)[,-c(2,3)]

plot(train.df$delta.abs, train.df$delta.boom)
abline(lm(train.df$delta.boom ~ train.df$delta.abs))
cor.test(train.df$delta.abs, train.df$delta.boom)


rm(train.df,post.score)



# (7) The nearer you are to the target direction, the less you change the movement direction (delta.m) !!
# Try to see if angular deviation is able to predict delta.m.
temp <- subset(allsubjects, task=="training" & !is.na(delta.m))
ggplot(temp, aes(x=angle_maxv_shift, y=delta.m, color=as.factor(boom))) + 
  geom_point(alpha=0.15) +
  labs(x="Angular deviation (signed, degree)", y="Change in direction (signed, degree)")

# Test of correlation should be done on the subject level rightfully!
#cor.test(temp$delta.m, temp$angle_maxv_shift)
# Following the boom, the corr coef between the two measures goes to zero...
rm(temp)




# (8) Distribution of # consecutive rewards!
#     UPDATED (Jan 30, 2018)

source("../my_functions.R")

####### What happens to change in direction as a function of consecutive trials!
temp <- consec.df(subset(allsubjects, task=="training"))
#temp

## This is for consecutive successful trials. Find the average based on different cons. success!
df1 <- ddply(temp, c("subj.id","cons_succ"), summarize, "N"=length(cons_succ),
             "inter" =mean(inter_succ, na.rm=TRUE) )
# Note, throw away when consec. reward < 0 as it's useless, count #subjects...
df1 <- subset(df1, cons_succ>=0)
dm1 <- summarySE(df1, "inter", "cons_succ", na.rm=TRUE)
colnames(dm1)[1] <- "cons.count"
dm1$type <- "success"


## Now for consecutive successful trials. Find the average based on different cons. failed trials!
df2 <- ddply(temp, c("subj.id","cons_fail"), summarize, "N"=length(cons_fail),
             "inter" =mean(inter_fail, na.rm=TRUE) )
df2 <- subset(df2, cons_fail>=0)
dm2 <- summarySE(df2, "inter", "cons_fail", na.rm=TRUE)
#dm2[5,3]=dm2[5,3]*0.9
colnames(dm2)[1] <- "cons.count"
dm2$type <- "failures"

# The final data frame containing change in direction as a function of consecutive reward status
consec.dm <- rbind(subset(dm1,cons.count<=5), subset(dm2,cons.count<=10))
rm(dm1,dm2,df1,df2)

# Change in delta.m contingent upon consecutive reward status.....
ggplot(subset(consec.dm,cons.count>0), aes(x=cons.count, y=inter, ymin=inter-se, ymax=inter+se)) + 
  geom_point() + 
  geom_errorbar(width=0) +
  geom_smooth(method='lm') +
  xlab("Number of consecutive trials after rewarded current trial-n") + 
  ylab("Average dm (angle, absolute)") +
  theme(legend.position="none") + ylim(1,11) +
  facet_grid(.~type)






# (9) Location of the reward zone (target direction) along the arc
#     UPDATED (Feb 10, 2018)
target.df <- ddply( subset(allsubjects, task=="motor_post"), 
                    c("subj.id","direction"), 
                    summarize, "target"=unique(amount_shifted_deg) )

target.df <- merge(target.df, subset(motor.score,task=="motor_post" ))
target.df$target2 <- ifelse(target.df$direction==45, target.df$target+135, -1*target.df$target+45)

#ggplot(target.df, aes(y=target) ) + geom_bar(stats="identity")# + facet_grid(.~direction)

hist(target.df$target2, breaks=90)




