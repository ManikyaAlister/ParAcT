library(tidyverse)
setwd("~/cloudstor/Gaze-Cueing/Data/dataset2/clean")
d = data
d = array(dim = 8)
d$trial = 1:length(d$ID)

for (i in 1:50) {
  load(paste("P",i,".RData",sep = ""))
  data$trial = 1:length(data$ID)
  d = rbind(d,data)
}
 d = d[-1,]

 sum = d %>% 
  group_by(trial) %>%
  summarise(mean = mean(Time),
            median = median(Time),
            acc = sum(Resp == 2))
  
plot(sum$trial,sum$mean,"l")
plot(sum$trial[1:30],sum$mean[1:30],"l")

plot(sum$trial,sum$median,"l")
plot(sum$trial,sum$acc)

plot(d$trial,d$Time)

setwd("~/cloudstor/Gaze-Cueing/Data/dataset1a/clean")
d = array(dim = 8)

for (i in 1:41) {
  load(paste("P",i,".RData",sep = ""))
  data$trial = 1:length(data$ID)
  d = rbind(d,data)
}
d = d[-1,]

sum = d %>% 
  group_by(trial) %>%
  summarise(mean = mean(Time),
            median = median(Time),
            acc = sum(Resp == 2))

plot(sum$trial,sum$mean,"l")
plot(sum$trial[1:30],sum$mean[1:30],"l")

plot(sum$trial,sum$median,"l")
plot(sum$trial,sum$acc)


setwd("~/cloudstor/Gaze-Cueing/Data/dataset3/clean")
d = array(dim = 8)

for (i in 1:71) {
  load(paste("P",i,".Rdata",sep = ""))
  data$trial = 1:length(data$PID)
  data$trialsTotal = length(data$PID)
  d = rbind(d,data)
}
d = d[-1,]

sum = d %>% 
  group_by(trial) %>%
  summarise(mean = mean(Time),
            median = median(Time),
            acc = sum(Resp == 2))

plot(sum$trial,sum$mean,"l")
plot(sum$trial[1:30],sum$mean[1:30],"l")

plot(sum$trial,sum$mean,"l")
plot(sum$trial,sum$acc)

nTrials = d %>%
  group_by(PID)%>%
  summarise(max(trial))
