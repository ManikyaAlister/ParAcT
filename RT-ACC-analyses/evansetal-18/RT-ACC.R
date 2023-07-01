library(here)
library(dplyr)

n = 9
allData = array()
for (i in 1:n){
load(paste0("data/evansetal-17/clean/P",i,".Rdata"))
allData=rbind(allData,data)
}
allData = allData[2:length(allData[,1]),]

sum = allData %>%
  group_by(Trial) %>%
  summarise(mean = mean(Time),
            median = median(Time),
            acc = mean(Resp)
            )

plot(1:length(sum$Trial),sum$mean,"l")
plot(1:length(sum$Trial),sum$acc)

plot(1:length(unique(allData$Trial)))