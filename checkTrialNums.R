lengths = c()
for ( useSub in 1:10){
  load(here(paste0("data/evansetal-17/clean/P",useSub,"-Optim-Trial.Rdata")))
  lengths[useSub] <- length(data[,1])
}

lengths


lengths = c()
for ( useSub in 1:11){
  load(here(paste0("data/evansetal-17/clean/P",useSub,"-Norm-Trial.Rdata")))
  lengths[useSub] <- length(data[,1])
}
lengths


lengths = c()
for ( useSub in 1:10){
  load(here(paste0("modelling/evansetal-17/optim/round-1/06_output/P",useSub,"_a-blocked-complex.Rdata")))
  lengths[useSub] <- length(data[,1])
}
lengths



lengths = c()
for ( useSub in 1:147){
  load(here(paste0("data/knowlesetal-19/clean/P",useSub,".Rdata")))
  lengths[useSub] <- length(data[,1])
}
lengths


t = data %>%
  filter(feedbackType =="Norm" & blockType == "Trial") %>%
  group_by(subject) %>%
  summarise(trials = length(trlNum), blocks = length(unique(blkNum)))
