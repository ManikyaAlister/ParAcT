
# Data structure with relevant file paths for all data sets ---------------
dataset_details <-
  data.frame(
    dataset_number = c(1, 2, 3), # data set number in manuscript 
    dataset_id = c("evans-optim", "evans-normal", "knowles"), # id name 
    dataset = c("evansetal-17", "evansetal-17", "knowlesetal-19"), # study data set is from 
    subvariant = c("/optim", "/normal", ""), # experiment condition in study
    raw_data_file = c("Optim-Trial", "Norm-Trial", "")
  )

save(dataset_details, file = here("data/dataset-details.Rdata"))
