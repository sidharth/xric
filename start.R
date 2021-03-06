# Load all data sets change according to your needs
id_age <- read.csv("~/Desktop/ml/xrci/id_age_train.csv")
id_label <- read.csv("~/Desktop/ml/xrci/id_label_train.csv")
id_time_labs <- read.csv("~/Desktop/ml/xrci/id_time_labs_train.csv")
id_time_vitals <- read.csv("~/Desktop/ml/xrci/id_time_vitals_train.csv") 

# Number of observations variable which is 3964
num_of_obs <- dim(id_age)[1]

# Make ID and LABEL a factor variable in data sets
# id_age$ID <- factor(id_age$ID)
# id_label$ID <- factor(id_label$ID)
# id_label$LABEL <- factor(id_label$LABEL, labels = c("Dead", "Alive"))
# id_time_labs$ID <- factor(id_time_labs$ID)
# id_time_vitals$ID <- factor(id_time_vitals$ID)

colnames(id_time_vitals) <- c("id",
                              "timestamp",
                              "systolic",
                              "diastolic",
                              "respirate_rate",
                              "heart_rate",
                              "oxygensaturation",
                              "temprature",
                              "icu")
colnames(id_time_labs) <- c ("id",
                             "timestamp",
                             "ph",
                             "carbondioxide",
                             "oxygenpressure",
                             "sodium",
                             "potassium",
                             "bicarbonate",
                             "bloodurea",
                             "serum",
                             "wbc",
                             "hematocrit",
                             "platelet",
                             "bilirubin",
                             "urine",
                             "cholestrol",                             
                             "lacticacid",
                             "troponini",
                             "troponint",
                             "randglucose",
                             "fastglucose",
                             "fio2",
                             "albumin",
                             "phosphate",
                             "alanine",
                             "hdlcholestrol",
                             "magnesium")

# Merging tables
age_label <- merge(id_age, id_label)
colnames(age_label) <- c("id", "age", "label")
all_obs <- merge(id_time_vitals, id_time_labs, by = c("id", "timestamp"))

complete_data <- merge(all_obs, age_label,by = "id")
# complete_data$label <- factor(complete_data$label, levels = c(0,1), labels = c("Survived", "Not survived"))
# For generating plots of all variables wrt to patient id
library(ggplot2)
for (i in 3:34) {
    qplot(complete_data$id,complete_data[[i]], xlab = "id", ylab = colnames(complete_data)[i], col = complete_data$label)
    ggsave(filename = paste(colnames(complete_data)[i] ,"-vs-id.jpg"), plot = last_plot() )
}

qplot(max_obs$id,complete_data[[4]], xlab = "id", ylab = colnames(max_obs)[4], col = max_obs$label)
ggsave(filename = paste(colnames(complete_data)[i] ,"-vs-id.jpg"), plot = last_plot() )

#histogram of age
hist(age_label$AGE)
dev.copy(jpeg, filename = "age-hist.jpg")
dev.off()

#calculating number of observations for each test case
obs_count_for_each_test <- c()
for (i in 3:34) {
    obs_count_for_each_test <- rbind(obs_count_for_each_test, c(colnames(all_obs)[i],sum(!is.na(all_obs[[i]]))))
    # print(paste(colnames(all_obs)[i],sum(!is.na(all_obs[[i]])), sep = " "))
}
obs_count_for_each_test <- data.frame(obs_count_for_each_test)
colnames(obs_count_for_each_test) <- c("Test", "Frequency")
obs_count_for_each_test$Frequency <- as.numeric(as.character(obs_count_for_each_test$Frequency))

#Mean of all tests
mean_obs <- aggregate(complete_data, list(complete_data$id), mean, na.rm = TRUE)
max_obs <- aggregate(complete_data, list(complete_data$id), max, na.rm = TRUE)
min_obs <- aggregate(complete_data, list(complete_data$id), min, na.rm = TRUE)
sd_obs <- aggregate(complete_data, list(complete_data$id), sd, na.rm = TRUE)

# max_obs2 <- aggregate(complete_data, list(complete_data$id), max, )
max_obs2 <- max_obs[is.finite(max_obs$diastolic),]
qplot(max_obs2$id,max_obs2[[4]], xlab = "id", ylab = colnames(max_obs2)[4], col = max_obs2$label)

sd_obs$id <- sd_obs$Group.1 #Because standard deviation of id's are zero
sd_obs$label <- age_label$label

# Filling na

# for (i in 4:37) {
#   qplot(mean_obs$id,mean_obs[[i]], xlab = "id", ylab = colnames(mean_obs)[i], col = mean_obs$label)
#   ggsave(filename = paste(colnames(complete_data)[i] ,"-vs-id.jpg",sep = ""), plot = last_plot() )
# }




                                    