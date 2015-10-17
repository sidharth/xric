id_age <- read.csv("~/Desktop/ml/xrci/id_age_train.csv")
id_label <- read.csv("~/Desktop/ml/xrci/id_label_train.csv")
id_time_labs <- read.csv("~/Desktop/ml/xrci/id_time_labs_train.csv")
id_time_vitals <- read.csv("~/Desktop/ml/xrci/id_time_vitals_train.csv") 

num_of_obs <- dim(id_age)[1]

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

age_label <- merge(id_age, id_label)
colnames(age_label) <- c("id", "age", "label")
all_obs <- merge(id_time_vitals, id_time_labs, by = c("id", "timestamp"))

complete_data <- merge(all_obs, age_label,by = "id")
rm(all_obs)

filled_data_survived <- complete_data[complete_data$label == 0, ]
filled_data_dead <- complete_data[complete_data$label == 1, ]

for (i in 3:34) {
    n <- sum(is.na(filled_data_dead[[i]]))
    filled_data_dead[ is.na(filled_data_dead[[i]]) , i] <- rnorm(n, mean = mean(filled_data_dead[[i]], na.rm = TRUE), sd = sd(filled_data_dead[[i]], na.rm = TRUE))
    # print(paste(colnames(filled_data_dead)[i], sum(is.na(filled_data_dead[[i]]))))
    
    n <- sum(is.na(filled_data_survived[[i]]))
    filled_data_survived[ is.na(filled_data_survived[[i]]) , i] <- rnorm(n, mean = mean(filled_data_survived[[i]], na.rm = TRUE), sd = sd(filled_data_survived[[i]], na.rm = TRUE))
    # print(paste(colnames(filled_data_dead)[i], sum(is.na(filled_data_dead[[i]]))))
}

complete_data <- rbind(filled_data_dead, filled_data_survived)
complete_data <- subset(complete_data, select = -c(urine))

mean_obs <- aggregate(complete_data, list(complete_data$id), mean, na.rm = TRUE)
max_obs <- aggregate(complete_data, list(complete_data$id), max, na.rm = TRUE)
min_obs <- aggregate(complete_data, list(complete_data$id), min, na.rm = TRUE)
sd_obs <- aggregate(complete_data, list(complete_data$id), sd, na.rm = TRUE)

sd_obs$id <- sd_obs$Group.1 #Because standard deviation of id's are zero
sd_obs$label <- age_label$label

mean_obs$label <- age_label$label

lr_data <- data.frame(id = mean_obs$id)
lr_data <- cbind(lr_data, mean_obs[,4:9], mean_obs[, 11:35])
# lr_data <- cbind(lr_data, min_obs[,4:9], min_obs[, 11:35])
# lr_data <- cbind(lr_data, max_obs[,4:9], max_obs[, 11:35])
# lr_data <- cbind(lr_data, sd_obs[,4:9], sd_obs[, 11:35])
#lr_data <- cbind(lr_data, icu_time = icu_time_df$X2)
lr_data <- cbind(lr_data, min_obs$age)
lr_data <- cbind(lr_data, label = age_label$label)
is.na(min_obs) <- sapply(min_obs, is.infinite)
is.na(max_obs) <- sapply(max_obs, is.infinite)
lr_data[is.na(lr_data)] <- 0
# lr_data_colnames <- colnames(lr_data)
num_col_for_rename <- length(colnames(lr_data)) - 2
colnames(lr_data) <- c("id", paste(rep("X", num_col_for_rename), 1:num_col_for_rename, sep = ""), "label")

test <- min_obs$id >= 2000 & min_obs$id < 3000

training_set <- lr_data[!test,]
testing_set <- lr_data[test,]
reg_model <- glm(label ~ . - id , data = training_set,family = "binomial")
# reg_model <- randomForest(label ~ . - id, data = training_set)
predictions <- predict(reg_model, testing_set, type = "response")
testing_set <- cbind(testing_set, prob = predictions)

table(testing_set$prob >= 0.50, testing_set$label == 1)
# qplot(testing_set$id, testing_set$prob, col = testing_set$label)

testing_set <- complete_data[complete_data$id >= 2000 & complete_data$id < 3000,]
testing_set <- data.frame(testing_set)
testing_set <- arrange(testing_set, id, timestamp)

answer <- data.frame()
# colnames(answer) <- c("id", "timestamp", "label")
for (i in unique(testing_set$id)) {
    patient <- complete_data[complete_data$id == i,]
    patient <- arrange(patient, timestamp )
    num_records <- dim(patient)[1]
    for (j in 1:num_records) {
        if (patient$icu[j] == 1) {
            running_mean <- patient[1:j,]
            running_mean <-  aggregate(. ~ id, data = running_mean, mean, na.rm = TRUE)
            running_mean <- cbind(Group.1 = running_mean$id, running_mean)
            running_mean <- cbind(running_mean$id, running_mean[, 4:9], running_mean[, 11:35], age_label$age[i])
            num_col_for_rename <- length(colnames(running_mean)) - 1
            colnames(running_mean) <- c("id", paste(rep("X", num_col_for_rename), 1:num_col_for_rename, sep = ""))
            prediction <- predict(reg_model, running_mean, type = "response")
            answer <-  rbind(answer, c(i,patient$timestamp[j],prediction[1]))
            # print(c(i,patient$timestamp[j],prediction[1]))
        }
    }
    print(i)
}
colnames(answer) <- c("id", "timestamp", "prob")
answer$label <- answer$prob > 0.5

median_time <- c()
for (i in unique(answer$id)) {
    patient <- answer[answer$id == i,]
    len <- dim(patient)[1]
    for (j in 1:len) {
        if (patient$label[j] == TRUE) {
            median_time <- c(median_time, patient$timestamp[len] - patient$timestamp[j])
            break
        }
    }
}
median(median_time)


bytimestamp <- aggregate(label ~ id, data = answer, sum)
table(age_label[age_label$id >= 2000 & age_label$id < 3000, c("label")] == 1, bytimestamp$label > 0)
