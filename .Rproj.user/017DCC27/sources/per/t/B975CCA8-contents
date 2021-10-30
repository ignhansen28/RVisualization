#read csv
participant <- read.csv("Participant.csv", fileEncoding = "UTF-8-BOM")
question <- read.csv("Question.csv", fileEncoding = "UTF-8-BOM")
result <- read.csv("Result.csv", fileEncoding = "UTF-8-BOM")

View(result)  

#remove na value
result <- na.omit(result)

#1. Visualize a pie chart with its legend, illustrating the result of the second question.
data_1 <- table(result$Question.2)
label_1 <- data_1 / sum(data_1) * 100
label_1 <- round(label_1,1)
label_legend_1 <- c("Strongly Disagree", 
                    "Disagree",
                    "Neutral",
                    "Agree",
                    "Strongly Agree")

pie(data_1, label_1, col = rainbow(5),
    main = "Result Percentage of Question 2: The lecturers of Bluejack University are masters of the corresponding courses taught to me")

legend("topright", label_legend_1, fill = rainbow(5), cex = 0.5)

#2. Visualize a pie chart with its legend, illustrating the result of the sixth question for female participants. 
data_merge <- merge(participant, result, by = "Participant.Number")

female_participant <- data_merge$Question.6[data_merge$Gender=="Female"]

female_participant_table <- table(female_participant)
female_participant_label <- female_participant_table / sum(female_participant_table) *100
female_participant_label <- round(female_participant_label, 1)

pie(female_participant_table, female_participant_label, col = c("Red", "LightBlue"), 
    main = "Result Percentage of Question 6 for Female Participants: The sanitaion facility of Bluejack University is well maintained and hygenic.")

female_participant_legend <- c("False", "True")

legend("topright", female_participant_legend, fill = c("Red", "LightBlue"), cex = 0.5)

#3. Visualize a group bar chart with its legend, illustrating the result of the first question and the gender of the participant.
data_merge <- merge(participant, result, by = "Participant.Number")
data_2 <- table(data_merge$Gender, data_merge$Question.1)

barplot(data_2, names.arg = label_legend_1, col = c("red", "orange"), main = "Result")

legend("topleft", legend = rownames(data_2), fill = c("red", "orange"))


#4. Visualize a histogram, illustrating the result of the fourth question.
data_merge <- merge(participant, result, by = "Participant.Number")

boxplot(Question.1 ~ Gender, data = data_merge, ylab = "Question 1 Answer")

#5.	Visualize a histogram, illustrating the result of the fourth question.
data_3 <- table(result$Question.4)

hist(result$Question.4, breaks= 1:5, ylim = c(0,50), col = rainbow(4), xlab="Question 4 Answers")

#6. Visualize a stacked bar chart, illustrating the results of question 1 to 5.
data_5 <- data.frame(
  question1 = as.vector(table(result$Question.1)),
  question2 = as.vector(table(result$Question.2)),
  question3 = as.vector(table(result$Question.3)),
  question4 = as.vector(table(result$Question.4)),
  question5 = as.vector(table(result$Question.5))
)

barplot(as.matrix(data_5), col = rainbow(5), ylim = c(0,100))

#7. Visualize a dot chart and a line chart, illustrating the amount of survey participants each day.
data_6 <- table(result$Date)

data_6_df <- data.frame(
  date = rownames(data_6),
  count = as.vector(data_6)
)

data_6_df2 <- data.frame(
  date = "7/15/2018",
  count = 0
)

merge_data_6 <- rbind(data_6_df, data_6_df2)
merge_data_6 <- merge_data_6[order(merge_data_6$date),]

plot(merge_data_6$count, type = "o", xaxt ="n")

axis(1, at=1:length(merge_data_6$date), labels = merge_data_6$date)

























