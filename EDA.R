#####installing libraries
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(lazyeval)
library(readr)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(funModeling)
library(scales)
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(gbm)
library(choroplethr)
library(choroplethrMaps)
library(microbenchmark)
library(doParallel)
library(e1071)
library(ggthemes)
library(RColorBrewer)
library(funModeling)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(mvinfluence)
library(car)
library(rgl)
library(xgboost)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(glmnet)
library(mlbench)
#library(psych)
library(dplyr)
library(lattice)
library(xgboost)


# set working directory
credit_data <- read.csv("C:/Users/srivi/OneDrive/Desktop/Excelr/Project/data.csv", stringsAsFactors = FALSE)
str(credit_data)

credit_data$grade <- factor(credit_data$grade, 
                        c("A", "B", "C", "D", "E", "F", "G"), ordered=TRUE)
credit_data$terms <- factor(credit_data$terms, 
                            c("36 months","60 months"), ordered=TRUE)

##Selecting the important variables
credit <- credit_data %>%
  select(loan_amnt,terms,Rate_of_intrst,annual_inc,delinq_2yrs,numb_credit,total.revol_bal,total_credits,total_rec_int,total_rec_late_fee,tot_colle_amt,tot_curr_bal)
summary(credit)


#####removing NA values
credit <- credit %>%
  filter(!is.na(annual_inc) ,!is.na(tot_curr_bal),
         !is.na(total_rec_int),!is.na(delinq_2yrs),!is.na(numb_credit),!is.na(total_credits),!is.na(tot_colle_amt))
summary(credit)
str(credit)

library(dplyr)
library(tidyr)

library(ggplot2)
library(scales)


#---------------------------UNIVARIATE ANALYSIS-------------------------------------------------------------------
#plot1 is for showing how many applicants have applied for loan for 36 months term or 60 months term
plot1 <- ggplot(credit_data, aes(x = factor(terms))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="terms", y="count")
plot1
# plot 1 shows 70% applicants have applied for 36 months term

#------------------------------------------------------------------------------------------------------------
#plot 2 shows 60% of loan applicant applied loans for debt consolidation purpose.
plot2 <- ggplot(credit_data, aes(x = factor(purpose, levels = names(sort(table(purpose), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="purpose", y="Frequency") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot2



#It would seem that the vast majority of loan applicants applied for the purpose of debt consolidation, with a smaller, but still significant number for credit card debts. 

#------------------------------------------------------------------------------------------------------------

# plot3 shows 50% of loan applicant applied loans have Mortagage and 40% have rented houses.
plot3 <- ggplot(credit_data, aes(x = factor(home_ownership, levels = names(sort(table(home_ownership), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Home ownership", y="Frequency")
plot3

#-----------------------------------------------------------------------------------------------------------

# plot4 shows 29% of loan applicant applied loans have comes under grade B.
plot4 <- ggplot(credit_data, aes(x = factor(grade, levels = names(sort(table(grade), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Grade", y="Frequency")
plot4

#---------------------------------------------------------------------------------------------------------

# plot5 shows 56% of loan applicant applied loans have comes nill inquiry in past 6 months.
plot5 <- ggplot(credit_data, aes(x = factor(inq_last_6mths, levels = names(sort(table(inq_last_6mths), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="inquiry in last 6 months", y="Frequency")
plot5

#-------------------------------------------------------------------------------------------------------------

# plot6 shows nearly 85% of loan applicant have nill derogatory public records.
plot6 <- ggplot(credit_data, aes(x = factor(pub_rec, levels = names(sort(table(pub_rec), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Derogatory public records", y="Frequency")
plot6

#-----------------------------------------------------------------------------------------------------------
# Plot7 shows 32% of loan applicant have 10+ years of Exprience
plot7 <- ggplot(credit_data, aes(x = factor(Experience, levels = names(sort(table(Experience), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="experience", y="count")
plot7
#-------------------------------------------------------------------------------------------------------

# plot8 shows 30% of loan applicant have not verified status.
plot8 <- ggplot(credit_data, aes(x = factor(verification_status, levels = names(sort(table(verification_status), decreasing=TRUE))))) +
  geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="verification status", y="count")
plot8
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
###intrest rate
summary(credit_data$Rate_of_intrst)

ggplot(data = credit_data, aes(Rate_of_intrst)) + geom_histogram() +
  geom_vline(xintercept = median(credit_data$Rate_of_intrst), color = "red") +
  geom_vline(xintercept = mean(credit_data$Rate_of_intrst), color = "blue") +
  labs(x = "interest rate (%)")
summary(credit_data$int_rate)

#####The red line represents the median value and the blue line the mean. With the
#mean, median, and mode all centered around the 13% mark, the distribution looks 
#almost normal between 5% and 20% with a sharp fall-off after 20%. In fact, only 
#7% of loans had interest rates above 20%.


#---------------------------------------------------------------------------------------------------------------
###Now let's look at the actual amount of money loaned out

ggplot(data = credit_data, aes(loan_amnt)) + geom_histogram() +
  labs(x = "loan amount ($)") + 
  geom_vline(xintercept = median(credit_data$loan_amnt), color = "red") +
  geom_vline(xintercept = mean(credit_data$loan_amnt), color = "blue")



#We notice from this plot the distinct peaks at "nice" amounts like \10 000, \15000, and \20000, which is to be expected. The red line in the above plot indicates the median of \$13000 and the blue line the mean of \14755. This indicates an overall right-skewed distribution with a larger number of loanssmaller than the mean rather than larger. We can see the distribution tapering into the higher values with, however, another peak at what appears to be a hard cap on maximum loans allowed to be given out of \$35 000.

#With these loan amounts in mind, we will now examine the annual income of loanapplicants.

#---------------------------------------------------------------------------------------------------------------
#now we will look into annual_income

ggplot(data = credit_data, aes(annual_inc)) + geom_histogram(binwidth=5000) +
  geom_vline(xintercept = median(credit_data$annual_inc, na.rm=TRUE), color = "red") +
  geom_vline(xintercept = mean(credit_data$annual_inc, na.rm=TRUE), color = "blue") +
  labs(x = "annual income ($)") +
  coord_cartesian(
    xlim = c(0, quantile(credit_data$annual_inc, probs = 0.99, na.rm = TRUE))) 

ggplot(data = credit_data, aes(annual_inc)) + geom_histogram(bins=60) +
  scale_x_log10(breaks = seq(0, 250000, 50000)) +
  labs(x = "annual income") +
  coord_cartesian(
    xlim = c(quantile(credit_data$annual_inc, probs = 0.01, na.rm = TRUE), 
             quantile(credit_data$annual_inc, probs = 0.99, na.rm = TRUE)))

summary(credit_data$annual_inc)


#Once again, the red line represents the median and the blue, the mean.As we can see from these plots, the distribution for the annual income is right-skewed with a long tail and extreme outliers (a maximum value of \9500 000).Transformed with a log(10) x-axis, the distribution appears to be somewhat normal. This means that there appear to be individuals with very high annual incomes (into the high 6-figures and beyond) that are applying for relatively small loans (up to a maximum of \35000). We should now look at who exactly the people taking out these loans are.



#---------------------------------------------------------------------------------------------------------------
#--------------------------multi-variate anlysis-----------------------------------------------------------

#relation between------- Terms& rate of intrest------Terms&Loan amount-----------------

ggplot(data = credit_data, aes(x = terms, y = Rate_of_intrst)) +
  geom_boxplot() +
  labs(y = "interest rate (%)")
ggplot(data = credit_data, aes(x = terms, y = loan_amnt)) +
  geom_boxplot() +
  labs(y = "loan amount ($)")

#We see immediately that loans with 60-month terms have on average almost 4%  higher interest than their shorter term counterparts. Continuing to plot versus term, we find also that the longer terms are associated with significantly larger loan amounts as well. The median loan amount for 60 months is almost double the amount taken in 36-month loans.


# Function to determine percentage breakdowns of var1 within each
# value of var2

percent_breakdown <- function(data, var1, var2) {
  
  sum_by <- data %>%
    select_(var1, var2) %>%
    group_by_(var2, var1) %>%
    summarise(count = n())
  
  count_by <- by(sum_by$count, sum_by[[var2]], sum)
  
  count_by<- sapply(count_by, I)
  
  count_by <- data.frame(levels(sum_by[[var2]]), count_by)
  
  colnames(count_by) <- c(var2, "total")
  
  breakdown <- merge(sum_by, count_by, by = var2)
  breakdown$percentage <- breakdown$count / breakdown$total
  
  return(breakdown)
}



#With the percentage breakdowns calculated, we can now plot.

term_grade_breakdown <- percent_breakdown(credit_data, "terms", "grade")

ggplot(data = term_grade_breakdown, aes(x = grade, y = percentage)) +
  geom_bar(stat = "identity", aes(fill = terms)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  labs(y = "percentage of loans")



#This plot makes it clear that the lower the grade, the higher the proportion of loans that are of longer length. The difference is extremely evident with only 3.5% of grade A loans having a 60-month term but 87.5% of grade G loans having the same term.


grade_term_breakdown <- percent_breakdown(credit_data, "grade", "terms")

ggplot(data = grade_term_breakdown, aes(x = terms, y = percentage)) +
  geom_bar(stat = "identity", aes(fill = grade)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  labs(y = "percentage of loans") +
  scale_fill_brewer()


#Reversing the breakdown and instead plotting the percentage breakdown of each  length of term by grade reveals similar information. 36-month loans are composed mostly of grade A, B, and C loans (comprising a total 83%). Moving to 60-month loans, the number of A and B grade loans drops off sharply (down 67%) while the proportion of grade D, E, F, and G loans increases.

#checking Relation Between Annual Income & rateof interest

ggplot(data = credit_data, aes(x = annual_inc, y = Rate_of_intrst)) +
  geom_point(alpha = 0.01, position="jitter") +
  coord_cartesian(
    xlim = c(quantile(credit_data$annual_inc, probs = 0.01, na.rm = TRUE), 
             quantile(credit_data$annual_inc, probs = 0.99, na.rm = TRUE))) +
  labs(x = "annual income ($)", y = "interest rate (%)")



