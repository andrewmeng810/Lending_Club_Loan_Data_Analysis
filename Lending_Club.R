
setwd("~/lending-club-loan-data")
loanbook <- read.csv("loan.csv", stringsAsFactors = FALSE)

# Check the number of observations and features
dim(loanbook)

colnames(loanbook)
head(loanbook)

# Unique purposes in applying loans

a<- unique(loanbook$purpose)
a

# Check obervation types of each feature
str(loanbook)


# Basic stats for each feature
summary(loanbook)


########
########  Exploration  #####

library(DescTools)

# Loan grade distribution
loan_grade = factor(loanbook$grade)
Desc(loan_grade, main = "Grade distribution", plotit = 1)

# Loan term distribution
loan_term = factor(loanbook$term)
Desc(loan_term, main = "Term Distribution", plotit = 1)

# Loan amount distribution
loan_amnt = loanbook$loan_amnt
Desc(loan_amnt, main = "Loan Amount Distribution", plotit = 1)

# Loan Purpose distribution
Desc(loanbook$purpose, main = "Loan Purpose Distribution", plotit = 1)


#############
######  gglot

library(ggplot2)

### Box plot for Loan Amount by Term
box_plane2 = ggplot(loanbook, aes(term, loan_amnt))
box_plane2 + geom_boxplot(aes(fill = term)) + 
  labs(title = "Loan Amount by Term", 
       x = "Term",
       y = "Loan Amount")

############
#### Covert some features to factor

Loan_status = factor(loanbook$loan_status)
loan_homeownership = factor(loanbook$home_ownership)
#  head(loan_homeownership)
#  head(loanbook$home_ownership)
loan_emp_length = factor(loanbook$emp_length)
loan_purpose = factor(loanbook$purpose)
loan_verification = factor(loanbook$verification_status)
table(Loan_status)
head(Loan_status)
summary(Loan_status)

table(loan_homeownership)
table(loan_emp_length)
Desc(loan_emp_length)


# Check portfolio by years
library(dplyr)

loan_issue_date = loanbook$issue_d = as.Date(gsub("^", "01-", loanbook$issue_d), 
                                             format = "%d-%b-%Y")
amount_table = loanbook %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(Amount = sum(loan_amnt))

summary(amount_table)
dev.off()
line_plane = ggplot(amount_table, aes(x = issue_d, y = Amount))
line_plane + geom_line() + 
  labs(title = "Loan Book growth by issue date", 
       x = "Issued Date", 
       y = "Amount")

# Group status

good_status = c("Fully Paid", "Does not meet the credit policy. Status:Fully Paid")

on_going_status = c("Current", "Issued")

## If statements

Status_Groups = ifelse(loanbook$loan_status %in% good_status, "Good",
                       ifelse(loanbook$loan_status %in% on_going_status, "On going",
                              "Bad"))
Status_Groups_factors = factor(Status_Groups)
Desc(Status_Groups_factors, main = "Status Group Distribution", plotit = 1)

library(ggplot2)

s_plane = ggplot(loanbook, aes(grade, fill = Status_Groups))
s_plane + geom_bar(position = "fill") + 
  labs(title = "Status Group by Grade", 
       x = "Grade", 
       y = "Rate")

