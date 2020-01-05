library(validate)
library(validatetools)

#devtools::install_github("data-cleaning/errorlocate")
library(errorlocate)

# load the data
load("./issues/data and rules.RData")

#load the rules
rr<-read.table("./issues/edits.txt", sep="\t", as.is=TRUE, header=T, row.names = NULL, na.strings="." )

# standardize rules
rr$edit<-toupper(rr$edit)

# # prepare the rules with editset
# rules_1<-editset(rr)

# names the column "rule"
names(rr)[1]<-"rule"

# prepare the rules with validator
rules_2 <- validator(.data=rr)

# summary(rules_1)
# summary(rules_2)


# the problems I am facing concernd mainly the variability of time requested to solve a given problem
# here a selection of rekords
rekords<-Rekords_to_be_corrected[1:10,]

cf <- confront(rekords, rules_2)
cfd <- as.data.frame(cf)
cfd[!cfd$value,]
# View(cfd)
# I noted that applying sequentially locate_errors on the same data
# with the same rules, it can results in some relevant differencies in the requested time.
# In some cases, it seems that R crashes
# if you fix the random numbers the requested time has less variability

#set.seed(1234)
# record 3 is a record that takes some time
le_c_2<- locate_errors(rekords[3,], rules_2, verbose="normal", timeout=10)

for (i in 1:4){
  t <- system.time(le_c_2<- locate_errors(rekords, rules_2, timeout = 5))
  n <- values(le_c_2)[1,]
  adapt <- names(n)[n]
  print(list(le_c_2=le_c_2, t=t, adapt=adapt))
}

#system.time(le_c_1<- localizeErrors(rules_1,rekords,mehod=c("bb")))


# we tried to solve the problem rekord by rekord in order to understand if the problems were
# caused by some given rekords, but even in rekord by rekord case the time requested can vary a lot
# Furthermore the system can crasches "randomly" on some rekords

for(i in 1:nrow(rekords)){
  rekord<-rekords[i,]
  print(i)
  le <- locate_errors(rekord, rules_2)
  print(le)
}



