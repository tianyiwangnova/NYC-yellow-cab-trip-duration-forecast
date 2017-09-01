#weekend or not?
weekend=NULL
for (i in 1:nrow(timedata)){
  y=0
  if (timedata$day_of_week[i]==1 | timedata$day_of_week[i]==7){y=1}
  weekend=c(weekend,y)
}
