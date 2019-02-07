getwd()
setwd("D:/R/CASE S AND ASSIGNS")
ter = read.csv("terror.csv")
View(ter)
str(ter)
library(dplyr)

ncol(ter)
nrow(ter)
names(ter)
#Q.1)	How many attacks happened in India?
nrow(subset(ter,ter$country_txt=='India'))
c = subset(ter,ter$country_txt=='India')
View(c)
#Q.2)	How many attacks happened in India and upto 3 people were killed
a = subset(ter,ter$country_txt=='India' & (ter$nkill>=1 & ter$nkill<=3))
nrow(a)


#Q.3) Extract the city and summary for attacks above
View(select(ter,city,summary))

#Q.4)	In a single terror incident in India, 
#find out top 5 cities by number killed

v = c%>%
  select(city,nkill)%>%
  arrange(-nkill)
head(v,5)

#Q.5)	In a single terror incident in India,
#find out top 5 cities by number killed and wounded 
d = c%>%
  select(city,nkill,nwound)%>%
  arrange(-nkill)%>%
  arrange(-nwound)
head(d,5)

#Q.6) How many attacks were successful that were suicide attacks?
sum(ter$suicide==1&ter$success==1)

#Q.7)label all the incidents where the number killed was more than 5 as severe. 
ter1=ter%>%
  filter(nkill>5)%>%
  mutate(severe='severe')
View(ter1)

# Q.8) write a function to label an incident that was both successful and suicidal
ter2 <- function(ter) {
  len=length(ter$suicide==1 & ter$success==1)
  len2 =length(ter$suicide!=1 & ter$success!=1) 
  terror = mutate(ter,success_sucidical=case_when((suicide==1 & success==1)~
                                                       (rownames(ter)[1:len]=paste("sucess and sucidical",1:len))
                                                     ,TRUE~(rownames(ter)[1:len2]=paste("others",1:len2))) )
  return (ter)
}
ter=ter2(ter)
View(ter)


#Q.9)	Create a new category representing if the incident occured in Afghanistan, Pakistan or India 
#as one level of the category and all the other countries as another level
for(i in 1:nrow(ter)){
if(ter[i,"country_txt"]=='India'|ter[i,"country_txt"]=='Pakistan'|ter[i,"country_txt"]=='Afghanistan'){
  ter[i,"Asian_category"]= 'IND-PAK-AFG'
}else{
  ter[i,"Asian_category"] = 'ROW'
 }
}  

#Q.10)	How many incidents happened in Af-Pak-India vs ROW?
table(ter$Asian_category)


#Q.11)	List the number of suicides attacks and average kills by Af-Pak-India vs ROW. 
#Rename columns in the output as Average_Kills and Number_Incidents.
ter%>%
  group_by(Asian_category)%>%
  summarise(Average_Kills = mean(nkill,na.rm=T),
            Number_Incidents = sum(suicide))%>%
  View()

