
G4_howell<-read.csv("G4_howell.csv")
View(G4_howell)

#Data cleaning

#1) Re-code the gender feature to Male and Female

G4_howell$Sex[howell$gender=="F" ]="Female"
G4_howell$Sex[howell$gender=="m"]="Male"

#2) show all the rows that have NA
G4_howell[!complete.cases(G4_howell) , ]

#3)Replace each NA in weight according to the mean of
#weight to all males for males and weight to all females for females

m_mean<-mean(G4_howell[G4_howell$Sex=="Male" , 'weight' ], na.rm=T)
G4_howell[is.na(G4_howell$weight) & G4_howell$Sex=="Male"  , 'weight']=m_mean

f_mean<-mean(G4_howell[G4_howell$Sex=="Female" , 'weight' ], na.rm=T)
G4_howell[is.na(G4_howell$weight) & G4_howell$Sex=="Female"  , 'weight']=f_mean

#4)Re_code age variable 
G4_howell$age[G4_howell$age <= 10]="0 _ 10"
G4_howell$age[G4_howell$age > 20 & G4_howell$age <= 40]="20 _ 40"
G4_howell$age[G4_howell$age >60 ]="60 _ .."

#5)remove the text(kg)in weight feature to convert it to numeric to use in the analysis

G4_howell$weight<-gsub(" kg" ,"" , G4_howell$weight)
G4_howell$weight<-as.numeric(G4_howell$weight)


#6)Re_code the height feature using if else

x<-mean(G4_howell$height)

G4_howell$heightCat<-as.factor(ifelse(G4_howell$height< x & G4_howell$age >10,"Abnormal person" 
                                   ,"Normal person"))
#7)Re_code of code

G4_howell$heightCat2[G4_howell$heightCat=="Normal person"]='0'
G4_howell$heightCat2[G4_howell$heightCat=="Abnormal person"]='1'
G4_howell$heightCat2<-as.factor(G4_howell$heightCat2)

#8)Display The ratio of normal and abnormal child 

a<-mean(G4_howell$heightCat2==0)
b<-mean(G4_howell$heightCat2==1)

#9)Subset only males who have weight greater than 40

sub1<-G4_howell[G4_howell$Sex=="Male" & G4_howell$wegith >40 , ]

#11)Subset persons who have weight greater than the median and have height 
#greater than or equal to 135  for specific features(age , sex)

sub3<-G4_howell[G4_howell$weight> median(G4_howell$weight) &
              G4_howell $height >= 135 ,c(1,5) ]
  
#12)sort the data set ascending according to 2 variables

sorted<-G4_howell[order(G4_howell$age ,G4_howell$height) , ]

#13)Get only the first 30 rows
h<-head(G4_howell ,30)

#14)Get only the last 50 rows
t<-tail(G4_howell ,50)
#______________________________________________ now the data is ready to visualization

library(ggplot2)

#15)display the effect of the height on weight (co_relation) using scatter plot,name the figure

fig1<-ggplot(anthro , aes(x=height  , y= weight))
fig1 + geom_point() + ggtitle("The co_relation between the Height and weight")

#16)display the effect of height on weight colored by the groups of age range using scatterplot
fig2<-ggplot(anthro , aes(weight , height))
fig2 + geom_point(aes(color=ageRange)) +stat_smooth(se=FALSE)  


#17)Show the distribution of weight using histogram,name the figure and rename the x,y
fig3<-ggplot(G4_howell , aes(weight))
fig3 + geom_histogram(binwidth = 8)
fig3 + geom_histogram(fill = "orange")+ ggtitle("person's weight distribution")+labs(x="weight" , y="Frequency")
b
#18)Show the distribution of Height using histogram ,name the figure
fig4<-ggplot( G4_howell, aes(height))
fig4 + geom_histogram(binwidth = 8)
fig4 + geom_histogram(fill = "red")+ ggtitle("person's Height distribution") 

#19)summarize the heightcat2 0,1 to Sex and ageRange groups using Bar chart

fig5<-ggplot( G4_howell, aes(x=heightCat2  ,fill= Sex))
fig5 +geom_bar()+labs(y=" Heightcat count" ,title="Height category rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~ageRange)
















