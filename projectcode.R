# ##------INSTALL PACKAGES--------------
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("leaflet")
# install.packages("rworldmap")
# install.packages("flexdashboard", type = "source")
# install.packages("ploty")
#------LOAD DATA------------
library(tidyverse)
library(leaflet)
library(readxl)
library(gridExtra)
library(broom)
library(plotly)
library(flexdashboard)

###### T I D Y I N G  D A T A ######
#------LOAD FILES---------
CCodesfile="~/restaurants-analysis/Country-Code.xlsx"
Zomatofile="~/restaurants-analysis/zomato.xlsx"
CurrencyFile="~/restaurants-analysis/CurrencyRates.xlsx"

CCodes=read_excel(CCodesfile) #importing country code file
zomato=read_excel(Zomatofile) #importing main zomato file
Currency=read_excel(CurrencyFile) #importing currency file

#------ GET RID OF SOME COLUMNS & ADD NEW ONES -------------
zomato=zomato %>% select (-c(Rating_Color,Switch_To_Order_Menu,
                             Price_Range,Address,Locality_Verbose,Currency))
#we remove currency column as it is incorrect and contains too many unique symbols

#add in new columns: Country Names according to Country Code 
#& proper currency names with conversion rates
zomato = zomato %>% left_join(CCodes) 
zomato = zomato %>% left_join(Currency) 

#add in new column: USD Cost (for equal comparison)
zomato = zomato %>% mutate(Avg_Cost_USD = Average_Cost_For_Two*`Conversion Rate (USD)`) 
#Multiplied Currency Column by it's conversion rate

#replacing Cuisines col. with Principal_Cuisines 
#(the first category of cuisines for every country row)
zomato= zomato %>% separate(Cuisines,into=c("Principal_Cuisines")) 
#store "primary" cuisine types into new column and replace old with this

#---------REMOVE MISSING VALUES----------
zomato[zomato == 0] = NA #remove values that have zero
zomato[zomato == "Not rated"] = NA #remove unrated values


#------- GRAPHING DATA ----------
#Country
ggplotly(ggplot(zomato,aes(Country,fill=Country))+geom_bar())

#clear that most of our data comes from India -> major skewiness
#extract india as seperate file
zomato.row = zomato %>% filter(Country != 'India')

ggplotly(ggplot(zomato.row,aes(Country,fill=Country))+geom_bar())
#still skewed by US

#------- BREAKING IT DOWN INTO 2 Types -------
zomato$Country_Type = ifelse(zomato$Country == "United States","US",
                               ifelse(zomato$Country == "India","India", "Other"))
zomato.2 = zomato %>% filter(Country_Type != "Other")

ggplotly(ggplot(zomato.2,aes(Country,fill=Country))+geom_bar())


#let's check how pricing & ratings compare using side by side boxplots
zomato.2 = zomato.2 %>% mutate
(Rating_Text=ordered(Rating_Text,levels=c("Excellent","Very Good","Good","Average","Poor")))
ggplot(zomato.2,aes(x=Rating_Text,y=Avg_Cost_USD, color = Country_Type))+geom_boxplot()

a = ggplot(zomato.2 %>% filter(Country_Type == "India"),aes(x=Aggregate_Rating,y=Avg_Cost_USD,color = Country))+geom_point()+geom_smooth(se=F)
b = ggplot(zomato.2 %>% filter(Country_Type == "US"),aes(x=Aggregate_Rating,y=Avg_Cost_USD,color = Country))+geom_point()+geom_smooth(se=F)
grid.arrange(a,b)

#let's see the relationship between Indian Food and India
df = c("Andhra","Assamese","Awadhi","Bengali","Bihari","Biryani",
         "Goan","Gujarati","Hyderabadi","Indian","Kashmiri","Kerala",
         "Lucknowi","Maharashtrian","Malwani","Mithai","Mughlai","Naga",
         "Nepalese","North","Oriya","Rajasthani","South","Street")
zomato.2$Food_Type = ifelse(zomato.2$Principal_Cuisines %in% df, "INDIAN","OTHER")
ggplot(zomato.2 %>% filter(Country=="India"),aes(x=Rating_Text,fill=Food_Type))+geom_bar(position="dodge")

#Let's check Indias Spread
ggplotly(
  ggplot(zomato.2 %>% filter(Country=="India"),aes(Rating_Text,fill=City))+geom_bar()
)

#clearly most data comes from New Dehli & Gurgaon

new_delhi<-india %>% filter(City=="New Delhi") %>% filter(!is.na(Rating_Text)) %>% group_by(budget)%>%summarize(rcnt=n())
new_delhi %>%
  plot_ly(labels=~budget,values=~rcnt) %>%
  add_pie(hole=0.3) 

gurgaon<-india %>% filter(City=="Gurgaon") %>% filter(!is.na(Rating_Text)) %>% group_by(budget)%>%summarize(rcnt=n())
gurgaon %>%
  plot_ly(labels=~budget,values=~rcnt) %>%
  add_pie(hole=0.3) 

noida<-india %>% filter(City=="Noida") %>% filter(!is.na(Rating_Text)) %>% group_by(budget)%>%summarize(rcnt=n())
noida %>%
  plot_ly(labels=~budget,values=~rcnt) %>%
  add_pie(hole=0.3) 

rest<-india %>% select(Restaurant_Name,Votes) %>% arrange(desc(Votes))%>%head(10)
ggplot(rest,aes(x=Restaurant_Name,fill=Votes))+geom_bar()


india$budget = ifelse(india$Avg_Cost_USD<4,"low budget",
                      ifelse(india$Avg_Cost_USD>=4 & india$Avg_Cost_USD<8.225, "avg budget",
                             ifelse(india$Avg_Cost_USD>=8.225 & india$Avg_Cost_USD<10,"high budget","expensive")))



#-------------MAPPING DATA-----------
## LEAFLET MAP - DYNAMIC

#start with America
zomato.2 %>% filter(Country == 'United States') %>% 
  leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(-99,39,zoom=4)
#clearly missing information on lots of states

#Check India
#remove NA long + lat:
zomato.2 = zomato.2[complete.cases(zomato.2[,c("Longitude", "Latitude")]),]
india = zomato.2 %>% filter(Country == "India")

#NEWDEHLI & GURGAON
leaflet(india) %>% addTiles() %>% addCircles(opacity=0.5) %>% setView(77.2090,28.6139,zoom=10)
leaflet(india) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=10)

india$ratingcol <- ifelse(india$Aggregate_Rating < 2.5, "red",
                              ifelse(india$Aggregate_Rating >= 2.5 & india$Aggregate_Rating<3.5, "orange",
                                     ifelse(india$Aggregate_Rating >= 3.5 & india$Aggregate_Rating<4.5, "green", "black")))

leaflet() %>% 
  addTiles() %>% 
  setView(77.2090,28.6139,zoom=10) %>% 
  addCircleMarkers(india$Longitude, 
                   india$Latitude,
                   color = india$ratingcol, 
                   radius = 0.5, 
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(india$Restaurant_Name, 
                                 sep = "")) %>%
  addLegend("bottomleft", 
            colors = c("red","orange", "green", "black"),
            labels = c("Poor Rating",
                       "Average Rating",
                       "Good Rating",
                       "Excellent Rating"), 
            opacity = 0.8)

leaflet(india) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  setView(78.9629,20,zoom=4)
#South has 167 restaurants  | North has 7784

#what about the best rated restaurants?
india %>% filter(Rating_Text == 'Excellent') %>% 
  leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#Northern and Southern India have most high rated restaurants
#How does the South compare in price to the North?
leaflet(lowbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(78.9629,20,zoom=4)
leaflet(avgbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(78.9629,20,zoom=4)
leaflet(highbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(78.9629,20,zoom=4)
leaflet(expensive) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(78.9629,20,zoom=4)
#South: 32 out of 167 are rated excellent (19%) & 14% cheap restaurants
#16% avg cost restaurants & 23% high budget restaurants & 47% Expensive
#North 54 out of 7784 are rated excellent (0.7%) & 51% cheap restaurants
#22% Avg cost Restaurants, 11% high budget restaurants & 16% expensive



#----------- REGRESSION ONLY ON INDIA -------
zomato.2 = zomato.2 %>% mutate(Transformed_Rating = log10(Aggregate_Rating/(5-Aggregate_Rating)))
##we need to set a limit so predicted ratings do not go above 5

#regression on cost
costreg= lm(Transformed_Rating~Avg_Cost_USD+log10(Avg_Cost_USD),data=zomato.2 %>% filter(Country=="India"),weights = Votes)
summary(costreg) #9.8%
ggplot(costreg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
costreg.outliers = ggplot(costreg,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
costreg.outliers #dense of the data is randomly scattered BUT below 0

cityreg = lm(Transformed_Rating~Locality,data=zomato.2 %>% filter(City == "New Delhi"),weights=Votes)
summary(cityreg)
ggplot(cityreg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)

cuisinereg = lm(Transformed_Rating~Principal_Cuisines,data=zomato.2 %>% filter(City == "New Delhi"),weights=Votes)
summary(cuisinereg)
ggplot(cuisinereg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)

multireg = lm(sqrt(Transformed_Rating)~Avg_Cost_USD+Principal_Cuisines+City,data=india,weights = Votes)
summary(multireg) #41.76% R Squared
ggplot(data=multireg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)

#check if outliers are dense
ggplot(data=multireg,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)

#DIAGNOSTIC CHECK ON TRANSFORMED MULTI REG
plot(multireg,1) #good
plot(multireg,2)
plot(multireg,3)


#######VOTES REGRESSION
ggplot(data=india,aes(y=Votes,x=Aggregate_Rating))+geom_point()+geom_smooth()
ggplot(data=india,aes(y=log(Votes),x=Aggregate_Rating))+geom_point()+geom_smooth()
Votes.1=lm(Aggregate_Rating~Votes+log(Votes),data=india)
summary(Votes.1) #increased R^2
ggplot(data=Votes.1,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)

  