#install.packages("jsonlite")
library(jsonlite)


######################################################################
######################################################################
######################################################################
################### EXOLORING Business DATA ##########################
######################################################################
######################################################################
######################################################################

business_df <- stream_in(file("JSON Yelp files/yelp_academic_dataset_business.json"))

library(dplyr)
#funkcija slicna str() daje osnovne podatke o data.frame-u
glimpse(business_df)

#zatim proveravamo da li se pojavljuju duplikati u Business bazi
which(duplicated(business_df$business_id)==T)
#nema duplikata

#Prikazivanje vrednosti koje nedostaju, takodje moze pomocu Amelia paketa
library(reshape2)
library(ggplot2)
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}


#uradicu 2 grafika, jedan za atribute lokala i drugi za ostale feature
ggplot_missing(business_df[,-14])
ggplot_missing(business_df$attributes)
#vidimo da u feature-u radno vreme i u atributima ima dosta 'NA' vrednosti

#prebacivanje grada u faktor, ima 444 grada(mozda podeliti u drzave?)
business_df$city <- factor(business_df$city)
#feature koji mi nema puno znacenja(korisniji je state)


summary(business_df$open)
prop.table(table(business_df$open))
#85% restorana radi, 15% ne, nema NA vrednosti


unique(as.character(unlist(business_df$categories)))
#1017 kategorija biznisa, mozda razvrsati u opstije kategorije? 

#izvlacim sve kategorije i sortiram ih prema frekvenciji
categories_df <-as.data.frame(table(as.character(unlist(business_df$categories))))
colnames(categories_df) <- c("categories", "freq")
attach(categories_df)
categories_df <- categories_df[order(-freq),]
detach(categories_df)
rownames(categories_df) <- NULL
categories_df <- mutate(categories_df, percentage = freq /nrow(business_df))
#pravim feature percentage koji pokazuje u koliko % lokala se pojavljuje ta kategorija

head(categories_df, 25)
#ovde mozemo videti da najvise ima restorana i shopping kategorija


#preko $state feature mozemo da odredimo drzavu i grad u kome se lokal nalazi
business_df$state <- factor(business_df$state)
#30 drzava ima, jedna je prazna("":ta prazna je Montreal, Quebec)

#koliko se pojavljuju drzave, od toga izvlacim kom ce od 9 ponudjenih gradova pripasti
table(business_df$state)

#prepravljanje pogresno unetih vrednosti(rucno pogledanih)
ind1 <- which(business_df$state =="FIF")
business_df[ind1,]$state <- "PA"

ind1 <- which(business_df$state %in% c("AK","AL","TX") )
business_df[ind1,]$state <- "AZ"

ind1 <- which(business_df$state %in% c("NM","TAM") )
business_df[ind1,]$state <- "NV"

ind1 <- which(business_df$state %in% c("ELN","FIF","HAM","KHL","MLN","NTH","SCB","XGL") )
business_df[ind1,]$state <- "EDH"

ind1 <- which(business_df$state %in% c("NW","RP") )
business_df[ind1,]$state <- "BW"

ind1 <- which(business_df$state =="" )
business_df[ind1,]$state <- "QC"

rm(ind1)

#trazeni gradovi + par izuzetaka a najveci je South carolina 325, Florida, California i Minnesota su mali bas
business_df$state <- factor(business_df$state)
levels(business_df$state) <- c("Phoenix","Karlsruhe","California","Edinburgh","Florida","Urban-Champaign","Minnesota","Charlotte",
                               "Las Vegas","Waterloo","Pittsburgh","Montreal","South Carolina","Madison")
#po mogucstvu mozemo dropovati lokale koji nisu u 10 zadatih gradova


#kreiranje novog feature-a, kontinent da bi lakse mogli da poredimo Severnu Ameriku i Evropu
business_df$continent <- ifelse(business_df$state == "Karlsruhe" | business_df$state == "Edinburgh","Europe","North America")

business_df$continent <- factor(business_df$continent)
#moguce je napraviti i feature sa samom drzavom: USA, Canada, UK and Germany

#da pogledamo koje su lokacije(gradovi) zastupljene i u kojoj meri:
lokacije_df <- data.frame(prop.table(table(business_df$state)))
names(lokacije_df) <- c("Locations","freq")
lokacije_df <- lokacije_df[order(-lokacije_df$freq),]
lokacije_df$freq <- lokacije_df$freq *100
names(lokacije_df) <- c("Locations","percent")
lokacije_df[,2] <- round(lokacije_df[,2], digits = 3)
lokacije_df[,2] <- paste(lokacije_df[,2],"%", sep = " ")
rownames(lokacije_df) <- NULL

#sada imamo data.frame koji govori tacno to



#kreiranje funkcije koja pravi data.frame sa sortiranim kategorijama uz zadatu kategoriju i sa zadatimm kontinentom /TOP50
categories_function<- function(x, continentW, categoryW){
  categories_df <-as.data.frame(table(as.character(unlist(subset(x,continent==continentW)$categories))))
  colnames(categories_df) <- c("categories", "freq")
  attach(categories_df)
  categories_df <- categories_df[order(-freq),]
  detach(categories_df)
  
  my_function1 <- function(x, stringW){
    if(length(x)==1 ){
      x==stringW
    }else{
      stringW %in% x
    }  
  }
  library(rlist)
  rownames(categories_df) <- NULL
  #kopija df-a koju cu subsetovati da sadrzi samo opservacije koje sadrze 'categotyW'
  restoraunt_cat <- select(subset(x,continent==continentW), business_id, categories)
  restoraunt_cat$is_restoraunt <- list.apply( restoraunt_cat$categories,my_function1,categoryW)
  restoraunt_cat <- subset(restoraunt_cat, is_restoraunt==TRUE)
  
  # na kraju smo dobili df samo sa restoranima, na osnovu koga cemo
  #izvuci samo one koje kategorije se najvise ponavljaju
  
  restoraunt_categories_df <-as.data.frame(table(as.character(unlist(restoraunt_cat$categories))))
  colnames(restoraunt_categories_df) <- c("categories", "freq")
  attach(restoraunt_categories_df)
  restoraunt_categories_df <- restoraunt_categories_df[order(-freq),]
  detach(restoraunt_categories_df)
  
  numberOfRestorants <- restoraunt_categories_df$freq[1]
  restoraunt_categories_df <- mutate(restoraunt_categories_df, percentage = freq/numberOfRestorants)
  restoraunt_categories_df <- restoraunt_categories_df[-1,]
  rownames(restoraunt_categories_df) <- NULL
  head(restoraunt_categories_df,50)
}

##TOP 50 tipova restorana u Americi i u Evropi
american_restoraunts <- categories_function(business_df,"North America","Restaurants")
europe_restoraunts <- categories_function(business_df,"Europe","Restaurants")
american_restoraunts 
europe_restoraunts
#vidimo da na Americkom kontinetu vladaju lokali sa nezdravom hranom 
#poput FastFood, dok je u Evropi to tek na 5. mestu

summary(business_df$stars)
#median je 4 zvezdice, da vidimo na histogramu
ggplot(business_df,aes(stars)) + geom_bar()

mean(business_df$review_count)
#prosecan broj review je 34.35236

library(ggmap)

#funkcija za pravljenje
plot_long <- function(df1,zoom1=4,color="red",maptype = "hybrid"){
  lon <- df1$longitude
  lat <- df1$latitude
  df <- as.data.frame(cbind(lon,lat))
  
  # getting the map
  map_cities <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = zoom1,
                        maptype = "hybrid", scale = 2)
  
  # plotting the map with some points on it
  ggmap(map_cities) +
    geom_point(data = df, aes(x = lon, y = lat, fill = "orange", alpha = 0.8), size = 2, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)
}

#pravljenje prikaza mape sa svim lokalima po kontinentima
na_locations_plot <- plot_long(subset(business_df, continent=="North America"), zoom1 = 3)
e_locations_plot <- plot_long(subset(business_df, continent=="Europe"),color = "orange", zoom1 = 5)

#prikaz mapa
na_locations_plot
e_locations_plot

#sad mozemo istrazivati podatke u odnosu na lokaciju\grad
options(scipen = 999)#izbegava notaciju sa eksponentom
prop.table(table(business_df$state, business_df$stars),margin  = 1)
#koliki procenat koje ocene prema odredjenom gradu

ggplot(business_df, aes(state, stars)) +
  stat_summary(fun.y = "mean", geom = "point")
#na ovom grafiku mozemo videti prosecnu ocenu za svaki state posebno,
#mozemo prmetiti da evropski restorani imaju dosta bolje ocene > sto se vidi i na sledecem
#grafiku

ggplot(business_df, aes(continent, stars)) +
  stat_summary(fun.y = "mean", geom = "point")


######################################################################
######################################################################
######################################################################
################### EXOLORING User DATA ############################## 
######################################################################
######################################################################
######################################################################


user_df <- stream_in(file("JSON Yelp files/yelp_academic_dataset_user.json"))

library(dplyr)
glimpse(user_df)

#proveravanje missing values-a
ggplot_missing(user_df)
#za votes i compliment ima dosta missing values-a

sum(duplicated(user_df$user_id))
#nema dupliranja korisnika

library(gender)
#proba
gender("Marko")

#wraper za gender funkciju koji stavlja "unknown" za imena kojima ne moze da odredi pol
my_function2 <- function(x){
  y <- gender(x, method = "ssa")$gender
  if(length(y)==0) y <- "unknown"
  y
}

#data.frame sa imenima, id, i polom svakog korisnika
name_of_yelpers <- data.frame(name = user_df$name, gender = NA,userID = user_df$user_id, stringsAsFactors = FALSE)
for(i in 1:nrow(name_of_yelpers)){
  name_of_yelpers$gender[i] = my_function2(name_of_yelpers$name[i])
  i <- i+1
}

#stao sam do 82160 opservacija pri prepoznavanju pola i statictika izgleda ovako:
name_of_yelpers_nna <- filter(name_of_yelpers,!is.na(gender))
gender_table <- data.frame(round(prop.table(table(name_of_yelpers$gender)), digits = 3)) 
gender_table$Freq <- gender_table$Freq * 100
names(gender_table) <- c("gender","percent")

#pravljenje pie grafika za pol
pie(gender_table$percent, labels = gender_table$gender, main="Pie Chart of Gender")

#pravljenje pie grafika za pol - ggplot
ggplot(name_of_yelpers_nna, aes(x = factor(1), fill = factor(name_of_yelpers_nna$gender))) + geom_bar(width = 1) +    coord_polar(theta = "y") + theme_void() + ggtitle("Pie Chart of Gender")


#prebacivanje 'yelping_since' featura u koristan format za pravljenje vremenske serije
user_df$Date_of_join <- as.Date(paste(user_df$yelping_since,"-01",sep=""))

#work on dates to compute timeseries number of users
datesOfJoin <- user_df$Date_of_join
table_dates <- table(datesOfJoin)
number_of_users <- cumsum(table_dates)

library(dygraphs)
number_of_users <- as.data.frame(number_of_users)
colnames(number_of_users) <- "Number of users"

#pravljenje interaktivne vremenske serije za broj usera Yelp-a od osnivanja
number_of_users_ts <-dygraph(number_of_users, main = "Time Series : Number of users") %>% dyRangeSelector() %>%
  dyOptions(stackedGraph = TRUE) 

number_of_users_ts

ggplot(user_df, aes(review_count, average_stars)) + geom_jitter(alpha = 0.2, col = "blue")



######################################################################
######################################################################
######################################################################
################### EXOLORING Checkin DATA ###########################
######################################################################
######################################################################
######################################################################

checkin_df <- stream_in(file("JSON Yelp files/yelp_academic_dataset_user.json"))

glimpse(checkin_df)

#u ovom df-u imamo checkin-ove po danima i satima za odredjene lokale

#da izvucemo za svaki lokal i svaki dan u kom periodu je bio najposeceniji?
hours_checkin <- checkin_df$checkin_info

#replace all NA's with 0 for easier calculation

hours_checkin[is.na(hours_checkin)] <- 0
days_in_week <- c("Sunday","Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday")
hours_in_day <- c()
for(i in 0:23){
  hours_in_day <- cbind(hours_in_day, paste0("h",i))
}
hours_in_day <- as.vector(hours_in_day)
hours_in_day

names_function <- function(){
  names_for_hours <- matrix(NA, nrow = 24, ncol = 7)
  for(i in 0:23){
    for(k in 0:6){
      names_for_hours[i+1,k+1] <- paste(i,k,sep = "-")
    }
  }
  names_for_hours <- as.data.frame(names_for_hours)
  
  colnames(names_for_hours) <- days_in_week
  rownames(names_for_hours) <- 0:23
  names_for_hours
} 
names_for_hours <-names_function()

monday_col <- which(names(hours_checkin) %in% names_for_hours$Monday)
tuesday_col <- which(names(hours_checkin) %in% names_for_hours$Tuesday)
wednesday_col <- which(names(hours_checkin) %in% names_for_hours$Wednesday)
thursday_col <- which(names(hours_checkin) %in% names_for_hours$Thursday)
friday_col <- which(names(hours_checkin) %in% names_for_hours$Friday)
saturday_col <- which(names(hours_checkin) %in% names_for_hours$Saturday)
sunday_col <- which(names(hours_checkin) %in% names_for_hours$Sunday)

hours_checkin$Monday <- rowSums(hours_checkin[,monday_col])
hours_checkin$Tuesday <- rowSums(hours_checkin[,tuesday_col])
hours_checkin$Wednesday <- rowSums(hours_checkin[,wednesday_col])
hours_checkin$Thursday <- rowSums(hours_checkin[,thursday_col])
hours_checkin$Friday <- rowSums(hours_checkin[,friday_col])
hours_checkin$Saturday <- rowSums(hours_checkin[,saturday_col])
hours_checkin$Sunday <- rowSums(hours_checkin[,sunday_col])

#ovo moze biti uradjeno i pomocu funkcije i for petlje, ali neki drugi put :)

cols_function_hours <- function(){
  columns_for_hours <- matrix(NA, nrow = 24, ncol = 7)
  
  t_names_for_hours <- t(names_for_hours)
  
  for(l in 1:24){
    columns_for_hours[l,] <- which(names(hours_checkin) %in% t_names_for_hours[,l])
  }
  rownames(columns_for_hours) <- 0:23
  columns_for_hours
}

columns_for_hours <- cols_function_hours()

adding_hours_var_to_df <- function(x){
  y <- x
  for(ijk in 0:23) 
  { 
    nam <- as.character(paste0("h", ijk))
    y[nam] <- rowSums(y[,columns_for_hours[ijk+1,]])
  }
  y
}
hours_checkin <- adding_hours_var_to_df(hours_checkin)
names(hours_checkin)



hours_checkin$TopDay <- colnames(hours_checkin[,days_in_week])[apply(hours_checkin[,days_in_week],1,which.max)]
hours_checkin$TopHour <- colnames(hours_checkin[,hours_in_day])[apply(hours_checkin[,hours_in_day],1,which.max)]
#we did days of week to see what day were busy, now we gonna do the same for the hours of day


#now we have data.frame with check-in for every day in week, and every hour in day
#so we can see what are most "crowded" hours and days at business, and further on particular locations

checkin_df$checkin_info <- hours_checkin
checkin_df$TopDay <- checkin_df$checkin_info$TopDay
checkin_df$TopHour <- checkin_df$checkin_info$TopHour


#now we have attributes TopHour (hour in day when was the most checkins) and 
#TopDay(day in week when was the most checkins)

#now we can merge checkins with business and see some trends on crowded days and hours
#among different locations and categories

#to simlify thing I'll just add  features :categories, state, continent

dummy_business_df <- select(business_df,business_id,continent,state, categories)

checkin_business_df <- merge(x = checkin_df, y = dummy_business_df, by = "business_id", all.x = TRUE)
rm(dummy_business_df)
names(checkin_business_df)

#more to simplify thing I'll remove $checkin_info and $type column

checkin_business_df <- select(checkin_business_df, -c(2,3))

glimpse(checkin_business_df)
#now we have data.frame with only useful features and we can to analysis

prop.table(table(checkin_business_df$TopDay,checkin_business_df$continent), margin = 2)
#we can see that Friday was, by far, most popular day of the week on both continents

prop.table(table(checkin_business_df$TopDay,checkin_business_df$state), margin = 2)
#also, Friday was the most popular in every city we analized, but we can spot 
#that Friday share was larger in US cities, than other

#now, let's see what are most crowded hours in day
p_t_1 <- prop.table(table(checkin_business_df$TopHour,checkin_business_df$continent), margin = 2)
p_t_1
which.max(p_t_1[,1])
which.max(p_t_1[,2])
#na Americkom kontinentu su se najcesce checkin-ovali izmedju 05h-06h, dok 
#je u Evropi drugacija situacija: 12h-13h

#further we can analyze frequency od checkin on hours and days on some
#particular categories, for exampe in  restoraunts
dummy_restoraunts_df <- checkin_business_df

dummy_restoraunts_df$isRestoraunt <- list.apply(dummy_restoraunts_df$categories,my_function1,"Restaurants")
dummy_restoraunts_df<- filter(dummy_restoraunts_df, isRestoraunt ==T)
#I kept only restaraunts in this data.frame, same this can be done for 
#desired category(cinema, shopping, bars etc.)

prop.table(table(dummy_restoraunts_df$TopDay,dummy_restoraunts_df$state), margin = 2)
#in most states, again, Friday was day for going to restaraunts

######################################################################
######################################################################
######################################################################
################### EXOLORING Review DATA ############################ 
######################################################################
######################################################################
######################################################################


review_df <- stream_in(file("JSON Yelp files/yelp_academic_dataset_review.json"))


glimpse(review_df)
#prebacivanje data u `Date` format radi lakseg kasnijeg manipulisanja
review_df$date <- as.Date(review_df$date)


#we can run recognition algorithm to determine what is language of certain
#review, we have {textcat} with textcat() function.
#install.packages("textcat")
library(textcat)

textcat("Ovo je proba")
#vraca bosanski jezik, mislim da bi dugo trebalo mom racunaru da obradi
#svih 2.6 miliona reviewa i razvrsta u jezike

#da bismo izbegli gomilanje podataka u ovom data.frame koristicu samo odredjene
#varijable iz business_df
dummy_business_df <- select(business_df,business_id,continent,state, categories)

review_business_df <- merge(x = review_df, y = dummy_business_df, by = "business_id", all.x = TRUE)
rm(dummy_business_df)
names(review_business_df)

review_business_df <- group_by(review_business_df, business_id)

glimpse(review_business_df)

range(review_df$date)
#iz cega vidimo datum prvog review-a i datum poslednjeg review-a
# 12.oktobar 2004 i 19.jul 2016

#we can classify this data to positive and negative reviews(either on stars, or on 
# text analysis), and do 
#the Semtiment Text Analysis to find some useful intel

######################################################################
######################################################################
######################################################################
################### EXOLORING Tip DATA ############################### 
######################################################################
######################################################################
######################################################################


tip_df <- stream_in(file("JSON Yelp files/yelp_academic_dataset_tip.json"))


glimpse(tip_df)
#prebacivanje `date` feature-a u Date format radi lakseg kasnijeg manipulisanja
tip_df$date <- as.Date(tip_df$date)

#pravljenje dummy df-a da bih preneo samo odredjenje feature
dummy_business_df <- select(business_df,business_id,continent,state, categories)
tip_business_df <- merge(x = tip_df, y = dummy_business_df, by = "business_id", all.x = TRUE)
rm(dummy_business_df)

#tipovi samo iz dvojezicnog grada Montreala:
tip_business_df_montreal <- tip_business_df[tip_business_df$state=="Montreal",] 


library(textcat)
textcat(tip_business_df_montreal$text[1])
#first tip on Montreal business was written in english
#now we can run this code on entire Monteral tip set : 13k tips

#creating language column, later to be filled with values
tip_business_df_montreal$language <- NA

#setting potential language for detection
my.profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "french")]

for(j in 1:nrow(tip_business_df_montreal)){
  tip_business_df_montreal$language[j] <- textcat(tip_business_df_montreal$text[j],p = my.profiles)
}

tip_business_df_montreal$language <- factor(tip_business_df_montreal$language)
prop.table(table(tip_business_df_montreal$language))
# 81% na engleskom jeziku , 19% na francuskom


######################################################################
######################################################################
######################################################################
################### EXOLORING Restoraunts ############################ 
######################################################################
######################################################################
######################################################################


#first let's drop all business that aren't restoraunts 
restoraunt_rows <- c()
for(i in 1:nrow(business_df)){
  if(my_function1(unlist(business_df$categories[i]),"Restaurants")){
    restoraunt_rows <- cbind(restoraunt_rows,i)
  }
}

restoraunt_rows <- as.vector(restoraunt_rows)
restoraunts_df <- business_df[restoraunt_rows,]
rm(col_to_drop)

#sad imamo df sa 26729 restorana, mogli bi ih podeliti prema kuhinjama(America, Thai, Italian, Mexican, itd.)
unique(unlist(restoraunts_df$categories))
#imamo 305 razlicitih kategorija za restorane
retoraunt_categories_df <-as.data.frame(table(as.character(unlist(restoraunts_df$categories))))
colnames(retoraunt_categories_df) <- c("categories", "freq")
attach(retoraunt_categories_df)
retoraunt_categories_df <- retoraunt_categories_df[order(-freq),]
detach(retoraunt_categories_df)
retoraunt_categories_df <- retoraunt_categories_df[-1,]
rownames(retoraunt_categories_df) <- NULL
#sad imamo sortiran df sa najcescim kategorijama restorana

cousine_categories <- c("Mexican","American (New)","American (Traditional)","Italian","Chinese","Japanese","Greek","Thai","Korean","Canadian","German","British",
                        "Turkish","Spanish","Portuguese","Vietnamese","Hawaiian","Pakistani","Filipino","Irish","Lebanese","Peruvian","Taiwanese","Scottish",
                        "Brazilian","Cuban","Ethiopian","Polish","Moroccan","Mongolian","International","Afghan","Belgian","Belgian","Indonesian",
                        "Russian","Argentine","Puerto Rican","Bangladeshi")
restoraunts_df$cousine <- NA
for(i in 1:nrow(restoraunts_df)){
  for(j in 1:length(cousine_categories)){
    if(cousine_categories[j] %in% unlist(restoraunts_df$categories[i])){
      restoraunts_df$cousine[i] <- cousine_categories[j]
      break
    }
  }
}
ind1<- which(restoraunts_df$cousine %in% c("American (New)", "American (Traditional)"))
restoraunts_df$cousine[ind1] <- "American"
#dodali smo feature za sve restorane: 'cousine'


sum(is.na(restoraunts_df$cousine))
kuhinje <- as.data.frame(table(restoraunts_df$cousine),stringsAsFactors = FALSE)


colnames(kuhinje) <- c("cousine", "freq")

attach(kuhinje)
kuhinje <- kuhinje[order(-freq),]
detach(kuhinje)
rownames(kuhinje) <- NULL

kuhinje
#sada imamo df sa najcescim nacionalnim kuhinjama i njihovim frekvencijama