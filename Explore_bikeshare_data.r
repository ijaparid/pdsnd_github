#Importing dataframes
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

wash$city='Washington'
ny$city='New York'
chi$city='Chicago'

# joining the three dataframes with common columns
DF<-(rbind(ny[,c(1:7,10)],wash, chi[,c(1:7,10)]))
# checking it worked well
head(DF)

# joining the two dataframes with common columns
DF1<-(rbind(ny, chi))
# checking it worked well
head(DF1)

# defining function name, arguments are the data frame, and the name of the column
myfun<- function(Q=DF, name='User.Type'){
    category<-unique(Q[,name]) #getting unique values of the columns
    A=data.frame(category) # making those unique values into a data frame
    A$count=0 # adding the count column with zeros

    # for every unique value I go over every entry in the provided column, if the entries are the same, I add 1 in the count column for that value
    for (j in 1: length(A$count)){
    for (i in 1:length(Q[,name])){
        if (Q[,name][i]==A$category[j]){
            A$count[j]=A$count[j]+1
                                    }
                                    }
                                    }
    A=A[order(-A$count),] # sorting by decerasing order.
    return(A)
    print(A)

}
# import ggplot library
library(ggplot2)

# month: using substring function
DF$month<-substring(DF$Start.Time, 6,7)
# day of the week: I use weekdays function.
DF$wday<-weekdays(as.Date(DF$Start.Time))
#hour: using substring function
DF$hour<-substring(DF$Start.Time, 12,13)
# checking the first several lines to make sure it worked well.
head(DF)

ggplot(DF,aes(wday))+geom_bar()+labs(title ="Most common day of week by city", x = "Days of week", y = "Count")+facet_grid(city ~ ., scales="free")

ggplot(DF,aes(month))+geom_bar()+labs(title ="Most common month by city", x = "Month", y = "Count")+facet_grid(city ~ ., scales="free")

ggplot(DF,aes(hour))+geom_bar()+labs(title ="Most common hour by city", x = "Hour", y = "Count")+facet_grid(city ~ ., scales="free")

# I create a data frame Stata whose first column has the names of the cities
Stats<-data.frame(unique(DF$city))
# chekcing it worked well
head(Stats)

# I add the total and mean columns, which at this stage contain 0s
Stats$total=0
Stats$mean=0
# checking it worked well
head(Stats)

# I use sum() and mean() functions to obtain relevant statistics for each city.
for (i in unique(DF$city)){
    Stats[ Stats$unique.DF.city.==i,'total']=sum(DF[DF$city==i & DF$User.Type=='Customer','Trip.Duration'], na.rm=T)
    Stats[ Stats$unique.DF.city.==i,'mean']=mean(DF[DF$city==i & DF$User.Type=='Customer','Trip.Duration'])
}
# checking it worked
head(Stats)

ggplot(Stats,aes(x=Stats$unique.DF.city. ,
                 y=Stats$total))+geom_bar(stat = 'identity')+labs(title ="Total travel time by city", x = "City", y = "Minutes")

ggplot(Stats,aes(x=Stats$unique.DF.city. ,y=Stats$mean))+geom_bar(stat = 'identity')+labs(title ="Mean travel time by city", x = "City", y = "Minutes")

# I apply the myfun function to count user types. Note that I could do this using bar plot, as categories are relatively small
myfun(Q=DF, name='User.Type')

# I apply the myfun function to count user gender.
myfun(Q=DF1, name='Gender')

max(ny$Birth.Year, na.rm=T)

min(ny$Birth.Year, na.rm=T)

# I apply the myfun function to count user gender. I As birth year has "NA" instead of "", I drop these observations
myfun(Q=DF1[!is.na(DF1$Birth.Year),], name='Birth.Year')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
