#load libraries
library(stringr)
library(rvest)
library(ggvis)
library(dplyr)
library(ggplot2)

#The first half of the URL..
site_first = "http://www.propwall.my/mont_kiara/classifieds?page=" 
#...and this is the second half of the URL
site_second = "&tab=Most%20Relevance&keywords=Mont%20Kiara%2C%20Kuala%20Lumpur&filter_id=17&filter_type=Location&listing=For%20Rent&view=list"

#concatenate them together, with the coerced digit in between them. This digit is the page number
siteCom = paste(site_first, as.character(1), site_second, sep = "")
siteLocHTML = html(siteCom)


#Extract the descriptions of the first page...
siteLocHTML %>% html_nodes("h4.media-heading a") %>% 
  html_text() %>% data.frame() -> x

#...and also the links to these postings 
siteLocHTML %>%  html_nodes("#list-content") %>% 
  html_nodes(".media") %>% 
  html_nodes(".media-heading") %>% 
  html_nodes("a") %>%
  html_attr("href") %>% 
  data.frame() -> y

#Since we already have the extractions for the first page, so no we...
#...can loop through numbers 2 to 100 and rbind them with page 1 extracts
for(i in 2:250){

  siteCom = paste(site_first, as.character(i), site_second, sep = "")
  siteLocHTML = html(siteCom)
  
siteLocHTML %>% html_nodes("h4.media-heading a") %>% 
  html_text() %>% data.frame() -> x_next

siteLocHTML %>%  html_nodes("#list-content") %>% 
  html_nodes(".media") %>% 
  html_nodes(".media-heading") %>% 
  html_nodes("a") %>%
  html_attr("href") %>% 
  data.frame() -> y_next

x = rbind(x, x_next)
y = rbind(y, y_next)

}

#column bind the description and links
complete = cbind(x,y)
complete[,2] = as.character(complete[,2])

names(complete) = c("Description", "Link")

#file backup
write.csv(complete, "complete_propwall.csv", row.names = FALSE)

#And remove the remaining dataframes from the environment
rm(x_next, y_next, x, y)

#Extraction of rental rates.

price = c()
dates = c()
furnish = c()
layout = c()

for(i in 1:nrow(complete)){
siteLocHTML = html(complete[i,"Link"])

siteLocHTML %>%  html_nodes(".clearfix") %>%
  html_nodes("#price") %>% html_text() %>% c() -> y

#Rentals come out in quadruples, but i need only one
y = unique(y)
price = c(price, y)


siteLocHTML %>% html_nodes("#content") %>% 
  html_nodes("p") %>% html_text() -> z

dates = c(dates, z)

siteLocHTML %>%  html_nodes(".clearfix") %>%
  html_nodes("td") %>% html_text() %>% c() -> a
b = a[8]
a = a[4]

furnish = c(furnish, a)
layout = c(layout, b)

}

#take only the first 4922 rows
final = complete[1:4922,] 

#cbind the new columns
final$price = price
final$furnish = furnish[1:4922]
final$layout = layout[1:4922]
final$posted = dates[1:4922]

#remove NAs
final_fil = na.omit(final)

#remove other NAs as shown in the website
final_fil = final_fil[!final_fil[,4] == "-NA-",]
final_fil = final_fil[!final_fil[,5] == "-NA-",]

#remove rownames
rownames(final_fil) = NULL

final_fil$furnish = as.factor(final_fil$furnish)
final_fil$layout = as.factor(final_fil$layout)

rm(y,z,a,b,price, furnish, layout, dates)

#Extracting dates and rentals
x = c()
for(i in 1:nrow(final_fil)){

  x[i] = substring(final_fil[i, "posted"], nchar(final_fil[i, "posted"])-9, nchar(final_fil[i, "posted"]))

}

rm(complete, final,a,b,furnish, i, layout, price)

final_fil$date = x

x = c()

for(i in 1:nrow(final_fil)){
  
y = gregexpr("\\(",final_fil[i,"price"])[[1]][1]

x[i] = substring(final_fil[i,"price"], 4, y-2)
  
}
z
x = as.integer(str_replace_all(x, ",", ""))

final_fil$rental = x

#Extract only the name of the residency
final_fil$residency = str_replace_all(final_fil[,1], ", Mont Kiara", "")

#Remove crazy outliers
final_fil = final_fil[!final_fil[,"rental"] > 100000,]

Mkiara_data = data.frame(summarise(group_by(final_fil, residency, layout), Average_Rental = mean(rental)))
rownames(Mkiara_data) = NULL

Mkiara_data$residency = as.factor(Mkiara_data$residency)

#To get maximums
group_by(Mkiara_data, residency) ->x
summarise(x, Maximum = max(Average_Rental)) -> maximums

Mkiara_data_max = right_join(Mkiara_data, maximums, by="residency")

Mkiara_complete = mutate(Mkiara_data_max, adjusted = Maximum - Average_Rental)

ggplot(x, aes(x=reorder(residency,Freq), y=Freq, fill = furnish, order=desc(furnish))) + 
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Dark2") +
  
#Number of posts per residency
x = data.frame(table(final_fil$residency)) 
names(x) = c("Residency", "Number_of_Posts")  

ggplot(x, aes(x=reorder(Residency,Number_of_Posts), y=Number_of_Posts)) + 
  geom_bar(stat="identity", color="white") + 
  xlab("Residency") + ylab("Number of posts") + coord_flip() + 
  ggtitle("Number of posts per residency") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Red"))

data.frame(table(final_fil[,c("residency, layout")]))
  
x = data.frame(table(final_fil[, c("residency", "furnish", "layout")]))

#Create table for residency and layout columns
y = data.frame(table(final_fil[,c("residency", "layout")]))
y = y[y[,"Freq"] != 0,]
names(y) = c("residency", "layout", "Posts")
rownames(y) = NULL

#Plot
ggplot(y, aes(x=residency, y=Posts, fill=layout)) + 
  geom_bar(stat="identity") + coord_flip() + 
  scale_fill_brewer(palette="Dark2") + 
  xlab("Residency") + ggtitle("Number of posts per residency, with layouts") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Black")) + 
  theme(axis.text.y=element_text(face = "bold", color = "black"), 
        axis.text.x=element_text(face = "bold", color = "black"))


#Create table for residency and furnishing type
z = data.frame(table(final_fil[,c("residency", "furnish")]))
z = z[z[,"Freq"] != 0,]
names(z) = c("residency", "furnishing", "Posts")
rowvnames(z) = NULL

#Plot
ggplot(z, aes(x=reorder(residency,Posts), y=Posts, fill=furnishing)) + 
  geom_bar(stat="identity") + coord_flip() + 
  scale_fill_brewer(palette="Dark2") + 
  xlab("Residency") + ggtitle("Number of posts per residency, with furnishing type") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Black")) + 
  theme(axis.text.y=element_text(face = "bold", color = "black"), 
        axis.text.x=element_tewdecext(face = "bold", color = "black"))

avgs_propwall = final_fil %>% group_by(residency, layout) %>% summarise(Average = mean(rental), Max = max(rental), Min = min(rental), Median = median(rental)*1.0)

Breaks= c()
for(i in 1:30){Breaks[i] = i*1000}

ggplot(final_fil, aes(x=layout, y=rental)) + geom_boxplot() + 
  stat_summary(fun.y = "mean", geom="point", shape = 22, size = 3, fill = "red") + 
  xlab("Layout") + ggtitle("Distribution of rentals per layout") + ylab("Rentals") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Black")) + 
  theme(axis.text.y=element_text(face = "bold", color = "black"), 
        axis.text.x=element_text(face = "bold", color = "black")) +
  scale_y_continuous(breaks=Breaks)

filter = final_fil[,"layout"] == levels(final_fil[,"layout"])[7]

ggplot(final_fil[filter,], aes(x=reorder(residency,rental), y=rental)) + geom_boxplot(fill = "orange") + 
  coord_flip() +  facet_grid(. ~ layout) + scale_y_continuous(breaks=Breaks) + 
  stat_summary(fun.y = "mean", geom="point", shape = 22, size = 2, fill = "red") + 
  xlab("Layout") + ggtitle("Average rentals, per residency, per layout") + ylab("Rentals") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Black")) + 
  theme(axis.text.y=element_text(face = "bold", color = "black"), 
        axis.text.x=element_text(face = "bold", color = "black")) + 
  theme(axis.text.x = element_text(angle =30, hjust=1, vjust=1))

levels(final_fil[,"layout"])

write.csv(avgs_propwall, "avgs_propwall.csv", row.names = FALSE)
