#load libraries
library(stringr)
library(rvest)
library(ggvis)
library(dplyr)
library(ggplot2)

#The site
Site = "http://www.carlist.my/"

#The first half of the URL..
site_first = "http://www.carlist.my/used-cars/perodua?page_number=" #Here You can select the URL of search results; used or new 
http://www.carlist.my/used-cars?page_number=
  
#concatenate them together, with the coerced digit in between them. This digit is the page number
siteCom = paste(site_first, as.character(1), sep = "")
siteLocHTML = html(siteCom)

siteLocHTML %>% html_nodes(".js-vr38dett-title") %>% 
  html_text() %>% data.frame() -> x

siteLocHTML %>% html_nodes(".js-vr38dett-title") %>% 
  html_attr("href") %>% data.frame() -> y

Links = function(limit){
  
  for(i in 2:limit){
    
    siteCom = paste(site_first, as.character(i), sep = "")
    siteLocHTML = html(siteCom)
    
    siteLocHTML %>% html_nodes(".js-vr38dett-title") %>% 
      html_text() %>% data.frame() -> x_next
    
    siteLocHTML %>% html_nodes(".js-vr38dett-title") %>% 
      html_attr("href") %>% data.frame() -> y_next
    
    x = rbind(x, x_next)
    y = rbind(y, y_next)
    
  }

z = cbind(x,y)
return(z)
}

complete = Links(250) #The 250 represents the search pages
rm(x,y)

for(i in 3:12){
  complete[,i] = NA
}

headers = c("Desc", "Link", "Make", "Model", "Year", "Engine.Cap", "Transm", "Mileage", "Color", "Car.Type", "Updated", "Price")

names(complete) = headers

write.csv(complete, "Links_CarList_Used.csv", row.names = FALSE) #export the links, for backup

Details = function(dataframe, i){

  for(j in 1:nrow(dataframe)){  
  
        link = html(paste(Site, dataframe[j, i], sep = ""))
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-make") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
          dataframe[j,"Make"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-model") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Model"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-year") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Year"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-engine") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Engine.Cap"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-transmission") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Transm"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-mileage") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, "  ", "")
        
        dataframe[j,"Mileage"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-color") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Color"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-car-type") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Car.Type"] = a
        
        link %>% html_nodes("#single-post-detail") %>% 
          html_nodes(".section-body") %>% 
          html_nodes(".row-fluid") %>% 
          html_nodes(".span6") %>% 
          html_nodes(".list") %>% 
          html_nodes(".tr-updated") %>% 
          html_nodes(".data") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        #a = str_replace_all(a, " ", "")
        
        dataframe[j,"Updated"] = a
        

        link %>% html_nodes("#single-post-header") %>% 
          html_nodes(".post-highlight") %>% 
          html_nodes(".price") %>% 
          html_text() -> a
        
        a = str_replace_all(a, "\n", "")
        a = str_replace_all(a, " ", "")
        
        dataframe[j,"Price"] = a
        
}

return(dataframe)

}

Final = Details(complete, 2)
write.csv(Final, "Final_CarList_Used.csv", row.names = FALSE)