headers  = names(New)

headers[8] = "Mileage"

headers

names(New) = headers

compiled = rbind(New, Used_1, Used_2) 

sum(duplicated(compiled[,2]))

levels(as.factor(compiled[,"Model"]))

diff = compiled[,"Model"] == "AXIA"

compiled[diff,"Model"] = "Axia"

compiled[diff,"Model"]

unique(compiled[,"Car.Type"])


#___________________________________________

prices = compiled[,"Price"]
prices = str_replace_all(prices, "RM", "")
prices = str_replace_all(prices, ",", "")

prices_int = as.integer(prices)

compiled[,"Price"] = prices_int

dates = as.POSIXct(compiled[,"Updated"], format="%B %d, %Y")
compiled[,"Updated"] = dates

#_______________________________________________

compiled_fac = compiled

factor.columns = c("Model", "Year", "Transm", "Car.Type", "Color", "Engine.Cap")

for(i in 1:length(factor.columns)){
  
  compiled_fac[,factor.columns[i]] = as.factor(compiled_fac[,factor.columns[i]]) 
  
}


str(compiled_fac)




#__________________________________________________

miles = compiled_fac[, "Mileage"]

#Remove "km" from all observations
miles = str_replace_all(miles, "km", "")

#Isolate observations that have "k" in a separate dataframe
miles_df = data.frame(miles[grep("k", miles)])

#Add two columns to this new df
miles_df[,"Start"] = NA
miles_df[,"Stop"] = NA



for(i in 1: nrow(miles_df)){
  
  #To assign the location of the hyphen in string
  limit = as.integer(gregexpr(" - ", miles_df[i,1]))
  
  #Extract only lower end of the range and assign to Start column
  miles_df[i, "Start"] = substr(miles_df[i,1], 0, limit - 1)
  
  #Extract only higher end of the range and assign to Stop column
  miles_df[i, "Stop"] = substr(miles_df[i,1], 
                               as.integer(gregexpr(" - ", miles_df[i,1])) + 3, 
                               nchar(as.character(miles_df[i,1])))
  
}

#Remove the "k" from lower end
miles_df[, "Stop"] = str_replace_all(miles_df[,"Stop"], "k ", "")

#check if any NAs
anyNA(as.integer(miles_df[, "Stop"])) 
anyNA(as.integer(miles_df[, "Start"]))

#Prepare columns for calculation
miles_df[,"Start"] = as.integer(miles_df[,"Start"])
miles_df[,"Stop"] = as.integer(miles_df[,"Stop"])


dplyr::mutate(miles_df, avg_miles = ((Start + Stop)/2)*1000) -> miles_df_adj
new_miles = miles_df_adj$avg_miles

miles[grep("k", miles)] = new_miles

miles = as.integer(str_replace_all(miles, ",", ""))

compiled_fac[,"Mileage"] = miles

write.csv(compiled_fac, "Final_CarList_Compiled.csv", row.names = FALSE)
