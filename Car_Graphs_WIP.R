#Graphs

Sum_data = data.frame(dplyr::summarise(group_by(compiled_fac, Car.Type, Model,Transm, Year, Engine.Cap), mean(Price)))

ggplot(table(compiled_fac[,c("Model", "Year")], aes(Model))) + geom_bar(stat = "identity")


plot(table(compiled_fac[,c("Model", "Car.Type")]))

vcd::mosaic(~ Model + Car.Type + Transm, data = compiled_fac, 
            direction = c("v", "h", "h"), highlighting="Car.Type", 
            highlighting_fill=c("light blue", "dark grey"))

vcd::mosaic(~ Model + Car.Type + Transm, data = compiled_fac, 
            direction = c("v", "h", "h"), shade=TRUE)

j = spacing_equal(sp = unit(1.5, "lines"))

vcd::mosaic(~ Model + Car.Type, data = compiled_fac, 
            direction = c("v", "h"), highlighting="Car.Type", 
            highlighting_fill=c("light blue", "dark grey"), spacing = j)


#Graph 1
simple_sum = summarise(group_by(compiled_fac, Year, Model), Count = length(Model))

ggplot(simple_sum, aes(reorder(Year,Count), Count, fill = Model)) + 
  geom_bar(stat = "identity") +   coord_flip() + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Model Year") + ggtitle("Number of sale posts, by Model and Year") + 
  theme(plot.title=element_text(size=16, face = "bold", color = "Black")) + 
  theme(axis.text.y=element_text(face = "bold", color = "black", size = 12), 
        axis.text.x=element_text(face = "bold", color = "black", size = 12))


compiled_fac = compiled_fac[-3925,] 

#Graph 2
simple_sum = filter(compiled_fac, Car.Type == "UsedCar", Year == "2014")

l = lm(Price ~ Mileage, simple_sum)

ggplot(simple_sum, aes(Price, Mileage)) + geom_point() + stat_smooth()
