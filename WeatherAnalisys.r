#Import file local
weatherDS <- read.csv("D:\\OneDrive\\Documentos\\TSOM\\Data Handling and Decision Making\\assignment2.csv",    
    stringsAsFactors = TRUE,
    na.string = "");
weatherDS;

#Transform the column year to a factor
weatherDS$year <- as.factor(weatherDS$year); 
#Verify it is a factor
class(weatherDS$year); 
#Add a new column with the concatenated value of year and month
weatherDS <- transform(weatherDS, 
            year_month = paste(year, "",month));
#Create a new data frame with only the unique values
weatherDS2 <- weatherDS[!duplicated(weatherDS$year_month,fromLast = "TRUE"),];
#Verify it only has 96 rows
nrow(weatherDS2);

#Create a sequence to repleace year
weatherDS2$year <- as.factor(rep(2000:2007, each = 12));
#Finding the month missing ans repleacing it
is.na(weatherDS2$month);
weatherDS2[18,2] <- "Jun";
summary(weatherDS2);
#Finding the average temperature by month and repleacing the missing value acording to its month
aggregate(weatherDS2[,3], by = list(month = weatherDS2$month), FUN = mean, na.rm = TRUE);
is.na(weatherDS2$temperature);
weatherDS2[25,3] <- is.numeric(-1.3);
weatherDS2[90,3] <- is.numeric(18.5);
summary(weatherDS2);
#Finding the average precipitation by month and repleacing the missing value acording to its month
aggregate(weatherDS2[,4], by = list(month = weatherDS2$month), FUN = mean, na.rm = TRUE);
is.na(weatherDS2$precipitation);
weatherDS2;
weatherDS2[4,4] <- is.numeric(58.94);
weatherDS2[54,4] <- is.numeric(103.27);
weatherDS2[83,4] <- is.numeric(72.08);
summary(weatherDS2);

#Define levels of season
weatherDS2$month <- factor(weatherDS2$month, levels=month.abb);

weatherDS2<- transform(weatherDS2,
            year_month = paste(year, "",month),
			season = cut(as.integer(month),
				   breaks = c(0,2,5,8,11,12),
			         labels = c("winter","spring","summer","fall","winter")));

weatherDS3<- transform(weatherDS2,
            temperatureF = (temperature * 9/5) + 32,
			precipitationCm = precipitation / 10);
summary(weatherDS3);

#Sumarize data

weatherDS4 <- data.frame(avg_temperature = mean(weatherDS3$temperature),
                avg_precipitation = mean(weatherDS3$precipitation),
                sd_temperature = sd(weatherDS3$temperature),
                sd_precipitation = sd(weatherDS3$precipitation),
                max_temperature = max(weatherDS3$temperature),
                max_precipitation = max(weatherDS3$precipitation),
                min_temperature = min(weatherDS3$temperature),
                min_precipitation = min(weatherDS3$precipitation));
weatherDS4;

weatherDS_m <- aggregate.data.frame(weatherDS3[,c(3:4)], by = list(weatherDS3$month), FUN = mean);
weatherDS_m;
weatherDS_s <- aggregate(weatherDS3[,c(3:4)], 
                by = list(weatherDS3$year, weatherDS3$season), 
                FUN = function(x) summary(x));
weatherDS_s;

#Save new file
write.csv(weatherDS2, "D:\\OneDrive\\Documentos\\TSOM\\Data Handling and Decision Making\\weatherDS2.csv");
