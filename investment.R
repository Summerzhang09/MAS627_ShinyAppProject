library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggpubr)
library(gridExtra)

# load data
setwd("C:/Users/rong.li/Desktop/CourseProject/data")
load("stocklist.RData")
load("stockprice.RData")

# delete duplicatess
stocklist %<>% distinct()
stockprice %<>% distinct()

# filter the stock don't have full trading days
a <- stockprice %>% group_by(symbol) %>% summarise(count = sum(year(date) == 2020))
a %<>% filter(count == 106)
stocklist %<>% filter(symbol %in% a$symbol)
stockprice %<>% filter(symbol %in% a$symbol)
rm(a)


# save the clean dataset
save(stocklist, file = "cleanlist.RData")
save(stockprice, file = "cleanprice.RData")


# find the stock I want to hold
melist <- stocklist %>% filter(symbol %in% c("MRNA", "ZM", "UBER", "PINS", "PENN", "CTVA"))
melist$company
# "Moderna, Inc."                   "Penn National Gaming, Inc."     
# "Zoom Video Communications, Inc." "Corteva, Inc."                  
# "Pinterest, Inc."                 "Uber Technologies, Inc."

melist$sector
melist$industry

# find the price of six stocks
meprice <- stockprice %>% filter(symbol %in% melist$symbol)

# find the price on 2020-07-01
mepricefirst <- meprice %>% filter(date == "2020-07-01")
#unique(mepricefirst$symbol)
# "ZM"   "CTVA" "PINS" "UBER" "MRNA" "PENN"

# Assume I have $250000 on July 1st
shareratio <- c(0.2, 0.1, 0.1, 0.2, 0.3, 0.1) * 250000 # buy 20% zoom, 10% CTVA, 10% PINS, 20% UBER, 30% MRNA, 10% PENN
shareset <- floor(shareratio/mepricefirst$close) # calculate the shares of each company
#round(sum(shareratio - shareset * mepricefirst$close), 2) # still left $345.59
shareset[1] <- shareset[1] + 1
shareset[3] <- shareset[3] + 1
shareset[5] <- shareset[5] + 1
rest <- round(sum(shareratio - shareset * mepricefirst$close), 2)
#rest = 1.59

shareratio <- (shareset * mepricefirst$close) / sum(shareset * mepricefirst$close)
mepricefirst %<>% mutate(share = shareset, amount = shareset * mepricefirst$close, percent = paste(round(shareratio*100), "%"))

df <- mepricefirst[order(mepricefirst$percent, decreasing = TRUE), c(1, 9, 10, 11)]
df$company <- c("Maderna", "Zoom", "Uber", "Corteva", "Pinterest", "Penn")
df %<>% select(symbol, company, share, amount, percent)
df2 <- data.frame(company = "Total", share = " ", amount = sum(df$amount), percent = paste(100, "%"))
rbind(df[, 2:5], df2)

#some donut graphs
df$labs <- paste0(df$company, " (", df$percent, ")")
p2 <- ggdonutchart(df, "amount",
             label = "labs", 
             lab.pos = "out", 
             fill = "company",
             lab.adjust = 0,
             lab.font = c(4, "bold", "grey"),
             color = "white",
             palette = "Greens") + 
  ggtitle("The six holdings") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  coord_polar(theta = "y", start = 0, clip = "off")

df3 <- melist[-5,] #delete PINS
df3 %<>% left_join(df, by = "symbol")
df3 %<>% select(sector, amount, percent)
df3$amount[3] <- 75014.81 
df3$percent[3] <- "30 %"
df3$labs <- paste0(df3$sector, " (", df3$percent, ")")
p3 <- ggdonutchart(df3, "amount",
             label = "labs", 
             lab.pos = "out", 
             fill = "sector",
             lab.adjust = 0,
             lab.font = c(4, "bold", "grey"),
             color = "white",
             palette = "Greens") + 
  ggtitle("The sectors") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  coord_polar(theta = "y", start = 0, clip = "off")
grid.arrange(p2, p3, ncol=2)


# calculate the revenue every day
# unique(meprice$symbol)
# "ZM"   "CTVA" "PINS" "UBER" "MRNA" "PENN"
revenue <- meprice[, c(1, 2, 6)] %>% pivot_wider(names_from = symbol, 
                                   values_from = close)
# forget to download data on 2020-12-01, add the close price of the six stocks
addrevenue <- data.frame(date = "2020-12-01", 
                         ZM = 406.31, 
                         CTVA = 38.31, 
                         PINS = 68.21, 
                         UBER = 49.63, 
                         MRNA = 141.01, 
                         PENN = 70.03)
addrevenue$date %<>% as.Date()
revenue <- rbind(revenue, addrevenue)
rm(addrevenue)

total <- as.matrix(revenue[, 2:7]) %*% as.matrix(shareset) + rest
revenue$total <- as.vector(total)
rm(total)
revenue$ZMr <- revenue$ZM * shareset[1]
revenue$CTVAr <- revenue$CTVA * shareset[2]
revenue$PINSr <- revenue$PINS * shareset[3]
revenue$UBERr <- revenue$UBER * shareset[4]
revenue$MRNAr <- revenue$MRNA * shareset[5]
revenue$PENNr <- revenue$PENN * shareset[6]

revenue %<>% select(date, total, ZM, ZMr, CTVA, CTVAr, PINS, PINSr, UBER, UBERr, MRNA, MRNAr, PENN, PENNr)

# draw picture
ggplot(revenue, mapping = aes(x = date, y = total)) + 
  geom_line() + 
  ylab("revenue") + 
  xlab("month") + 
  ggtitle("The asset values every day") + 
  theme(plot.title = element_text(hjust = 0.5))

 
