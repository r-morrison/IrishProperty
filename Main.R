today <- gsub("-", "", as.Date(Sys.time()))

# Load data
priceData <- read.csv("PPR-ALL.csv")

# Format data
priceData$Address = as.character(priceData$Address)
priceData$County = as.character(priceData$County)
priceData$Price = as.numeric(format_prices(priceData$Price))
priceData$DateOfSale <- as.Date(priceData$DateOfSale, "%d/%m/%Y")
priceData$Month <- as.Date(priceData$DateOfSale, format="%m/%Y")

priceDataDublin <- priceData %>%
  filter(County == "Dublin")

# Theme
theme_plot2 <- theme(
  axis.text.x = element_text(size = 16, color = 'black', face = 'bold'),
  panel.background = element_rect(fill = 'white', colour = 'gray50'),
  panel.grid.minor = element_line('gray90'),
  panel.grid.major = element_line(colour = 'gray90'),
  axis.text.y = element_text(size = 16, color = 'black', face = 'bold'),
  axis.title.x = element_text(size = 16, face = 'bold'),
  axis.title.y = element_text(size = 16, face = 'bold'),
  legend.title = element_blank(),
  legend.text = element_text(size = 16, face = 'bold'),
  legend.position = "bottom",
  plot.title = element_text(size = 20, face = 'bold'),
  strip.text.x = element_text(size = 18, face = 'bold')
)

# Remove euro sign and commas from Price
format_prices <- function(x) {
  gsub('Ç|,|€', '', x)
}

#
millions<-function(x) {
  x<-x/1000000
  str_c("$",x,"M")
}

# Units sold per month
monthly <- priceData %>%
  mutate(month = as.Date(priceData$DateOfSale, format="%m/%Y")) %>%
  group_by(month) %>%
  summarise(total = length(Price), avg_price = mean(Price))

# Un-comment lines below to create jpeg of graph
#filename1 <- paste0("IrelandSalesCount_", today, ".jpeg")
#jpeg(filename = filename1, width = 1200, height = 800, quality = 100)
ggplot(data=monthly, aes(x=month, y=total)) +
  geom_line(size = 2) + theme_plot2 +
  scale_x_date(date_breaks = "12 months", date_labels = "%b\n%Y") +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  ggtitle("House sales per year") + xlab("") + ylab("") +
  expand_limits(y = 0)
#dev.off()

# Average price per month
ggplot(data=monthly,aes(x=month,y=avg_price)) +
  geom_line(size = 2) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Average price by month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,1500000,by=250000),labels=millions) +
  coord_cartesian(ylim=c(0,1250000)) +
  stat_summary(fun.y=mean,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")

# Median sale price by month
priceData$month_only<-format(priceData$Month,"%b")
priceData$month_only<-factor(priceData$month_only,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
priceData$year_only<-format(priceData$Month,"%Y")

month<-priceData %>% group_by(month_only,year_only) %>% summarise(LEN=length(Price),MED=median(Price))


ggplot(data=month,aes(x=month_only,y=MED,group=month_only)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_y_continuous(breaks=seq(0,1500000,by=100000),labels=millions) +
  ggtitle("Median price by month of year") + xlab("") + ylab("") +
  expand_limits(y=c(0)) 
