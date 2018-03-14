library(tidyverse)
library(sf)
library(QuantPsyc)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}
risk <- st_read("Export_Output.shp")
pallete4_10_colors <- c("#E8DB4C","#E2B645","#DC923E","#D66D37","#D14931","#AF483E","#8D474B","#6B4758","#494665","#274672")
ggplot() +
  geom_sf(data=risk, aes(fill=a_v3), colour=NA) +
  labs(title= "Burglaries by Fishnet") +
  scale_fill_gradientn(colors = pallete4_10_colors,                                                                                        breaks=c(1,10),labels=c("Low\nScore","High\nScore"),
                       name = "burglary count") +
  mapTheme()
ggplot(risk, aes(a_v3)) + geom_histogram(binwidth = 1)
ggplot(risk, aes(log(Count_))) + geom_histogram(binwidth = 1)
#poisson distribution
set.seed(612312)
n <- 1000
x <- runif(n)
mean <- exp(x)
y <- rpois(n,mean)
hist(y)
risk2 <- risk[,c(4:7,9:16,25,35,45,48)]
names(risk2)
library(dplyr)

colnames(risk2)[colnames(risk2)=="MEAN"] <- "B_DENS"
colnames(risk2)[colnames(risk2)=="MEAN_1"] <- "L_DENS"
colnames(risk2)[colnames(risk2)=="MEAN_12"] <- "G_DENS"
names(risk2)
#regression
reg <- glm(count ~ lag.hour +temperature+precipitation+PopDensity+PopUnder18+Pop65Over, family = "poisson", 
           data= d)
summary(reg)
reg <- glm(Count_ ~ ., family = "poisson", 
           data= risk2 %>% 
             as.data.frame %>% 
             dplyr::select(-geometry, ))
summary(reg)
#
standardized <- as.data.frame(lm.beta(reg))
standardized$variable <- row.names(standardized)
colnames(standardized)[1] <- "std_coefficient"
standardized

ggplot(standardized, aes(x=variable, y=std_coefficient, fill=variable)) + 
  geom_bar(stat="identity")
ggplot(standardi zed, aes(x=variable, y=abs(std_coefficient), fill=variable)) + 
  geom_bar(stat="identity")

standardized$absCoef <- abs(standardized$std_coefficient)

ggplot(standardized, aes(x=reorder(variable,-absCoef), y=absCoef, fill=variable)) + 
  geom_bar(stat="identity")
#
range(exp(reg$fitted.values))

risk <- cbind(risk2,reg$fitted.values)

ggplot() +
  geom_sf(data=risk2, aes(fill=reg.fitted.values), colour=NA) +
  labs(title= "Predicted Burglaries") +
  scale_fill_gradientn(colors = pallete4_10_colors,                                                                                        breaks=c(1,10),labels=c("Low\nScore","High\nScore"),
                       name = "Predicted count") +
  mapTheme()
write.csv(risk2, "rtmOutput.csv")

countComparisons <- read_excel("countComparisons.xlsx")
countComparisons <- 
  countComparisons %>%
  dplyr::select(GRIDCODE, kernelCnt, fittedCnt)
countComparisons<-countComparisons[-6,]
countComparisons <- cbind(
  countComparisons,
  data.frame(Category = c("90% - 100%", "70% - 89%", "50% - 69%", "30% - 49%", "1% - 29%")))

countComparisons <- 
  countComparisons %>%
  dplyr::mutate(kernelPct = kernelCnt / sum(kernelCnt),
                fittedPct = fittedCnt / sum(fittedCnt))

countComparisons

countComparisonsLong <-
  countComparisons %>% 
  gather(Variable, Value, kernelPct:fittedPct)

ggplot(data=countComparisonsLong, aes(Category,Value)) +
  geom_bar(aes(fill = Variable), position = "dodge", stat="identity") +
  scale_fill_manual(values = c("darkgreen", "blue"),
                    labels=c("Risk Terrain", "Kernel Density")) +
  labs(x= "Predicted Risk Levels",
       y="Percent of correctly predicted cases",
       title= "Goodness of Fit: Risk Terrain vs. Kernel Density hotspot") 
