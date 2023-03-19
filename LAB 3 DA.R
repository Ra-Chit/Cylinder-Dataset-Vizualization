data= read.table("E:/SEM 6/DATA VIZ/LAB/DATASET/bands.data",sep=',')
data[data == '?'] ="NA"


data= setNames(data, c("timestamp","cylinder","customer","job_number","grain_screened"
                       ,"ink_color","proof_on_ctd_ink","blade_mfg","cylinder_division","paper_type","ink_type"," direct_steam",
                       "solvent_type","solvent_type","press_type","press","unit_number","cylinder_size","location","plating_tank",
                       "proof_cut","viscosity","caliper","ink_temperatur","humifity","roughness","blade_pressure",
                       "varnish_pct"," press_speed","ink_pct","solvent_pct","ESA_Voltage","ESA_Amperage","wax","hardener","roller_durometer","urrent_density","anode_space_ratio",
                       "chrome_content","band_type"
))

View(data)
str(data)

#data cleaning
colSums(is.na(data))
colnames(data) <- make.unique(names(data))
data <- data[!duplicated(data), ]

data <- data                                              # Duplicate data frame
for(i in 1:ncol(data)) {                                   # Replace NA in all columns
  data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
}

colSums(is.na(data))

View(data)

#BAR PLOT
library(dplyr)

df1 <- data %>% group_by(ink_type)
View(df1)
df1=type.convert(df1)
df1 <- df1 %>% summarise(sum(urrent_density))
str(df1)
View(df1)
names(df1)
names(df1)[2] <- "urrent_density"
#GGPLOT
library(ggplot2)
my_plot = ggplot(data = df1,aes(x = ink_type, y = urrent_density))
my_plot + 
  geom_bar( stat = "identity", fill = "#42EADDFF") +
  ggtitle("20BDS0008") 
 
#cord flip
 
my_plot + coord_flip()

#Error bars
df2 <- data %>% group_by(ink_type, band_type) %>%
  summarise(urrent_density=sum(urrent_density),
            anode_space_ratio = sum(anode_space_ratio),
            .groups = 'drop')
str(df2)
View(df2)
names(df2)
my_plot1 <- ggplot(data = df2) +
  geom_bar(aes(x = factor(urrent_density), y = anode_space_ratio), stat = "summary", fun.y = "mean")
my_plot1 + 
  geom_errorbar(aes(x = factor(urrent_density), y = anode_space_ratio, ymin = anode_space_ratio - sd(anode_space_ratio), 
                    ymax = anode_space_ratio + sd(anode_space_ratio)), 
                width = 0.2, color = "#97BC62FF", size = 1.5)+ggtitle("20BDS0008")
#STACKED BAR PLOTS

ggplot(data, aes(fill=band_type, y=ink_type, x=anode_space_ratio)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values = c("#F93822FF", "orange"))+ggtitle("20BDS0008")

#Scatter plot

df3 <- data %>% group_by(customer, press_type) %>%
  summarise(urrent_density=sum(ink_temperatur),
            anode_space_ratio = sum(humifity),
            .groups = 'drop')
View(df3)
str(df3)
names(df2)
#1

ggplot(df3,aes(press_type,urrent_density))+geom_point(shape=23)
ggplot(df3,aes(urrent_density,anode_space_ratio))+geom_point(shape=25)

#HEAT MAP
data1=data[1:529,]
data1=matrix(c(data1$viscosity),nrow = 23)
View(data1)

heatmap(data1,main = "20BDS0008")

#DIVERGNCE 
library(gplots)
heatmap.2(data1, col=redblue(75), trace="none", dendrogram="none",
          key=TRUE, keysize=1.5, key.title="Value", symkey=FALSE, symkeysize=0.5, density.info="none", main="20BDS0008")

#3d Pie chart
str(data)
data=type.convert(data)
df5 <- data %>% group_by(ink_type) %>% 
  summarise(viscosity=sum(viscosity))
View(df5)
str(df5)
x=df5$ink_type
label=c(x)
label
library(plotrix)
values = df5$viscosity
pie3D(values, col = rainbow(length(values)),labels = label,main="20BDS0008")


