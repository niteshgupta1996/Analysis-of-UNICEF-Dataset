---
title: "IE6600_Sec03_Group08_Hackathon"
author: "Group 08"
date: "3/1/2021"
output:
  html_document:
    df_print: paged
fig_width: 10
fig_height: 6
---

                                         1.1 Overview
The UNICEF's Dataset contains the key issues affecting the State of the World's Children. We are going to assess the situation of children and women in the areas of education, health, gender equality, rights and protection in form of visualization using R software and try to gain some insight from those visualization to compare the situation of the children and women in the world right now based on different factors which affect them the most.

                                     1.2 Problem statement
Our objective is to find the disparities in school enrollment across various regions, compare child labor and violence discipline with literacy rates achieved . In this report, the main objective is split into 3 sections.
First Section identifies countries that are yet to achieve literacy on par with the other nations, disparities in school enrollment at various levels and identifies how gender and other demographics factors are influencing the enrollment. Second section talks on how child labor is associated with lower secondary school enrollment and gender. Section 3 is visualizes violent discipline and its relation with literacy.

```{r}
knitr::opts_chunk$set(warning = FALSE,message = FALSE)
```
 
```{r Loading Libraries}
library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(extrafont)
loadfonts(device = "win")
library(corrplot)
library(tidyr)
library(data.table)
library(scales)
library(rio)
library(readxl)
library(lubridate)
library(stringr)
library(ggalluvial)
```

```{r Loading the Dataset}
Data <- read_excel("Hackathon_Unicef_Dataset.xlsx", sheet = "Data" )
```
# Section 1
```{r}
req_indicators <- data %>% 
  filter(data$Indicator == ("Pre-primary school participation - Gross enrolment ratio (%), male") | 
  data$Indicator ==("Pre-primary school participation - Gross enrolment ratio (%), female")| 
           data$Indicator==("Primary school participation - Net enrolment ratio (%), male") | 
           data$Indicator == ("Primary school participation - Net enrolment ratio (%), female") |
           data$Indicator ==("Lower secondary school participation - Net enrolment ratio (%), male") |
           data$Indicator == ("Lower secondary school participation - Net enrolment ratio (%), female") |  
           data$Indicator == ("Primary school participation - Out-of-school rate of children of primary school age (%), male")|
           data$Indicator==("Primary school participation - Out-of-school rate of children of primary school age (%), female")|
           data$Indicator==("Primary school participation - Survival rate to last primary grade (%) , male") |
           data$Indicator==("Primary school participation - Survival rate to last primary grade (%) , female") |
           data$Indicator==("Primary school net attendance ratio, Urban") |
           data$Indicator==("Primary school net attendance ratio, Rural")
         )

req_regions <- req_indicators %>%
  filter((Countries.and.areas=='Central African Republic') | 
        (Countries.and.areas=='East Asia and Pacific')  |
        (Countries.and.areas=='Europe and Central Asia') |
        (Countries.and.areas=='Eastern Europe and Central Asia') |
        (Countries.and.areas=='Western Europe') | 
        (Countries.and.areas=='Middle East and North Africa') |
        (Countries.and.areas=='Eastern and Southern Africa') | 
        (Countries.and.areas=='West and Central Africa')
          )
pivot_data <- req_regions %>%
  select(Countries.and.areas,Indicator, Value) %>%
  pivot_wider(names_from = Indicator, values_from = Value )
# Selecting required columns
req_df <- data %>% 
  filter(data$Indicator == ("Total adult literacy rate (%)") | data$Indicator ==("Primary school net enrolment ratio (%)")| 
           data$Indicator==("Pre-primary school participation - Gross enrolment ratio (%), male") | 
           data$Indicator == ("Pre-primary school participation - Gross enrolment ratio (%), female") |
           data$Indicator ==("Primary school participation - Gross enrolment ratio (%), male") |
           data$Indicator == ("Primary school participation - Gross enrolment ratio (%), female") |  
           data$Indicator == ("Primary school participation - Out-of-school rate of children of primary school age (%), male") |
           data$Indicator==("Primary school participation - Out-of-school rate of children of primary school age (%), female") |
           data$Indicator==("Primary school participation - Survival rate to last primary grade (%) , male") |
           data$Indicator==("Primary school participation - Survival rate to last primary grade (%) , female") |
           data$Indicator==("Father's support for learning") |
           data$Indicator == ("Primary school net attendance ratio, Ratio of richest to poorest") |
           data$Indicator == ("Primary school net enrolment ratio (%)"))
# Pivoting the data on Indicators
p_data <- req_df %>%
  select(Countries.and.areas,Indicator, Value) %>%
  pivot_wider(names_from = Indicator, values_from = Value )
p_data <- p_data %>%
  rename(out_of_school_male=`Primary school participation - Out-of-school rate of children of primary school age (%), male`) %>%
  rename(out_of_school_female=`Primary school participation - Out-of-school rate of children of primary school age (%), female`)
```
Most of the countries achieved literacy yet almost 40% of the countries are still under the average literacy rate of 80.


```{r}
p_data$newcol<-0
colnames(p_data)[14] <- "adult_literacy_rate_percenatge"
p_data$adult_literacy_rate_percenatge <- cut(p_data$`Total adult literacy rate (%)`, c(0,80,100), c("literacy below average", "literacy above average" ))
d5<-p_data %>%
  group_by(adult_literacy_rate_percenatge) %>%
  summarise(number_of_countries= n()) %>%
  drop_na()
chart_5 <-d5 %>%
  ggplot(aes(x =adult_literacy_rate_percenatge , y = number_of_countries)) + 
    geom_bar(stat = 'identity', fill='red3', width = 0.35, alpha=0.6) + theme(axis.text.x = element_text(angle=0, hjust = 1)) + geom_text(aes(label=number_of_countries),vjust=1.6,  color="white",  position = position_dodge(0.5), size=3.5)+
    labs(y="number of countries", x = "literacy") +labs(title= "62 countries are still under average literacy")+ theme(legend.position = "bottom")+ scale_fill_brewer(palette = "Set1")
chart_5
```
# Percenatge of children enrolling in Primary schools
```{r}
p_data$newcol<-0
colnames(p_data)[15] <- "primary_school_net_enrollment_ratio"
p_data$primary_school_net_enrollment_ratio <- cut(p_data$`Primary school net enrolment ratio (%)`, c(0,10,20,30,40,50,60,70,80,90, 100), c("0-10", "10-20", "20-30","30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100" ))
d4<-p_data %>%
  group_by(primary_school_net_enrollment_ratio) %>%
  summarise(number_of_countries= n()) %>%
  drop_na()
chart_6 <-d4 %>%
  ggplot(aes(x =primary_school_net_enrollment_ratio , y = number_of_countries)) + 
    geom_bar(stat = 'identity', fill='red3', alpha=0.6) + theme(axis.text.x = element_text(angle=0, hjust = 1)) + geom_text(aes(label=number_of_countries),vjust=-0.5,  color="red3",  position = position_dodge(1), size=4)+
    labs(y="number of countries", x = "literacy") +labs(title= "Primary school enrollment ratios across various countries")+ theme(legend.position = "bottom")+ scale_fill_brewer(palette = "Set1")
chart_6
```

```{r fig, fig.width=11}
library(ggalluvial)
low_attendance<-p_data %>%
  select(Countries.and.areas, `Primary school net enrolment ratio (%)`) %>%
  arrange(`Primary school net enrolment ratio (%)`) %>%
  head(n=4)
chart_7 <- ggplot(data = low_attendance,
               aes(axis1 = Countries.and.areas, axis2 = `Primary school net enrolment ratio (%)`, y=`Primary school net enrolment ratio (%)`))+ scale_x_discrete(limits = c("Country", "Enrollment"), expand = c(.02, .02)) + geom_alluvium(aes(fill =Countries.and.areas  )) +
               geom_stratum() +
               geom_text(stat = "stratum", aes(label = after_stat(stratum)), size=2.5) +   
               theme_minimal() +
               ggtitle("<50 percent net enrollement in Primary school") + scale_fill_brewer(palette = "Set1") 
chart_7
```

In these 4 countries, less than 50 per cent of the children are attending primary schools.

```{r}
d<- mutate(pivot_data, pre_primary = `Pre-primary school participation - Gross enrolment ratio (%), male` +
             `Pre-primary school participation - Gross enrolment ratio (%), female`)
d1<- mutate(d, Primary = `Primary school participation - Net enrolment ratio (%), male` +
             `Primary school participation - Net enrolment ratio (%), female`)
d2<- mutate(d1, Lower_secondary = `Lower secondary school participation - Net enrolment ratio (%), male` +
             `Lower secondary school participation - Net enrolment ratio (%), female`)
chart_2<- d2 %>%
  select(Countries.and.areas, pre_primary, Primary, Lower_secondary) %>%
  drop_na() %>%
  gather(level, ratio, 2:4) %>%
  ggplot(aes(x = reorder(Countries.and.areas, -ratio) , y = ratio , fill = level)) + 
    geom_bar(stat = 'identity', alpha=0.7) + theme(axis.text.x = element_text(angle=45, hjust = 1)) + theme(legend.position = "top")+
    labs(y="ratio", x = "region") +labs(title= "Pre-Primary, Primary, Lower-Secondary School enrollment in various regions") + scale_fill_brewer(palette = "Set1") +  theme(legend.justification = "top")
chart_2
```

Ratio of children enrolling in Lower Secondary school is less in regions involving African countries. In East and Central Africa, number of children enrolling in school after primary level has gradually decreased.


```{r}
d3 <- pivot_data %>%
  select(Countries.and.areas, `Primary school net attendance ratio, Urban`, `Primary school net attendance ratio, Rural`) %>%
  gather(region, net_ratio, 2:3) %>%
  drop_na()
chart_4 <- d3 %>%
  ggplot(aes(x = reorder(Countries.and.areas, -net_ratio) , y = net_ratio , fill = region)) + 
    geom_bar(stat = 'identity', alpha=0.7) + theme(axis.text.x = element_text(angle=45, hjust = 1)) + theme(legend.position = "top") +
    labs(y="ratio", x = "region") +labs(title= " Primary School Net Attendance ratios in Rural vs Urban areas") + scale_fill_brewer(palette = "Set1") +  theme(legend.justification = "top")
chart_4
```

In developed and Developing nations, the ratios of children attending primary school are almost equal but in African regions, the differences in urban and rural education are more emphasized .

```{r}
chart_3<- pivot_data %>%
  select(Countries.and.areas,`Primary school participation - Out-of-school rate of children of primary school age (%), male`,
         `Primary school participation - Out-of-school rate of children of primary school age (%), female`) %>%
  rename(out_of_school_male=`Primary school participation - Out-of-school rate of children of primary school age (%), male`) %>%
  rename(out_of_school_female=`Primary school participation - Out-of-school rate of children of primary school age (%), female`) %>%
  drop_na() %>%
  gather(out_of_school, percentage, 2:3) %>%
  ggplot(aes(x = reorder(Countries.and.areas, -percentage) , y = percentage , fill = out_of_school)) + 
    geom_bar(stat = 'identity', alpha=0.7) + theme(axis.text.x = element_text(angle=45, hjust = 1)) + theme(legend.position = "top") +
    labs(y="ratio", x = "region") +labs(title= "Out of School percentages female vs male in various regions") + scale_fill_brewer(palette = "Set1") +  theme(legend.justification = "right") 
chart_3
```
Out of school percentages are greater for girls compared to boys.These differences are more pronounced in Africa.


```{r}
df1<-p_data %>%
  select(Countries.and.areas, out_of_school_male, out_of_school_female)
#Finding ratio of female and male out of school percentages.
df1<- mutate(df1, out_of_school_ratio_female_to_male= out_of_school_female/out_of_school_male )
# Filtering rows where there is highest disparity. ratio > 2
index1<- which(df1$out_of_school_ratio_female_to_male>=2 )
df2 <- df1[index1, c(1:4)]
```
```{r}
chart_1<- df2 %>%
  select(Countries.and.areas, out_of_school_male, out_of_school_female) %>%
  drop_na() %>%
  arrange(desc(out_of_school_female)) %>%
  top_n(20, out_of_school_female) %>%
  gather(perc, n, 2:3) %>%
  ggplot(aes(x = reorder(Countries.and.areas, -n) , y = n , fill = perc)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha=0.7) + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    labs(y="perecentage", x = "Country", color="Gender") +labs(title= "Highligting differences in Out of school percenatages - female vs male")+ theme(legend.position = "top")+ scale_fill_brewer(palette = "Set1")
chart_1
```
Least developed countries have a high percentages of children not attending schools. If we compare enrollment of male and females in school, more females are out of schools when compared to males. These differences are obvious in the below countries. In these countries, out of 3 children, there at least 2 girls that are not attending school. This stands true not just for less developed nations but also to highly developed ones such as UK and Norway. Though the out-of-school percentages are really low in these nations, it still holds true.

# Section 2
```{r}
df_partchildm <- Data %>% 
  filter(Data$Indicator ==("Lower secondary school participation - Net attendance ratio (%), male") | Data$Indicator ==("Child labour (%), male"))
library(dplyr)
df_partchildm1 <- df_partchildm %>%
    dplyr::select("Countries and areas",Indicator,Value)
df_partchildm1 <- transform(df_partchildm1, Value = as.integer(Value))
df_partchildm1 <- df_partchildm1 %>%
        pivot_wider(names_from = Indicator, values_from = Value )
partchildm <- ggplot(df_partchildm1, aes(y=`Lower secondary school participation - Net attendance ratio (%), male`
,x=`Child labour (%), male`))  + geom_point()
df_partchildf <- Data %>% 
  filter(Data$Indicator ==("Lower secondary school participation - Net attendance ratio (%), female") | Data$Indicator ==("Child labour (%), female"))
library(dplyr)
df_partchildf1 <- df_partchildf %>%
    dplyr::select("Countries and areas",Indicator,Value)
df_partchildf1 <- transform(df_partchildf1, Value = as.integer(Value))
df_partchildf1 <- df_partchildf1 %>%
        pivot_wider(names_from = Indicator, values_from = Value )
partchildf <- ggplot(df_partchildf1, aes(y=`Lower secondary school participation - Net attendance ratio (%), female`, x=`Child labour (%), female`))  + geom_point() + scale_fill_brewer(palette = "Set1")
grid.arrange(partchildm,partchildf,ncol = 2)
```
Conclusion -The plot is approximately same for male and female regardless of their gender. we can conclude that the country with the highest school participation has the lowest child labor and vice versa.
```{r}
df_childfm <- Data %>% 
  filter(Data$Indicator == ("Child labour (%), female") | Data$Indicator == ("Child labour (%), male"))
df_childfm1 <- df_childfm %>%
    dplyr::select("Countries and areas",Indicator,Value)
df_childfm1 <- transform(df_childfm1, Value = as.integer(Value))
childfm <- df_childfm1 %>% 
  filter(df_childfm1$Value > 30 )
ggchildfm <-ggplot(childfm,aes(x=Countries.and.areas,y =Value, color = Indicator ))  +
  geom_line() + geom_point() + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y="Value(%)", x = "Countries") +labs(title= "Child Labour Male vs Female")+ scale_fill_brewer(palette = "Accent")
ggchildfm
```
Conclusion - The child labor in these countries are more than the 20(%). In some country the (%) of male child labor is more than the (%) of female child labor and vice versa. The country such as Benin, Chad, Guinea-Bissau, mali, and somalia have the child labor more than 50(%) for male as well as female.
```{r}
area_childfm1 <- ggplot(df_childfm1, aes(x = Value, fill=Indicator)) +
           geom_area(stat="bin") +
           ggtitle("Stacked Area Graph for Comparison of child labour(%) of male and female")
area_childfm1 + scale_fill_brewer(palette="Accent") 
```

Conclusion -The height of each colored stack represents the proportion of the child labor at given point. Here, we can see that there is a spike in the child labor for female at the start of the graph that is greater than the male child labor %. 

Section 3 
```{r}
df_discf <- Data %>% 
  filter(Data$Indicator == ("Violent discipline (%), female") | Data$Indicator ==("Youth (15–24 years) literacy rate (%), female") )
df_discf1 <- df_discf %>%
   select(`Countries and areas`,Indicator,Value)
df_discf1 <- transform(df_discf1, Value = as.integer(Value))
discf1 <- df_discf1 %>%
        pivot_wider(names_from = Indicator, values_from = Value )
ggdiscf <- ggplot(discf1, aes(y=`Youth (15–24 years) literacy rate (%), female`
,x=`Violent discipline (%), female`))  + geom_point() + geom_vline(xintercept = 60 , color = "red", linetype = "dashed", size=2) + geom_hline(yintercept = 75 , color = "red", linetype = "dashed", size=2)
df_discm <- Data %>% 
  filter(Data$Indicator == ("Violent discipline (%), male") | Data$Indicator ==("Youth (15–24 years) literacy rate (%), male") )
df_discm1 <- df_discm %>%
   select(`Countries and areas`,Indicator,Value)
df_discm1 <- transform(df_discm1, Value = as.integer(Value))
discm1 <- df_discm1 %>%
        pivot_wider(names_from = Indicator, values_from = Value )
ggdiscm <- ggplot(discm1, aes(y=`Youth (15–24 years) literacy rate (%), male`
,x=`Violent discipline (%), male`))  + geom_point() + geom_vline(xintercept = 60 , color = "red", linetype = "dashed", size=2) + geom_hline(yintercept = 75 , color = "red", linetype = "dashed", size=2)
grid.arrange(ggdiscf,ggdiscm,ncol= 2)
```
Conclusion - Both male and females in the age group 1-14 are subjected to physical punishment or psychological aggression equally. Most countries that adopt violent discipline also happen to be the the countries with good literacy rates.

```{r}
discf2 <- discf1 %>% 
  filter(discf1$`Youth (15–24 years) literacy rate (%), female` >= 95)
discf2 <- discf2 %>% 
  filter(discf2$`Violent discipline (%), female` > 70)
ggdiscf2 <- ggplot(discf2, aes(y=`Youth (15–24 years) literacy rate (%), female`
,x=`Violent discipline (%), female`,color = `Countries.and.areas`))  + geom_point()  +labs(title= "Violent Discipline vs Youth Literacy Rate (Female)") + labs(color="Countries")
ggdiscf2 
```

Conclusion - This is the scatter plot of "Youth (15–24 years) literacy rate (%)" Vs. "Violent Discipline (%). female". The violent discipline for female is more than 70% despite being the literacy rate more than 90% in these countries.

                                         Conclusion

While the average literacy for the countries around the world is 80, 40 per cent of the countries are still below this average. These countries are mainly from African continent. In 4 of these regions, less than 50 per cent of the children attend primary schools. Moreover, the number of children continuing lower secondary education is less than from primary. Other than in some of the African regions, urban and rural demographics do not seem to impact the enrollment in primary education. Also, we noticed that more females are of out school compared with males. The country with the highest school participation has the lowest child labor and vice versa. Both male and females in the age group 1-14 are subjected to physical punishment or psychological aggression equally. The country such as Benin, Chad, Guinea-Bissau, mali, and somalia have the child labor more than 50(%) for male as well as female.  Most countries that adopt violent discipline also happen to be the the countries with good literacy rates. The violent discipline for female is more than 70% despite being the literacy rate more than 90% in these countries.  

