```{r }
library(reshape2)
library(ggplot2)
library(dplyr)
library(XML)
f_data2 <- f_data %>%
    select(Department, Attrition, Gender) %>%
    group_by(Attrition, Gender, Department) %>%
    summarise(Count = n()) %>%
    group_by(Gender, Department) %>%
    mutate(Countp = round(prop.table(Count)*100,2)) %>%
    mutate(Countp = ifelse(Gender == "Male", Countp *-1, Countp)) %>%
    subset(Attrition == "Yes")



ggplot(f_data2, aes(x = Department, y = Countp, fill = Gender)) +
    geom_bar(data = subset(f_data2, Gender == "Female"), stat = "identity") +
    geom_bar(data = subset(f_data2, Gender == "Male"), stat = "identity") +
    coord_flip() +
    scale_y_continuous(
        breaks = seq(-100, 100, 10),
        labels = paste0(as.character(c(seq(100, 0, -10), seq(10, 100, 10))), "%")) 

```


Winsorizing
```{r}
#Created by Dr. Tate & Dr. Ghafur of San Francisco State University 
winsorizer = function(stuff) {
    interim <- as.vector(stuff)
    
    boxs <- boxplot(interim)
    miny <- boxs$stats[1]
    maxy <- boxs$stats[5]
    
    interim[interim > maxy] <- maxy
    interim[interim < miny] <- miny
    
    winsorized.data <<- interim
}

f_data1 <- f_data
f_data1$MonthlyIncome <- winsorizer(f_data$MonthlyIncome)
boxplot(f_data1$MonthlyIncome)
boxplot(f_data$MonthlyIncome)

```

```{r}
ggthemr('dust')
f_data %>%
    ggplot(aes(MonthlyIncome)) + 
    geom_histogram(aes(fill = Attrition), bins = 20) +
    labs( x = "Monthly Income", y = "count", title = "Attribution by Income", subtitle = "Can making less money lead to attrition?") 
```

