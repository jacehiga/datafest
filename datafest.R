---
title: "Datafest"
author: "Jace Higa"
date: "2023-04-22"
output:
  pdf_document: default
  html_document: default
---

## Loading in Data
```{r}
attorney <- read.csv("questions.csv")
set.seed(1)
row <- sample(nrow(attorney), 202879)
attorney <- attorney[row,] 

```


## Installing Packages
```{r}
#install.packages("tidyverse")
library(tidyverse)
```

## Different Problems Needed For Attorneys
```{r}
pop_attorney <- attorney %>%
  count(Subcategory) %>%
  arrange(desc(n))
```

## Ordered 10 Most Common Subcategories
```{r}
pop_sub<-pop_attorney%>%
  mutate(Subcategory2=fct_reorder(Subcategory, n))%>%
  slice_max(n, n=10)
```

## Graphic For 10 Most Popular Subcategories
```{r}
ggplot(pop_sub, aes(Subcategory2, n)) +
  geom_col() +
  coord_flip() +
  xlab("Subcategories") +
  ylab("Number of Cases")
```


## Seperating the words
```{r}
#install.packages("tidytext")
library(tidytext)

## Tokenizing
token_att<- attorney%>%
  unnest_tokens(word, Subcategory)

count_words<-token_att%>%
  count(word)%>%
  arrange(desc(n))
```

```{r}
elim_attorney<-attorney%>%
  unnest_tokens(word, Subcategory)%>%
  anti_join(stop_words)

elim_count<-elim_attorney%>%
  count(word)%>%
  arrange(desc(n))
```


## Graphic For 10 Most Popular Words
```{r}
pop_word <- elim_count%>%
  mutate(word2=fct_reorder(word, n))%>%
  slice_max(n, n=10)

ggplot(pop_word, aes(word2, n)) +
  geom_col() +
  coord_flip() +
  xlab("Word") +
  ylab("Number of Cases")
```


## Word Cloud
```{r}
#install.packages("wordcloud")
library(wordcloud)

word_counts <- elim_attorney %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 23
)
```

## Finding Most Attorneys By State
```{r}
attorney_state <- attorney %>%
  count(StateName) %>%
  arrange(desc(n))
attorney_state
```
## Top 10 Pro Bono Attorneys By States
```{r}
top_state <- attorney_state %>%
  mutate(StateName2 = fct_reorder(StateName, n)) %>%
  slice_max(n, n = 10)

ggplot(top_state, aes(StateName2, n)) +
  geom_col() +
  coord_flip() +
  xlab("State") +
  ylab("Number of Attorneys")
```

## Grouping into Regions
```{r}

West <- c("Washington", "Montana", "Idaho", "Oregon", "Wyoming", "California", "Nevada", "Utah", "Colorado", "New Mexico", "Arizona")

Pacific <- c("Alaska", "Hawaii")

Midwest <- c("North Dakota", "Minnesota", "South Dakota", "Nebraska", "Kansas", "Iowa", "Missouri", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio") 

Northeast <- c("New York", "Pennsylvania", "New Jersey", "Connecticut", "Rhode Island", "Massachusetts", "New Hampshire", "Vermont", "Maine")

South <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Mississippi", "Alabama", "Tennessee", "Kentucky", "West Virginia", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida")



State<-attorney$StateName

Region <- data.frame(StateName = State) %>%
  mutate(Region=NA)

for(i in 1:dim(attorney)[1]){
  
    if(Region$StateName[i] %in% West){
    Region$region[i]="West"
    }
    if(Region$StateName[i] %in% Pacific){
    Region$region[i]="Pacific"
    }
    if(Region$StateName[i] %in% Midwest){
    Region$region[i]="Midwest"
    }
    if(Region$StateName[i] %in% Northeast){
    Region$region[i]="Northeast"
    }
    if(Region$StateName[i] %in% South){
    Region$region[i]="South"
    }
}

```
## Counting Attorneys by Region
```{r}
region_attorney <- Region %>%
  count(region) %>%
  arrange(desc(n))

region_attorney
```


## Top Regions
```{r}
top_region <- region_attorney %>%
  mutate(region2 = fct_reorder(region, n)) %>%
  slice_max(n, n = 5)

ggplot(top_region, aes(x = "", y = n, fill = region)) +
  geom_bar(width = 1, stat = "identity", position = "fill") +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  labs(title = "Attorneys by Region") +
  scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c( "limegreen", "red", "yellow", "cornflowerblue", "orange")) +
  theme(plot.title = element_text(hjust = 0.5))


```

```{r}
ggplot(top_region, aes(x = region2, y = n, fill = region)) +
  geom_col() +
  labs(title = "Attorneys by Region") +
  xlab("Region") +
  ylab("Number of Attorneys") +
  scale_fill_manual(values = c("limegreen", "red", "yellow", "cornflowerblue", "orange")) +
  theme(plot.title = element_text(hjust = 0.5))
```
