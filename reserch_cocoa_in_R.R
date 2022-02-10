library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
# --------------------------File loc---------------------------------
setwd("D:/Github_version_file_R/data_set/cacao")

# ------------------------CSV File Import-----------------------------------
cacao_df <- read.csv("cacao.csv", encoding = "utf-8")
cacao_df <- clean_names(cacao_df)

# ------------------------CSV File View-----------------------------------
str(cacao_df)
head(cacao_df)
colnames(cacao_df)
ncol(cacao_df)
nrow(cacao_df)

# --------------------------Change DataType---------------------------------
cacao_df <- cacao_df %>%
    mutate(
        maker = as.factor(maker),
        origin = as.factor(origin),
        percent = as.factor(cocoa_percent),
        maker_location = as.factor(maker_location),
        bean_type = as.factor(bean_type),
        bean_origin = as.factor(bean_origin),
        year = review_date,
        rate = round(rating, digits = 2)
    )

# ------------------------Check Data Value-----------------------------------
summary(cacao_df)
summary(cacao_df$year)
summary(cacao_df$rate)
summary(cacao_df$percent)
summary(cacao_df$maker_location)
summary(cacao_df$bean_type)
sum(cacao_df == "NULL")

# ------------------------Check NA-----------------------------------
if (sum(is.na(cacao_df) > 0)) {
    print(sum(is.na(cacao_df)))
} else {
    print("no na")
}

# ------------------------Change NULLs-----------------------------------
levels(cacao_df$bean_type) # 33 > null
levels(cacao_df$maker)
levels(cacao_df$origin)
levels(cacao_df$maker_location)
levels(cacao_df$bean_origin) # 54 > null
levels(cacao_df$percent)

null_beanType <- which(levels(cacao_df$bean_type) == "NULL")
levels(cacao_df$bean_type)[null_beanType] <- "no_record"

null_beanOrigin <- which(levels(cacao_df$bean_origin) == "NULL")
levels(cacao_df$bean_origin)[null_beanOrigin] <- "no_record"

# ------------------------Analyze Problem & Viualization-----------------------------------
# 多少製造商評分大於四分 >> 45個
maker_rate <- cacao_df %>%
    group_by(maker) %>%
    summarise(rate = max(rate)) %>%
    filter(rate >= 4.00) %>%
    arrange(desc(rate)) %>%
    view()

ggplot(data = maker_rate) +
    geom_bar(mapping = aes(x = maker, y = rate, fill = rate), stat = "identity") +
    xlab("Maker") +
    ylab("Rating") +
    labs(title = "Maker & Avg Rating", subtitle = "the most popular maker") +
    theme(axis.text.x = element_text(angle = 30)) +
    ylim(0, 5.00)

# 那些廠商的評價較低
maker_rate <- cacao_df %>%
    group_by(maker) %>%
    summarise(rate = max(rate)) %>%
    filter(rate <= 2.00) %>%
    arrange(desc(rate)) %>%
    view()

# 高於4.00評分的可可含量有那些
percent_high_rate <- cacao_df %>%
    group_by(rate, percent) %>%
    summarise(rate = mean(rate)) %>%
    filter(rate >= 4.00) %>%
    arrange(desc(rate)) %>%
    view()

ggplot(data = percent_high_rate) +
    geom_bar(mapping = aes(x = percent, fill = rate)) +
    xlab("Cacao Percent") +
    ylab("Count") +
    labs(title = "High Rating Of The Cacao Percent", subtitle = "most favorite ratio content") +
    theme(axis.text.x = element_text(angle = 45))


# 低於2.00評分的可可含量有那些
percent_lower_rate <- cacao_df %>%
    groups_by(rate, percent) %>%
    summarise(rate = mean(rate)) %>%
    filter(rate <= 2.00) %>%
    arrange(desc(rate)) %>%
    view()

ggplot(data = percent_lower_rate) +
    geom_bar(mapping = aes(x = percent, fill = rate)) +
    xlab("Cacao Percent") +
    ylab("Count") +
    labs(title = "Lower Rating Of The Cacao Percent", subtitle = "least favorite ratio content") +
    theme(axis.text.x = element_text(angle = 45))


# 各年間對於可可產品的綜合評分
year_rate <- cacao_df %>%
    group_by(year) %>%
    summarise(top_rate = max(rate), bottom_rate = min(rate)) %>%
    arrange(desc(year)) %>%
    view()

ggplot(data = year_rate) +
    geom_line(mapping = aes(x = year, y = top_rate), color = "blue", size = 1.5) +
    geom_line(mapping = aes(x = year, y = bottom_rate), color = "red", size = 1.5) +
    xlab("Year") +
    ylab("The Heigh And Lower Rating") +
    labs(title = "The Relationship Between Years And Rating", subtitle = "ratings tend to be average") +
    xlim(2006, 2017)


# 甚麼樣形態的可可豆評分較好
bean_type_rate <- cacao_df %>%
    group_by(rate, bean_type) %>%
    summarise(rate = max(rate)) %>%
    filter(rate >= 4.00, bean_type != "no_record") %>%
    arrange(desc(rate)) %>%
    view()

ggplot(data = bean_type_rate) +
    geom_bar(mapping = aes(x = bean_type, y = rate, fill = rate), size = 0.5, stat = "identity") +
    ylim(0, 5.00) +
    xlab("Bean Type") +
    ylab("Rating") +
    labs(title = "Bean Type & Rating", subtitle = "the bean type effect rating") +
    theme(axis.text.x = element_text(angle = 30)) +
    coord_fixed(ratio = 1)


# 高評分的可可豆產地集中於何處
origin_rate <- cacao_df %>%
    group_by(bean_origin, rate) %>%
    summarise(rate = max(rate)) %>%
    filter(rate >= 4.00, bean_origin != "no_record") %>%
    arrange(desc(rate)) %>%
    view()

ggplot(data = origin_rate) +
    geom_bar(mapping = aes(x = bean_origin, y = rate, fill = rate), stat = "identity") +
    ylim(0, 5.00) +
    xlab("Bean Origin") +
    ylab("Rating") +
    labs(title = "Bean Origin & Rating", subtitle = "the bean origin effect rating") +
    theme(axis.text.x = element_text(angle = 30))