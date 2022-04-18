getwd()
library(tidyverse)
library(lubridate)
library(kableExtra)
library(dplyr)

new_theme <- theme_bw(base_size = 13) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(colour = "black"))
theme_set(new_theme)

MSA.Data <- read.csv('/Users/ricpro/Documents/Duke MPP Coursework/Spring 2022/Policy Analysis II 804/Client Report Data/2019appendix_MSA Data.csv', stringsAsFactors = TRUE) %>%
  rename("2015" = 'X2015.Percent', '2017' = 'X2017.Percent', '2019 Unbanked Rate' = 'X2019.Percent') %>% select(c("MSA","2019 Unbanked Rate")) %>%
  top_n(10) %>% arrange(desc('2019 Unbaked Rate'))

MSA.Data$MSA <- as.factor(MSA.Data$MSA)

MSA.Data %>% arrange('2019')

table_2 <- kbl(MSA.Data) %>%
  kable_classic_2(bootstrap_options = c("striped", "hover", "condensed"), html_font = "Times New Roman")
print(table_2)

Savings.Method.By.Banking.Status <- read.csv('/Users/ricpro/Documents/Duke MPP Coursework/Spring 2022/Policy Analysis II 804/Client Report Data/Savings Method By Banking Status.csv', stringsAsFactors = FALSE) %>% 
  filter(Banking.Status == 'Unbanked')

bar_chart_1 <- ggplot(Savings.Method.By.Banking.Status, aes(x = Savings.Method, y = Percent))+
  geom_bar(stat = 'identity', width = 0.7, position = "dodge", fill = "#1565C0") +
  geom_text(aes(label=Percent),hjust=-0.5)+
  scale_fill_hue(c = 40) +
  labs(title = "Savings Methods for Unbanked Households, 2017")+
  #scale_x_discrete("Banking Status")+
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 13), axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right")+
  coord_flip()
plot(bar_chart_1)
