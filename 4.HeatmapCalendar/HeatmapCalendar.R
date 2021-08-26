

library(tidyverse)
library(lubridate)
library(ragg)
library(ggplot2)
library(dplyr)

library(readxl)
GL2018 <- read_excel("Desktop/Project/AllFolders/DateMosaic/GL2018.xlsx")


#changenameColumn
colnames(GL2018)[which(names(GL2018) == "date")] <- "Date1"

c<-GL2018 %>% group_by(month,Date, Date1) %>% 
  summarise(n = n(), .groups = 'drop')

c <- c %>% 
  complete(Date1 = seq(ymd("2018-01-01"), 
                       ymd("2018-12-31"), 
                       "day")) %>%
  mutate(weekday = wday(as.Date(Date1), label = T, week_start = 1), # can put week_start = 1 to start week on Monday
         month = month(Date1, label = T, abbr = T),
         week = isoweek(Date1),
         day = day(Date1))


#replace NA to 0 in column Date and n
c1 <- mutate_at(c, c("Date", "n"), ~replace(., is.na(.), 0))
View(c1)

#delete Row
c2 <- c1[-c(366), ]
View(c2)

c2 <- mutate(c2, 
             week = case_when(month == "Dec" & week == 1 ~ 53,
                              month == "Jan" & week %in% 52:53 ~ 0,
                              TRUE ~ week),
             pcat = cut(n, c(-1, 0, .5, 1:3,4:6, 7, 9, 15, 20, 25, 30,40,50,60,70,300)),
             text_col = ifelse(pcat %in% c("(1,3]", "(3,6]", "(6,20]", "(20,25]", "(25,40]", "(40,60]", "(60,80]", "(80,100]", "(100,150]","(150,300]"), 
                               "white", "black"))



# color ramp
red1 <- RColorBrewer::brewer.pal(9, "Reds")
col_p <- colorRampPalette(red1)


#Set theme
theme_calendar <- function(){
  
  theme(aspect.ratio = 1/5,
        
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(family = "sans"),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "sans", face = "bold", size = 15),
        
        legend.position = "top",
        legend.text = element_text(family = "sans", hjust = .5),
        legend.title = element_text(family = "sans", size = 9, hjust = 1),
        
        plot.caption =  element_text(family = "sans", hjust = 1, size = 8),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(family = "sans", hjust = .5, size = 26, 
                                  face = "bold", 
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(family = "sans", hjust = .5, size = 16)
  )
}


#PlotGraph

ggplot(c2, 
       aes(weekday, -week, fill = pcat)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = day, colour = text_col), size = 2.5) +
  guides(fill = guide_colorsteps(barwidth = 25, 
                                 barheight = .4,
                                 title.position = "top")) +
  scale_fill_manual(values = c("White", col_p(35)),
                    na.value = "grey90", drop = FALSE) +
  scale_colour_manual(values = c("black", "white"), guide = "none") + 
  facet_wrap(~ month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Number of transactions in general ledger 2018", 
       subtitle = "Count by day",
       caption = "Data: GL2018",
       fill = "QTY") +
  theme_calendar()
