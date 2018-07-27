#------------------------------------------------------------------------------#
#
### Generate combined plot of sentiment, topic, and status
#
#------------------------------------------------------------------------------#

### Load and--if necessary--install packages ----------------------------------#
package.list <- c("data.table", "dplyr", "openxlsx", "ggplot2", "scales", "ggrepel", "grid", "gridExtra")
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only = TRUE)
}

#------------------------------------------------------------------------------#
### Build case-level data ------------------------------------------------------
#------------------------------------------------------------------------------#

### Read mock data
dt <- 
  read.xlsx("data/mock-data.xlsx", namedRegion = "mock_data") %>% 
  melt(id.vars = c("ID", "Date")) %>% 
  mutate(Date = as.Date("1900-01-01") + Date,
         variable = gsub("\\.", " ", variable)) %>%
  data.table()

### Develop status data
statusSize <- 0.4
statusVars <- c("Preventive Services", "CPS Investigation", "Employed", "Housing Placement", "Homeless Shelter", "In School")
dt_status <- 
  filter(dt, variable %in% statusVars) %>%
  group_by(ID) %>%
  mutate(Date2 = lead(Date),
         time = Date2 - Date,
         time = ifelse(time < 0, NA, time),
         fStatus = factor(variable, levels = statusVars),
         nStatus = as.numeric(fStatus),
         low  = nStatus - statusSize/2,
         high = nStatus + statusSize/2,
         color = ifelse(variable %in% c("Preventive Services", "CPS Investigation"), "Treat", "Non-Treat")) %>%
  filter(!is.na(time), value == 1) %>% 
  arrange(nStatus)
  
  
#------------------------------------------------------------------------------#
### Build case-level visualization ---------------------------------------------
#------------------------------------------------------------------------------#
  
myTheme <- theme(legend.position = "none",
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 title = element_text(size = 26),
                 axis.title = element_text(size = 20),
                 axis.text = element_text(size = 18))
myXAxis <- scale_x_date(limits = with(dt, c(min(Date), max(Date))),
                        breaks = unique(dt$Date),
                        labels = unique(dt$Date))
marg_lr = 2
myMargin <- theme(plot.margin = grid::unit(c(1,2,0,1), unit = "cm"))

### Sentiment plot ------------------------------------------------------------#

dt_sent <-
  dt[variable == "Sentiment"] %>% 
  .[, neg_val := ifelse(value < 0, value, NA)]
plot_sent <- 
  ggplot(dt_sent, aes(x = Date)) +
    geom_line(aes(y = value),   color = "green", size = 1.5) + 
    geom_line(aes(y = neg_val), color = "red", size = 2.0) + 
    geom_point(aes(y = value), color = "black", size = 3.0) +
    geom_hline(aes(yintercept = 0)) +
    scale_color_discrete(c("green", "red")) +
    #scale_y_continuous(position = "right") +  # sec.axis = dup_axis(name = ""), 
    myXAxis + 
    labs(title = "Multi-Domain View of Case #123456789", x = "", y = "Sentiment Score") +
    theme_minimal() +
    myTheme +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = -11)) +
    myMargin

### Topic scores --------------------------------------------------------------#

dt_topic <- 
  dt[variable %in% c("Truancy", "Substance Use", "Domestic Violence")] %>% 
  .[, value := value + runif(nrow(.))*10] # Add a bit of noise for illustration
plot_topic <-
  ggplot(dt_topic, aes(x = Date, y = value, color = factor(variable))) + 
    geom_line(size = 1) +
    geom_label_repel(data = dt_topic[Date == max(Date)],
              aes(label = variable), size = 6, hjust = 1, vjust = -5) +
    myXAxis + 
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + # sec.axis = dup_axis(name = ""), position = "right"
    labs(x = "", y = "Topic Score") +
    theme_minimal() + 
    myTheme +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), vjust = 1)) +
    myMargin
 
### Status plot ---------------------------------------------------------------#

plot_status <- 
  ggplot(filter(dt_status, value == 1),
         aes(xmin = Date, xmax = Date2,
             ymin = low,  ymax = high, fill = factor(value))) +
    geom_rect(aes(fill = color)) +
    scale_fill_manual(values = c("Treat" = "blue", "Non-Treat" = "plum")) + 
    scale_y_continuous(breaks = unique(dt_status$nStatus), labels = levels(dt_status$fStatus)) + # position = "right"
    myXAxis +
    theme_minimal() + 
    myTheme +
    myMargin

### Combine plots -------------------------------------------------------------#

# See this forum post for guidance on an example of adding a title via textGrob():
#  https://stackoverflow.com/questions/31640916/how-can-i-add-a-title-to-a-tablegrob-plot

grid.newpage()
png(filename = "img/viz_sentiment-cans-status.png", height = 1200, width = 800)
grid.draw(rbind(ggplotGrob(plot_sent),
                ggplotGrob(plot_topic),
                ggplotGrob(plot_status), size = "last"))
dev.off()
