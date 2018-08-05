#------------------------------------------------------------------------------#
#
### Generate combined plot of sentiment, topic, and status
#
#------------------------------------------------------------------------------#

### Load and--if necessary--install packages ----------------------------------#
package.list <- c("data.table", "dplyr", "openxlsx", "devtools", "ggplot2", "scales",
                  "plotly", "crosstalk", "ggrepel", "grid", "gridExtra")
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]){
    if (p == "ggplot2"){
      devtools::install_github("hadley/ggplot2") # This installation has the dev version of ggplotly()
    } else {
      install.packages(p)  
    }
  }
  library(p, character.only = TRUE)
}
setwd("~/GitHub/acdhs-demo/")

cn <- function(x) colnames(x)
grepv <- function(p, x, ...) grep(p, x, value = TRUE, ...)

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
  dt[!variable %in% c(statusVars, grepv("^cans", dt$variable))] %>% 
  .[, value := value + runif(nrow(.))*10] # Add a bit of noise for illustration
dt_topic_select <- 
  dt_topic %>% 
  filter(variable %in% c("Domestic Violence", "Substance Use", "Truancy")) %>% 
  data.table()
plot_topic <-
  ggplot(dt_topic_select, aes(x = Date, y = value, color = factor(variable))) + 
    geom_line(size = 1) +
    geom_label_repel(data = dt_topic_select[Date == max(Date)],
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
    theme_minimal()

plotly_status <- ggplotly(plot_status)
  # Convert before adding formatting that otherwise doesn't work with the other plotly elements

plot_status <-
  plot_status + 
  myTheme +
  myMargin
  # Complete the formatting of the static image

### Combine the static plots --------------------------------------------------#

# See this forum post for guidance on an example of adding a title via textGrob():
#  https://stackoverflow.com/questions/31640916/how-can-i-add-a-title-to-a-tablegrob-plot

grid.newpage()
png(filename = "img/viz_sentiment-cans-status.png", height = 1200, width = 800)
grid.draw(rbind(ggplotGrob(plot_sent),
                ggplotGrob(plot_topic),
                ggplotGrob(plot_status), size = "last"))
dev.off()

#------------------------------------------------------------------------------#
### Develop interactive plot analogs -------------------------------------------
#------------------------------------------------------------------------------#

### Generate interactive sentiment plot ---------------------------------------#

plotly_sent <-
  plot_ly(data = dt_sent, x = ~Date) %>% 
  add_lines(y = ~value,   line = list(color = "green")) %>% 
  add_lines(y = ~neg_val, line = list(color = "red") ) %>% 
  add_markers(y = ~value, marker = list(color = "black")) %>% 
  layout(title = "Multi-Domain View of Case #123456789",
         showlegend = FALSE,
         xaxis = list(title = "", showticklabels = FALSE), # only allowing the status plot to show x-axis
         yaxis = list(title = "Sentiment Score"))

### Generate interactive topic plot -------------------------------------------#
# See the code under section 4.2.5 of this link: https://plotly-book.cpsievert.me/linking-views-without-shiny.html
# Actually, this presentation--specifically slide 6--has the code which generates
# the plot: https://workshops.cpsievert.me/20171118/slides/day2/#6

sd <- SharedData$new(dt_topic, key = ~variable, "Select a Topic")
base <-
  plot_ly(sd, color = I("black"), height = 400) %>% 
  group_by(variable)
topic_avg <-
  base %>%
  group_by(variable) %>% 
  summarize(Score = round(mean(value), 1)) %>%
  arrange(Score) %>%
  add_bars(x = ~Score, y = ~factor(variable, levels = variable), hoverinfo = "x+y") %>%
  layout(
    barmode = "overlay",
    xaxis = list(title = ""), # "Average Topic Score" ... this is held off to not complicate the display
    yaxis = list(title = "")) 
topic_trend <-
  base %>%
  mutate(Score = round(value, 1)) %>% 
  add_lines(x = ~Date, y = ~Score, alpha = 0.3, text = ~variable, hoverinfo = "text") %>%
  layout(xaxis = list(title = "",
                      showticklabels = FALSE)) # Only allowing the status plot to have the x-axis

topic_viz <- 
  subplot(topic_avg, topic_trend, 
        titleX = TRUE, widths = c(0.3, 0.7)) %>% 
  layout(margin = list(l = 120)) %>%
  hide_legend() %>%
  highlight(persistent = TRUE, selectize = TRUE, dynamic = TRUE)
save(topic_viz, file = "img/interactive_topic_viz.Rda")

### Generate interactive status involvement plot ------------------------------#

# Note: this is generated using ggplotly above

# Don't run the below code--it still requires more development to be a direct
# implementation of plotly for the status graph
if (FALSE){
  plotly_status <- 
    plot_ly(filter(dt_status, value == 1) %>% 
              mutate(Treat_time = ifelse(color == "Treat", time, 0),
                     NonTreat_time = ifelse(color == "Non-Treat", time, 0)),
            x = ~Date, y = ~fStatus, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)')) %>%
    add_trace(x = ~Treat_time,    marker = list(color = 'rgba(55, 128, 191, 0.7)',
                                                line = list(color = 'rgba(55, 128, 191, 0.7)',
                                                            width = 2))) %>%
    add_trace(x = ~NonTreat_time, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                                line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                            width = 2))) %>% 
    layout(title = 'Status Involvements',
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           barmode = 'stack',
           # paper_bgcolor = 'rgba(245, 246, 249, 1)',
           # plot_bgcolor = 'rgba(245, 246, 249, 1)',
           showlegend = FALSE)  
}


### Assemble the interactive elements -----------------------------------------#

# See this link for arranging subplots: https://plotly-book.cpsievert.me/merging-plotly-objects.html
# Note the ability to use "plotly_empty()" to fill a space in the grid with a blank

# Attempt inclusion of dynamic topic selection
subplot(plotly_empty(), plotly_sent,
        topic_avg,      topic_trend, 
        plotly_empty(), plotly_status,
        nrows = 3, widths = c(0.3, 0.7),
        titleX = TRUE) %>%  # , shareX = TRUE
  layout(margin = list(l = 120)) %>%
  hide_legend() %>%
  highlight(persistent = TRUE, selectize = TRUE, off = "plotly_deselect")

subplot(plotly_sent,
        topic_trend, 
        plotly_status,
        nrows = 3,
        titleX = TRUE) %>% #, shareX = TRUE
  layout(margin = list(l = 120)) %>%
  hide_legend()

# /!\ Additional to dos for formatting:
# - need to get aspect ratio right
# - brushing for interactive topic visualization isn't being preserved
# - need to rotate the dates in the status figure