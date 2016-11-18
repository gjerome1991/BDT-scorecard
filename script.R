######################################################
## SCRIPT TO GENERATE TABLES FOR BDT SCORECARD DEMO ##
######################################################

#### 00. libraries and others ####

rm(list=ls())

library(maps)
library(ggplot2)
library(xlsx)

actual <- read.xlsx('data.xlsx',sheetIndex=1)
actual_target <- read.xlsx('data.xlsx',sheetIndex=2)
SNAP_targets<- read.xlsx('data.xlsx',sheetIndex=3)
MEDICARE_targets <- read.xlsx('data.xlsx',sheetIndex=4)
RENT_targets <- read.xlsx('data.xlsx',sheetIndex=5)

all_states <- map_data("state")
#names(all_states)[5] <- 'state'

BDT_states <- c(31,7,8,6,37,29,45,32,47,28,18,19,20,38,44)
BDT_states <- unique(all_states$region)[sort(BDT_states)]
n_states <- length(BDT_states)

#### 02. filter the data and ggplot ####
all_states <- all_states[all_states$region %in% BDT_states,]

#filter the data
program = 'SNAP'
target='out_mail'
time_frame= 'YTD'

ids2filter <- actual_target$program==program & actual_target$target==target
db <- data.frame(value=actual_target[ids2filter,time_frame],region  = actual_target[ids2filter,'state'])

ggplot() + 
     geom_map(data=all_states, map=all_states,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15) + 
    geom_map(data=db, map=all_states,
                    aes(fill=value, map_id=region),
                    color="#ffffff", 
                    size=0.15) +
  scale_fill_gradient2(low="red", mid="yellow",high="#2ca25f", limits=c(0.8,1.3),midpoint = 0.99) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  annotate("text",y=35.326, x=-70.33,label = "62%",size=28)


ids2filter <- actual_target$program==input$program & actual_target$target==input$target
if(input$state=='all') {
  ids2filterState <- ids2filter
  
} else {
  ids2filterState <- ids2filter&actual_target$state==input$state
}

df_2 <- as.data.frame(t(actual[ids2filter,c(4:13)]))
colnames(df_2) <- actual[ids2filter,'state']
df_2$month <- rownames(df_2)


#transform ds as dynamic table 
ggplot(db_barplot,aes(x = month, y =value,fill=state)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=150000)


#### 01. generates fake data ####
set.seed(0)

SNAP_target <- 800000+sample(1:20,length(BDT_states))*100000
MEDICARE_target <- 100000+sample(1:20,length(BDT_states))*100000
MEDICAID_target <- 200000+sample(1:20,length(BDT_states))*100000
RENT_target <- 3000 +sample(1:20,length(BDT_states))*1000
ENERGY_target <- 30000 +sample(1:20,length(BDT_states))*10000


#outgoing mail
BDT_df <- data.frame(state=BDT_states, 
                     SNAP_target=SNAP_target, 
                     SNAP_actual=round(SNAP_target*runif(n_states,0.6,1.2),0),
                     MEDICARE_target= MEDICARE_target,
                     MEDICARE_actual= round(MEDICARE_target*runif(n_states,0.6,1.2),0),
                     MEDICAID_target=MEDICAID_target,
                     MEDICAID_actual = round(MEDICAID_target*runif(n_states,0.6,1.2),0),
                     RENT_target=RENT_target,
                     RENT_actual= round(RENT_target*runif(n_states,0.6,1.2),0),
                     ENERGY_target= ENERGY_target,
                     ENERGY_actual=round(ENERGY_target*runif(n_states,0.6,1.2),0)
                    )

BDT_df$SNAP <- BDT_df$SNAP_actual/SNAP_target
BDT_df$MEDICARE <- BDT_df$MEDICARE_actual/MEDICARE_target
BDT_df$MEDICAID <- BDT_df$MEDICAID_actual/MEDICAID_target
BDT_df$RENT <- BDT_df$RENT_actual/RENT_target
BDT_df$ENERGY <- BDT_df$ENERGY_actual/ENERGY_target
BDT_df$target <- 'out_mail'

View(BDT_df)

#applications
BDT_df_ap <- data.frame(state=BDT_states, 
                     SNAP_target=SNAP_target/100, 
                     SNAP_actual=round(SNAP_target/100*runif(1,0.6,1.2),0),
                     MEDICARE_target= MEDICARE_target/100,
                     MEDICARE_actual= round(MEDICARE_target/100*runif(1,0.6,1.2),0),
                     MEDICAID_target=MEDICAID_target/100,
                     MEDICAID_actual = round(MEDICAID_target/100*runif(1,0.6,1.2),0),
                     RENT_target=RENT_target/100,
                     RENT_actual= round(RENT_target/100*runif(1,0.6,1.2),0),
                     ENERGY_target= ENERGY_target/100,
                     ENERGY_actual=round(ENERGY_target/100*runif(1,0.6,1.2),0)
)

BDT_df_ap$SNAP <- BDT_df_ap$SNAP_actual/SNAP_target
BDT_df_ap$MEDICARE <- BDT_df_ap$MEDICARE_actual/MEDICARE_target
BDT_df_ap$MEDICAID <- BDT_df_ap$MEDICAID_actual/MEDICAID_target
BDT_df_ap$RENT <- BDT_df_ap$RENT_actual/RENT_target
BDT_df_ap$ENERGY <- BDT_df_ap$ENERGY_actual/ENERGY_target
BDT_df_ap$target <- 'applications'

BDT_df <- rbind(BDT_df,BDT_df_ap)


#### 02. filter the data and ggplot ####
all_states <- all_states[all_states$region %in% BDT_states,]
Total <- merge(all_states, Prison, by="region")
p <- ggplot() + 
     geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=Total$bwRatio),colour="white") + 
     scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
     theme_bw()  + 
      labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="") +
     scale_y_continuous(breaks=c()) + 
     scale_x_continuous(breaks=c()) + 
     theme(panel.border =  element_blank())
