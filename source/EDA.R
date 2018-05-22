# IMPORT DATA
churn_data_raw <- read.csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
glimpse(churn_data_raw)
# Data quality
summary(churn_data_raw)

# PREPROCESS DATA

# Checking for missing value
sapply(churn_data_raw, function(x) sum(is.na(x)))
# 11 missing values in TotalCharge

# Remove unnecessary data
churn_data_tbl <- churn_data_raw %>%
  select(-customerID) %>% # Dropping customerID column since has no predictive info
  drop_na() %>% # This will remove 11 rows where TotalCharge is NA
  select(Churn, everything()) # Reordering the columns to keep churn as first

glimpse(churn_data_tbl)
summary(churn_data_tbl)

library(scales)
#show_col(hue_pal()(4))
hue_pal()(4)

p1 <- churn_data_tbl %>% ggplot(aes(x=Churn, fill = Churn)) +
  geom_bar(aes(y = (..count..))) +
  labs(y = "Count") +
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), 
            stat="count", colour="darkgreen") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D")) #+ theme(legend.position = "none")

p2 <- churn_data_tbl %>% group_by(gender,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=gender, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Gender") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~gender) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p3 <- churn_data_tbl %>% mutate(SeniorCitizen=ifelse(SeniorCitizen == 0, 'No', "Yes")) %>%
  group_by(SeniorCitizen,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=SeniorCitizen, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Senior Citizen") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~SeniorCitizen) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p4 <- churn_data_tbl %>% group_by(Partner,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=Partner, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Has Partner") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~Partner) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p5 <- churn_data_tbl %>% group_by(Dependents,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=Dependents, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Has Dependents") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~Dependents) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p6 <- churn_data_tbl %>% group_by(PhoneService,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=PhoneService, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Phone Service") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~PhoneService) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p7 <- churn_data_tbl %>% group_by(MultipleLines,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=MultipleLines, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Multiple Lines") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~MultipleLines) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p8 <- churn_data_tbl %>% group_by(InternetService,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=InternetService, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Internet Service") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~InternetService) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p9 <- churn_data_tbl %>% group_by(OnlineSecurity,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=OnlineSecurity, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Online Security") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~OnlineSecurity) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p10 <- churn_data_tbl %>% group_by(OnlineBackup,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=OnlineBackup, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Online Backup") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~OnlineBackup) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p11 <- churn_data_tbl %>% group_by(DeviceProtection,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=DeviceProtection, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Device Protection") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~DeviceProtection) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p12 <- churn_data_tbl %>% group_by(TechSupport,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=TechSupport, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Tech Support") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~TechSupport) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p13 <- churn_data_tbl %>% group_by(StreamingTV,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=StreamingTV, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Streaming TV") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~StreamingTV) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p14 <- churn_data_tbl %>% group_by(StreamingMovies,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=StreamingMovies, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Streaming Movies") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~StreamingMovies) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p15 <- churn_data_tbl %>% group_by(Contract,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=Contract, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Contract") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~Contract) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p16 <- churn_data_tbl %>% group_by(PaperlessBilling,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=PaperlessBilling, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Paperless Billing") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~PaperlessBilling) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

p17 <- churn_data_tbl %>% group_by(PaymentMethod,Churn) %>%
  summarise(n = n()) %>% 
  mutate(percentage=paste0(round(n/sum(n)*100,1),"%")) %>% 
  ggplot(aes(x= Churn, y=n, group=PaymentMethod, fill=Churn)) + 
  geom_bar(stat="identity") +
  labs(y = "Count", x = "Payment Method") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(~PaymentMethod) + theme(legend.position = "none") + 
  scale_fill_manual( values = c("#00BFC4", "#F8766D"))

# Monthly charge plot
p <- churn_data_tbl %>%
  ggplot(aes(MonthlyCharges)) +
  geom_histogram(binwidth = 4) 
MonthlyCharges_df <- layer_data(p,1)[,c('x','y')]
colnames(MonthlyCharges_df) <- c("MonthlyCharges", "Freq")
p_xmax <- layer_data(p,1)$xmax
churn_data_tbl_tmp <- churn_data_tbl
churn_data_tbl_tmp$bin <- sapply(churn_data_tbl$MonthlyCharges,function(x) length(p_xmax)-sum(x < p_xmax)+1)

mean_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>%
  summarise(Freq=n(),mean=mean(MonthlyCharges))
churn_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>% filter(Churn=='Yes') %>%
  summarise(n_churn=n())
merge_df <- merge(mean_df, churn_df, by = "bin", all.x = T) %>%
  mutate(percentage=round(n_churn/Freq*100,1))
merge_df$percentage[is.na(merge_df$percentage)] <- 0

p21 <- merge_df %>%
  ggplot()+
  scale_fill_gradient(low = "green", high = "red")+
  geom_bar(aes(x=mean, y = Freq, fill = mean), width=1.7, stat = "identity") +
  geom_line(aes(x=mean, y = percentage*10), linetype="dashed") +
  labs(y = "Count", x = "Monthly Charges [$]") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1000)) + 
  scale_y_continuous(
                     sec.axis = sec_axis(~./10, name = "Churn Rate [%]"))

# Total Charges
p <- churn_data_tbl %>%
  ggplot(aes(TotalCharges)) +
  geom_histogram(binwidth = 100) 

TotalCharges_df <- layer_data(p,1)[,c('x','y')]
colnames(TotalCharges_df) <- c("TotalCharges", "Freq")
p_xmax <- layer_data(p,1)$xmax
churn_data_tbl_tmp <- churn_data_tbl
churn_data_tbl_tmp$bin <- sapply(churn_data_tbl$TotalCharges,function(x) length(p_xmax)-sum(x < p_xmax)+1)

mean_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>%
  summarise(Freq=n(),mean=mean(TotalCharges))
churn_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>% filter(Churn=='Yes') %>%
  summarise(n_churn=n())
merge_df <- merge(mean_df, churn_df, by = "bin", all.x = T) %>%
  mutate(percentage=round(n_churn/Freq*100,1))
merge_df$percentage[is.na(merge_df$percentage)] <- 0

moving_avg=merge_df[10:(nrow(merge_df)),]
moving_avg$moving_percentage <- rollapply(merge_df$percentage, 10, mean)

p22 <- merge_df %>%
  ggplot()+
  scale_fill_gradient(low = "green", high = "red")+
  geom_bar(aes(x=mean, y = Freq, fill = mean), width=25, stat = "identity") +
  geom_line(aes(x=mean, y = percentage*10), linetype="dotdash") +
  geom_line(data=moving_avg, aes(x=mean, y = moving_percentage*10), linetype="solid", colour="red") +
  labs(y = "Count", x = "Total Charges [$]") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1000)) + 
  scale_y_continuous(
    sec.axis = sec_axis(~./10, name = "Churn Rate [%]"))

# Tenure
churn_data_tbl_tmp <- churn_data_tbl %>% mutate(tenure=ceiling(tenure/12))
p <- churn_data_tbl_tmp %>%
  ggplot(aes(tenure)) +
  geom_histogram(binwidth = 1) 

tenure_df <- layer_data(p,1)[,c('x','y')]
colnames(tenure_df) <- c("tenure", "Freq")
p_xmax <- layer_data(p,1)$xmax
churn_data_tbl_tmp$bin <- sapply(churn_data_tbl_tmp$tenure,function(x) length(p_xmax)-sum(x < p_xmax)+1)

mean_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>%
  summarise(Freq=n(),mean=mean(tenure))
churn_df <- churn_data_tbl_tmp %>%
  group_by(bin) %>% filter(Churn=='Yes') %>%
  summarise(n_churn=n())
merge_df <- merge(mean_df, churn_df, by = "bin", all.x = T) %>%
  mutate(percentage=round(n_churn/Freq*100,1))
merge_df$percentage[is.na(merge_df$percentage)] <- 0

p23 <- merge_df %>%
  ggplot()+
  scale_fill_gradient(low = "red", high = "green")+
  geom_bar(aes(x=mean, y = Freq, fill = mean*2), width=.6, stat = "identity") +
  geom_line(aes(x=mean, y = percentage*25), linetype="dashed") +
  labs(y = "Count", x = "Tenure [Years]") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 2500)) + 
  scale_y_continuous(sec.axis = sec_axis(~./25, name = "Churn Rate [%]"))


# Total Charges
churn_data_tbl_tmp <- churn_data_tbl
p <- churn_data_tbl_tmp %>%
  ggplot(aes(TotalCharges)) +
  geom_histogram(bins = 200) 

TotalCharges_df <- layer_data(p,1)[,c('x','y')]
colnames(TotalCharges_df) <- c("TotalCharges", "Freq")

p24 <- TotalCharges_df %>%
  ggplot()+
  scale_fill_gradient(low = "green", high = "red")+
  geom_bar(aes(x=TotalCharges, y = Freq, fill = TotalCharges), width=30, stat = "identity") +
  labs(y = "Count", x = "Total Charges [$]") +
  theme(legend.position = "none")
p24

# LOG Total Charges
churn_data_tbl_tmp <- churn_data_tbl
churn_data_tbl_tmp$TotalCharges <- log(churn_data_tbl_tmp$TotalCharges)
p <- churn_data_tbl_tmp %>%
  ggplot(aes(TotalCharges)) +
  geom_histogram(bins = 200) 

TotalCharges_df <- layer_data(p,1)[,c('x','y')]
colnames(TotalCharges_df) <- c("TotalCharges", "Freq")

p25 <- TotalCharges_df %>%
  ggplot()+
  scale_fill_gradient(low = "green", high = "red")+
  geom_bar(aes(x=TotalCharges, y = Freq, fill = TotalCharges), width=.02, stat = "identity") +
  labs(y = "Count", x = "LOG(Total Charges) [$]") +
  theme(legend.position = "none")
p25

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p8, p13, p15, p16, layout=layout)

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p21, p22, p17, p23, layout=layout)

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p24, p25, layout=layout)
