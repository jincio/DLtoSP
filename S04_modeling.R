####REPLICATION FILE FOR From Drug Lords to Police State: The Effects of Order Transition on Local Economies####
library(lfe)
library(tidyr)
library(ggplot2)
library(stargazer)
library(readxl)
library(tibble)
library(jtools)
library(ggeffects)
library(gridExtra)
library(coefplot)

##########################Data Manipulation Main Analysis#################################

##Read In Data
load("SetsAnalysis07232021.RData")
data<- data_analysis07232021

#Extract Only Rade (Day/Night Band) from Data
data_rd<- data %>%
  dplyr::filter(type == "rade9h.tif")

#Create Wave variable to measure Wave of Observation
data_rd<- data_rd %>% dplyr::group_by(Codfavela) %>% dplyr::mutate(wave = seq_len(dplyr::n())) %>% dplyr::ungroup()

#Create Count2Treat variable, measure of time until treatment centered at 0
data_rd$occupation_wave <- ifelse(data_rd$year == data_rd$year.occupation & data_rd$month== data_rd$month.occupation, data_rd$wave, 0)
data_rd$occupation_wave <- ifelse(is.na(data_rd$occupation_wave), 82, data_rd$occupation_wave)
data_rd<- data_rd %>% dplyr::group_by(Codfavela) %>% dplyr::mutate(occupation_wave = max(occupation_wave))
data_rd$count2treat <- data_rd$wave - data_rd$occupation_wave

#Create Log Measure of Luminosity
data_rd$valueMean_log <- log(1+data_rd$valueMean)
data_rd$valueSum_log <- log(1+data_rd$valueSUM)

#Create Interaction variable for ITS
data_rd$posttrend<-data_rd$count2treat*data_rd$treatment_final

#Get all favelas with Treatment for reference
datatreattotal <-  data_rd %>% dplyr::filter(Treatment == 1) %>% dplyr::filter(count2treat==1)
## Here it could be one step with dplyr::select
datatreattotal<-subset(datatreattotal, select = c(Name, data_ocupacao))

# Number of untreated favelas in study
datanotreattotal <-  data_rd %>% dplyr::filter(Treatment == 0) %>% dplyr::filter(count2treat==1)

datanotreattotal<-subset(datanotreattotal, select = c(Name, data_ocupacao))


#Drop observations treated before availability of data
data_rd<- data_rd%>% dplyr::filter(year.occupation >= 2012 |is.na(year.occupation))

#Keep Observations only until 2018
data_rd<- data_rd %>% dplyr::filter(year <= 2018)

#Create Data if Occupation Occured 6 months After available measures (or 2? Months)
data_3pr<- data_rd%>% dplyr::filter(occupation_wave >= 2)
#Create Data with only first 6 months of Observation Averaged by Treated and Non-Treated Units
data_3<- data_3pr%>% dplyr::filter(wave <= 3)
data_3<- data_3 %>% dplyr::group_by(Treatment,wave) %>% dplyr::summarise(valueSum_log=mean(valueSum_log),
                                                                         valueMean_log=mean(valueMean_log),
                                                                         valueMean=mean(valueMean)) %>% dplyr::ungroup()

#####################Main Analysis########################

############Table of Treatment Group################

datatreat <-  data_rd %>% dplyr::filter(Treatment == 1) %>% dplyr::filter(count2treat==0)

datatreat <-subset(datatreat, select = c(Name, data_ocupacao))

stargazer(datatreat,type = "text", summary = F,out="./Tables/TableA1.txt")


############2-way Fixed Effects/Diff-in-Diff################

#Plot Parallel Trends (Treated  with 3 months of Pre-treatment Observations and non-Treated Units)
pdf("./figures/Figure1.pdf", width = 10, height =5)
ggplot(data_3, aes(x = factor(wave), y = valueMean_log, group = Treatment, color = factor(Treatment))) + 
  geom_line()+
  labs(x ="Wave", y = "Log Mean Luminosity", color = "Treatment Status") + 
  scale_color_manual(labels = c("Control", "Treatment"), values = c("gray55", "gray0")) + 
  theme(panel.background = element_rect(fill = "gray90"),
        panel.grid.major = element_line(colour = "gray99", size = 0.2),
        panel.grid.minor = element_line(colour = "gray100",size = 0.2))+
  ggtitle("Figure 1: Logged Luminosity Time Trends by Treatment Groups")
dev.off()

#Run Models With Mean Luminosity (Main Model)
m1 <- felm(valueMean_log ~ treatment_final | wave + Codfavela, exactDOF = TRUE, data= data_rd)
summary(m1)
stargazer(m1)

############Interupted Time Series (ITS) Model ################

m2 <- lm(valueMean_log ~  treatment_final+ count2treat+ posttrend + as.factor(Codfavela) + as.factor(wave), data= data_rd)
options(max.print=100000)
summary(m2)
stargazer(m2,keep =  c("treatment_final", "count2treat","posttrend","Constant"))

stargazer(m1,m2,keep =  c("treatment_final", "count2treat","posttrend","Constant"),
          title="Table 1: Luminosity Analysis",
          covariate.labels = c("Treatment","Pre-Treatment Trend","Post Treatment Trend"),
          column.labels = c("DD","ITS"),
          dep.var.caption = "Model Specification",
          dep.var.labels.include = FALSE,
          type="text",out="./Tables/Table1.txt")


p <-ggpredict(m2, c("count2treat[-81:0]"))
p2 <-ggpredict(m2, c("posttrend[0:78]"))

p2$predicted <-p2$predicted + 3.761e-02
p2$conf.low <- p2$conf.low + 3.761e-02
p2$conf.high <- p2$conf.high + 3.761e-02

### should I keep this?
0.03761 + (-0.002404*12) 
0.03761 + (-0.002404*18)
0.03761 + (-0.002404*24)

pdf("./figures/Figure2.pdf",  width = 10, height =5)
ggplot() + 
  geom_line(data= p, aes(x=x, y=predicted)) + 
  geom_ribbon(data= p, aes(ymin=conf.low, ymax=conf.high, x=x, fill = "band"), alpha = 0.3, show.legend = F)+
  geom_line(data= p2, aes(x=x, y=predicted)) + 
  geom_ribbon(data= p2, aes(ymin=conf.low, ymax=conf.high, x=x), alpha = 0.3, show.legend = F) +
  labs(x ="Time to Treatment (0 Centered)", y = "Log Mean Luminosity") + 
  scale_fill_manual("",values="grey12") +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "grey50", size=1.5, show.legend = F)+
  ggtitle("Figure 2: Pre and Post-Treatment Marginal Effects")
dev.off()

##########################Data Manipulation Mechanism Analysis#################################
##Read In Data
datab <- read.csv("./Dependencies/UppEvolucaoMensalDeTitulos.csv", sep=";", comment.char="#")

##Put dates in date format
datab$date<- as.Date(paste(as.numeric(datab$mes), "01", datab$ano, sep="-"), 
                     format = "%m-%d-%Y")

##Create Wave Variable
datab<- datab %>% dplyr::group_by(cod_upp) %>% dplyr::mutate(wave = seq_len(dplyr::n()))

##Load in Occupation Date Variable
ddate <- read_excel("./Dependencies/UppDatasDeOcupacaoeInstalacao.xlsx")

ddate<- ddate %>%
  dplyr::mutate(year = lubridate::year(data_ocupacao), 
                month = lubridate::month(data_ocupacao), 
                day = lubridate::day(data_ocupacao))

##Put Occupation Date Variable in Date Format
ddate$occupation_date <- as.Date(paste(as.numeric(ddate$month), "01", ddate$year, sep="-"), 
                                 format = "%m-%d-%Y")

#Merge the two data sets
ddate <- ddate %>% dplyr::select(upp, cod_upp, occupation_date)
datab<- merge(datab , ddate , by = c("cod_upp"), sort = TRUE, all=T)

#Create Treatment Variable, 1 for all months after treatment
datab$treatment <- ifelse(datab$date >= datab$occupation_date,1,0)

#Create Treatment Onset Variable, 1 for the first month of treatment
datab$treatment_onset <- ifelse(datab$date == datab$occupation_date,1,0)

#Create Occupation Wave Variable (What wave occupation took place in)
datab$occupation_wave <- ifelse(datab$date == datab$occupation_date, datab$wave, 0)
datab<- datab %>% dplyr::group_by(cod_upp) %>% dplyr::mutate(occupation_wave = max(occupation_wave))

#Create Count2Treat variable, measure of time until treatment centered at 0
datab$count2treat <- datab$wave - datab$occupation_wave

#####################Mechanism Analysis########################

#####################ITC + 2-Way Fixed Effect Number of Prison Mandates Fufilled  ########################
m6 <- glm(cump_mandado_prisao~  treatment + as.factor(cod_upp) + as.factor(wave), data= datab, family = "poisson")
summary(m6)
m6a <- glm(cump_mandado_prisao~  treatment + count2treat+ count2treat:treatment+as.factor(cod_upp) + as.factor(wave), data= datab, family = "poisson")
summary(m6a)
stargazer(m6a,m6,keep =  c("treatment", "count2treat","posttrend","Constant"),type="text",out="./Tables/m6.txt")
exp(0.970)
1- exp(-0.019)

#####################ITC + 2-Way Fixed Effect Number of Cases of Drug Apprehension########################
m7 <- glm(apreensao_drogas~  treatment + as.factor(cod_upp) + as.factor(wave), data= datab, family = "poisson")
summary(m7)
m7a <- glm(apreensao_drogas~  treatment + count2treat+ count2treat:treatment+as.factor(cod_upp) + as.factor(wave), data= datab, family = "poisson")
summary(m7a)
stargazer(m7,m7a,type="text",out="./Tables/m7.txt")
exp(1.094)
1- exp(-0.009)


########Combine all ITC into 1 graph

stargazer(m6a, m7a,
          keep =  c("treatment", "count2treat","posttrend","Constant"),
          title="Table 2: Mechanism Analysis Interrupted Time Series Model",
          covariate.labels = c("Treatment","Pre-Treatment Trend","Post Treatment Trend","Constant"),
          column.labels = c("Arrest Warrant Fulfilled","Drug Apprehension"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels.include = FALSE,
          type="text",out="./Tables/Table2.txt")

#####################Appendix########################

#####################Unloged Parralel Trend########################

pdf("./figures/Figure4.pdf", width = 10, height =5)
ggplot(data_3, aes(x = factor(wave), y = valueMean, group = Treatment, color = factor(Treatment))) + 
  geom_line()+
  labs(x ="Wave", y = "Mean Luminosity", title = "", color = "Treatment Status") + 
  scale_color_manual(labels = c("Control", "Treatment"), values = c("gray55", "gray0")) + 
  theme(panel.background = element_rect(fill = "gray90"),
        panel.grid.major = element_line(colour = "gray99", size = 0.2),
        panel.grid.minor = element_line(colour = "gray100",size = 0.2))+
  ggtitle("Figure 4: Luminosity Time Trends by Treatment Groups")
dev.off()

#####################Analysis following Callaway Sant'Anna (2019) 2-setp DD Estimation########################
#devtools::install_github("bcallaway11/did")
library(did)

data_rd$first.treat <- ifelse(data_rd$occupation_wave ==82,0,data_rd$occupation_wave)

data_rd<- as.data.frame(data_rd)

data_rd<-data_rd %>% 
  dplyr::mutate(Codfavela=as.numeric(Codfavela))

out <-att_gt(yname = "valueMean_log",
             gname = "first.treat",
             idname = "Codfavela",
             tname = "wave",
             xformla = ~1,
             data = data_rd,
             est_method = "reg")

#Table 4A: Grab the overall ATT for each of these aggregation strategies

agg.simple <- aggte(out, type = "simple")
summary(agg.simple)
group_effects <- aggte(out, type = "group")
summary(group_effects)
agg.gs <- aggte(out, type = "dynamic")
summary(agg.gs)
agg.ct <- aggte(out, type = "calendar")
summary(agg.ct)

#Figure 5
pdf("./figures/FigureX.pdf", width = 15, height =3)
ggdid(out,ncol=3)
dev.off()

#Figure 6
pdf("./figures/Figure6.pdf", width = 12.5, height =5)
ggdid(agg.gs)
dev.off()

#####################Check non-linear effects through squared terms########################
mAsqr <- lm(valueMean_log ~  treatment_final+ count2treat+ I(count2treat^2)  + treatment_final:count2treat + treatment_final:I(count2treat^2) + as.factor(Codfavela) + as.factor(wave), data= data_rd)
summary(mAsqr)
stargazer(mAsqr,type="text",out="./Tables/mAsqr.txt")

data_rdT1 <-  data_rd%>% dplyr::filter(treatment_final == 1)
data_rdT0 <-  data_rd%>% dplyr::filter(treatment_final == 0)

mAsqr2 <- lm(valueMean_log ~ count2treat+ I(count2treat^2) + as.factor(Codfavela) + as.factor(wave), data= data_rdT0)
summary(mAsqr2)
mAsqr3 <- lm(valueMean_log ~ count2treat+ I(count2treat^2) + as.factor(Codfavela) + as.factor(wave), data= data_rdT1)
summary(mAsqr3)

stargazer(mAsqr2,mAsqr3, 
          keep =  c("count2treat","I(count2treat^2)","Constant"),
          title="Table A5: Quadratic Analysis",
          covariate.labels = c("count","count2"),
          dep.var.caption = "Log Mean Luminosity",
          colnames = FALSE,
          type="text",out="./Tables/TableA5.txt")

#####################With 6 Pre-Treatment Months########################

#Creat Data if Occupation Occured 6 months After available measures
data_6pr<- data_rd%>% dplyr::filter(occupation_wave >= 6)
#Create Data with only first 6 months of Observation Averaged by Treated and Non-Treated Units
data_6<- data_6pr%>% dplyr::filter(wave <= 6)
data_6<- data_6 %>% dplyr::group_by(Treatment,wave) %>% dplyr::summarise(valueSum_log=mean(valueSum_log),
                                                                         valueMean_log=mean(valueMean_log)) %>%
  ungroup()

#Plot Parallel Trends (Treated  with 6 months of Pre-treatment Observations and non-Treated Units)
pdf("./figures/Figure3.pdf", width = 10, height =5)
ggplot(data_6, aes(x = factor(wave), y = valueMean_log, group = Treatment, color = factor(Treatment))) + 
  geom_line()+
  labs(x ="Wave", y = "Log Mean Luminosity", color = "Treatment Status") + 
  scale_color_manual(labels = c("Control", "Treatment"), values = c("gray55", "gray0")) + 
  theme(panel.background = element_rect(fill = "gray90"),
        panel.grid.major = element_line(colour = "gray99", size = 0.2),
        panel.grid.minor = element_line(colour = "gray100",size = 0.2)) +
  ggtitle("Figure 3: Logged Luminosity Time Trends by Treatment Groups")
dev.off()

#Run Models With Mean Luminosity (Main Model)
m1A <- felm(valueMean_log ~ treatment_final | wave + Codfavela, exactDOF = TRUE, data= data_6pr)
summary(m1A)
stargazer(m1A,
          title="Table A2: Difference-in-Differences",
          covariate.labels = c("Treatment"),
          column.labels = c("Log Mean Luminosity"),
          dep.var.caption = "Dependent Variable",
          type="text",out="./Tables/TableA2.txt")

############Interupted Time Series (ITS) Model ################
data_6pr$posttrend<-data_6pr$count2treat*data_6pr$treatment_final
m2A <- lm(valueMean_log ~  treatment_final+ count2treat+ posttrend + as.factor(Codfavela) + as.factor(wave), data= data_6pr)
options(max.print=10000)
summary(m2A)
stargazer(m2A, keep =  c("treatment_final", "count2treat","posttrend"),
          title="Table A3: Interrupted Time Series Model",
          covariate.labels = c("Treatment","Pre-Treatment Trend","Post Treatment Trend"),
          column.labels = c("Log Mean Luminosity"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels.include = FALSE,type="text",out="./Tables/TableA3.txt")

####pixels#####

data_rd%>% dplyr::select(Name,pixels)%>% ## Favelas and Pixels
  distinct()%>%summary(pixels)

data_rd%>% dplyr::select(Name,pixels)%>% ## frequency table
  distinct()%>%group_by(pixels)%>%
  summarize(n=n())



