## Step 01
# get links from website

require(rvest) #to scrap the website
require(dplyr)

## Urls ---------------------------------------------------

d1="https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10/"

dates=d1%>%read_html()%>%html_nodes('a')%>%
  html_text()%>%grep("/",.,value=TRUE)%>%
  tibble()%>%
  mutate(char=nchar(.))%>%filter(char>6) ### because there from 2015 there is links with all the pictures for one year. We need the montly picture

url1="https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10/"
url2="vcmcfg/"
url3=list()
for (i in 1:nrow(dates)) 
{url3[i]=paste0(url1,dates[i,1],url2)}

url4=list()
for (i in 1:length(url3)) 
{
  link=url3[[i]]
  file=link%>%read_html()%>%html_nodes('a')%>%
    html_text()%>%grep("00N060W.*tgz",., value=TRUE)
  url4[i]=paste0(link,"/",file)
  pool<-seq(1,3,length.out = 200)
  Sys.sleep(sample(pool,1))
}

save(url4,dates, file="DatesUrlsNgdc.Rdata")
