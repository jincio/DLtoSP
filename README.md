# Replication files: From Drug Lords to Police State: The Effects of Order Transition on Local Economies

## Abstract

What is the effect on local economies when the state intervenes to capture its own territoriesback from non-state actors? In 2008, the government of Rio de Janeiro, Brazil implemented apolicy to take control offavelasthat were previously dominated by criminal organizations. Weuse day and night luminosity to assess the effects of this program on economic growth.  Thedifference-in-differences design shows that state intervention has a significant and negative av-erage treatment effect on thefavelasthat received the intervention.  We further a mechanismsto explain this economic downturn: institutional replacement. Based on crime data, we demon-strate that this effect is caused by the destruction of local markets, especially illicit activities.The data highlight the perils of order transition, even when OCGs are removed by state actors.Furthermore, this paper reinforces the need for policies that are mindful of the externalities ofinstitutional shifts

## Files 

First step: download,  and process images to extract the data. 

  S01_DatesUrlsScript.R: gets the dates and URLs for each satellite's images.
  
Output: **DatesUrlsNgdc.Rdata**

  S02_codeDLtoPS.R: retrieve, and process the satellite images. This script uses the file *functions.R*

Output: **PolygonsLights.xlsx**   

Second step: merge files. 

  S03_merge_dataAnalysis.R: merges PolygonsLights.xlsx with favelas' shapes files and UPP's information (/Dependencies)
  
Output: **SetsAnalysis07232021.RData**

Analysis: code that produces regression tables and graphs. Tables and graphs are saved in folders (figure, Tables)

  S04_modeling.R
  
Output: 
  /figures
  /Tables. 


