# Description -------------------------------------------------------------
## Functions used to retrive and raster satellite images

# Functions ----------------------------------------------------------------


rnight_incio_loop=function(links) ## step0
{
  ### Download, unzip, raster, and create dataframe. 
  ### imput: list of links to satellite images 
  ### output: data for all the polygons times the number of images in a link
  dataf=data.frame(Name=NA, Codfavela=NA,valueSUM=NA, valueMean=NA,pixels=NA,
                   type=NA, date=NA)
  for (i in 1:length(links))
  {
    datatemp=rnight_incio(links[[i]]) ## step01
    unlink("temp1", recursive = TRUE) #deletes the directory after each link
    dataf=rbind(dataf,datatemp) ## append the data after each link
    save(dataf,file="TempPolygonsLights.Rdata")
  } 
  dataf[-1,]
}

rnight_incio=function(link) ## step01
{
  ### Imput: link to the website
  ### output: data for all the polygons times the number of images in that link. 
  down_unzip(link) #step02 
  images=list.files(path="./temp1",pattern=".tif")
  rnight_data2(images) #step03
  ### Pending: create an if error condition.. in case the link dosn't wor.
  ### should print an error message to know which immage didn't got
}

down_unzip=function(file) #step02
{
  ## This function download a zipped file and unzip it.
  ## formats: tgz or tar. 
  ## Creates a folder temp1
  ## imput: url address to the .tgz file 
  ## Outputs: folder temp with the tif images. 
  ## Removes the compressed file because is too large
  download.file(file, "test1.tgz")
  untar("test1.tgz", exdir = "temp1") #unzip
  file.remove("test1.tgz") #remove the file
}

rnight_data2=function(listfiles) #step03
{
  ## imput: list with paths to tif. files
  ## output: data with favela, vaue sum, value mean, type (of tif), tif's date 
  data2=data.frame(Name=NA, Codfavela=NA,valueSUM=NA, valueMean=NA, pixels=NA,
                   type=NA, date=NA)
  for (i in 1:length(listfiles))
  {
    temp1=raster(rimage(listfiles[i])) #step04 -completa el path
    type= substrRight(listfiles[i],10) #step05 
    data=rnight_data(temp1) #step06
    data$type=type
    data$date=substr(listfiles[i],11,16)
    data2=rbind(data2,data) #append df
  }
  data2[-1,]
}

rimage=function(path1) # step04
{
  ## creates a path to the image
  ## imput: name of the .tif
  ## output: path to the tif file
  paste0("./temp1","/",path1)
}

substrRight <- function(x, n) #step05
{
  #substr starting from the end
  substr(x, nchar(x)-n+1, nchar(x))
}

rnight_data=function(tifraster) #step06
{
  ## imput: a rastered file 
  ## output: a data set with the favela, sum, mean for each poylgon
  ## note: for this to work a list "polygons" has to be created before. 
  #data=data_frame(Name=NA, Codfavela=NA,valueSUM=NA, valueMean=NA,pixels=NA)
  Name=list()
  Codfavela=list()
  valueSUM=list()
  valueMean=list()
  pixels=list()
  for (i in 1:length(polygons)) #This should be in the enviroment
  {
    Name[i]=as.character(polygons[[i]]$Nome)
    Codfavela[i]=as.character(polygons[[i]]$Codfavela)
    valueMean[i]=crop_stats(tifraster,polygons[[i]])[1] #step09
    valueSUM[i]=crop_stats(tifraster,polygons[[i]])[2] #step09 
    pixels[i]=crop_stats(tifraster,polygons[[i]])[3] #step09
  }
  data=data.frame(Name=unlist(Name),Codfavela=unlist(Codfavela),
                  valueSUM=unlist(valueSUM), valueMean=unlist(valueMean),
                  pixels=unlist(pixels))
  return(data)
}

crop_mean=function(tifRaster,polygon) #step07
{
  ## This program raster a tiff image and crop it to a 
  ## Polygon 
  ## Imput: a tif file, and a polygon (spatial object)
  ## output: a numeric value for each polygon. In this   
  ##case is the mean of the pixeles. The original      
  ##Rnighlights package use the sum. 
  polygon=spTransform(polygon, CRS(proj4string(tifRaster)))
  temp2=crop(x =tifRaster ,y = polygon)
  mean(temp2@data@values)
}

crop_sum=function(tifRaster,polygon) #step08
{
  ## This program raster a tiff image and crop it to a 
  ## Polygon 
  ## Imput: a tif file, and a polygon (spatial object)
  ## output: a numeric value for each polygon. In this  
  ##case is the sum of the pixeles. The original      
  ##Rnighlights package use the sum. 
  polygon=spTransform(polygon, CRS(proj4string(tifRaster)))
  temp2=crop(x =tifRaster ,y = polygon)
  sum(temp2@data@values)
}


crop_stats=function(tifRaster,polygon) #step09
{
  ## This program raster a tiff image and crop it to a 
  ## Polygon 
  ## Imput: a tif file, and a polygon (spatial object)
  ## output: a numeric value for each polygon. In this   
  ##case is the mean of the pixeles. The original      
  ##Rnighlights package use the sum. 
  polygon=spTransform(polygon, CRS(proj4string(tifRaster)))
  temp2=crop(x =tifRaster ,y = polygon)
  mean=mean(temp2@data@values)
  sum1=sum(temp2@data@values)
  pixels=length(temp2@data@values)
  values=c(mean,sum1,pixels)
  return(values)
}

#### Things to improve

# Get the nuber of pixeles
# Add argument if crop sum - crop mean can be one step

