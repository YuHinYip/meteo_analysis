#' Pick libraries needed
#' @export
libraries_for_this_analysis<-function(){
  #libraries needed in this weather analysis
  requireNamespace(ggplot2)
  requireNamespace(ggfortify)
  requireNamespace(GGally)
}

#' Combine meteo dataset
#' @export
#' @param first_dataset data.frame
#' @param second_dataset data.frame
combine_meteo_data<- function(first_dataset,second_dataset){
  #if use 2 dataset, one way to analyse is to merge them
  meteo1<-read.table(first_dataset, header = T)
  meteo2<-read.table(second_dataset, header = T)

  #add "Location" variable to differentiate them by adding their title(make sure to name the right title)
  meteo1$Location<-first_dataset
  meteo2$Location<-second_dataset
  meteo1$Location<-as.factor(meteo1$Location)
  meteo2$Location<-as.factor(meteo2$Location)

  #merge them together
  combinaison<-rbind(meteo1,meteo2)

  #make sure they are numeric
  combinaison$Precipitation<-as.numeric(combinaison$Precipitation)
  combinaison$Year<-as.numeric(combinaison$Year)
  combinaison$Month<-as.numeric(combinaison$Month)

  return(combinaison)
}

#' Add seasons
#' @export
#' @param dataset data.frame
add_seasons<-function(dataset){
  #Use for adding season, to look if in each season there is a pattern
  dataset$Season[dataset$Month==12|dataset$Month==1|dataset$Month==2]<-"Winter"
  dataset$Season[dataset$Month==3|dataset$Month==4|dataset$Month==5]<-"Spring"
  dataset$Season[dataset$Month==6|dataset$Month==7|dataset$Month==8]<-"Summer"
  dataset$Season[dataset$Month==9|dataset$Month==10|dataset$Month==11]<-"Autumn"
  dataset$Season<-as.factor(dataset$Season)
  return(dataset)
}

#' First ggplot
#' @export
#' @param dataset data.frame
first_graph_analysis<-function(dataset){
  #a first glance on the data to have an idea of the evolution of the temperature
  ggplot(dataset,aes(x=Year,y=Temperature,color=Location))+geom_point()+geom_smooth(method = lm)+ggtitle("Evolution of the temperature for 2 Locations")
}
#' Second plot, ggpairs
#' @export
#' @param dataset data.frame
second_graph_analysis<-function(dataset){
  #a more general view on other variables
  ggpairs(dataset,aes(color=Location))+theme(axis.text.x = element_text(angle = 90, vjust=1, hjust=1), axis.text.y = element_text(size=3))
}
#' Find suitable linear model
#' @export
#' @param dataset data.frame
best_linear_model_in_this_analysis<-function(dataset){
  #instead of doing manual again, if using the same data, should have the same best linear model to explain the dataset
  #put all the filtration here, in case the person using this r packages wants to know which interaction or variables I choose to not use
  lm1<-lm(Temperature~Precipitation*Season*Location*Year, dataset)
  lm2<-update(lm1,.~.-Precipitation:Location:Year)
  lm3<-update(lm2,.~.-Precipitation:Location)
  lm4<-update(lm3,.~.-Season:Year)
  lm5<-update(lm4,.~.-Precipitation:Season)
  lm6<-update(lm5,.~.-Precipitation:Season:Location)
  lm7<-update(lm6,.~.-Season:Location-Season:Location:Year)
  #in case of the person wants to remove other variables or interaction, it have to do it manually
  return(lm7)
}
#' Autoplot, for validation
#' @export
#' @param linear_model lm
thrid_graph_analysis<-function(linear_model){
  #a validation of the linear model
  autoplot(linear_model,c(1:4))
}
