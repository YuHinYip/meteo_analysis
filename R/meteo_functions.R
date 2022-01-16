#' Combine meteo dataset
#' @param first_dataset file
#' @param second_dataset file
#' @return the combination of \code{first_dataset} and \code{second_dataset} + add a variable \strong{Location}
#' @description Use this function when you want to analyse 2 different weather dataset together. It will add a variable \strong{Location} to differentiate them by adding their title(make sure to name right your file)

#' @export
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
#' @param dataset data frame
#' @description Use for adding seasons, to look if in each season there is a pattern
#' @return Add a variable \strong{Season}, which has \emph{Winter}, \emph{Spring}, \emph{Summer} and \emph{Autumn}

#' @export
add_seasons<-function(dataset){
  dataset_season$Season[dataset_season$Month==12|dataset_season$Month==1|dataset_season$Month==2]<-"Winter"
  dataset_season$Season[dataset_season$Month==3|dataset_season$Month==4|dataset_season$Month==5]<-"Spring"
  dataset_season$Season[dataset_season$Month==6|dataset_season$Month==7|dataset_season$Month==8]<-"Summer"
  dataset_season$Season[dataset_season$Month==9|dataset_season$Month==10|dataset_season$Month==11]<-"Autumn"
  dataset_season$Season<-as.factor(dataset_season$Season)
  return(dataset_season)
}

#' First ggplot
#' @param dataset data frame
#' @description A first glance on the data to have an idea of the evolution of the temperature
#' @return a ggplot of the \code{dataset}
#' @details Expected the data is the combination of two weather data frame

#' @export
first_graph_analysis<-function(dataset){
  ggplot(dataset,aes(x=Year,y=Temperature,color=Location))+geom_point()+geom_smooth(method = lm)+ggtitle("Evolution of the temperature for 2 Locations")
}
#' Second plot, ggpairs
#' @param dataset data.frame
#' @description a more general view on other variables
#' @return a ggppairs of \code{dataset}
#' @details Expected the data is the combination of two weather data frame

#' @export
second_graph_analysis<-function(dataset){
  ggpairs(dataset,aes(color=Location))+theme(axis.text.x = element_text(angle = 90, vjust=1, hjust=1), axis.text.y = element_text(size=3))
}
#' Find suitable linear model
#' @param dataset data frame
#' @description Instead of doing manual again, if you are using the same data, it should have the same best linear model to explain \code{dataset}. In case of you want to remove other variables or interaction, it have to do it manually
#' @return a linear model to explain \code{dataset}
#' @details expected the data is the combination of two weather data, to have the right formula, if using the same kind of data

#' @export
best_linear_model_in_this_analysis<-function(dataset){
  #put all the filtration here, in case the person using this r packages wants to know which interaction or variables I choose to not use
  lm1<-lm(Temperature~Precipitation*Season*Location*Year, dataset)
  lm2<-update(lm1,.~.-Precipitation:Location:Year)
  lm3<-update(lm2,.~.-Precipitation:Location)
  lm4<-update(lm3,.~.-Season:Year)
  lm5<-update(lm4,.~.-Precipitation:Season)
  lm6<-update(lm5,.~.-Precipitation:Season:Location)
  lm7<-update(lm6,.~.-Season:Location-Season:Location:Year)
  return(lm7)
}
#' Autoplot, for validation
#' @param linear_model lm
#' @description a validation of \code{linear model}
#' @return the autoplot of \code{linear_model}

#' @export
third_graph_analysis<-function(linear_model){
  autoplot(linear_model,c(1:4))
}
