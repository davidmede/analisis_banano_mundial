setwd('D:/Sioma/Datos_banano')
dir()
data<-read.csv("FAOSTAT_2020_paises.csv")
unique(data$Element)

library(tidyr)
library(dplyr)
library(ggplot2)

###data_2<-pivot_wider(data,names_from='Element', values_from='Value')


area<-filter(data, Element=='Area harvested')
rendimiento<-filter(data, Element=='Yield')
produccion<-filter(data, Element=='Production')


############# Area

area <- filter(area, Area != "China")

area<-arrange(area, desc(Value))

area2<-area[1:50,]

ggplot(area2,aes(x=reorder(Area, -Value),y=Value))+
        geom_col(fill='#de2d26', width = 0.8)+ theme(axis.text.x = element_text(angle = 90))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        labs(x="País", y="Área cosechada (ha)", 
             title='Top 50 de países con mayor área cosechada \nde banano en el mundo (2020)')+
        theme(text=element_text(family="Times New Roman", size=10))
        
############ Produccion

install.packages('scales')
library(scales)

produccion <- filter(produccion, Area != "China") 

produccion<-arrange(produccion, desc(Value))

produccion2<-produccion[1:50,]

ggplot(produccion2,aes(x=reorder(Area, -Value),y=Value))+
        geom_col(fill='#de2d26', width = 0.8)+ theme(axis.text.x = element_text(angle = 90))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        labs(x="País", y="Producción (ton)", 
             title='Top 50 de países con mayor producción \nde banano en el mundo (2020)')+
        theme(text=element_text(family="Times New Roman", size=10))+
        scale_y_continuous(labels = scales::comma)


############ Rendimiento


rendimiento <- arrange(rendimiento, desc(Value))

rendimiento <- filter(rendimiento, Area != "China") 

rendimiento2<-rendimiento[1:50,]

rendimiento2$rend_ton<-rendimiento2$Value/10000

 

ggplot(rendimiento2,aes(x=reorder(Area, -rend_ton),y=rend_ton))+
        geom_col(fill='#de2d26', width = 0.8)+ theme(axis.text.x = element_text(angle = 90))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        labs(x="País", y="Rendimiento (ton/ha)", 
             title='Top 50 de países con mayor rendimiento \nde banano en el mundo (2020)')+
        theme(text=element_text(family="Times New Roman", size=10))+
        scale_y_continuous(labels = scales::comma)

