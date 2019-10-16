library(dplyr)
library(reshape2)
library(ggplot2)

for (i in 1:length(buffers)) {
  
  announced100 <- do.call(rbind,lapply(linear_crosstab[[i]],function(x){
    data.frame(x)  }))
  
  announced100 = announced100 %>% 
    # group_by(esa_1992,layer) %>%
    # summarise(Freq=sum(Freq))  %>% 
    left_join(.,legend[,1:2]) %>%
    left_join(.,legend[,3:4])) %>%
    group_by(group_esa_1992,group_esa_2014) %>%
    summarise(Freq=sum(Freq,na.rm = T)) %>%  dcast(.,group_esa_1992~group_esa_2014,value.var = "Freq")
  
  crosstab_92_14[[i]] <- announced100
}
  
names(crosstab_92_14) <- list.files(pattern=".shp",recursive = T)
  
crosstab_92_14 <- list()


ggplot(data = announced100,mapping=aes(x=group_esa_1992, y=group_esa_2014, fill=Freq)) + 
  geom_tile()+
  geom_text(aes(label = Freq), color = "white", size = 4)
