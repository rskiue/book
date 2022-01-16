
library(tidyverse)


cross_graph<-function(dt,x,y,y_label){
  x<-enquo(x)
  y<-enquo(y)
  out<-dt %>%
    count(!!x,!!y) %>%
    group_by(!!x)%>%
    mutate(percent=n/sum(n))%>%
    mutate(item =  paste0(!!x,"\n(N=",sum(n),")")) %>%
    ggplot(aes(x = item, y = percent, fill = factor(!!y)))+
    geom_bar(stat = "identity", position = "fill",colour = "black", size = 0.25)+
    geom_text(aes(label = paste0(round(percent, 3) * 100, "%")), position = position_stack(.5)) +
    scale_fill_brewer(palette="Pastel2",
                      label=y_label)+
    scale_y_reverse()+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y=element_text(hjust = 1,size = 14),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position="bottom",
          legend.title=element_blank())+
    coord_flip()
return(out)
}

