rm(list=ls())

library(ggplot2)

getNBPRates = function(year=2021){
    dataset=readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"), 
                encoding="ASCII")
    dataset=dataset[-2]
    dataset=dataset[-c((length(dataset)-3):length(dataset))]
    
    x=read.table(textConnection(dataset,encoding = "UTF-8"), sep=";", dec=",", header=T)
    x=x[,-c((ncol(x)-2):ncol(x))]
    
    x=x[,grep("data|USD|EUR", colnames(x))]
    colnames(x)=gsub("X1","",colnames(x))
    x$data=as.Date(as.character(x$data),format="%Y%m%d")
    
    return(x)
}
d2013=getNBPRates("2013")
d2014=getNBPRates("2014")
d2015=getNBPRates("2015")
d2016=getNBPRates("2016")
d2017=getNBPRates("2017")
d2018=getNBPRates("2018")
d2019=getNBPRates("2019")
d2020=getNBPRates("2020")
d2013_2020 = rbind(d2013,d2014,d2015,d2016,d2017,d2018,d2019,d2020)

plot1 <- (
    ggplot(data = d2013_2020, aes(x=data)) +
        geom_line(aes(y = USD), colour="blue") +
		geom_line(aes(y=EUR), colour="red") +
        xlab("date") +
        ylab("usd") +
        ggtitle("Wykres kursów œrednich NBP dla EUR i USD")
)

x11(width=10)
print(plot1)