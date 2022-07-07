rm(list=ls())

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(jpeg)
library(lattice)
library(latticeExtra)
library(scales)


# REPRODUCTION OF THE 1ST GRAPH

wig=read.csv("wig.csv", header=T, sep=",", dec=".")
wig$Data = as.Date(wig$Data)

wykres_1 = xyplot(
  Otwarcie+Najwyzszy+Najnizszy+Zamkniecie~Data,
  data=wig,
  type="s",
  col.line=c("Blue","Pink","Green","Red"),
  pch=3,
  lwd=1,
  lty=1,
  xlab="Data",
  ylab="PLN",
  main="Ceny",
  auto.key=list(columns=4, rectangles=!F)
)

wykres_2 = xyplot(
  Wolumen~Data,
  data=wig,
  type="s",
  col.line=c("Dodgerblue"),
  pch=3,
  lwd=1,
  lty=1,
  xlab="Data",
  ylab="Wolumen",
  main="Wolumen"
)

x11(width=10);
plot(wykres_1, split=c(1,1,1,2), more=T)
print(wykres_2, split=c(1,2,1,2), more=F)


# REPRODUCTION OF THE 2ND GRAPH

wig=read.csv("wig.csv", header=T, sep=",", dec=".")
wig$Data = as.Date(wig$Data)

logo <- jpeg::readJPEG("logo.jpg")

wykres_1 <- ggplot(wig, aes(x=Data)) +
    geom_line(aes(y = Otwarcie, color = "Otwarcie")) + 
    geom_line(aes(y = Najwyzszy, color="Najwyzszy")) +
    geom_line(aes(y = Najnizszy, color="Najnizszy")) +
    geom_line(aes(y = Zamkniecie, color="Zamkniecie")) +
    labs(x="", y="", color="Legenda") +
    ggtitle("Notowanie WIG") +
    theme(legend.key = element_rect(fill="white", color="white"),
          legend.key.size = unit(0.5, "lines"))

wykres_2 <- ggplot(wig, aes(x=Data, y=Wolumen)) +
    geom_line() + 
    xlab("") +
    ylab("") +
    ggtitle("Wolumen") +
    theme(plot.title = element_text(family="Wolumen",size=7),
          text = element_text(size=6))

wykresy <-ggarrange(wykres_1, wykres_2, 
                    ncol = 1, nrow = 2)
					
x11(width=9)
print(wykresy)


# REPRODUCTION OF THE HISTOGRAM

wig$roznice_zam_otw=(wig$Zamkniecie-wig$Otwarcie)

logo <- matrix(rgb(logo[,,1],logo[,,2],logo[,,3], logo[,,3] * 0.5), nrow=dim(logo)[1])

hist_roznice <- ggplot(wig, aes(x=roznice_zam_otw)) +
	annotation_custom(rasterGrob(logo, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
							   -Inf, Inf, -Inf, Inf) +
	geom_histogram(color="darkblue", fill="lightblue") +
	labs(x = "", y="", title="Histogram roznic kursow zamkniecia i otwarcia") +
	theme(plot.title = element_text(hjust = 0.5))

x11(width=8);
plot(hist_roznice)


# REPRODUCTION OF THE PIE CHART

wig$Dni <- weekdays(wig$Data)
wig$Dzien <- recode(wig$Dni, 
                    "Monday"="Poniedzialek",
                    "Tuesday"="Wtorek",
                    "Wednesday"="Sroda",
                    "Thursday"="Czwartek",
                    "Friday"="Piatek")

procenty <- data.frame(Dzien=character(),
                       Wolumen=integer())

suma <- sum(wig$Wolumen, na.rm=T)

for (i in unique(wig$Dzien))
{
    x <- filter(wig, Dzien == i)
    suma_x <- sum(x$Wolumen, na.rm=T)
    proc_x <- suma_x/suma*100
    procenty[nrow(procenty) + 1,] = c(i, proc_x)
}

procenty$Wolumen <- as.numeric(procenty$Wolumen)

pie_chart <- ggplot(procenty, aes(x="", y=Wolumen, fill=Dzien)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    geom_text(aes(label = paste(round(Wolumen,2),'%', sep="")),
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette="RdPu") +
    labs(y="", title="Wolumen dla dni tygodnia")

pie_chart_v2 <- (ggdraw() +
                 draw_image(logo, scale = 0.4, x=0.02, y=0.70, hjust = -0.32, vjust = 0.37) +
                 draw_plot(pie_chart + theme_half_open() + theme_void() + 
                           theme(plot.title = element_text(color = "darkblue", 
                                                           size=20, 
                                                           face="bold", 
                                                           hjust=0.5),
                                 legend.title = element_text(color = "darkblue", face = "bold"),
                                 legend.text = element_text(color = "darkblue"))))

x11(width=8);
plot(pie_chart_v2)


# HISTOGRAM GENERATION

wig$roznice_zam_otw=(wig$Zamkniecie-wig$Otwarcie)

hist_roznice <- histogram(
  ~roznice_zam_otw,
  data=wig,
  breaks=36,
  col="darkviolet",
  type="count",
  xlab="",
  ylab="",
  main="Histogram roznic kursow zamkniecia i otwarcia"
)
x11(width=8);
plot(hist_roznice)


# BOXPLOT GENERATION

wig$roznice_max_min <- (wig$Najwyzszy-wig$Najnizszy)
wig$dni <- weekdays(wig$Data)
wig$dni_tyg <- recode(wig$dni, 
                        "Monday"="Poniedzialek",
                        "Tuesday"="Wtorek",
                        "Wednesday"="Sroda",
                        "Thursday"="Czwartek",
                        "Friday"="Piatek")
wig$dni_tyg <- factor(wig$dni_tyg, levels=c('Poniedzialek', 'Wtorek', 'Sroda', 'Czwartek', 'Piatek'))
boxplot_roznice <- bwplot(roznice_max_min~dni_tyg,
                       data=wig,
                       fill="lightblue",
                       ylab = "Roznice kursow",
                       main="Roznice kursow max i min dla dni tygodnia",
                       col=F,
                       horizontal=F)

x11(width=8);
plot(boxplot_roznice)