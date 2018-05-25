

for (package in package_Vector) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}



tablica <- ddply(dane, c("cumf", "totaln", "xmonth"),mean(na.rm=TRUE))


daneTest <-dane[is.na(dane$cfin1),]
View(daneTest)

for (icol in c("cfin1","cfin2","chel1","chel2","lcop1","lcop2","sst")){
  for (irow in row.names(daneTest)){
    if (is.na(daneTest[irow,icol])) daneTest[irow,icol] <- max(dane[dane$cumf==daneTest$cumf && dane$totaln==daneTest$totaln && dane$xmonth ==daneTest$xmonth,icol])
  }
}

mode



f1 <- function(x) {
  for (icol in 1:ncol(x)) {
    ifelse(!is.na(x[,icol]),(x[,icol]),(x[,icol] <- subset(srednieGrupowe, (srednieGrupowe$totaln == x$totaln) & (srednieGrupowe$xmonth == x$xmonth),colnames(x)[icol])))
    }
  return (x)
  }


daneTest2 <- ddply(daneTest,1,f1)
daneTest2


subset(srednieGrupowe,srednieGrupowe$totaln>750000 & srednieGrupowe$xmonth ==2,)


daneTest <- dane
daneTest2 <- as.data.frame(apply(dane[,c("length", "cfin1", "cfin2", "chel1", "chel2", "lcop1", "lcop2")],2,function(x) {ifelse((x>(mean(x)+3*sd(x))) | (x<(mean(x)-3*sd(x))),NA,x)}))
summary(daneTest2)

meltData <- melt(daneTest2[,c("length", "cfin1", "cfin2", "chel1", "chel2", "lcop1", "lcop2", "cumf", "sst", "sal", "nao")],na.rm = TRUE)
p <- ggplot(meltData, aes(factor(Var2),value))
p + geom_boxplot() + facet_wrap(~Var2, scale = "free")




df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")

p <- df %>%
  plot_ly(
    x = ~day,
    y = ~total_bill,
    split = ~day,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Day"
    ),
    yaxis = list(
      title = "Total Bill",
      zeroline = F
    )
  )





plot_ly(dane,
    x = ~fbarFactor,
    y = ~length,
    split= ~fbarFactor,
    type='box',
    mode ="markers"
)

p <- dane %>%
  plot_ly(
    x = ~fbarFactor,
    y = ~length,
    split= ~fbarFactor,
    type='box',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = Tdev
    )
  ) %>%
  layout(
    xaxis = list(
      title = "Odsetek pozostawionego narybku"
    ),
    yaxis = list(
      title = "Rozkład długości śledziocha",
      zeroline = F
    )
  )




















df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")
plot_ly(y =~ df$total_bill,type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)) %>% 
  layout(xaxis = list(title = ""),yaxis = list(title = "Total Bill",zeroline = F))
