library(fpp2)
#Problem 6-2
autoplot(plastics)
plastics %>% decompose(type="multiplicative") %>% autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
of Plastics index")
plastics.decomp <- decompose(plastics, type = "multiplicative")
autoplot(plastics.decomp$trend+plastics.decomp$random)+ggtitle("Seasonally adjusted")
#add outlier
plastics[4]=2400
autoplot(plastics)
plastics.decomp <- decompose(plastics, type = "multiplicative")
autoplot(plastics.decomp$trend+plastics.decomp$random)+ggtitle("Seasonally adjusted")
# Problem 6-3
retaildata <- readxl::read_excel("/Users/vnagh/Desktop/untitled_folder_4/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))
autoplot(myts)
library(seasonal)
myts %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of retail A3349873A")
autoplot(myts, series="Data") + autolayer(trendcycle(fit), series="Trend") + autolayer(seasadj(fit), series="Seasonally Adjusted") + xlab("Year") + ylab("Retail A3349873A") + ggtitle("Retail") + scale_colour_manual(values=c("gray","blue","red"),breaks=c("Data","Seasonally Adjusted","Trend"))
#Problem 6-5
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)
#STL decomposition
cangas %>%stl(t.window=13, s.window=13, robust=TRUE) %>% autoplot()
#X11 decomposition
cangas %>% seas(x11="") %>% autoplot() +ggtitle("X11 decomposition of Canadian gas production ")
#Problem 6-6
bricksq.decomp <- stl(bricksq, t.window=7, s.window=7, robust=F) 
autoplot(bricksq.decomp)
trendcycle(bricksq.decomp)
autoplot(seasadj(bricksq.decomp))+ggtitle("Seasonally adjusted")
naive(seasadj(bricksq.decomp))
autoplot(seasadj(bricksq.decomp), series = "seasonally adjusted")+ggtitle("Seasonally adjusted")+autolayer(naive(seasadj(bricksq.decomp)), series = "naive forecast")
autoplot(seasadj(bricksq.decomp), series = "seasonally adjusted")+ggtitle("STLF forecast")+autolayer(stlf(seasadj(bricksq.decomp)), series = "naive forecast")
checkresiduals(stlf(seasadj(bricksq.decomp)))
bricksq.decomp <- stl(bricksq, t.window=7, s.window=7, robust=F) 
checkresiduals(stlf(seasadj(bricksq.decomp)))
# Problem 6-7
autoplot(writing)
(lambda <- BoxCox.lambda(writing))
writing.boxCox <- BoxCox(writing,lambda)
autoplot(writing.boxCox)
autoplot(writing.boxCox, series = "Box-Cox data")+autolayer(stlf(writing.boxCox, method="naive"), series = "Naive forecast")
autoplot(writing.boxCox, series = "Box-Cox data")+autolayer(stlf(writing.boxCox, method="rwdrift"), series = "rwdrift forecast")
checkresiduals(stlf(writing.boxCox, method="naive"))
checkresiduals(stlf(writing.boxCox, method="rwdrift"))
#Problem 6-8
autoplot(fancy)
(lambda <- BoxCox.lambda(fancy))
fancy.boxCox <- BoxCox(fancy,lambda)
autoplot(fancy.boxCox)
autoplot(fancy.boxCox, series = "Box-Cox data")+autolayer(stlf(fancy.boxCox, method="naive"), series = "Naive forecast")
autoplot(fancy.boxCox, series = "Box-Cox data")+autolayer(stlf(fancy.boxCox, method="rwdrift"), series = "rwdrift forecast")
checkresiduals(stlf(fancy.boxCox, method="naive"))
checkresiduals(stlf(fancy.boxCox, method="rwdrift"))
