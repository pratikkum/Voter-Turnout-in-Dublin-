# Voter Turnout

# Leap 

# install.packages("leaps")
# install.packages("GWmodel")
# install.packages("PerformanceAnalytics")

# library(robustbase)

# install.packages("robustbase","RColorBrewer")
# install.packages('spDataLarge',repos='https://nowosad.github.io/drat/', type='source')
# install.packages("corrplot")
install.packages("spdep")


library(spDataLarge)
library(GWmodel)
library(leaps)
library(PerformanceAnalytics)
library(RColorBrewer)
library(corrplot)
library(spdep)
library(classInt)

# Exploratory Analysis 

View(Dub.voter)

my_data <- as.data.frame(Dub.voter[, c(4:12)])
chart.Correlation(my_data, histogram=TRUE, pch=19)

corr <- round(cor(my_data), 2)
res1 <- cor.mtest(my_data, conf.level = .95)
res2 <- cor.mtest(my_data, conf.level = .99)
p.mat <- cor.mtest(my_data)$p
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


fit_lm_r <- lm(GenEl2004 ~ . ,data=Dub.voter)

# Moran's I Test

Dub.mi <- poly2nb(Dub.voter)
Dub.list <- nb2listw(Dub.mi)

moran.test(Dub.voter$GenEl2004,Dub.list)
# P-value is << 0.05 and it is evident that pattern is not random

lm.morantest(fit_lm_r,Dub.list)
# Moran test on residuals suggests that residuals are auto-correlated

# Voter Turnout 

gw.ss.bx <- gwss(Dub.voter, vars = c("GenEl2004", "LARent", "Unempl", "DED_ID"),
                 kernel = "boxcar", adaptive = TRUE, bw = 48, quantile = TRUE)
gw.ss.bs <- gwss(Dub.voter,vars = c("GenEl2004", "LARent", "Unempl", "DED_ID"),
                 kernel = "bisquare", adaptive = TRUE, bw = 48)

map.na = list("SpatialPolygonsRescale", layout.north.arrow(),
              offset = c(329000, 261500), scale = 4000, col = 1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(),
                   offset = c(326500, 217000), scale = 5000, col = 1,
                   fill = c("transparent", "blue"))
map.scale.2 = list("sp.text", c(326500, 217900), "0", cex = 0.9, col = 1)
map.scale.3 = list("sp.text", c(331500, 217900), "5km", cex = 0.9, col = 1)
map.layout <- list(map.na, map.scale.1, map.scale.2, map.scale.3)
mypalette.1 <- brewer.pal(8, "Reds")
mypalette.2 <- brewer.pal(5, "Blues")
mypalette.3 <- brewer.pal(6, "Greens")
X11(width = 10, height = 12)

names(gw.ss.bx$SDF)

spplot(gw.ss.bx$SDF, "Cov_GenEl2004.DED_ID", key.space = "right",
       col.regions = mypalette.1, cuts = 7,
       main = "GW standard deviations for GenEl2004 (basic)",
       sp.layout = map.layout)

spplot(gw.ss.bx$SDF, "GenEl2004_LSD", key.space = "right",
       col.regions = mypalette.1, cuts = 7,
       main = "GW standard deviations for GenEl2004 (basic)",
       sp.layout = map.layout)
X11(width = 10, height = 12)
spplot(gw.ss.bx$SDF, "GenEl2004_IQR", key.space = "right",
       col.regions = mypalette.1, cuts = 7,
       main = "GW inter-quartile ranges for GenEl2004 (robust)",
       sp.layout = map.layout)

X11(width = 10, height = 12)
spplot(gw.ss.bx$SDF, "Corr_GenEl2004.LARent", key.space = "right",
       col.regions = mypalette.2, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0),
       main = "GW correlations: GenEl2004 and LARent (box-car kernel)",
       sp.layout = map.layout)
X11(width = 10, height = 12)
spplot(gw.ss.bs$SDF, "Corr_GenEl2004.LARent", key.space = "right",
       col.regions = mypalette.2, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0),
       main = "GW correlations: GenEl2004 and LARent (bi-square kernel)",
       sp.layout = map.layout)
X11(width = 10, height = 12)
spplot(gw.ss.bs$SDF, "Corr_LARent.Unempl" ,key.space = "right",
       col.regions = mypalette.3, at = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
       main = "GW correlations: LARent and Unempl (basic)",
       sp.layout = map.layout)
X11(width = 10, height = 12)
spplot(gw.ss.bs$SDF, "Spearman_rho_LARent.Unempl",key.space = "right",
       col.regions = mypalette.3, at = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
       main = "GW correlations: LARent and Unempl (robust)",
       sp.layout = map.layout)


# Linear Models - Full Model

fit_lm <- lm(GenEl2004 ~ . ,data=Dub.voter)

summary(fit_lm)

fit_lm1 <- lm(GenEl2004 ~ DiffAdd+LARent+SC1+Unempl+LowEduc+
                Age18_24+Age25_44+Age45_64  ,data=Dub.voter)

summary(fit_lm1)

# DED_ID, X, Y
# LARent , SC1 , Unempl , Age18_24 , Age25_44

mypalette_un <- rev(brewer.pal(8, "RdYlBu"))

spplot(Dub.voter[c(5,7,9,10)],  col.regions = mypalette_un , cuts = 7,
       main = "Variation of Predictors")

# Linear Models - Reduced Model

fit_lm_red <- lm(GenEl2004 ~ LARent+Unempl+
                   Age18_24+Age25_44 ,data=Dub.voter)

summary(fit_lm_red)

# Leaps

All_fits <- regsubsets(GenEl2004 ~ DiffAdd+LARent+SC1+Unempl+LowEduc+
                         Age18_24+Age25_44+Age45_64  , data=Dub.voter,really.big = T)
summary(All_fits)$which
summary(All_fits)$cp

par(mar=c(3,3,0,0))
plot(All_fits, scale="r2", col="blue", main="Best")

# PCA

fit_pca <- prcomp(my_data[,c(1:8)], scale = TRUE)

summary(fit_pca)

fit_pca$rotation[,1:4]

# PCA - reduced

fit_pca_red <- prcomp(my_data[,c(2,4,6,7)], scale = TRUE)

summary(fit_pca_red)

fit_pca_red$rotation[,1:4]

# All PCA Code

Data.scaled <- scale(as.matrix(Dub.voter@data[,4:11]))
pca.basic <- princomp(Data.scaled, cor = F)
(pca.basic$sdev^2 / sum(pca.basic$sdev^2))*100
pca.basic$loadings


R.COV <- covMcd(Data.scaled, cor = F, alpha = 0.75)
pca.robust <- princomp(Data.scaled, covmat = R.COV, cor = F)
pca.robust$sdev^2 / sum(pca.robust$sdev^2)*100
round(pca.robust$loadings[,1:3],2)

Coords <- as.matrix(cbind(Dub.voter$X, Dub.voter$Y))

# GWPCA

Data.scaled.spdf <-  SpatialPointsDataFrame(Coords,as.data.frame(Data.scaled))

bw.gwpca.basic <- bw.gwpca(Data.scaled.spdf,vars = colnames(Data.scaled.spdf@data), 
                           k = 3, robust = FALSE,adaptive = TRUE)
bw.gwpca.basic                                                  
bw.gwpca.robust <- bw.gwpca(Data.scaled.spdf,vars=colnames(Data.scaled.spdf@data), 
                            k = 3, robust = TRUE,adaptive = TRUE)
bw.gwpca.robust   # 119

gwpca.basic <- gwpca(Data.scaled.spdf,vars = colnames(Data.scaled.spdf@data), bw = 131, 
                     k = 8,robust = FALSE, adaptive = TRUE)

gwpca.basic  # 0.7359110

gwpca.robust <- gwpca(Data.scaled.spdf,vars = colnames(Data.scaled.spdf@data), bw = 119, 
                      k = 8,robust = TRUE, adaptive = TRUE)
gwpca.robust  # 0.7359110



prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components])/
            rowSums(gwpca.obj$var))*100)
}
var.gwpca.basic <- prop.var(gwpca.basic, 3)
var.gwpca.basic
var.gwpca.robust <- prop.var(gwpca.robust, 3)
var.gwpca.robust
Dub.voter$var.gwpca.basic <- var.gwpca.basic
Dub.voter$var.gwpca.robust <- var.gwpca.robust

mypalette.4 <-brewer.pal(8, "YlGnBu")
mypalette.4.1 <- brewer.pal(7, "Blues")
X11(width = 10,height = 12)
spplot(Dub.voter, "var.gwpca.basic", key.space = "right",
       col.regions = mypalette.4.1, cuts = 6,
       main = "PTV for local components 1 to 3 (basic GW PCA)",
       sp.layout = map.layout)
X11(width = 10,height = 12)
spplot(Dub.voter, "var.gwpca.robust", key.space = "right",
       col.regions = mypalette.4.1, cuts = 6,
       main = "PTV for local components 1 to 3 (robust GW PCA)",
       sp.layout = map.layout)

# Map GWPCA 

loadings.pc1.basic <- gwpca.basic$loadings[,,1]
win.item.basic = max.col(abs(loadings.pc1.basic))
loadings.pc1.robust <- gwpca.robust$loadings[,,1]
win.item.robust = max.col(abs(loadings.pc1.robust))
Dub.voter$win.item.basic <- win.item.basic
Dub.voter$win.item.robust <- win.item.robust
mypalette.5 <- c("lightpink", "blue", "grey", "purple",
                 "orange", "green", "brown", "yellow")
X11(width = 10,height = 12)
spplot(Dub.voter, "win.item.basic", key.space = "right",
       col.regions = mypalette.5, at = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
       main = "Winning variable: highest abs. loading on local Comp.1 (basic)",
       colorkey = F, sp.layout = map.layout)

X11(width = 10,height = 12)
spplot(Dub.voter, "win.item.robust", key.space = "right",
       col.regions = mypalette.5, at = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
       main = "Winning variable: highest abs. loading on local Comp.1 (robust)",
       colorkey = F, sp.layout = map.layout)

# Spatial pattern

x <- Dub.voter$X
y <- Dub.voter$Y

Spatial <- lm(GenEl2004 ~ x+y , data = Dub.voter) 
summary(Spatial)
AIC(Spatial)

# GWR - 8 Predictor

Gwr_8 <- bw.gwr(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl + LowEduc +
                kernel = "bisquare", adaptive = TRUE)
Gwr_res_8 <- gwr.basic(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl + LowEduc 
                     + Age18_24 + Age25_44 + Age45_64, data = Dub.voter,
                     bw = Gwr_8, kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)
# GWR - 4 Predictor

Gwr_4 <- bw.gwr(GenEl2004 ~ LARent + Unempl + Age18_24 + Age25_44
                , data = Dub.voter, approach = "AICc",
                kernel = "bisquare", adaptive = TRUE)

Gwr_res_4 <- gwr.basic(GenEl2004 ~ LARent + Unempl + Age18_24 + Age25_44, data = Dub.voter,
                     bw = Gwr_4, kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)

# Map

map.na = list("SpatialPolygonsRescale", layout.north.arrow(),
                  offset = c(329000, 261500), scale = 4000, col = 1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(),
                       offset = c(326500, 217000), scale = 5000, col = 1,
                       fill = c("transparent", "blue"))
map.scale.2 = list("sp.text", c(326500, 217900), "0", cex = 0.9, col = 1)
map.scale.3 = list("sp.text", c(331500, 217900), "5km", cex = 0.9, col = 1)
map.layout <- list(map.na, map.scale.1, map.scale.2, map.scale.3)
mypalette.1 <- brewer.pal(8, "Reds")
mypalette.2 <- brewer.pal(5, "Blues")
mypalette.3 <- brewer.pal(6, "Greens")

names(Gwr_res_8$SDF)
mypalette.6 <- brewer.pal(6, "Spectral")
X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "LowEduc", key.space = "right",
           col.regions = mypalette.6, at = c(-8, -6, -4, -2, 0, 2, 4),
           main = "Basic GW regression coefficient estimates for LowEduc",
           sp.layout=map.layout)
# try

spplot(Gwr_res_8$SDF, "Age18_24", key.space = "right",
       col.regions = mypalette.6, at = c(-8, -6, -4, -2, 0, 2, 4),
       main = "GW coefficient estimates for Age18_24",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "Age18_24", key.space = "right",
       col.regions = brewer.pal(2, "Spectral"), at = c( -2, 0, 2),
       main = " GW coefficient estimates for Age18_24",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_4$SDF, "Age25_44", key.space = "right",
       col.regions = brewer.pal(2, "Spectral"), at = c( -2, 0, 2),
       main = "GW coefficient estimates for Age25_44",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_4$SDF, "LARent", key.space = "right",
       col.regions = brewer.pal(2, "Spectral"), at = c(-2, 0, 2),
       main = "GW coefficient estimates for LARent",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_4$SDF, "Unempl", key.space = "right",
       col.regions = brewer.pal(4, "Spectral"), at = c( -4, -2, 0, 2, 4),
       main = "GW coefficient estimates for Unempl",
       sp.layout=map.layout)




X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "Age18_24", key.space = "right",
       col.regions = brewer.pal(6, "Spectral"), at = c( -8, -6, -4, -2, 0, 2, 4),
       main = " GW coefficient estimates for Age18_24",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "Age25_44", key.space = "right",
       col.regions = brewer.pal(6, "Spectral"), at = c(  -8, -6, -4, -2, 0, 2, 4),
       main = "GW coefficient estimates for Age25_44",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "LARent", key.space = "right",
       col.regions = brewer.pal(2, "Spectral"), at = c(-2, 0, 2),
       main = "GW coefficient estimates for LARent",
       sp.layout=map.layout)

X11(width=10,height=12)
spplot(Gwr_res_8$SDF, "Unempl", key.space = "right",
       col.regions = brewer.pal(4, "Spectral"), at = c( -4, -2, 0, 2, 4),
       main = "GW coefficient estimates for Unempl",
       sp.layout=map.layout)

Gwr_res_4$SDF

# try


rGwr_res_8 <- gwr.robust(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl +
                         LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = Gwr_8,
                         kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)
print(rGwr_res_8)
X11(width = 10, height = 12)
spplot(rGwr_res_8$SDF, "LowEduc", key.space = "right",
          col.regions = mypalette.6, at = c(-8, -6, -4, -2, 0, 2, 4),
          main = "Robust GW regression coefficient estimates for LowEduc",
          sp.layout=map.layout)

# LCR

lcr8_bw <- bw.gwr.lcr(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl +
                         LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter,
                       kernel = "bisquare", adaptive=TRUE, lambda.adjust = TRUE, cn.thresh = 30)

lcr8_gw <- gwr.lcr(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl + LowEduc +
                     Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = lcr8_bw,
                     kernel = "bisquare", adaptive=TRUE, lambda.adjust = TRUE, cn.thresh = 30)


lcr4_bw <- bw.gwr.lcr(GenEl2004 ~ LARent + Unempl + Age18_24 + Age25_44 , data = Dub.voter,
                , kernel = "bisquare", adaptive = TRUE, lambda.adjust = TRUE, cn.thresh = 30)

lcr4_gw <- gwr.lcr(GenEl2004 ~ LARent + Unempl + Age18_24 + Age25_44 ,
                   data = Dub.voter, bw = lcr4_bw,kernel = "bisquare", adaptive = TRUE, 
                   lambda.adjust = TRUE, cn.thresh = 30)

max(lcr8_gw$SDF$Local_CN)
max(lcr4_gw$SDF$Local_CN)







# Misc


mypalette.7 <- brewer.pal(8, "Reds")
X11(width = 10, height = 12)
spplot(lcrm2$SDF, "Local_CN", key.space = "right",
          col.regions = mypalette.7, cuts=7,
          main="Local condition numbers from basic GW regression",
          sp.layout=map.layout)

lcrm3.bw <- bw.gwr.lcr(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl +
                            LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter,
                          kernel = "bisquare", adaptive = TRUE, lambda.adjust = TRUE, cn.thresh = 30)
lcrm3.bw

lcrm3 <- gwr.lcr(GenEl2004 ~ DiffAdd + LARent + SC1+ Unempl + LowEduc +
                  Age18_24 + Age25_44 + Age45_64, data=Dub.voter, bw = lcrm3.bw,
                   kernel = "bisquare", adaptive = TRUE, lambda.adjust = TRUE, cn.thresh = 30)
summary(lcrm3$SDF$Local_CN)

X11(width = 10, height = 12)
spplot(lcrm3$SDF, "Local_CN", key.space = "right",
        col.regions = mypalette.7, cuts = 7,
        main = "Local condition numbers before adjustment",
        sp.layout=map.layout)

X11(width = 10, height = 12)
spplot(lcrm3$SDF, "Local_Lambda", key.space = "right",
         col.regions = mypalette.7,cuts = 7,
           main = "Local ridge terms for LCR GW regression",
          sp.layout=map.layout)

gwr.cv <- gwr.basic(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl +
                        LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = gwr.cv.bw,
                        kernel = "bisquare", adaptive = TRUE)
small <- min(min(gwr.cv$SDF$LARent), min(lcrm3$SDF$LARent))
large <- max(max(gwr.cv$SDF$LARent), max(lcrm3$SDF$LARent))
X11(w=10,h=10)
plot(gwr.cv$SDF$LARent,lcrm3$SDF$LARent,
       main = " LARent coefficients: basic vs. locally compensated",
      xlab = "GW regression coefficient", ylab = "LCR GW regression coefficient",
      xlim = c(small, large), ylim = c(small, large))
lines(lowess(gwr.cv$SDF$LARent, lcrm3$SDF$LARent), col = "blue")
abline(0, 1, col = "gray60")


# Test

test.CN <- function(model, data) {
  lcrmx.bw <- bw.gwr.lcr(model, data = data, kernel = "bisquare",
                         adaptive = TRUE)
  print(model)
  print(lcrmx.bw)
  lcrmx <- gwr.lcr(model, data = data, bw = lcrmx.bw,
                   kernel = "bisquare", adaptive = TRUE)
  print(summary(lcrmx$SDF$Local_CN))
  lcrmx$SDF$Local_CN
}
data <- Dub.voter
model <- as.formula(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl +
                      LowEduc + Age18_24 + Age25_44 + Age45_64)
AllD <- test.CN(model,data)


# Quick Map

quick.map <- function(spdf, var, legend.title, main.title) {
  x <- spdf@data[,var]
  cut.vals <- pretty(x)
  x.cut <- cut(x, cut.vals)
  cut.levels <- levels(x.cut)
  cut.band <- match(x.cut, cut.levels)
  colors <- rev(brewer.pal(length(cut.levels), 'YlOrRd'))
  par(mar = c(1, 1, 1, 1))
  plot(ewoutline, col = 'olivedrab', bg = 'lightblue1')
  title(main.title)
  plot(spdf, add = TRUE, col = colors[cut.band], pch = 16)
  legend('topleft', cut.levels, col = colors, pch = 16, bty = 'n',
         title = legend.title)
}




quickmap <- function(SDF, Var, Title, nclass=8,Method="quantile") {
  Pallete <- rev(brewer.pal(nclass,"YlOrRd"))
  Classes <- classIntervals(Var,nclass,Method,dataPrecision=2)
  Colours <- findColours(Classes, Pallete)
  plot(SDF, col = Colours, main = Title,cex.main=0.9)
  legend('topright', 
         legend=names(attr(Colours,'table')),
         bty="n",cex=0.8
         )
}

quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$Unempl, "Unemployment")
quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$Age18_24, "Age18_24")
quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$Age25_44, "Age25_44")
quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$LARent, "LARent")


quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$LARent, "Unemployment")
quickmap(Gwr_res_4$SDF,Gwr_res_4$SDF$LARent, "Unemployment")

# quick.map(Gwr_res_4$SDF,"ABC","DEF","GHI")

# quick.map(gwss.object,variable.name,legend.title,main.map.title)
# 


