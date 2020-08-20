
library(tidyverse)
library(lavaan)
library(lavaanPlot)

library(blavaan)

CFA <-read.csv("CombinedFile_CFA.csv",header=TRUE, sep = ",")

# Center and scale the factors
CFA[,-(1:6)] = scale(CFA[,-(1:6)])


fit_model = function(model) {
    fit = bcfa(model, data = CFA, orthogonal=TRUE, adapt = 2000, burnin = 2000)
    return(fit)
}


model.MF2012 <- 'F1_WM =~  WMUpdating   + WMBoxes + WMGrid
                F2_Shifting =~  CD_all + Shelf + Tray
                F3_CommonEF =~  InhibGrid + Cylinder + GlassCeiling + Shelf + CD_all + Tray + WMUpdating+ WMBoxes + WMGrid'



model.3factors <- 'F1_WM =~  WMUpdating + WMBoxes + WMGrid
                F2_Shifting =~  CD_all + Shelf + Tray
                  F3_Inhibition =~  InhibGrid + Cylinder + GlassCeiling'

model.1factor <- 'F1_CommonEF =~  InhibGrid + Cylinder + GlassCeiling + Shelf + CD_all + Tray + WMUpdating+ WMBoxes + WMGrid'

model.3factors_ind <- 'F1_WM =~  WMUpdating + WMBoxes + WMGrid
                F2_Shifting =~  CD_all + Shelf + Tray
                  F3_Inhibition =~  InhibGrid + Cylinder + GlassCeiling'

model.2factors1 <- 'F1_2_WM_Shifting =~  WMUpdating + WMBoxes + WMGrid+ CD_all + Shelf + Tray
                   F3_Inhibition =~  InhibGrid + Cylinder + GlassCeiling'

model.2factors2 <- 'F1_3_WM_Inh =~  WMUpdating + WMBoxes + WMGrid + InhibGrid + Cylinder + GlassCeiling
                    F2_Shifting =~  CD_all + Shelf + Tray'

model.2factors3 <- 'F1_WM =~  WMUpdating + WMBoxes + WMGrid
                    F2_3_Shifting_Inh=~ CD_all + Shelf + Tray + InhibGrid + Cylinder + GlassCeiling'

# Fit each of the models
bfit.MF2012 = fit_model(model.MF2012)
bfit.3factors = fit_model(model.3factors)
bfit.1factor = fit_model(model.1factor)
bfit.3factors_ind = fit_model(model.3factors_ind)
bfit.2factors1 = fit_model(model.2factors1)
bfit.2factors2 = fit_model(model.2factors2)
bfit.2factors3 = fit_model(model.2factors3)


# Compare models to the MF2012 model
blavCompare(bfit.MF2012, bfit.MF2012)
blavCompare(bfit.MF2012, bfit.3factors)
blavCompare(bfit.MF2012, bfit.1factor)
blavCompare(bfit.MF2012, bfit.3factors_ind)
blavCompare(bfit.MF2012, bfit.2factors1)
blavCompare(bfit.MF2012, bfit.2factors2)
blavCompare(bfit.MF2012, bfit.2factors3)


# Calculate WAIC values for the models.
get_waic = function(object1) {
    lavopt1 <- object1@Options
    lavopt1$estimator <- "ML"
    ll1 = case_lls(object1@external$mcmcout, object1@Model, 
        object1@ParTable, object1@SampleStats, lavopt1, object1@Cache, 
        object1@Data, make_mcmc(object1@external$mcmcout))
    return(waic(ll1))
}
environment(get_waic) <- asNamespace('blavaan')
get_waic(bfit.MF2012)
get_waic(bfit.3factors)
get_waic(bfit.1factor)
get_waic(bfit.3factors_ind)
get_waic(bfit.2factors1)
get_waic(bfit.2factors2)
get_waic(bfit.2factors3)

