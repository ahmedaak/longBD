rm(list=ls())

setwd("C:/Users/khalila/Desktop/longBD/voxelwise_vals")

subjects <- c("sub0056", "sub0147", "sub0156", "sub0179", "sub0186", "sub0189","sub0028", "sub0030", "sub0036", "sub0058", "sub0127", "sub0153", "sub0177", "sub0203", "sub0206")
days <- c("D0", "D1")
refs <- c("VS", "HH")
rois <- c("BL", "D1IG", "VT", "CL")

BD_vals_allsubs <- c()
for (a in subjects){
  BD_vals_alldays <- c()
  for (b in days){
    BD_vals_allrefs <- c()
    for (c in refs){
      BD_vals_allrois <- c()
      for (d in rois){
        # load in BD values
        BD_vals <- read.csv(paste(a, "/TSA_" , paste(c, d, b, sep = "_"), ".txt" , sep = ""), header = F, sep = "")$V4
        # create label of ROIs
        roi_labels <- c(rep(x=d, times = length(BD_vals)))
        # append label of ROIs to BD values
        BD_vals_l <- cbind(BD_vals, roi_labels)
        # add column names
        BD_vals_allrois <- rbind(BD_vals_allrois, BD_vals_l)
      }
      # create label of refs
      ref_labels <- c(rep(x=c, times = nrow(BD_vals_allrois)))
      # append label of refs to BD values
      BD_refs_l <- cbind(BD_vals_allrois, ref_labels)
      # add column names
      BD_vals_allrefs <- rbind(BD_vals_allrefs, BD_refs_l)
    }
    # create label of days
    day_labels <- c(rep(x=b, times = nrow(BD_vals_allrefs)))
    # append label of days to BD values
    BD_days_l <- cbind(BD_vals_allrefs, day_labels)
    # add column names
    BD_vals_alldays <- rbind(BD_vals_alldays, BD_days_l)
  }
  # create label of subs
  subs_labels <- c(rep(x=a, times = nrow(BD_vals_alldays)))
  # append label of subs to BD values
  BD_subs_l <- cbind(BD_vals_alldays, subs_labels)
  # is this patient a recanalizer?
  if (is.element(a, c("sub0056", "sub0147", "sub0156", "sub0179", "sub0186", "sub0189"))==TRUE){
    # create label of recanalization
    recan_labels <- c(rep(x="Yes", times = nrow(BD_vals_alldays)))
  } else {
    # create label of recanalization
    recan_labels <- c(rep(x="No", times = nrow(BD_vals_alldays)))
  }
  # append label of recan to BD values
  BD_subs_l2 <- cbind(BD_subs_l, recan_labels)
  # add column names
  colnames(BD_subs_l2) <- c("BDvals","ROIs", "REFs", "Days", "Subject", "Recan")
  BD_vals_allsubs <- rbind(BD_vals_allsubs, BD_subs_l2)
}

BD_vals_allsubs <- as.data.frame(BD_vals_allsubs)
BD_vals_allsubs$BDvals <- as.numeric(as.character(BD_vals_allsubs$BDvals))

# fit model 
longBD_mm <- lmer(BDvals ~ Recan*Days*ROIs*REFs + (1|Subject), data = BD_vals_allsubs)

# anova of model 
anova(longBD_mm)

# summary of model
summary(longBD_mm)

# build effects object
longBD_eff <- allEffects(longBD_mm)

# plot all effects
plot(longBD_eff, multiline=TRUE, ci.style="bars")
