subjects <- c("sub0177","sub0189","sub0056","sub0147","sub0186","sub0028","sub0030","sub0036","sub0058","sub0127","sub0153","sub0203","sub0206","sub0147")
setwd("C:/Users/khalila/Desktop/long_BD_files/newdata_08062017/cross_corr_all/")
dwi_maxcc_all <- c()
cl_maxcc_all <- c()
tsa_maxcc_all <- c()

#par(mfrow=c(5,3))
for (i in subjects) {
# load in data
dwi_maxcc <- read.table(paste(i, "DWI_maxcc.txt", sep = "_"))
dwi_maxcc <- dwi_maxcc[,4]

cl_maxcc <- read.table(paste(i, "cl_maxcc.txt", sep = "_"))
cl_maxcc <- cl_maxcc[,4]

tsa_maxcc <- read.table(paste(i, "TSA_maxcc.txt", sep = "_"))
tsa_maxcc <- tsa_maxcc[,4]

dwi_maxcc_all <- rbind(dwi_maxcc_all, dwi_maxcc)
cl_maxcc_all <- rbind(cl_maxcc_all, cl_maxcc)
tsa_maxcc_all <- rbind(tsa_maxcc_all, tsa_maxcc)

# calculate kernel densities
dwi_maxcc_d <- density(dwi_maxcc)
cl_maxcc_d <- density(cl_maxcc)
tsa_maxcc_d <- density(tsa_maxcc)


# plot kernel density plots
plot(cl_maxcc_d, ylim = c(0, max(cl_maxcc_d$y, dwi_maxcc_d$y, tsa_maxcc_d$y)), lwd = 2)
lines(dwi_maxcc_d, col = "red", lwd = 2)
lines(tsa_maxcc_d, col = "blue", lwd = 2)
legend("topright", legend = c("Contralateral","DWI lesion","BOLD delay lesion"),col=c("black","red","blue"),lty=1,lwd=2)

}

dwi_maxcc_all_d <- density(dwi_maxcc_all)
cl_maxcc_all_d <- density(cl_maxcc_all)
tsa_maxcc_all_d <- density(tsa_maxcc_all)

plot(cl_maxcc_all_d, ylim = c(0, max(cl_maxcc_all_d$y, dwi_maxcc_all_d$y, tsa_maxcc_all_d$y)), lwd = 2)
lines(dwi_maxcc_all_d, col = "red", lwd = 2)
lines(tsa_maxcc_all_d, col = "blue", lwd = 2)
legend("topright", legend = c("Contralateral","DWI lesion","BOLD delay lesion"),col=c("black","red","blue"),lty=1,lwd=2)
