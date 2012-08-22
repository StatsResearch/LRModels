TTG_DIR<-'/Users/rob/PhDStuff/ThesisSoftware/TrgTestGenerator/output'

# === 10_5 ===
window.size<-5

trg.filename<-paste(sep='', TTG_DIR,'/TTG_Training_10_',window.size,'_seq_2.csv')
all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

attach(all.trg)
model1<-glm(case_label~HRT_mean_10_5+BPs_mean_10_5+BPd_mean_10_5+BPm_mean_10_5 + BPp_mean_10_5
                        + HRT_sd_10_5+BPs_sd_10_5+BPd_sd_10_5+BPm_sd_10_5 + BPp_sd_10_5
                        + HRT_slope_10_5+BPs_slope_10_5+BPd_slope_10_5+BPm_slope_10_5 + BPp_slope_10_5
            ,family='binomial')

detach(all.trg)

test.filename<-paste(sep='', TTG_DIR,'/TTG_Test_10_',window.size,'_seq_2.csv')
all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


> all.HRT<-data.frame(all.trg[,6],all.trg[,11],all.trg[,16])
> colnames(all.HRT)<-c('Mean','SD','Slope'))
Error: unexpected ')' in "colnames(all.HRT)<-c('Mean','SD','Slope'))"
> colnames(all.HRT)<-c('Mean','SD','Slope')
> pairs(all.HRT)
> all.HRT