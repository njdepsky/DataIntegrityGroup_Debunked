# Remove all Stored Variables
rm(list=ls())

# Load Relevant Packages
library(pacman)
p_load(dplyr, magrittr, scales, ggplot2, sf, data.table, reshape2)

dat.dir = '~/Documents/GitHub-Repos/DataIntegrityGroup_Debunked/data/'
out.dir = '~/Documents/GitHub-Repos/DataIntegrityGroup_Debunked/plots/'

theme_plot <- theme(
  legend.position = "right",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.title = element_blank())

# Load PA and GA County-Level Data
setwd(dat.dir)
pa = read.csv("PA_County.csv", check.names = F, stringsAsFactors = F)
ga = read.csv("Full_GA_County.csv", stringsAsFactors = F, check.names = F)

# Format County Timestamp values
pa$time_cty = pa$County_last_updated
pa$time_cty = gsub("T", " ", pa$time_cty)
pa$time_cty = gsub("Z", "", pa$time_cty) %>% as.POSIXct()
pa %<>% arrange(County, time_cty)

ga$time_cty = ga$County_last_updated
ga$time_cty = gsub("T", " ", ga$time_cty)
ga$time_cty = gsub("Z", "", ga$time_cty) %>% as.POSIXct()
ga %<>% arrange(County, time_cty)

# Create Delta columns for GA
ga$Trump_A_Delta = NA
ga$Trump_O_Delta = NA
ga$Biden_A_Delta = NA
ga$Biden_O_Delta = NA

for(cty in unique(ga$County)){
  idx = which(ga$County == cty)
  ga$Trump_A_Delta[idx] = ga$Trump_Absentee[idx] - shift(ga$Trump_Absentee[idx])
  ga$Trump_O_Delta[idx] = ga$Trump_Other_Votes[idx] - shift(ga$Trump_Other_Votes[idx])
  ga$Biden_A_Delta[idx] = ga$Biden_Absentee[idx] - shift(ga$Biden_Absentee[idx])
  ga$Biden_O_Delta[idx] = ga$Biden_Other_Votes[idx] - shift(ga$Biden_Other_Votes[idx])
}

# Define Vote Plotting Function for Given County
vplot = function(dat, # data frame of vote values
                 county, # selected county name
                 max_date = "11-16", # last date to plot
                 lwd = 4, # width of lines in plots
                 opa = 0.8 # opacity of lines
                 ){
  
  # Filter by county and time period
  filt = dat %>% 
    filter(County==county & time_cty <= as.POSIXct(paste0("2020-",max_date," 24:00:00"))) %>%
    arrange(time_cty)
  
  # Define vote count columns to plot
  vcols = c('Trump_Absentee','Biden_Absentee','Trump_Other_Votes','Biden_Other_Votes')
  lcols = c(rep('coral',2), rep('dodgerblue',2)) # line colors
  ltys = rep(c('dotted','solid'),2) # line types
  
  ## Plots 
  # set plot space
  par(mfrow=c(2,1)) 
  par(mar=c(2,3,1,0.8)) # plot margins
  par(oma=c(0, 0, 1.5, 5))

  # Specify time axis tick labels
  xts = seq(1, nrow(filt), length.out = 10)
  xlabs = seq.POSIXt(filt$time_cty[1],
             filt$time_cty[nrow(filt)],
             length.out = 10)
  xlabs = gsub("2020-","",xlabs)
  xlabs = substr(xlabs, 1, gregexpr(":",xlabs)[[1]][2]-1)

  # Absentee Votes Plot
  plot(1:nrow(filt), rep(NA,nrow(filt)), ylim = c(0,max(filt[,vcols], na.rm = T)), xlab=NA, xaxt='n', ylab=NA) # empty plot for absentee
  axis(1, at=xts, labels = F)
  axis(1, at = xts, tck=1, lty=2, lwd = 0.5, col='grey', labels = F)# vert grids
  axis(2, tck=1, lty=2, col='grey', lwd = 0.5) # horiz grids
  title('Absentee (Mail-In) Votes', adj=0)
  legend(par('usr')[2], par('usr')[4], legend = c('trump','biden'), col = c('coral','dodgerblue'), cex=0.8, box.lty=0, lty=c(1,1), lwd = c(lwd,lwd), xpd = NA, bty='n')
  
  lines(1:nrow(filt), filt$Trump_Absentee, type = 'l', lwd = lwd, col = alpha('coral',opa))
  lines(1:nrow(filt), filt$Biden_Absentee, type = 'l', lwd = lwd, col = alpha('dodgerblue',opa))
  box()
  
  # In Person/Other Votes Plot
  plot(1:nrow(filt), rep(NA,nrow(filt)), ylim = c(0,max(filt[,vcols], na.rm = T)), ylab=NA, xlab = NA, xaxt='n') # empty plot for in-person
  axis(1, at=xts, labels = xlabs, cex.axis=0.7) # vert grids
  axis(1, at = xts, tck=1, lty=2, lwd = 0.5, col='grey', labels = F) # vert grids
  axis(2, tck=1, lty=2, col='grey', lwd = 0.5) # horiz grids
  title('In Person Votes', adj=0)
  legend(par('usr')[2], par('usr')[4], legend = c('trump','biden'), col = c('coral','dodgerblue'), cex=0.8, box.lty=0, lty=c(1,1), lwd = c(lwd,lwd), xpd = NA, bty='n')
  
  lines(1:nrow(filt), filt$Trump_Other_Votes, type = 'l', lwd = lwd, col = alpha('coral',opa))
  lines(1:nrow(filt), filt$Biden_Other_Votes, type = 'l', lwd = lwd, col = alpha('dodgerblue',opa))
  box()
}

### [PA] Plot all counties for full time range, save as single PDF
setwd(out.dir)
pdf("PA_CountyVoteTotals_Nov3_Nov16.pdf", width=11,height=8, paper='a4r')
for(c in unique(pa$County)){
  vplot(pa,c)
  mtext(paste(c,'County'), outer=TRUE,  cex=1.5, line=0.2, font=2)
}
dev.off()

### [GA] Plot all counties for full time range, save as single PDF
setwd(out.dir)
pdf("GA_CountyVoteTotals_Nov3_Nov16.pdf", width=11,height=8, paper='a4r')
for(c in unique(ga$County)){
  vplot(ga,c)
  mtext(paste(c,'County'), outer=TRUE,  cex=1.5, line=0.2, font=2)
}
dev.off()

# [PA] Sums of "removed" Votes
removed.pa = data.frame(Trump_A=rep(NA,2), 
                    Trump_O=rep(NA,2),
                    Trump_tot=rep(NA,2),
                    Biden_A=rep(NA,2), 
                    Biden_O=rep(NA,2),
                    Biden_tot=rep(NA,2),
                    row.names = c("votes","number_decr"))

for(c in names(removed.pa)[c(1:2,4:5)]){
  cname = paste0(c,"_Delta")
  removed.pa[1,c] = sum(pa[pa[,cname] < 0,cname], na.rm = T)
  removed.pa[2,c] = sum(pa[,cname] < 0, na.rm = T)
}
removed.pa$Trump_tot = removed.pa$Trump_A+removed.pa$Trump_O
removed.pa$Biden_tot = removed.pa$Biden_A+removed.pa$Biden_O

# [GA] Sums of "removed" Votes
removed.ga = data.frame(Trump_A=rep(NA,2), 
                       Trump_O=rep(NA,2),
                       Trump_tot=rep(NA,2),
                       Biden_A=rep(NA,2), 
                       Biden_O=rep(NA,2),
                       Biden_tot=rep(NA,2),
                       row.names = c("votes","number_decr"))

for(c in names(removed.ga)[c(1:2,4:5)]){
  cname = paste0(c,"_Delta")
  removed.ga[1,c] = sum(ga[ga[,cname] < 0,cname], na.rm = T)
  removed.ga[2,c] = sum(ga[,cname] < 0, na.rm = T)
}
removed.ga$Trump_tot = removed.ga$Trump_A+removed.ga$Trump_O
removed.ga$Biden_tot = removed.ga$Biden_A+removed.ga$Biden_O

#### Precinct Level Analysis ####
setwd(dat.dir)

# Import PA Precincts (from Data Integrity Group)
paprec = read.csv("PA_precincts_byType.csv", check.names = F, stringsAsFactors = F)
print(length(unique(paprec$geo_id)))

paprec = read.csv("PA_precincts_total.csv", check.names = F, stringsAsFactors = F)
print(length(unique(paprec$geo_id)))

# PA data looks incomplete (only 3100-4200  precincts out of 9100 in the two tables)

# Import GA (from Georgia Sec of State website)
gaprec = st_read("GA_GenElecRes2020_precinct_wACS.geojson") %>% st_drop_geometry()
print(sum(gaprec$trump_all_pct > 75, na.rm = T))
print(sum(gaprec$biden_all_pct > 75, na.rm = T))
