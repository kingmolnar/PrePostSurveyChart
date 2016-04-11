library(plyr)
#data <- read.csv("data/impressions_raw.csv")
#data[data == "NULL"] <- NA
#colnames(data) <- c("Timestamp", "OfficerId", "Value", "UseOfForce","Privacy", "Complaints", "Assault", "Transparancy", "Public", "PrePost")                                                                                   
#tab <- merge(x = data[data$PrePost == "Pre",], y = data[data$PrePost == "Post Axon",], by = "OfficerId")

data <- read.csv("data/DataEntryImpressionSurveyResponses.csv")
data[data == "NULL"] <- NA
colnames(data) <- c("Timestamp", "OfficerId", "Value", "UseOfForce","Privacy", "Complaints", "Assault", "Transparancy", "Public", "PrePost", "Bubble", "Mid")                                                                                   
tab <- merge(x = data[data$Bubble == "Pre",], y = data[data$Bubble == "Post",], by = "OfficerId")
write.csv(tab, file="temp_tab.csv")
dummy = c(-1, -1, -1, -1)
rscale <- 0.3
lim <- c(-1.0, 5.5)

for (f in c("Value", "UseOfForce","Privacy", "Complaints", "Assault", "Transparancy", "Public")) {
  before <- sprintf("%s.x", c(f))
  after <- sprintf("%s.y", c(f))
  
  ## new: Before on y-axis, After x-axis
  freq.before <- na.omit(count(tab, before))
  colnames(freq.before) <- c("y", "f")
  freq.before$x = 0.0
  freq.before$radius = rscale*sqrt(freq.before$f)/pi
  
  freq.after <- na.omit(count(tab, after))
  colnames(freq.after) <- c("x", "f")
  freq.after$y = 0.0
  freq.after$radius = rscale*sqrt(freq.after$f)/pi
  
  freq <- na.omit(count(tab, c(before, after)))
  colnames(freq) <- c("y", "x", "f")
  freq$radius = rscale*sqrt(freq$f)/pi
  
  plot.name <- sprintf("figs/%s.pdf", c(f))
  print(plot.name)
  pdf(file=plot.name, pointsize=16)
  par(pin=c(7,7))
  plot(lim, lim, type="n", xlim=lim, ylim=lim, axes = FALSE, xlab = NA, ylab = NA, frame.plot = FALSE, mar=c(0,0,0,0), oma=c(0,0,0,0))
  abline(0,1.0, lty=2, lwd=3,  xlim=c(0.5, 4.5), ylim=c(0.5, 4.5), col="gray")
  abline(h=0, col="gray")
  abline(v=0, col="gray")

  symbols(freq.before$x, freq.before$y, circles=freq.before$radius, add=TRUE, inches=FALSE, bg="white", fg="black", xlim=lim, ylim=lim, xlab = NA, ylab = NA)
  symbols(freq.after$x, freq.after$y, circles=freq.after$radius, add=TRUE, inches=FALSE, bg="white", fg="black", xlim=lim, ylim=lim, xlab = NA, ylab = NA)
  symbols(freq$x, freq$y, circles=freq$radius, add=TRUE, inches=FALSE, lwd=4, bg="black", fg="white", xlim=lim, ylim=lim, xlab = NA, ylab = NA)
  for (k in 1:4) {
    text(5.0, k, k, cex=1.5)
    text(k, 5.0, k, cex=1.5)
  }
  for ( k in 1:dim(freq.before)[1] ) {
    if (freq.before[k, "f"] > 3) {
      text(freq.before[k, "x"], freq.before[k, "y"], freq.before[k, "f"])
    }
  }
  for (k in 1:dim(freq.after)[1]) {
    if (freq.after[k, "f"] > 3) {
      text(freq.after[k, "x"], freq.after[k, "y"],  freq.after[k, "f"])
    }
  }
  for (k in 1:dim(freq)[1]) {
    if (freq[k, "f"] > 3) {
      text(freq[k, "x"], freq[k, "y"], freq[k, "f"], col="white", cex=1)
    }
  }
  text(1,-1, "Final", cex=1.5)
  text(-1,1, "Initial",  cex=1.5, srt=90)
  dev.off()
}
