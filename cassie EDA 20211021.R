# cassie <- read.table("clipboard", header = T, sep="\t", stringsAsFactors = T)
# write.csv(cassie, file="cassie.csv", row.names = F)
# cassie <- read.csv(file = "cassie.csv", stringsAsFactors = T)
# cassie$Repeat.Number <- as.factor(cassie$Repeat.Number)
# cassie$Filter_Size <- as.factor(cassie$Filter_Size)
# cassie$Sample.Code <- as.character(cassie$Sample.Code)
# cassie$Drain <- as.character(cassie$Drain)
# cassie$Site <- as.character(cassie$Site)
# cassie$Drain <- gsub("H","O", cassie$Drain)
# cassie$Site <- gsub("H","O", cassie$Site)
# cassie$Drain <- as.factor(cassie$Drain)
# cassie$Site <- as.factor(cassie$Site)
# str(cassie)

library(DataExplorer)
plot_histogram(cassie)
with(cassie, 
     stripchart(counts_corr_perL ~ Site, 
                method = "jitter", 
                col=rainbow(12, v = 0.85, end=0.8), pch=c(15,17,18,19), 
                las=1, ylim=c(12.656,0.344)))
abline(h=seq(0.5,12.5,1), col="grey",lty=3)

par(mar = c(0,0,0,0), oma = c(4,12,1,1), mgp = c(2,0.3,0), tcl = 0.3, font.lab = 2,
    lend="square", ljoin = "mitre", xpd=T)
palette(c("black",colorRampPalette(c("#FFE6A3", "#B1609F", "#5560A4"))(6)))
layout(matrix(c(1,2,2), ncol=1))
with(cassie, boxplot(counts_corr_perL ~ Drain, xlim=c(6.656,0.344),
                     horizontal = T, las = 1, xaxt="n", cex = 1.6, 
                     col=seq(2,7), cex.lab = 1.5, cex.axis = 1.2,
                     xlab="", yaxt="n"))
axis(2, las = 2, cex.axis = 1.6, at = seq(1,6,1),
     labels = c("Bayswater","Claisebrook","Kitchener",
                "Osborne Pk.","Sth Belmont", "Sth Coolup"))
mtext("Catchment",2,9,font=2,cex=1.2)
text(0.7,0.65,labels="(a)", font = 2, cex = 2)
axis(1, labels=NA, tcl = 0.3)
axis(1, labels=NA, tcl = -0.3)
with(cassie, boxplot(counts_corr_perL ~ Site, xlim=c(12.656,0.344),
                     horizontal = T, las = 1, cex = 1.6, 
                     col=rep(seq(2,7), each=2), cex.lab = 1.5, cex.axis = 1.6,
                     xlab=""))
mtext("Microplastic counts (particles/L)",1,2,font=2, cex = 1.2)
mtext("Sampling Site",2,9,font=2,cex=1.2)
text(0.7,0.4,labels="(b)", font = 2, cex = 2)

layout(matrix(c(1), ncol=1))

# write.csv(colnames(cassie), file="cassie_cols.csv", row.names = F)
# colnames(cassie) <- c("Sample", "Drain", "Site", "Rep", "Vol",
# "Fragment.Count", "Fragment.L", "Film.Count", "Film.L", "Bead.Count",
# "Bead.L", "Fibre.Count", "Blank.deduction", "Fibre.Blank.Correction",
# "Fibre.L", "Total.MP.Blank.Correct", "counts_corr", "counts_corr_perL",
# "count_corr_across_sizes", "count_corr_across_sizes.L", "catchment_area",
# "catchment_pop", "residential", "industrial", "services", "agricultural",
# "natural", "public_open", "rainfall")
