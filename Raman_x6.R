par(mfrow = c(6,1), mar = c(0,6,0,1), oma = c(4,0,2,0), mgp = c(4, 0.4, 0),
    tcl = 0.2, lend = 0, ljoin = 0, font.lab = 2, xpd=TRUE, cex.lab = 1.4, las=1,
    cex.axis = 1.4)
palette(c("black",colorRampPalette(c("#93721A", "#A51890", "#5560A4"))(6)))
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(K_PP, plot(V2 ~ V1, type = "l", col = 7,
                xaxt = "n", cex.axis=1.4,
                ylab = "", xlim = c(0,3150)))
axis(1, labels=F);axis(3)
mtext("Polypropylene (PP)", side = 3, line = -1.5, adj = 0.7, col = 7)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(SC_PE, plot(V2 ~ V1, type = "l", col = 6,
                 xaxt = "n", xlab = "", cex.axis=1.4,
                 ylab = "", xlim = c(0,3150)))
mtext("Polyethylene (PE)", side = 3, line = -1.5, adj = 0.7, col = 6)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(H_PTFE, plot(V2 ~ V1, type = "l", col = 5,
                  xaxt = "n", yaxt = "n", xlab="",
                  ylab = "", xlim = c(0,3150), ylim = c(0,5500)))
rect(-20,5000,100,par("usr")[4],lwd=0.01, border="white",col="white");box()
with(H_PTFE, lines(V2~V1, col = 3, lty=3))
axis(1, labels=F)
axis(2, at=c(0,2000,4000), labels=c(0,2000,4000), cex.axis=1.4)
mtext("Polytetrafluoroethylene (PTFE)", side = 3,line = -1.5,adj = 0.7,col = 5)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(SB_PVDF, plot(V2 ~ V1, type = "l", col = 4,
                   xaxt = "n", cex.axis=1.4,
                   ylab = "", xlim = c(0,3150)))
axis(1, labels=F)
mtext("Polyvinylidene fluoride (PVDF)", side = 3, line = -1.5, adj = 0.7,col=4)
mtext("Intensity", 2, 3, cex = 1.2, adj=1, font=2, las=0)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(H_Cotton, plot(V2 ~ V1, type = "l", col = 3,
                    xaxt = "n",yaxt = "n",
                    ylab = "", xlim = c(0,3150), ylim = c(0,5500)))
rect(-20, 5000, 100, par("usr")[4], lwd=0.01, border="white", col="white");box()
with(H_Cotton, lines(V2~V1, col = 2, lty=3))
axis(1, labels=F)
axis(2, at=c(0,2000,4000), labels=c(0,2000,4000), cex.axis=1.4)
mtext("Cotton", side = 3, line = -1.5, adj = 0.7, col = 3)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
with(K_Wool, plot(V2 ~ V1, type = "l", col = 2,
                  yaxt="n",
                  ylab = "", xlim = c(0,3150)))
axis(1, labels=F)
axis(2, at=c(0,200,400), labels=c(0,200,400), cex.axis=1.4)
mtext("Wool", side = 3, line = -1.5, adj = 0.7, col = 2)
mtext(expression(bold(paste("Relative wavenumbers (",cm^"-1",")"))), 1, 2.2)
#  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
