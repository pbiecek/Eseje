λ (nm)  Photopic	Scotopic	λ (nm)	Photopic	Scotopic	λ (nm)	Photopic	Scotopic	λ (nm)	Photopic	Scotopic

datt <- scan()
380	0.000039	0.000589	480	0.139020	0.793000	580	0.870000	0.121200	680	0.017000	0.000072
390	0.000120	0.002209	490	0.208020	0.904000	590	0.757000	0.065500	690	0.008210	0.000035
400	0.000396	0.009290	500	0.323000	0.982000	600	0.631000	0.033150	700	0.004102	0.000018
410	0.001210	0.034840	510	0.503000	0.997000	610	0.503000	0.015930	710	0.002091	0.000009
420	0.004000	0.096600	520	0.710000	0.935000	620	0.381000	0.007370	720	0.001047	0.000005
430	0.011600	0.199800	530	0.862000	0.811000	630	0.265000	0.003335	730	0.000520	0.000003
440	0.023000	0.328100	540	0.954000	0.655000	640	0.175000	0.001497	740	0.000249	0.000001
450	0.038000	0.455000	550	0.994950	0.481000	650	0.107000	0.000677	750	0.000120	0.000001
460	0.060000	0.567000	560	0.995000	0.328800	660	0.061000	0.000313	760	0.000060	0.000000
470	0.090980	0.676000	570	0.952000	0.207600	670	0.032000	0.000148	770	0.000030	0.000000

relative <- cbind(datt[seq(1, length(datt), 3)],
      datt[seq(2, length(datt), 3)],
      datt[seq(3, length(datt), 3)])

pdf("/Users/pbiecek/camtasia/GitHub/Eseje/Percepcja/skotopoweFotopowe.pdf",12,8)

relative <- relative[order(relative[,1]),]
par(xaxs="i", yaxs="i")
matplot(relative[,1], relative[,2:3], type="l", 
        lty=1, las=1, lwd=5, bty="n", ylab="względna czułość", 
        xlab="długość fali",
        main="widzenie fotopowe i skotopowe", xaxt="n", yaxt="n", ylim=c(0,1), col=c("#00007777", "#00000077"))
matlines(relative[,1], relative[,2:3], type="h", lty=1, col=c("#0000bb77", "#00000077"), lwd=3)
axis(1, seq(360,780,20))
axis(2, seq(0,1,.1), las=1)

dev.off()









pdf("/Users/pbiecek/camtasia/GitHub/Eseje/Percepcja/axis.pdf",4,12)

plot(c(1,1), c(-6,8), yaxt="n")
axis(2, seq(-6,8), 10^seq(-6,8), las=1)

dev.off()
