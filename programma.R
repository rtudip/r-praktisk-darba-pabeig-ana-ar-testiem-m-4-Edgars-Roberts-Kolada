kordat <- read.csv("variants16.txt", header = TRUE, dec= ",", sep = "\t", stringsAsFactors = FALSE, strip.white = TRUE, row.names=1)

kordat <- kordat[!apply(kordat,1,anyNA), ]
kordat[9:ncol(kordat)] <- lapply(kordat[9:ncol(kordat)], as.factor)


sink("results.txt")
print(summary(kordat[9:ncol(kordat)], maxsum = 15))


sl.by.b <- split(kordat$Slope, kordat$b)
print(sl.by.b)

kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)

std.by.f <- tapply(kordat$Average, kordat$f, sd, na.rm = TRUE)
print(std.by.f)
cat("\n")

prockordat <- kordat[abs(kordat$adj.r.squared) > 0.7, ]

prockordat$Slope <- (1 - (1 / prockordat$Slope))

print(prockordat)
cat("\n")


visi.lim <- unlist(kordat[9:13])
visi.lim <- visi.lim[!grepl("^.0", unlist(kordat[9:13]))]
biezkais.lim <- names(which.max(table(visi.lim)))


print(prockordat[grepl(biezkais.lim, rownames(prockordat)), ])
cat("\n")

sink()



svg("scatter.svg", width=8, height=8)
plot(kordat$MAD, kordat$Average,
     main = "Izkliedes grafiks",
     xlab = "MAD",
     ylab = "Average")
dev.off()


svg("boxplot.svg", width=8, height=8)
boxplot(Intercept ~ f, data = kordat,
        main = "Kastu grafiks",
        xlab = "f faktors",
        ylab = "Intercept")
dev.off()
