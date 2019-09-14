freqFromNote <- function(n) {
    2^((n-49)/12) * 440
}

noteFromFreq <- function(f) {
    round(12 * log2(f/440) + 49)
}

d <- read.table('notes.txt', stringsAsFactors=FALSE, header=TRUE)

freqFromLength <- function(fr, l) {
    fr/l
}

bpm <- 152
dt <- 60/bpm
## t <- seq(0, (6+1/4+1/64)*dt, 0.01)
t <- seq(0, (6+1/4+1/32)*dt, 0.01)
l <- seq(1, 1/16, length.out=length(t))

fr <- d$freq[d$name=='Ds3']
f <- freqFromLength(fr, l)

plot(t, f, ylab='frequency [Hz]', xlab='time [s]')
grid()

plot(t, log2(f/fr), ylab='octave', xlab='time [s]')
grid()

plot(t/dt, log2(f/fr), ylab='octave', xlab='time [beats]')
grid()

beats <- t/dt + 1
bb <- seq(1, max(beats), 1/4)
ff <- approx(beats, f, bb)$y

findNearest <- function(x, value, na.val=-9999) {
    if (inherits(x, 'POSIXt')) x <- as.numeric(x); value <- as.numeric(value)
    na <- is.na(x)
    x[na] <- na.val
    out <- NULL
    for (i in 1:length(value)) {
        outtmp <- which(abs(x-value[i])==min(abs(x-value[i])))
        if (length(outtmp) > 1) outtmp <- outtmp[1] ## simple way to resolve ties
        out <- c(out, outtmp)
    }
    return(out)
}

## nn <- d$note[findNearest(d$freq, ff)]
## df <- rev(d$freq[d$note %in% nn]) - ff

plot(beats, log2(f/fr), ylab='octave', xlab='time [beats]')
grid()
abline(v=bb, lty=2)
abline(h=log2(ff/fr))
abline(h=log2(d$freq/fr), lty=2, col='grey')

plot(beats, f, type='l', ylab='Freq [Hz]', xlab='time [beats]')
grid()
abline(v=bb, lty=2)
abline(h=ff)
abline(h=d$freq, lty=2, col='grey')

## find the times that each distinct note is played
noteTimes <- approx(f, t, d$freq)$y
fff <- d$freq[!is.na(noteTimes)]
noteTimes <- noteTimes[!is.na(noteTimes)]
noteBeats <- noteTimes/dt + 1
plot(noteBeats, fff, xlab='time [beats]')
grid()


plot(noteBeats, fff, xlab='time [beats]', ylab='freq [Hz]', log='y')
## axis(2, at=d$freq, labels=d$name)
axis(4, at=d$freq, labels=d$name)
abline(v=bb)
abline(h=d$freq, col=2)

plot(noteBeats, log2(fff/fr), xlab='time [beats]', ylab='octave')
## axis(2, at=d$freq, labels=d$name)
axis(4, at=log2(d$freq/fr), labels=d$name, col=2, col.axis=2)
abline(v=bb[seq(1, length(bb), 2)])
abline(v=seq(1, max(beats), 1/4), lty=2)
abline(h=log2(d$freq/fr), col=2)
abline(v=bb+1/16, lty=3)
abline(v=bb-1/16, lty=3)

beat <- note <- NULL
for (i in seq_along(bb)) {
    db <- abs(bb[i] - noteBeats)
    if (min(db) < 1/16) {
        beat <- c(beat, bb[i])
        note  <- c(note, d$name[noteFromFreq(fff[which.min(db)]) == d$note])
    }
}

points(beat, rep(par('usr')[4], length(beat)), pch=25, bg=1)
mtext(note, side=3, at=beat, cex=0.75)
