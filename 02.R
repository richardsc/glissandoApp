library(tuneR)
setWavPlayer('/usr/bin/afplay')

## Global variables can go here
freqFromNote <- function(n) {
    2^((n-49)/12) * 440
}

noteFromFreq <- function(f) {
    round(12 * log2(f/440) + 49)
}

freqFromLength <- function(fr, l) {
    fr/l
}

lengthFromFreq <- function(f, fr) {
    fr/f
}

x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
w1 <- Wave(left = channel)

f  <- seq(440, 880, length.out=length(x))
channel <- round(32000 * sin(f * x))
w2 <- Wave(left = channel)

d <- read.table('notes.txt', stringsAsFactors=FALSE, header=TRUE)
bpm <- 152
dt <- 60/bpm
beats <- 6.25
t <- seq(0, (beats)*dt, 0.001)

start <- 'Ds3'
end <- 'Ds7'
l <- seq(1, lengthFromFreq(d$freq[d$name == end], d$freq[d$name == start]),
         length.out=length(t))
fr <- d$freq[d$name==start]
f <- freqFromLength(fr, l)

x <- seq(0, 2*pi, length = 44100*max(t))
ff <- approx(seq(0, 2*pi, length.out=length(t)), f, x)$y
channel <- round(32000 * sin(ff * x))
w3 <- Wave(left = channel)
