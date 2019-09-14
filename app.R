library(shinyWidgets)

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

d <- read.table('notes.txt', stringsAsFactors=FALSE, header=TRUE)


# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel("Glissando app"),

  fluidRow(column(12, checkboxInput("instructions", "Show instructions", value=TRUE))),
  fluidRow(conditionalPanel(condition="input.instructions",
                                          includeMarkdown("help.md"))),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      numericInput("beats",
                   label="Number of beats",
                   value=6.25, min = 1, max = 10, step = 0.25),
      sliderTextInput("start",
                      "Start Note",
                      choices=rev(d$name),
                      selected='Ds3', grid=TRUE),
      sliderTextInput("end",
                      "End Note",
                      choices=rev(d$name),
                      selected='Ds7', grid=TRUE)
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server code
server <- function(input, output) {
    output$plot <- renderPlot({
        bpm <- 152
        dt <- 60/bpm
        t <- seq(0, (input$beats+1/32)*dt, 0.001)

        l <- seq(1, lengthFromFreq(d$freq[d$name == input$end], d$freq[d$name == input$start]),
                 length.out=length(t))
        fr <- d$freq[d$name==input$start]
        f <- freqFromLength(fr, l)
        beats <- t/dt + 1
        bb <- seq(1, max(beats), 1/4)
        ff <- approx(beats, f, bb)$y

        ## find the times that each distinct note is played
        noteTimes <- approx(f, t, d$freq)$y
        fff <- d$freq[!is.na(noteTimes)]
        noteTimes <- noteTimes[!is.na(noteTimes)]
        noteBeats <- noteTimes/dt + 1

        beat <- note <- NULL
        for (i in seq_along(bb)) {
            db <- abs(bb[i] - noteBeats)
            if (min(db) < 1/16) {
                beat <- c(beat, bb[i])
                note  <- c(note, d$name[noteFromFreq(fff[which.min(db)]) == d$note])
            }
        }
        
        plot(noteBeats, log2(fff/fr), xlab='time [beats]', ylab='octave')
        axis(4, at=log2(d$freq/fr), labels=d$name, col=2, col.axis=2)
        abline(v=bb[seq(1, length(bb), 2)])
        abline(v=seq(1, max(beats), 1/4), lty=2)
        abline(h=log2(d$freq/fr), col=2)
        abline(v=bb+1/16, lty=3)
        abline(v=bb-1/16, lty=3)
        points(beat, rep(par('usr')[4], length(beat)), pch=25, bg=1)
        mtext(note, side=3, at=beat, cex=0.75)
    })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
