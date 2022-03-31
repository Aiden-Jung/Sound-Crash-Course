library(shiny)
library(tuneR)
library(seewave)
library(base64enc)
library(stringr)
library(shinyjs)
library(bslib)
library(thematic)
library(r2d3)
library(filesstrings)

thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(bootswatch = "solar"),
  tags$head(tags$style(
    HTML(
      "audio::-webkit-media-controls-mute-button {
         display: none !important;
       }
       audio::-webkit-media-controls-volume-slider {
         display: none !important;
       }
      "
    )
  )),
  tags$style(
    HTML("
      img {
  display: block;
  margin-left: auto;
  margin-right: auto;
}
      ")
  ),
  tags$h1("You Hear That?"),
  tabsetPanel(
    tabPanel(
      "Get Familiar",
      tags$br(),
      tags$p(
        tags$b(style = "color:#A69A83;", "Hello there!"),
        "If you opened our app, you're probably interested in
                    learning more about sound waves. Well, you have come to the
                    right place! Even if you don't know anything about sound
                    waves (yet), fear not as we will walk you through
                    everything you need to know. And, if you're a pro at
                    dealing with audio waves, we have something for you as well
                    when we talk about how computer scientists work with audio
                    data and come up with crazy applications! So, let's get
                    going!"
      ),
      tags$p(
        "The main thing about sound waves you need to know is that
                    they are nothing more than vibrations. For example, a
                    speaker has a diaphragm that moves to and fro. This pushes
                    the air particles next to it, and the air particles start
                    vibrating. Then the particles next to those start
                    vibrating. Ultimately, our eardrum picks up on these
                    vibrations, and the brain interprets these vibrations as
                    sound. Because scientists are not the most creative people,
                    they represent sound as a wave - a line that goes up and
                    down."
      ),
      tags$img(
        src = "wave.png",
        width = "624px",
        height = "248px"
      ),
      tags$br(),
      tags$p(
        "Yes, this looks so ugly! But hear us out; it's really
                    informative. The dotted white line in the center is the
                    position of the particles when they are not moving. When
                    the particles vibrate, they move in one direction first,
                    and then go in the opposite direction. This is what the
                    golden line represents. When the line is up, the particle
                    is one direction, and, when the line is going down, the
                    particle is going in the opposite direction. When the
                    particle completes one vibration, we say that 1 wave has
                    passed."
      ),
      tags$p(
        "There are 2 other things you should know about sound
                    waves. The greater a particle moves from its rest position
                    (the taller or steeper the yellow line), the louder the
                    sound. We refer to the distance between the topmost
                    position and rest position as amplitude. And lastly, we
                    define frequency as the number of waves produced per
                    second. You might have noticed that when a baby and an
                    adult cry at the same loudness (or amplitude!), the sound
                    of a baby feels more piercing. This is because the baby's
                    voice has a higher frequency, and higher frequency sounds
                    are perceived more strongly by our ears."
      ),
      tags$p(
        "Hover over to the next tab - 'Play Around' - to see how
                    changing the frequency of sound affects how we hear it.
                    What's more? You can even upload your own audio to see the
                    effects!"
      )
    ),
    tabPanel(
      "Play Around",
      tags$br(),
      tags$p(
        "In this tab, we'll explore why the frequency of a sound is
                    probably sound's most important trait. To start, why don't
                    you upload an audio clip in the box below. Don't worry, if
                    you don't have an audio clip, you can choose from the 2
                    options below: one is of Neil Armstrong saying 'One small
                    step for a man, one giant leap for mankind', and the other
                    clip is about the sounds you hear in a jungle."
      ),
      fluidRow(
        selectInput("files", "Choose file", list.files('www/set1')),
        column(6,
               fileInput(
                 inputId = "file",
                 label = "Upload a (stereo) mp3/wav file",
                 accept = c(".mp3", ".wav")
               ))
      ),
      tags$audio(id = "playAroundAud", controls = NA),
      tags$script(
        "
                           var source = document.getElementById('playAroundAud');
                            Shiny.addCustomMessageHandler('filename', function(name) {
                              source.src = '/set1/'.concat(name);
                            });
                            Shiny.addCustomMessageHandler('vol', function(vol) {
                              source.volume = vol;
                            });
                            Shiny.addCustomMessageHandler('tfile', function(tfile) {
                              source.src = tfile;
                            });

                            "
      ),
      tags$br(),
      tags$p(
        "What do you see now? This is the wave representation of the audio
             clip you uploaded/selected. Below these representations, you'll
             see 2 bars. One adjusts the frequency (the sharpness of sound),
             and the other adjusts the amplitude (the loudness of sound).
             Frequency is measured in Hertz (Hz), and amplitude is measured in
             decibels (dB). It would be great if you played around to
             first-handedly experience why these 2 parameters are so important.
             Just slide the pointers along the respective bars, click Update,
             and enjoy the results."
      ),
      d3Output("d3"),
      fluidRow(
        column(
          4,
          sliderInput(
            inputId = "freqShift",
            label = HTML("Select shift from</br>
                                      original frequency"),
            min = -1000,
            max = 1000,
            value = 0,
            ticks = FALSE,
            post = " Hz"
          )
        ),
        column(
          4,
          sliderInput(
            inputId = "ampScale",
            label = HTML("Select scaling factor from</br>
                                     original amplitude"),
            min = 0,
            max = 1,
            value = 0.5,
            ticks = FALSE
          )
        ),
        uiOutput("playAroundOptions"),
        tags$br(),
        fluidRow(column(
          10,
          actionButton(inputId = "reset",
                       label = "Reset to original sound")
        ))
      ),
      tags$br(),
      tags$p(
        HTML(
          "So clearly, changing the frequency and amplitude have
                         very audible effects. But what if we told you that
                         there is one more property of sound that is arguably
                         even more important? That's called timbre. We'll dive
                         into that in the next tab &#128578;."
        )
      )
    ),
    tabPanel(
      "Same Sounds?",
      tags$br(),
      tags$p(
        "Why is it that your voice and your friend's voice sound
                    very different even though they have similar frequencies?
                    Why is it that a rock concert from a distance and cars
                    honking sound so different even though they might have the
                    same amplitude? This is due to the third property of sound
                    - timbre or tone quality."
      ),
      tags$p(
        "Many sounds, in fact almost all, don't have the simple up
                    and down wave representation we showed you earlier. Waves
                    come in all sorts of shapes. Have a look at the picture
                    below. These sounds have roughly the same frequencies and
                    amplitudes but are very different."
      ),
      tags$img(
        src = "timbre.png",
        width = "677px",
        height = "367px"
      ),
      tags$p(
        "And it's this wave shape that's so important. It makes
                    each sound be perceived differently. Below, we have a fun
                    exercise for you. Choose a frequency range on the left,
                    Then, you'll see 3 audio clips pop up that have frequencies
                    within the range you selected. Hear them out and you'll see
                    what a dramatic effect that timbre has on sound."
      ),
      selectInput(
        inputId = "freqRange",
        label = "Select frequency range of sound",
        choices = c("range(1000-2000)", "range(2000-3000)", "range(5000-6000)")
      ),
      uiOutput("samesound"),
      tags$br(),
      tags$p(
        "Now that you are a pro at understanding sound waves, we'll
                    show you how this information can be used. First, we'll
                    dive into how wearing masks during the pandemic affects how
                    others hear our voices. Moving on, we'll discuss how
                    programs like Google Assistant or Siri are able to detect
                    what song you're listening to. And lastly, we'll introduce
                    you to a cool process of associating sound with images, and
                    how we can detect spam videos. Let's go!"
      )
    ),
    tabPanel(
      "Masks & Covid",
      tags$br(),
      tags$p(
        "Many of us still wear a mask in public spaces for fear of
                    contracting Covid-19. You may have noticed that people
                    wearing masks sound different from how they normally sound.
                    Why is this so?"
      ),
      tags$p(
        "To examine how masks affect voices, researchers from the
                    University of Illinois at Urbana-Champaign found
                    relationships between sound frequency (Hz) and sound level
                    (dB). The (THAT'S SCARY) picture below is a quick overview
                    of their findings."
      ),
      tags$img(
        src = "masks.png",
        width = "469px",
        height = "347px"
      ),
      tags$p(
        "Let's break down these lines for you. The most important
                    thing is that whenever a line goes down, it means that
                    sound intensity (loudness or amplitude) is being decreased
                    by the mask. You'll see that the bottom axis is labeled
                    'Frequency'. We can observe that lines trend downwards as
                    sound frequency increases. This implies that masks have a
                    stronger effect on high frequency (high-pitched) sounds.
                    Because most words of most human languages generally have
                    high frequencies, it makes sense that masks alter the sound
                    of words that get spoken through them."
      ),
      tags$p(
        "What's also interesting is that each mask material
                    decreases sound intensity by different degrees, but we can
                    still see an approximately linear pattern between sound
                    intensity and frequency (we have straight lines and not
                    weird curvy ones). A mask made out of thin material, such
                    as a thin scarf or a surgical mask, decreases sound
                    intensity less than a mask made out of thick materials,
                    such as an N-95 mask or a heavy scarf. Therefore, when we
                    talk to someone while wearing a mask, we may need to speak
                    more loudly to be heard."
      ),
      tags$p(
        "If you want to learn more about the effects of masking on
                    speech, check out this ",
        tags$a(
          href = str_c(
            "https://publish.illinois.edu/augmented",
            "listening/face-mask-acoustics/"
          ),
          "link"
        ),
        " in which researchers from the University of Illinois at
                    Urbana-Champaign talk about their work. Or, if you are in a
                    hurry, skip along to the next tab that shows how song
                    detection works!"
      )
    ),
    tabPanel(
      "Song Detection",
      tags$br(),
      tags$p(
        "Do you remember when Shazam came out in the early 2000s?
                    It was all the rage and everybody had it on their phones
                    and computers. All of a sudden, people could identify any
                    song and never forget a good memory. Moreover, the song
                    detection was really quick. Just 10 seconds and you were
                    done! No lyrics? No problem, as simply the music was
                    enough. Nowadays, song detection algorithms have gotten so
                    fast that tech companies include this feature in apps that
                    need to be very light, such as Google Assistant or Siri.
                    So, how does song detection work? Let's dig deeper."
      ),
      tags$p(
        "The secret ingredient to detecting a song correctly is to
                    work with recorded audio's timbre, or wave shape. For
                    detection, the given sound gets adjusted to have the same
                    frequency and amplitude as the numerous sample sounds in
                    the huge databases of these tech companies. Then, the
                    timbre of the given sound is matched with the timbre of
                    the sound clips in the databases, since different sounds
                    have their own unique waveforms. This is how a program can
                    find out the name of a song based on its sound waves."
      ),
      tags$img(
        src = "song.png",
        width = "422px",
        height = "235px"
      ),
      tags$p(
        "Now that we know how songs can be detected, let's move on
                    to another insanely cool application of the knowledge you
                    now have. What if we tell you that there is a way we can
                    detect which videos are spam and which are genuine by
                    matching the audio with the video frames? Trust us, this
                    process is truly awesome. So, let's not waste any time and
                    move on to the next tab!"
      )
    ),
    tabPanel(
      "Spam Videos",
      tags$br(),
      tags$p(
        "Visual and audio events tend to occur together. For
                    example, a musician plucking guitar strings is accompanied
                    by a melody, a wine glass shattering is accompanied by a
                    crashing sound, and a motorcycle accelerating is
                    accompanied by a roar. These visual and audio stimuli are
                    concurrent because they share a common cause. Understanding
                    the relationship between visual events and their associated
                    sounds is a fundamental way that we make sense of the world
                    around us."
      ),
      tags$p(
        "To evaluate the correspondence between visual and audio
                    streams, DeepMind (they're a big name in the AI world) and
                    Oxford University invented an algorithm called the Audio-
                    Visual Object Localization Network (oooo big words!) that
                    is abbreviated as AVOL-Net. In simple words, this algorithm
                    works on a video and determines if the images of the video
                    seem to match the audio (they compare the image with the
                    sound frequency and timbre). This task is not easy. It took
                    the best AI minds in the world to get it working. But once
                    we know if audio matches the visuals, we can detect if a
                    given video is spam or not. For instance, if someone
                    overlays a video of Obama speaking with Donald Trump's
                    voice, this process can successfully detect this spam, as
                    the frequency and timbre of Obama's voice are way different
                    than that of Trump."
      ),
      tags$p(
        "Here is some information for those experienced with
                    Machine Learning. The architecture of AVOL-Net is
                    relatively simple. First, it compresses audio and visual
                    information using a ConvNet, i.e., a convolutional neural
                    network. Then, it computes the dot product of compressed
                    audio and visual information to get a spatial
                    representation of audio for each location in the image.
                    Next, it uses the dot product as a parameter of a ConvNet
                    to classify whether a given audio and video clip correspond
                    or not. Seeing is believing. Below is a rudimentary
                    implementation of AVOL-Net. You can get an intuition on
                    what AVOL-Net does by reading the documentation we wrote,
                    training models using a given dataset, and testing a model
                    by uploading a video in mp4 format to it"
      ),
      tags$p(
        "You can find our dataset at this Google drive ",
        tags$a(
          href = str_c(
            "https://drive.google.com/drive/folders",
            "/1cofd02hNhopuCQgomr9R8TbsTr9PXnsn?usp",
            "=sharing"
          ),
          "link,"
        ),
        " and you can find our model in Jupyter Notebook at this
                    Google drive ",
        tags$a(
          href = str_c(
            "https://colab.research.google.com/",
            "drive/1sct5KkC87WABq3_lHUtdB7KJbHdrLCW",
            "b?usp=sharing"
          ),
          "link."
        )
      ),
      tags$img(
        src = "avol-net.png",
        width = "393px",
        height = "499px"
      ),
      tags$p(
        "Reference: ",
        tags$a(
          href = str_c("https://deepmind.com/blog/article/",
                       "objects-that-sound"),
          "DeepMind Objects that Sound article"
        )
      ),
      tags$p(
        "Thank you for joining us on this crash course. We hope you
                    now feel confident with basic audio terms and know how each
                    sound property affects how we perceive sound. And, if you
                    were already familiar with these terminologies, we hope
                    that you have seen potential useful applications of this
                    knowledge."
      ),
      tags$p(
        "Until next time,",
        tags$br(),
        "Your friendly neighborhood Grinnell College Developers"
      )
    )
  )
)


server <- function(input, output, session) {
  values <- reactiveValues(
    oldWave = readMP3(paste(
      "www/set1/", list.files("www/set1/")[1], sep = ""
    )),
    newWave = readMP3(paste(
      "www/set1/", list.files("www/set1/")[1], sep = ""
    )),
    sameWave1 = readWave('www/set2/range(1000-2000)/Sample1.wav'),
    sameWave2 = readWave('www/set2/range(1000-2000)/Sample2.wav'),
    sameWave3 = readWave('www/set2/range(1000-2000)/Sample3.wav'),
    temppath = NULL
  )
  
  observeEvent(input$file, {
    move_files(input$file$datapath, 'www/set1/', overwrite = TRUE)
    file.rename('www/set1/0.mp3',
                paste0("www/set1/", input$file$name, sep = ""))
  })
  
  observeEvent(input$reset, {
    updateSliderInput(inputId = "freqShift", value = 0)
    updateSliderInput(inputId = "ampScale", value = 0.5)
    updateSliderInput(inputId = "timeScale", value = 1)
    values$newWave <- values$oldWave
  })
  
  observeEvent(input$freqShift, {
    oldWave <- values$oldWave
    wl <- 1024
    bit <- oldWave@bit
    if (abs(input$freqShift) < oldWave@samp.rate / wl) {
      return("You must select a frequency shift larger in magnitude")
    } else {
      left_level <- max(abs(oldWave@left)) / (2 ^ (bit - 1) - 1)
      right_level <- max(abs(oldWave@right)) / (2 ^ (bit - 1) - 1)
      left <- lfs(
        oldWave,
        channel = 1,
        wl = wl,
        shift = as.integer(input$freqShift),
        output = "Wave"
      )
      right <- lfs(
        oldWave,
        channel = 2,
        wl = wl,
        shift = as.integer(input$freqShift),
        output = "Wave"
      )
      left_scaled <- normalize(left, unit = as.character(bit),
                               level = left_level)
      right_scaled <- normalize(right, unit = as.character(bit),
                                level = right_level)
      values$newWave <- stereo(left_scaled, right_scaled)
      temppath <- tempfile(fileext = ".wav")
      writeWave(values$newWave, filename = temppath)
      values$temppath <- dataURI(file = temppath, mime = "audio/wav")
      session$sendCustomMessage("tfile", tfile())
    }
  })
  
  
  output$playAroundOptions <- renderUI({
    sliderInput(
      inputId = "timeScale",
      label = HTML("Select time range"),
      min = 0,
      max = round(length(values$newWave@left) / values$newWave@samp.rate, 2),
      value = 1,
      ticks = FALSE,
      step = 0.01
    )
  })
  
  output$samesound <- renderUI({
    verticalLayout(
      tags$audio(
        id = "sameSoundsAud",
        src = paste0("set2/", input$freqRange, "/", list.files(
          paste0("www/set2/", input$freqRange, sep = "")
        )[1], sep = ""),
        controls = NA,
        controlsList = "nodownload",
        type = "audio/wav"
    ),
    renderPlot({
      plot(values$sameWave1)
    }),
      tags$audio(
        id = "sameSoundsAud",
        src = paste0("set2/", input$freqRange, "/", list.files(
          paste0("www/set2/", input$freqRange, sep = "")
        )[2], sep = ""),
        controls = NA,
        controlsList = "nodownload",
        type = "audio/wav"
    ),
    renderPlot({
      plot(values$sameWave2)
    }),
      tags$audio(
        id = "sameSoundsAud",
        src = paste0("set2/", input$freqRange, "/", list.files(
          paste0("www/set2/", input$freqRange, sep = "")
        )[3], sep = ""),
        controls = NA,
        controlsList = "nodownload",
        type = "audio/wav"
    ),renderPlot({
      plot(values$sameWave1)
    }))
  })
  
  observeEvent(input$freqRange, {
    values$sameWave1 <-
      readWave(paste0(
        "www/set2/",
        input$freqRange,
        "/",
        list.files(paste0("www/set2/", input$freqRange, sep = ""))[1],
        sep = ""
      ))
    values$sameWave2 <-
      readWave(paste0(
        "www/set2/",
        input$freqRange,
        "/",
        list.files(paste0("www/set2/", input$freqRange, sep = ""))[2],
        sep = ""
      ))
    values$sameWave3 <-
      readWave(paste0(
        "www/set2/",
        input$freqRange,
        "/",
        list.files(paste0("www/set2/", input$freqRange, sep = ""))[3],
        sep = ""
      ))
  })
  
  
  has.new.files <- function() {
    unique(list.files('www/set1'))
  }
  get.files <- function() {
    list.files('www/set1')
  }
  
  # store as a reactive instead of output
  my_files <-
    reactivePoll(10, session, checkFunc = has.new.files, valueFunc = get.files)
  
  # any time the reactive changes, update the selectInput
  observeEvent(my_files(), ignoreInit = T, ignoreNULL = T, {
    updateSelectInput(session, 'files', choices = my_files())
  })
  
  filename <- function() {
    input$files
  }
  
  observe({
    session$sendCustomMessage("filename", filename())
  })
  
  observeEvent(input$files, {
    values$oldWave <- readMP3(paste("www/set1/", input$files, sep = ""))
    values$newWave <- values$oldWave
    updateSliderInput(inputId = "freqShift", value = 0)
    updateSliderInput(inputId = "ampScale", value = 0.5)
    updateSliderInput(inputId = "timeScale", value = 1)
  })
  
  observeEvent(values$newWave, {
    endTime <- length(values$newWave@left) / values$newWave@samp.rate
    
    updateSliderInput(inputId = "timeScale",
                      max = round(endTime, 2),
                      value = 1)
  })
  
  observeEvent(values$sameWave1, {
    print(tail(values$sameWave1@left))
  })
  
  time <- function() {
    input$timeScale
  }
  
  observe({
    session$sendCustomMessage("time", time())
  })
  
  amp <- function() {
    input$ampScale
  }
  
  observe({
    session$sendCustomMessage("amp", amp())
  })
  
  observeEvent(input$call, {
    print(input$call)
  })
  
  vol <- function() {
    input$ampScale
  }
  
  observe({
    session$sendCustomMessage("vol", vol())
  })
  
  rate <- function() {
    2000
    #values$newWave@samp.rate
  }
  
  observe({
    session$sendCustomMessage("rate", rate())
  })
  
  tfile <- function() {
    values$temppath
  }
  
  observeEvent(input$callback, {
    session$sendCustomMessage("amp", amp())
    session$sendCustomMessage("time", time())
    session$sendCustomMessage("rate", rate())
  })
  
  output$d3 <- renderD3({
    r2d3(
      values$newWave@left,
      script = "visualization.js"
    )
  })
}

shinyApp(ui = ui, server = server)
