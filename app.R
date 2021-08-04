library(shiny)
library(shinythemes)
library(shinydashboard)
library(stringr)
library(shinyLP)


ordnerlisteTokens= str_remove((ordner=list.dirs(recursive=F)),"./")
ordnerlisteTokens= ordnerlisteTokens[ordnerlisteTokens != "www"]

NBformats=c("AVI|mp4|mov|wmv|mp3|wav")

if(!dir.exists("www")){
  dir.create("www")
}


### bildermenge
mengevec=c()
for(i in ordnerlisteTokens){
  for(j in list.files(i)){
    mengevec=c(mengevec,paste0(i,"/",j))
  }
}
vidInOrdnern=str_replace(mengevec[grepl(NBformats,mengevec)],"/","_")
mengevec=mengevec[!grepl(NBformats,mengevec)]

##videos

filesinfolderto=list.files("www")
filesinfolderto=filesinfolderto[grepl(NBformats,filesinfolderto)]

for (i in ordnerlisteTokens){
  
  filesinfolderfrom=list.files(i)
  filesinfolderfrom=filesinfolderfrom[grepl(NBformats,filesinfolderfrom)]
  
  test=!(paste0(i,"_",filesinfolderfrom) %in% filesinfolderto)
  if (length(filesinfolderfrom)==0){ 
    test=F}
  
  filesnotinfolderto=filesinfolderfrom[test]
  if(length(na.omit(test)) != 0){
    for(g in filesnotinfolderto){
      altpad=paste0(i,"/",g)
      neupad=paste0("www/",i,"_",g)
      file.copy(altpad,"www")
      file.rename(paste0("www/",g),neupad)
    }
  }
}

videolist=list.files("www")

videotags=c()
for (i in strsplit(videolist,"_")){
  videotags=c(videotags,i[1])
}
videotags=unique(videotags)


MainFoto=fluidRow(
  column(2, wellPanel(
    actionButton("FotoButton", "New \nImage")
  )),
  column(8,
         imageOutput("image", width="auto", height = "90vh")
  )
)

MainVideo=fluidRow(
  column(2, wellPanel(
    actionButton("VideoButton", "New")
  )),
  column(8,uiOutput("vidlink", width="auto", height = "90vh")
  )
)

MainSettings=fluidRow(
  column(3, 
         radioButtons("uniformdist", "Show media:",
                      c("Per Folder", "All","Only in folder"))
  ),
  column(3,
         selectInput("Med", "Which folders photos:", ordnerlisteTokens)
  ),
  column(3,
         selectInput("MedVideo", "Which folders videos:", videotags)
  )
)

ui<-navbarPage("Media",
               tabPanel("Foto",
                        MainFoto
               ),
               tabPanel("Video",
                        mainPanel(
                          HTML('<style>
                                 video {
                                   height: 90vh;
                                   width: 100%;
                                 }
                               </style>'),
                          MainVideo
                          #actionButton("VideoButton", "New"),
                          #HTML('<video src="milena_VID-20190503-WA0054.mp4" type="video/mp4" controls="controls"></video>'),
                          #tags$video(src="milena_VID-20190503-WA0054.mp4",type="video/mp4", controls="controls")
                          
                        )
               ),
               tabPanel("Settings",
                        mainPanel(
                          MainSettings
                        )
               )
)

server <- function(input, output, session) {
  
  pic2=eventReactive(input$FotoButton, {
    
    if(input$uniformdist == "Per Folder"){
      
      ordnerteil=sample(ordnerlisteTokens,1)
      bilder=list.files(ordnerteil)
      bilder=bilder[!grepl(NBformats,bilder)]
      bildteil=sample(bilder,1)
      link=paste0(ordnerteil,"/",bildteil)
      
    }else if(input$uniformdist == "All"){
      link=sample(mengevec,1)
      
    }else if(input$uniformdist == "Only in folder"){
      
      ordnerteil=input$Med
      bilder=list.files(ordnerteil)
      bilder=bilder[!grepl(NBformats,bilder)]
      bildteil=sample(bilder,1)
      link=paste0(ordnerteil,"/",bildteil)
    }
    return(link)
  })
  
  videoO=eventReactive(input$VideoButton, {
    
    if(input$uniformdist == "Per Folder"){
      ordnerteil=sample(videotags,1)
      bilder=videolist[grepl(ordnerteil,videolist)]
      link=sample(bilder,1)
      
    }else if(input$uniformdist == "All"){
      link=sample(videolist,1)
      
    }else if(input$uniformdist == "Only in folder"){
      
      ordnerteil=input$MedVideo
      bilder=videolist[grepl(ordnerteil,videolist)]
      link=sample(bilder,1)
    }
    return(link)
  })
  
  output$vidlink=renderUI({tags$video(type="video/mp4",#
                                      src=videoO(),
                                      controls="controls",
  )})
  
  output$image <- renderImage({list(
    src = pic2(),
    width = "auto",
    height = "100%",
    alt = "ggg"
  )},deleteFile = FALSE)
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser=T))