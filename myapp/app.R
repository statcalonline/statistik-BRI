

#library(webr)


#library(plyr)
library(readxl)
library(openxlsx)

library(shiny)
library(corrplot)
library(ggplot2)
library(reshape2)


#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plm)


########################################
########UI (User Interface)#############
########################################

modul_bank_BRI_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    uiOutput(ns("nama_tahun")), 
    br(),
    DT::DTOutput(ns("tampilkan_data_BRI")), 
    
    
    br(),
    br(),
    
    
    
    h2("Correlation Matrix", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:40px;
    font-weight: bold; 
    text-align:center
    
    "),
    
    
    
    br(),
    br(),
    
    uiOutput(ns("nama_variabel_matriks_korelasi")), 
    
    
    br(),
    br(),
    
    
    plotOutput(ns("matriks_korelasi1"), width="100%"),
    
    
    br(),
    
    br(),
    br(),
    
    
    
    h2("Time Series Plot", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:40px;
    font-weight: bold; 
    text-align:center
    
    "),
    
    
    br(),
    
    
    uiOutput(ns("nama_variabel_grafik_garis_tahun")), 
    #
    
    
    
    sliderInput( ns("number_column_line_chart"),      
    h4("Number of Column (Facet):",style="color:orange;text-shadow: -1px 0 black,
     0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 1,20,2, step = 1 ),
    
    
    
    br(),
    
    
    plotOutput(ns("grafik_garis_tahun"), width = "100%"),
    
    
    
    
    
    
    
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_bank_BRI_ui

#Akhir dari modul_bank_BRI_ui
#Akhir dari modul_bank_BRI_ui
#Akhir dari modul_bank_BRI_ui
#Akhir dari modul_bank_BRI_ui











































































########################################
################Server##################
########################################



modul_bank_BRI_server <- function(input, output, session) {
  
  
  kirim_tahun <- function()
  {
    
    dat <- read_xlsx("www/Data Keuangan Bank BRI.xlsx")
    
    dat <- as.data.frame(dat)
    
    nama_tahun <- dat[,"Tahun"]
    
    return(nama_tahun)
    
    
  }
  
  
  ################
  
  output$nama_tahun <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_tahun"), 
                       label="Pilih Tahun:", choices = c(kirim_tahun()), 
                       selected=c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), inline = TRUE)
    
    
    
    
    
    
  }) #buka_pilih_topik
  
  
  
  
  
  
  
  
  
  
  
  
  #############
  
  
  
  
  
  
  
  
  

  ambil_data_BRI <- function()
  {
    
    dat <- read_xlsx("www/Data Keuangan Bank BRI.xlsx")
    
    dat <- as.data.frame(dat)
    
    return(dat)
    
  }
  
  ############
  
  
  
  
  
  
  
  output$tampilkan_data_BRI <- DT::renderDT({
    
    
    dat <- ambil_data_BRI()
    
    terpilih_nama_tahun <- input$terpilih_nama_tahun
    terpilih_nama_tahun <- as.numeric(terpilih_nama_tahun)
    
    full_tahun <- dat[,"Tahun"]
    
    
    indeks <- full_tahun %in% terpilih_nama_tahun 
    indeks <- which(indeks==TRUE)
    
    dat2 <- dat[c(indeks),]
    print(dat2)
    
    
    
  })
  
  
  
  
  #############Nama Variabel
  
  kirim_nama_variabel_matriks_korelasi <- function()
  {
    
    dat <- read_xlsx("www/Data Keuangan Bank BRI.xlsx")
    
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  ##############
  ##############
  
  
  
  output$nama_variabel_matriks_korelasi <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_variabel_matriks_korelasi"), 
                       label="Pilih Variabel:", choices = c(kirim_nama_variabel_matriks_korelasi()), 
                       selected=c("ROA (%)", "ROE (%)", "NIM (%)", "BOPO (%)", "LDR (%)", "Earning per Share (Rp)", "CAR (%)", "NPL (%)"), inline = TRUE)
    
    
    
    
    
    
  }) #buka_pilih_topik
  
  
  
  
  
  #################
  
  
  output$matriks_korelasi1 <- renderPlot({
    
    data_lengkap <- ambil_data_BRI()
   
    
    nama_pilih <- input$terpilih_nama_variabel_matriks_korelasi
    
    dat_proses <- data_lengkap[c(nama_pilih)]
    
    
    ############
    ############
    
    
    
    terpilih_nama_tahun <- input$terpilih_nama_tahun
    terpilih_nama_tahun <- as.numeric(terpilih_nama_tahun)
    
    full_tahun <- data_lengkap[,"Tahun"]
    
    
    indeks <- full_tahun %in% terpilih_nama_tahun 
    indeks <- which(indeks==TRUE)
    
    dat_proses <- dat_proses[c(indeks),]
    
    
    
    
    
    
    korelasi <- cor(dat_proses)
    
    gambar <- corrplot(korelasi, type = "upper", order = "hclust", 
             tl.col = "black", tl.srt = 45,
             addCoef.col = 1)
    
    
    print(gambar)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################Grafik Garis
  
  
  
  
  output$nama_variabel_grafik_garis_tahun <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_variabel_grafik_garis_tahun"), 
                       label="Pilih Variabel:", choices = c(kirim_nama_variabel_matriks_korelasi()), 
                       selected=c("ROA (%)", "ROE (%)"), inline = TRUE)
    
    
    
    
    
    
  }) #buka_pilih_topik
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$grafik_garis_tahun <- renderPlot({
    
    
    dat <- ambil_data_BRI()
    
    terpilih_nama_tahun <- input$terpilih_nama_tahun
    terpilih_nama_tahun <- as.numeric(terpilih_nama_tahun)
    
    full_tahun <- dat[,"Tahun"]
    
    
    indeks <- full_tahun %in% terpilih_nama_tahun 
    indeks <- which(indeks==TRUE)
    
    dat2 <- dat[c(indeks),]

    #####################
    #####################
    
    
    
    nama_pilih <- input$terpilih_nama_variabel_grafik_garis_tahun
    
    dat_proses <- dat2[c("Tahun",nama_pilih)]
    
    
    ################
    
    
    susun_data = reshape2::melt(data = dat_proses, id.vars = c("Tahun")   )
    
    gambar <-  ggplot(susun_data, aes(x = Tahun, y = value, fill = variable))+
      facet_wrap( ~variable,  scales = "free",  ncol = input$number_column_line_chart) +
      geom_line(color="turquoise4") +
      theme_minimal() 
    
    
    print(gambar)
    
    
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_bank_BRI_server

#akhir dari modul_bank_BRI_server
#akhir dari modul_bank_BRI_server
#akhir dari modul_bank_BRI_server

















































































ui <- fluidPage(
  
  
  includeHTML("intro_home.html"),
  
  
  uiOutput("modul_bank_BRI"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_bank_BRI <- renderUI({
    
    
    
    #source("module//modul_bank_BRI.R")
    callModule(module = modul_bank_BRI_server, id = "modul_bank_BRI")
    modul_bank_BRI_ui(id = "modul_bank_BRI")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














