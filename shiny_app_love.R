
#------------------------------------------------------------------------#
# Proyecto: ShinApp del barómetro del amor en España @40dB. 
# Autor: Pablo García Guzmán
# Sígueme en Twitter: @pablogguz_
#------------------------------------------------------------------------#

library(XML)
library(stringr)
library(dplyr)
library(haven)
library(tm)
library(tidytext)
library(ggplot2)
library(whoami)
library(shiny)
library(rsconnect)
library(grid)
library(plyr)
library(shinythemes)
library(thematic)

#------------------------------ 1. Read data ------------------------------#

data <- read_dta("https://github.com/pablogguz/barometro_amor_40dB/blob/main/proc/amor_40db_shiny.dta?raw=true")

# Sex

data$sexo2 <- ifelse(data$sexo==1,
                     "Hombre",
                     NA)
data$sexo2 <- ifelse(data$sexo==2,
                     "Mujer",
                     data$sexo2)

# Sexual orientation

data$orient2 <- ifelse(data$sex_orientation==1,
                     "Heterosexual",
                     NA)
data$orient2 <- ifelse(data$sex_orientation==2,
                     "LGTBIQ+",
                     data$orient2)
# Electoral vote

data$voto2 <- ifelse(data$voto==1,
                     "PSOE",
                     NA)
data$voto2 <- ifelse(data$voto==2,
                     "PP",
                     data$voto2)
data$voto2 <- ifelse(data$voto==3,
                     "Ciudadanos",
                     data$voto2)
data$voto2 <- ifelse(data$voto==4,
                     "Unidas \nPodemos",
                     data$voto2)
data$voto2 <- ifelse(data$voto==5,
                     "Vox",
                     data$voto2)

# Ideology

data$ideologia2 <- ifelse(data$ideologia_bis==1,
                            "Izquierda",
                            NA)
data$ideologia2 <- ifelse(data$ideologia_bis==2,
                            "Centro",
                            data$ideologia2)
data$ideologia2 <- ifelse(data$ideologia_bis==3,
                            "Derecha",
                            data$ideologia2)

# Social class

data$clasesocial2 <- ifelse(data$clase_social_r==1,
                            "Alta / Media alta",
                            NA)
data$clasesocial2 <- ifelse(data$clase_social_r==2,
                            "Media",
                            data$clasesocial2)
data$clasesocial2 <- ifelse(data$clase_social_r==3,
                            "Media baja / Baja",
                            data$clasesocial2)

# Age group

data$grupoedad2 <- ifelse(data$edad_r==1,
                            "18-24",
                            NA)
data$grupoedad2 <- ifelse(data$edad_r==2,
                            "25-34",
                            data$grupoedad2)
data$grupoedad2 <- ifelse(data$edad_r==3,
                            "35-44",
                            data$grupoedad2)
data$grupoedad2 <- ifelse(data$edad_r==4,
                          "45-54",
                          data$grupoedad2)
data$grupoedad2 <- ifelse(data$edad_r==5,
                          "55-64",
                          data$grupoedad2)
data$grupoedad2 <- ifelse(data$edad_r==6,
                          "65+",
                          data$grupoedad2)

# CCAA 

data$ccaa2 <- ifelse(data$ccaa==1,
                          "Andalucía",
                          NA)
data$ccaa2 <- ifelse(data$ccaa==7,
                          "Castilla \ny León",
                          data$ccaa2)
data$ccaa2 <- ifelse(data$ccaa==9,
                          "Cataluña",
                          data$ccaa2)
data$ccaa2 <- ifelse(data$ccaa==10,
                          "Com. \nValenciana",
                          data$ccaa2)
data$ccaa2 <- ifelse(data$ccaa==12,
                          "Galicia",
                          data$ccaa2)
data$ccaa2 <- ifelse(data$ccaa==13,
                          "Madrid",
                          data$ccaa2)

# Function for computing weighted means and CIs
weighted.summarySE <- function(data=NULL, measurevar,  groupvars=NULL, weights, na.rm=FALSE,
                               conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  #weighted - SD function!
  w.sd <- function(x, w,na.rm=TRUE )  ( (sum(w*x*x, na.rm=na.rm)/sum(w, na.rm=na.rm)) - weighted.mean(x,w, na.rm=na.rm)^2 )^.5
  
  # This does the summary. For each group's data frame, return a vector with
  datac <- ddply(data, groupvars,
                 .fun = function(xx, col, weights) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = weighted.mean(xx[[col]], xx[[weights]], na.rm=na.rm),
                     sd   = w.sd(xx[[col]], xx[[weights]], na.rm=na.rm)
                   )
                 },
                 measurevar, weights
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#------------------------------ 2. Shiny app ------------------------------#

# Define UI for app ----
ui <- fluidPage(theme = shinytheme("flatly"),

  # App title ----
  titlePanel("Barómetro del amor en España 💘"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("Explora los microdatos de la encuesta de", a("40dB.", href = "https://twitter.com/40dbes?lang=es"),
        "sobre el amor en España con esta app."),
      
      h5("Puedes descargar el fichero de microdatos", a("aquí.", href = "https://elpais.com/sociedad/2022-06-05/consulte-todos-los-datos-internos-de-la-encuesta-de-el-pais-sobre-la-percepcion-del-amor-cuestionarios-y-respuestas-individuales.html")),
      
      h6(a("pablogguz.github.io", 
           href = "https://pablogguz.github.io/"), "| Sígueme en Twitter: ", a("@pablogguz_", href = "https://twitter.com/pablogguz_")
      ),
      
      # Input: Selector for choosing first level of aggregation  ---- 
      selectInput(inputId = "first",
                  label = "1️⃣ Elige un primer nivel de agregación:",
                  choices = c("Por sexo", "Por orientación sexual")),
      
      # Input: Selector for choosing second level of aggregation  ---- 
      selectInput(inputId = "second",
                  label = "2️⃣ Elige un segundo nivel de agregación:",
                  choices = c("Por grupo de edad", "Por clase social", "Por recuerdo de voto", "Por ideología", "Por CCAA*")),
      
      # Input: PANEL A ---- 
      selectInput(inputId = "panela",
                  label = "Panel A: % de personas que...",
                  choices = c("Tienen pareja", 
                              "Nunca han tenido pareja", 
                              "Responden que su pareja actual es el amor \nmás importante de su vida", 
                              "Han sido infieles a su actual pareja",
                              "Tienen una pareja monógama",
                              "Creen que se es más feliz en pareja")),
      
      # Input: PANEL B ---- 
      selectInput(inputId = "panelb",
                  label = "Panel B: Tendría una relación con...",
                  choices = c("Una persona al menos 20 años mayor", 
                              "Una persona al menos 20 años menor", 
                              "Una persona de distinta ideología", 
                              "Una persona de distinta raza",
                              "Una persona que viviese en otro país",
                              "Una persona con hijos de otra pareja",
                              "Una persona con mucho más dinero",
                              "Una persona con mucho menos dinero")),
      
      # Input: PANEL C ---- 
      selectInput(inputId = "panelc",
                  label = "Panel C: ¿Qué estarías dispuesto a hacer por tu pareja?",
                  choices = c("Perdonar una infidelidad", 
                              "Dejar mi trabajo", 
                              "Cambiar mis condiciones de trabajo", 
                              "Tener hijos",
                              "Irme a vivir a otra ciudad y/o país",
                              "Relacionarme con amigos y familiares, \naunque no sean de mi agrado")),
      
      # Input: PANEL D ---- 
      selectInput(inputId = "paneld",
                  label = "Panel D: Sobre tu actual pareja",
                  choices = c("Grado de satisfacción (0-10)", 
                              "Probabilidad de ruptura", 
                              "Experimenta falta de ilusión \no aburrimiento", 
                              "Experimenta afecto y comprensión",
                              "Experimenta escucha activa \ny comunicación",
                              "Experimenta celos",
                              "Experimenta admiración y respeto",
                              "Experimenta relaciones sexuales \nplacenteras",
                              "Experimenta relaciones sexuales \nno placenteras")),
      
      h6("*Sólo se muestran las CCAAs con > 100 observaciones."),
      
      h6("Se reportan intervalos de confianza al 90%. Todos los estadísticos se calculan utilizando los pesos (i.e., ponderaciones) de la encuesta."),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        column(6, plotOutput("panela")), 
        column(6, plotOutput("panelb"))
      ),
      
      fluidRow(
        column(6, plotOutput("panelc")),
        column(6, plotOutput("paneld"))
      )
      
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
# PANEL A:
  output$panela <- renderPlot({
    
    if (input$first == "Por sexo") {
      data$group1 <- data$sexo2
      
    } else {
      data$group1 <- data$orient2
    }
    
    if (input$second == "Por recuerdo de voto") {
      data$group2 <- data$voto2
    } 
    
    if (input$second == "Por grupo de edad") {
      data$group2 <- data$grupoedad2
    } 
    
    if (input$second == "Por CCAA*") {
      data$group2 <- data$ccaa2
    } 
    
    if (input$second == "Por clase social") {
      data$group2 <- data$clasesocial2
    } 
    
    if (input$second == "Por ideología") {
      data$group2 <- data$ideologia2
    } 
    
    # Outcomes
    
    if (input$panela == "Tienen pareja") {
      data$outcome <- data$couple
    } 
    
    if (input$panela == "Nunca han tenido pareja") {
      data$outcome <- data$no_pareja_nunca
    } 
    
    if (input$panela == "Responden que su pareja actual es el amor \nmás importante de su vida") {
      data$outcome <- data$gran_amor_couple
    } 
    
    if (input$panela == "Han sido infieles a su actual pareja") {
      data$outcome <- data$cuernos
    } 
    
    if (input$panela == "Creen que se es más feliz en pareja") {
      data$outcome <- data$mas_feliz_pareja
    } 
    
    if (input$panela == "Tienen una pareja monógama") {
      data$outcome <- data$monogamia
    } 
    
    data_a <- data %>% filter(!is.na(outcome) & !is.na(group1) & !is.na(group2))
    
    x <- weighted.summarySE(data_a, measurevar="outcome", groupvars=c("group1", "group2"), weights = "ponde", conf.interval=.90)
    dodge <- position_dodge(width = 0.9)

    ggplot(data = x, aes(x = group2, y = outcome, fill = group1)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax = outcome + ci, ymin = outcome - ci), position = dodge, width = 0.2) +
      scale_fill_manual(values = alpha(c("royalblue", "red"), 0.6)) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      ggtitle(paste0("Panel A: ", input$panela, " (%)")) +
      theme_classic() +
      theme(
        plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = c(0.8, 0.95),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18)
      ) 
    
  })
  
  # PANEL B:
  output$panelb <- renderPlot({
    
    if (input$first == "Por sexo") {
      data$group1 <- data$sexo2
      
    } else {
      data$group1 <- data$orient2
    }
    
    if (input$second == "Por recuerdo de voto") {
      data$group2 <- data$voto2
    } 
    
    if (input$second == "Por grupo de edad") {
      data$group2 <- data$grupoedad2
    } 
    
    if (input$second == "Por CCAA*") {
      data$group2 <- data$ccaa2
    } 
    
    if (input$second == "Por clase social") {
      data$group2 <- data$clasesocial2
    } 
    
    if (input$second == "Por ideología") {
      data$group2 <- data$ideologia2
    } 
    
    # Outcomes
    
    if (input$panelb == "Una persona al menos 20 años mayor") {
      data$outcome <- data$p7_1_bin
    } 
    
    if (input$panelb == "Una persona al menos 20 años menor") {
      data$outcome <- data$p7_2_bin
    } 
    
    if (input$panelb == "Una persona de distinta ideología") {
      data$outcome <- data$p7_3_bin
    } 
    
    if (input$panelb == "Una persona de distinta raza") {
      data$outcome <- data$p7_4_bin
    } 
    
    if (input$panelb == "Una persona que viviese en otro país") {
      data$outcome <- data$p7_5_bin
    } 
    
    if (input$panelb == "Una persona con hijos de otra pareja") {
      data$outcome <- data$p7_6_bin
    } 
    
    if (input$panelb == "Una persona con mucho más dinero") {
      data$outcome <- data$p7_7_bin
    } 
    
    if (input$panelb == "Una persona con mucho menos dinero") {
      data$outcome <- data$p7_8_bin
    } 
    
    data_b <- data %>% filter(!is.na(outcome) & !is.na(group1) & !is.na(group2))
    
    x <- weighted.summarySE(data_b, measurevar="outcome", groupvars=c("group1", "group2"), weights = "ponde", conf.interval=.90)
    dodge <- position_dodge(width = 0.9)
    
    ggplot(data = x, aes(x = group2, y = outcome, fill = group1)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax = outcome + ci, ymin = outcome - ci), position = dodge, width = 0.2) +
      scale_fill_manual(values = alpha(c("royalblue", "red"), 0.6)) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      ggtitle(paste0("Panel B: Tendría una relación con \n", tolower(input$panelb), " (%)")) +
      theme_classic() +
      theme(
        plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = c(0.8, 0.95),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        plot.title = element_text(size = 18)
      ) 
    
  })
  
  # PANEL C:
  output$panelc <- renderPlot({
    
    if (input$first == "Por sexo") {
      data$group1 <- data$sexo2
      
    } else {
      data$group1 <- data$orient2
    }
    
    if (input$second == "Por recuerdo de voto") {
      data$group2 <- data$voto2
    } 
    
    if (input$second == "Por grupo de edad") {
      data$group2 <- data$grupoedad2
    } 
    
    if (input$second == "Por CCAA*") {
      data$group2 <- data$ccaa2
    } 
    
    if (input$second == "Por clase social") {
      data$group2 <- data$clasesocial2
    } 
  
    if (input$second == "Por ideología") {
      data$group2 <- data$ideologia2
    } 
    
    # Outcomes
  
    if (input$panelc == "Perdonar una infidelidad") {
      data$outcome <- data$infidelidad
    } 
    
    if (input$panelc == "Dejar mi trabajo") {
      data$outcome <- data$dejar_trabajo
    } 
    
    if (input$panelc == "Cambiar mis condiciones de trabajo") {
      data$outcome <- data$cambiar_cond
    } 
    
    if (input$panelc == "Tener hijos") {
      data$outcome <- data$tener_hijos
    } 
    
    if (input$panelc == "Irme a vivir a otra ciudad y/o país") {
      data$outcome <- data$otra_ciudad_pais
    } 
    
    if (input$panelc == "Relacionarme con amigos y familiares, \naunque no sean de mi agrado") {
      data$outcome <- data$amigos_familiares
    } 
  
    data_c <- data %>% filter(!is.na(outcome) & !is.na(group1) & !is.na(group2))
    
    x <- weighted.summarySE(data_c, measurevar="outcome", groupvars=c("group1", "group2"), weights = "ponde", conf.interval=.90)
    
    dodge <- position_dodge(width = 0.9)
    
    ggplot(data = x, aes(x = group2, y = outcome, fill = group1)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax = outcome + ci, ymin = outcome - ci), position = dodge, width = 0.2) +
      scale_fill_manual(values = alpha(c("royalblue", "red"), 0.6)) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      ggtitle(paste0("Panel C: Estaría dispuesto a \n", tolower(input$panelc), " (%)")) +
      theme_classic() +
      theme(
        plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = c(0.8, 1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18)
      ) 
    
  })
  
  # PANEL D:
  output$paneld <- renderPlot({
    
    if (input$first == "Por sexo") {
      data$group1 <- data$sexo2
      
    } else {
      data$group1 <- data$orient2
    }
    
    if (input$second == "Por recuerdo de voto") {
      data$group2 <- data$voto2
    } 
    
    if (input$second == "Por grupo de edad") {
      data$group2 <- data$grupoedad2
    } 
    
    if (input$second == "Por CCAA*") {
      data$group2 <- data$ccaa2
    } 
    
    if (input$second == "Por clase social") {
      data$group2 <- data$clasesocial2
    } 
    
    if (input$second == "Por ideología") {
      data$group2 <- data$ideologia2
    } 
    
    # Outcomes 
    
    if (input$paneld == "Grado de satisfacción (0-10)") {
      data$outcome <- data$satisfaccion_pareja
    } 
    
    if (input$paneld == "Probabilidad de ruptura") {
      data$outcome <- data$probabilidad_ruptura
    } 
    
    if (input$paneld == "Experimenta falta de ilusión \no aburrimiento") {
      data$outcome <- data$falta_ilusion
    } 
    
    if (input$paneld == "Experimenta afecto y comprensión") {
      data$outcome <- data$afecto_comprension
    }   
    
    if (input$paneld == "Experimenta celos") {
      data$outcome <- data$celos
    } 
    
    if (input$paneld == "Experimenta escucha activa \ny comunicación") {
      data$outcome <- data$escucha_activa
    } 
    
    if (input$paneld == "Experimenta admiración y respeto") {
      data$outcome <- data$admiracion_respeto
    } 
    
    if (input$paneld == "Experimenta relaciones sexuales \nplacenteras") {
      data$outcome <- data$relaciones_placenteras
    } 
    
    if (input$paneld == "Experimenta relaciones sexuales \nno placenteras") {
      data$outcome <- data$relaciones_noplacenteras
    } 
    
    data_d <- data %>% filter(!is.na(outcome) & !is.na(group1) & !is.na(group2))
    
    x <- weighted.summarySE(data_d, measurevar="outcome", groupvars=c("group1", "group2"), weights = "ponde", conf.interval=.90)
    
    dodge <- position_dodge(width = 0.9)
    
    if (input$paneld != "Grado de satisfacción (0-10)") { # plot with % re-scaling
      ggplot(data = x, aes(x = group2, y = outcome, fill = group1)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymax = outcome + ci, ymin = outcome - ci), position = dodge, width = 0.2) +
        scale_fill_manual(values = alpha(c("royalblue", "red"), 0.6)) +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        ggtitle(paste0("Panel D: ", input$paneld)) +
        theme_classic() +
        theme(
          plot.margin = unit(c(1, 1, 4, 1), "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title=element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          legend.position = c(0.8, 0.95),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18)
        ) 
    } else {
      ggplot(data = x, aes(x = group2, y = outcome, fill = group1)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymax = outcome + ci, ymin = outcome - ci), position = dodge, width = 0.2) +
        scale_fill_manual(values = alpha(c("royalblue", "red"), 0.6)) +
        ggtitle(paste0("Panel D: ", input$paneld)) +
        theme_classic() +
        theme(
          plot.margin = unit(c(1, 1, 4, 1), "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title=element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          legend.position = c(0.8, 0.95),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18)
        ) 
      
    }
    
  })
  
  
}

thematic_shiny(font = "auto")
shinyApp(ui = ui, server = server)