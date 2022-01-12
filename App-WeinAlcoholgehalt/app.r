

################
# Im folgenden Abschnitt wird das User Interface (UI) definiert
ui <- fluidPage(

  # Titel der App
  titlePanel("Qualitaet zu alcohol ratio"),

  # Layout fuer die Eingaben in die App und die Ausgaben
  sidebarLayout(

    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(

      # Eine Ueberschrift mit Linie darunter
      h3("Fuegen Sie Ihre gewuenschten Daten ein:",align="left"),
      hr(style="height: 1px; background: black"),

      # Ein Slider fuerr die Qualitaet des Weins
      # der Slider geht hier von 0 (min) bis 10 (max),
      # die Voreinstellung ist 5 (value)
      sliderInput(inputId = "quality",
                  label = "Waehlen Sie die Qualitaet (0-10)",
                  min = 0,
                  max = 10,
                  value = 5
      ),

      # Die Wein dichte als numerische Eingabe
      # die Auswahlmoeglichkeiten sind "Rotwein"und "Weisswein",
      # die entsprechende Zuordnung mit Zahlen 1, 2 und 3 sind wie im Datensatz,
      # die Voreinstellung ist 1 (selected) - also eine "normale Lage"
      sliderInput(inputId = "density",
                  label = "Waehlen Sie Ihre gewuenschte Dichte",
                  min = 0.987110,
                  max = 1.03898,
                  value = 0.99
      ),
    
    ),
    

    # der Hauptbereich der Nutzeroberflaeche fuer die Ausgabe der Ergebnisse
    mainPanel(

      imageOutput("img.jpg"),

      # Ausgabe des Histogramms
      plotOutput(outputId = "VerteilungR"),
      
      # Ausgabe der PrognoseR
      textOutput("PrognoseR"),
      
      #Ausgabe des zweiten Histogramms
      plotOutput(outputId = "VerteilungW"),

      # Ausgabe der PrognoseW
      textOutput("PrognoseW"),

    )
  )
)
############


server <- function(input, output) {

  # Innerhalb dieser Funktion werden die Bilder fuer die Ausgabe
  # erzeugt und die Ergebnisse berechnet
  # Folgende Funktion berechnet die Prognose fuer die eingegeben Werte
  prognoseR <- reactive({

    # Speichere die Daten unter neuem Namen
    WeinR.neu <- WeinR

    # Ersetze die erste Zeile im neuen Datensatz nun mit den neuen, eingegebenen Werten

    # zunächst die Werte fuer quality, alcohol und density
    WeinR.neu[1,"quality"] <- input$quality
    WeinR.neu[1,"density"] <- input$density

    # Berechne die Prognosen fuer WeinR.neu
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    X.neu <- model.matrix(alcohol ~ quality + density, WeinR.neu)
    X.neu <- X.neu[,-1]   # entferne den Intercept

    prognosevektor <- predict(modelR,X.neu)$predictions
    prog <- prognosevektor[1]

    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)

    # der errechnete Wert soll als Ergebnis der Funktion zurückgegeben werden
    prog
  })

  # diese Funktion erzeugt das Histogramm und speichert es als Ausgabebild
  # mit dem Namen output$VerteilungR
  output$VerteilungR <- renderPlot({

    # die errechnete Prognose aus der oben geschriebenen Funktion prognose()
    prog <- prognoseR()

    # Speichere die Daten der Einflussvariablen in ein Objekt X
    # und die Daten der Zielvariable in y.
    # Berechne dann die Abweichungen zwischen den Prognosen und den realen Werten
    X <- WeinR[,c("quality","alcohol","density")]
    X <- model.matrix(alcohol ~ quality + density, WeinR)
    X <- X[,-1]   # entferne den Intercept
    y <- WeinR[,"alcohol"]
    abweichungen <- y-predict(modelR,X)$predictions

    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert den Gehalt des alkohols im Rotwein.
    hist(prog+abweichungen, col = "red", main = "Verteilung des alcohol gehalts im Rotwein",xlim=c(0,15))

  })
  
  # Definition einer Textausgabe mit dem namen output$PrognoseR
  # In dieser Textausgabe soll der in der Funktion prognose()
  # errechnete Prognosewert ausgegeben werden
  output$PrognoseR <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognoseR()
    
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog
    Ausgabe <- paste("Durchschnittlicher Alkohol gehalt in Rotwein: ", prog)
  })
  
  # Folgende Funktion berechnet die Prognose fuer die eingegeben Werte
  prognoseW <- reactive({
    
    # Speichere die Daten unter neuem Namen
    WeinW.neu <- WeinW
    
    # Ersetze die erste Zeile im neuen Datensatz nun mit den neuen, eingegebenen Werten
    
    # zunächst die Werte fuer quality, alcohol und density
    WeinW.neu[1,"quality"] <- input$quality
    WeinW.neu[1,"density"] <- input$density
    
    # Berechne die Prognosen fuer WeinW.neu
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    X.neu <- model.matrix(alcohol ~ quality + density, WeinW.neu)
    X.neu <- X.neu[,-1]   # entferne den Intercept
    
    prognosevektor <- predict(modelW,X.neu)$predictions
    prog <- prognosevektor[1]
    
    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)
    
    # der errechnete Wert soll als Ergebnis der Funktion zurueckgegeben werden
    prog
  })
  
  # diese Funktion erzeugt das Histogramm und speichert es als Ausgabebild
  # mit dem Namen output$VerteilungW
  output$VerteilungW <- renderPlot({
    
    # die errechnete Prognose aus der oben geschriebenen Funktion prognose()
    prog <- prognoseW()
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    # und die Daten der Zielvariable in y.
    # Berechne dann die Abweichungen zwischen den Prognosen und den realen Werten
    X <- WeinW[,c("quality","alcohol","density")]
    X <- model.matrix(alcohol ~ quality + density, WeinW)
    X <- X[,-1]   # entferne den Intercept
    y <- WeinW[,"alcohol"]
    abweichungen <- y-predict(modelW,X)$predictions
    
    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert den Gehalt des alkohols im Weisswein
    hist(prog+abweichungen, col = "white", main = "Verteilung des alcohol gehalts im Weisswein",xlim=c(0,15))
    
  })
  
  # Definition einer Textausgabe mit dem namen output$PrognoseR
  # In dieser Textausgabe soll der in der Funktion prognose()
  # errechnete Prognosewert ausgegeben werden
  output$PrognoseW <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognoseW()
    
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog
    Ausgabe <- paste("Durchschnittlicher Alkohol gehalt in Weisswein: ", prog)
  })

}


# Aufruf der App-Funktionen
###############

shinyApp(ui = ui, server = server)

###############
