

################
# UI mithilfe von fluidPage
ui <- fluidPage(
   # Hintergrund
  setBackgroundImage(
    src = "https://www.bonvinitas.com/images/vinho-verde_Typ_web.jpg"
  ),
  # Titel der App
  titlePanel(title = span("Alkoholgehalt zu Qualitaet ratio",
                          style = "background-color: white; font-size: 28px; transparent: false")),

  # Spalte mit dem Kreis und Schieberegler
  sidebarLayout(

    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(

      # Eine Ueberschrift mit Linie darunter
      h3("Fuegen Sie Ihre gewuenschten Daten ein:",align="left"),
      hr(style="height: 1px; background: black"),

      # Ein Kreis mit der Moeglichkeit die Qualitaet des Weins zu waehlen
      # der Slider geht hier von 0 (min) bis 10 (max),
      # die Voreinstellung ist 5 (value)
      sliderInput(inputId = "alcohol",
                  label = "Waehlen Sie Ihren gewuenschten Alkoholgehalt (0-15)",
                  min = 0,
                  max = 15.2,
                  value = 10
      ),

      # Die Wein dichte als numerische Eingabe
      sliderInput(inputId = "density",
                  label = "Waehlen Sie Ihre gewuenschte Dichte",
                  min = 0.987110,
                  max = 1.03898,
                  value = 0.99
      ),
    
    ),
    

    # der Hauptbereich der Nutzeroberflaeche fuer die Ausgabe der Ergebnisse
    mainPanel(

      # Ausgabe des Histogramms fuer Rotwein
      plotOutput(outputId = "VerteilungR"),
      
      # Ausgabe der PrognoseR
      htmlOutput("PrognoseR"),
      
      
      #Ausgabe des zweiten Histogramms fuer Weisswein
      plotOutput(outputId = "VerteilungW"),

      # Ausgabe der PrognoseW
      htmlOutput("PrognoseW"),
      

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

    # zunaechst die Werte fuer quality, alcohol und density
    WeinR.neu[1,"alcohol"] <- input$alcohol
    WeinR.neu[1,"density"] <- input$density

    # Berechne die Prognosen fuer WeinR.neu
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    X.neu <- model.matrix(quality ~ alcohol + density, WeinR.neu)
    X.neu <- X.neu[,-1]   # entferne den Intercept

    prognosevektor <- predict(modelR,X.neu)$predictions
    prog <- prognosevektor[1]

    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)

    # der errechnete Wert soll als Ergebnis der Funktion zurueckgegeben werden
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
    X <- model.matrix(quality ~ alcohol + density, WeinR)
    X <- X[,-1]   # entferne den Intercept
    y <- WeinR[,"quality"]
    abweichungen <- y-predict(modelR,X)$predictions

    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert die Qualitaet des Rotweins mit den gewaehlten werten.
    hist(prog+abweichungen, col = "red", main = "Verteilung der Qualitaetsstufen im Rotwein",xlim=c(0,10))

  })
  
  # Definition einer Textausgabe mit dem namen output$PrognoseR
  # In dieser Textausgabe soll der in der Funktion prognose()
  # errechnete Prognosewert ausgegeben werden
  output$PrognoseR <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognoseR()
    
    # der Prognosewert wird noch auf 0 Stellen hinter dem Komma (digits=0) gerundet.
    prog <- round(prog,digits=0)
    
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog sowie einem Stueck HTML fuer einen farbigen Text
    Ausgabe <- paste("<font color=\"#FF0000\"><b>","Durchschnittliche Qualitaet im Rotwein: ", prog, "</b></font>")
  })
  
  # Folgende Funktion berechnet die Prognose fuer die eingegeben Werte
  prognoseW <- reactive({
    
    # Speichere die Daten unter neuem Namen
    WeinW.neu <- WeinW
    
    # Ersetze die erste Zeile im neuen Datensatz nun mit den neuen, eingegebenen Werten
    
    # zunaechst die Werte fuer quality, alcohol und density
    WeinW.neu[1,"alcohol"] <- input$alcohol
    WeinW.neu[1,"density"] <- input$density
    
    # Berechne die Prognosen fuer WeinW.neu
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    X.neu <- model.matrix(quality ~ alcohol + density, WeinW.neu)
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
    X <- model.matrix(quality ~ alcohol + density, WeinW)
    X <- X[,-1]   # entferne den Intercept
    y <- WeinW[,"quality"]
    abweichungen <- y-predict(modelW,X)$predictions
    
    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert die Qualitaet des Weissweins mit den gewaehlten werten.
    hist(prog+abweichungen, col = "white", main = "Verteilung der Qualitaetsstufen im Weisswein",xlim=c(0,10))
    
  })
  
  # Definition einer Textausgabe mit dem namen output$PrognoseR
  # In dieser Textausgabe soll der in der Funktion prognose()
  # errechnete Prognosewert ausgegeben werden
  output$PrognoseW <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognoseW()
    
    # der Prognosewert wird noch auf 0 Stellen hinter dem Komma (digits=0) gerundet.
    prog <- round(prog,digits=0)
    
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog sowie einem Stueck HTML fuer einen farbigen Text
    Ausgabe <- paste("<font color=\"#FFFFFF\"><b>","Durchschnittliche Qualitaet im Weisswein: ", prog, "</b></font>")
  })

}


# Aufruf der App-Funktionen
###############

shinyApp(ui = ui, server = server)

###############
