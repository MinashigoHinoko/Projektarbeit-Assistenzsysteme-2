

################
# Im folgenden Abschnitt wird das User Interface (UI) definiert
ui <- fluidPage(

  # Titel der App
  titlePanel("Qualit�t zu alcohol ratio"),

  # Layout für die Eingaben in die App und die Ausgaben
  sidebarLayout(

    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(

      # Eine Überschrift mit Linie darunter
      h3("Qualit�t:",align="left"),
      hr(style="height: 1px; background: black"),

      # Ein Slider für die Qualit�t des Weins
      # der Slider geht hier von 0 (min) bis 10 (max),
      # die Voreinstellung ist 5 (value)
      sliderInput(inputId = "quality",
                  label = "Qualit�t von 0-10",
                  min = 0,
                  max = 10,
                  value = 5
      ),

      # Der alkohol gehalt als numerische Eingabe
      # die Werte gehen von 0 (min) bis 15 (max) in 0,5 schritten (step)
      # die Voreinstellung ist 10 (value)
      numericInput(inputId="alcohol",
                    label="Alkoholgehalt:",
                        value = 10,
                        min=0,max=15,step=0.5
      ),

      # Die Wein dichte als numerische Eingabe
      # die Auswahlmöglichkeiten sind "Rotwein"und "Wei�wein",
      # die entsprechende Zuordnung mit Zahlen 1, 2 und 3 sind wie im Datensatz,
      # die Voreinstellung ist 1 (selected) - also eine "normale Lage"
      sliderInput(inputId = "density",
                  label = "Die dichte des Weins",
                  min = 0.987110,
                  max = 1.03898,
                  value = 0.99
      ),

      # eine Überschrift für die weiteren Weinarten
      #h5(strong("Weinarten:",align="left"),

      # die weiteren drei Ausstattungsmerkmale (kueche, bad, zh) mit
      # Boxen zum Anklicken
      # die Voreinstellung ist jeweils FALSE (value), das heißt, es ist als
      # Voreinstellung keine Box angeklickt
      #checkboxInput(inputId="luxuskueche", label="Luxusküche", value = FALSE),
      #checkboxInput(inputId="luxusbad", label="Luxusbad", value = FALSE),
      #checkboxInput(inputId="zentralheizung", label="mit Zentralheizung", value = FALSE),

    ),

    # der Hauptbereich der Nutzeroberfläche für die Ausgabe der Ergebnisse
    mainPanel(

      # Ausgabe des Histogramms
      plotOutput(outputId = "Verteilung"),

      # Ausgabe der Prognose
      textOutput("Prognose"),

    )
  )
)
############


server <- function(input, output) {

  # Innerhalb dieser Funktion werden die Bilder für die Ausgabe
  # erzeugt und die Ergebnisse berechnet

  # Folgende Funktion berechnet die Prognose für die eingegeben Werte
  prognose <- reactive({

    # Speichere die Daten unter neuem Namen
    Daten.neu <- Daten

    # Ersetze die erste Zeile im neuen Datensatz nun mit den neuen, eingegebenen Werten

    # zunächst die Werte für quality, alcohol und density
    Daten.neu[1,"quality"] <- input$quality
    Daten.neu[1,"alcohol"] <- input$alcohol
    Daten.neu[1,"density"] <- input$density

    # Berechne die Prognosen für Daten.neu
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    X.neu <- model.matrix(alcohol ~ quality + density, Daten.neu)
    X.neu <- X.neu[,-1]   # entferne den Intercept

    prognosevektor <- predict(model,X.neu)$predictions
    prog <- prognosevektor[1]

    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)

    # der errechnete Wert soll als Ergebnis der Funktion zurückgegeben werden
    prog
  })

  # diese Funktion erzeugt das Histogramm und speichert es als Ausgabebild
  # mit dem Namen output$Verteilung
  output$Verteilung <- renderPlot({

    # die errechnete Prognose aus der oben geschriebenen Funktion prognose()
    prog <- prognose()

    # Speichere die Daten der Einflussvariablen in ein Objekt X
    # und die Daten der Zielvariable in y.
    # Berechne dann die Abweichungen zwischen den Prognosen und den realen Werten
    X <- Daten[,c("quality","alcohol","density")]
    X <- model.matrix(alcohol ~ quality + density, Daten)
    X <- X[,-1]   # entferne den Intercept
    y <- Daten[,"alcohol"]
    abweichungen <- y-predict(model,X)$predictions

    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert die bandbreite der Mieten für diese Wohnung
    hist(prog+abweichungen, col = "blue", main = "Verteilung des alcohol gehalts",xlim=c(0,15))

  })

  # Definition einer Textausgabe mit dem namen output$Prognose
  # In dieser Textausgabe soll der in der Funktion prognose()
  # errechnete Prognosewert ausgegeben werden
  output$Prognose <- renderText({

    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognose()

    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog
    Ausgabe <- paste("Durchschnittlicher Alcohol gehalt: ", prog," ???")
  })

}



# Aufruf der App-Funktionen
###############

shinyApp(ui = ui, server = server)

###############
