metrics <- c("euclidean", "maximum", "manhattan", "canberra")
datasets <- c("2dnormals", "shapes", "cassini", "smiley", "spirals")

pageWithSidebar(
  headerPanel('Algorytm K-median'),
  sidebarPanel(
    selectInput('dataset', 'Zbiór danych', datasets),
    sliderInput("n", "Liczna punktów (n):", min = 60, max = 250, value = 100),
    actionButton('generateDataOneMoreTime', 'Wygeneruj dane jeszcze raz'),
    hr(),
    sliderInput("k", "Liczba klastrów (k):", min = 2, max = 9, value = 3),
    selectInput('metric', 'Miara niepodobieństwa', metrics),
    actionButton('runAlgorithmOneMoreTime', 'Oblicz jeszcze raz')
  ),
  mainPanel(
    plotOutput('nominal_plot'),
    plotOutput('plot'),
    plotOutput('silhouette')
  )
)
