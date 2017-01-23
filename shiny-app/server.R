require(cluster)
require(mlbench)
require(RColorBrewer)

# Implementacja algorytmu k-medoid (PAM Algorithm)
# metrics: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
kmedoids <- function (data, k, metric="euclidean") {
  medoids_idxs <- sample(nrow(data), k);
  dist_obj <- dist(rbind(data), method = metric)
  dist_mat <- as.matrix(dist_obj)
  
  repeat {
    clusters <- apply(dist_mat[, medoids_idxs], 1, which.min)
    configuration_changed <- FALSE;
    
    for(c in 1:k) {
      current_cluster <- clusters==c
      if (sum(current_cluster) == 1) next;
      
      current_cost <- sum(dist_mat[medoids_idxs[c], current_cluster]);
      costs_of_new_medoids <- apply(dist_mat[current_cluster, current_cluster], 1, sum)
      
      if (min(costs_of_new_medoids) < current_cost) {
        medoids_idxs[c] <-  as.numeric(names(which.min(costs_of_new_medoids)));
        configuration_changed <- TRUE;
      }
    }
    if(configuration_changed == FALSE) {
      break;
    }
  }
  list(medoids = data[medoids_idxs,],
       clustering = clusters,
       dist = dist_obj,
       data = data);
}
  
function(input, output, session) {
  
  # dummy variable to invalidate input and run kmedoids or generate data one more time 
  dummy <- reactiveValues(run = 0, data = 0)
  observeEvent(input$runAlgorithmOneMoreTime, {dummy$run <- dummy$run + 1})
  observeEvent(input$generateDataOneMoreTime, {dummy$data <- dummy$data + 1})
  
  selectedData <- reactive({
    dummy$data
    p <- NULL
    if(input$dataset == "2dnormals") {
      p <- mlbench.2dnormals(input$n, 2, r=1.6)
    } else if (input$dataset == "shapes") {
      p <-mlbench.shapes(input$n)
    } else if (input$dataset == "cassini") {
      p <- mlbench.cassini(input$n)
    } else if (input$dataset == "smiley") {
      p <- mlbench.smiley(input$n, sd1 = 0.15, sd2 = 0.08)
    } else if (input$dataset == "spirals") {
      p <-mlbench.spirals(input$n, 1.5, 0.035)
    } else{
      p <- mlbench.2dnormals(input$n, 2, r=1.6)
    }
    p
  })

  kmed <- reactive({
    dummy$run
    kmedoids(selectedData()$x, input$k, input$metric)
  })
  
  output$nominal_plot <- renderPlot({
    plot(selectedData(), main="Wykres nominalnego przyporządkowania punktów do klastrów", xlab="x", ylab="y")
  })
  
  output$plot <- renderPlot({
    colors <- brewer.pal(9, "Set1")
    
    k <- length(unique(kmed()$clustering));
    plot(kmed()$data, type = "n", main="Wykres przyporządkowania punktów do klastrów przez alg K-median", xlab="x", ylab="y")
    
    for(i in 1:k) {
      points(kmed()$data[kmed()$clustering==i, ], col = colors[i]);
    }
    points(kmed()$medoids, col = "black", pch = 4, cex = 2, lwd = 2)
  })
  
  output$silhouette <- renderPlot({
    plot(silhouette(kmed()$clustering, kmed()$dist), main="Wykres Silhouette")
  })
}