require(cluster)
require(mlbench)

# Implementacja algorytmu k-medoid (PAM Algorithm)
# metrics: "euclidean", "maximum", "manhattan", "canberra", "binary"
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


plot_kmed <- function(kmed) {
  colors <- c("black", "blue", "orange", "yellow");
  k <- length(unique(kmed$clustering));
  plot(kmed$data, type = "n", main="Wykres przyporządkowania punktów do klastrów przez alg K-median", xlab="x", ylab="y")
  for(i in 1:k) {
    points(kmed$data[kmed$clustering==i, ], col = colors[i]);
  }
  points(kmed$medoids, col = "red", pch = 19)
}

# Wizualizacja wyników  

# 4 grupy separowalne grupy
k <- 4
p <- mlbench.hypercube(n=100, d=2)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")


# dwie grupy blisko siebie
k <- 2
p <- mlbench.2dnormals(200, 2, r=1.6)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")


#  4 różne kształty
k <- 4
p <-mlbench.shapes(n=200)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")

# Trzy kształty o różnych proporcjach
k <- 3
p <- mlbench.cassini(250)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")


# Emoticon z 4 grup
k <- 4
p <- mlbench.smiley(140, sd1 = 0.15, sd2 = 0.08)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")


# Dwie spirale z szumem
k <- 2
p <-mlbench.spirals(200, 1.5, 0.035)
plot(p)

kmed <- kmedoids(p$x, k)
plot_kmed(kmed)
plot(silhouette(kmed$clustering, kmed$dist),  main="Wykres Silhouette")


