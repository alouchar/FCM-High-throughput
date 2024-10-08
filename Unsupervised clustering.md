---
title: "Flow Cytometry Pipeline for automated traits analysis"
author: Arnaud Louchart & Dedmer Van de Waal
date: 02 October 2023
output: html_document
---

Several methods are available to cluster your datasets.

-   k-means:

    k-means is a centroid-based algorithn or a distance-based algorithm. k-means group similar data points together and discover underlying patterns. The centroid is the location representing the center of the cluster. The algorithm identifies *k* number of centroids and then allocates every point to the nearest cluster while keeping the centroids as small as possible.

-   Partition Around Medoids (PAM):

    PAM is a medoids-based algorithm or a distance-based algorithm. k-means group similar data points together and discover underlying patterns. The medoid is the location representing the center of the cluster.

-   Hierarchical Density-Based Spectral Clustering of Applications with Noise (HDBSCAN):

    HDSCAN performs DBSCAN at different 𝜺 values and integrates the result to find a clustering that the best stability over 𝜺. HDBSCAN is ideal for explanatory data analysis.

-   Gaussian Mixture Model:

    To be developped
