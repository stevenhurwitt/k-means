import os
import math
import numpy as np
import pandas as pd

os.chdir('/Volumes/YPNHS/Python Code/ML Classes')

#class to implement k means clustering
#takes data matrix X, num clusters k & tolerance
class Kmeans():

    def __init__(self, X, k, tol):
        self.X = X
        self.k = k
        self.tol = tol
        self.centroids = self.X[np.random.choice(self.X.shape[0], 3, replace=False)]
        self.clusters = self.cluster_assign(self.X)

    def distance(self, vec1, vec2):
        self.vec1 = vec1
        self.vec2 = vec2
        return sum((self.vec2-self.vec1)**2)
        
    def cluster_assign(self, X):
        m = self.X.shape[0]
        distances = []
        for i in range(0,m):
            dist_per_obs = []
            
            for j in range(0, self.k):
                dist_per_obs.append(self.distance(self.X[i], self.centroids[j]))
            distances.append(dist_per_obs)
            
        self.clusters = np.argmin(distances, axis=1)
        return self.clusters
                               

    def cluster_update(self):
        new_clusters = self.cluster_assign()
        new_centroids = []
        for i in range(0, self.k):
            new_centroids.append(self.X[new_clusters == i].mean(axis=0))

        if (self.distance(self.centroids, new_centroids) < self.tol):
            print("centroids finalized")
            quit
        else:
            self.centroids = np.array(new_centroids)
###########################

def main():
    iris = pd.read_csv("iris.csv", delimiter = ",", header = 0)
    iris_vars = np.array(iris.columns)
    iris = np.array(iris)

    X = iris[:,0:4]
    y = iris[:,4]
    labels = np.unique(y)

    k_means = Kmeans(X, 3, .001)
    print(k_means.clusters)

if __name__ == '__main__':
    main()
