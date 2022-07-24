iris2 <- iris %>%
    select(Sepal.Length, Sepal.Width)


dat <- princomp(iris2)
dat <- dat$scores %>%
    as.data.frame() %>%
    select(Comp.1, Comp.2)

rand <- sample(1:nrow(iris2), 4)

clusters <- iris2[rand,]

cluster_vec <- c()
last_vec <- c(0)
d_mins <- c()
iter <- 0
stop <- 0

iter <- iter + 1

for (i in 1:nrow(iris2)) {

    dist <- iris2[2,] %>%
        rbind(clusters) %>%
        dist()

    i_cluster <- dist[1:4] %>%
        which.min()

    d_min <- dist[1:4] %>%
        min()

    cluster_vec[i] <- i_cluster

    d_mins[i] <- d_min

}
