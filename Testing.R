iris2 <- iris %>%
    select(Sepal.Length, Sepal.Width)


iris2 <- princomp(iris2)
iris2 <- iris2$scores %>%
    as.data.frame() %>%
    select(Comp.1, Comp.2)

rand <- sample(1:nrow(iris2), 4)

clusters <- iris2[rand,]

cluster_vec <- c()
last_vec <- c(0)
iter <- 0
stop <- 0

iter <- iter + 1

for (i in 1:nrow(iris2)) {

    dist <- iris2[2,] %>%
        rbind(clusters) %>%
        dist()

    i_cluster <- dist[1:4] %>%
        which.min()

    cluster_vec[i] <- i_cluster

}

last_vec <- cluster_vec

clusters <- iris2 %>%
    cbind(cluster_vec) %>%
    group_by(cluster_vec) %>%
    summarize_all(mean)

clusters <- clusters[, -1]


iris2 %>%
    cbind(cluster_vec) %>%
    count(cluster_vec) %>%
    pull(n)











iris2 <- iris %>%
    select(Sepal.Length, Sepal.Width)

set.seed(945)
rand <- sample(1:nrow(iris2), 3)

clusters <- iris2[rand,]

cluster_vec <- c()
last_vec <- c(0)
iter <- 1
stop <- 0


all_df <- data.frame()
all_center_df <- data.frame()


while (stop == 0) {

    for (i in 1:nrow(iris2)) {

        dist <- iris2[i,] %>%
            rbind(clusters) %>%
            dist()

        i_cluster <- dist[1:3] %>%
            which.min()

        cluster_vec[i] <- i_cluster
    }

    if (all(cluster_vec == last_vec)) {
        stop <-  1
    }

    last_vec <- cluster_vec

    df <- iris2 %>%
        add_column(cluster = cluster_vec) %>%
        add_column(iteration = iter)

    center_df <- clusters %>%
        add_column(cluster = c(1:3)) %>%
        add_column(iteration = iter)

    all_df <- rbind(all_df, df)

    all_center_df <- rbind(all_center_df, center_df)

    clusters <- iris2 %>%
        cbind(cluster_vec) %>%
        group_by(cluster_vec) %>%
        summarize_all(mean)

    clusters <- clusters[, -1]

    df <- data.frame()
    center_df <- data.frame()
    iter <- iter + 1

}

return(list(all_df, all_center_df ))
