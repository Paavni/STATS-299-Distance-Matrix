myclusGap = function (x, dist_mat, FUNcluster, K.max, B = 100, verbose = interactive(), tech="hclust") 

{
    stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 
        2, (n <- nrow(x)) >= 1, (p <- ncol(x)) >= 1)
    if (B != (B. <- as.integer(B)) || (B <- B.) <= 0) 
        stop("'B' has to be a positive integer")
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    ii <- seq_len(n)
    W.k <- function(X, dist_mat, kk) {
        clus <- if (kk > 1)
        	{ 	
        		if(tech == "hclust"){
        			hc = hclust(dist_mat, method = "average")
            		FUNcluster(hc, kk)$cluster
            		}
            	if(tech == "pam"){
        			FUNcluster(dist_mat, kk)$cluster
            		}		
            }
        else rep.int(1L, nrow(X))
        0.5 * sum(vapply(split(ii, clus), function(I) {
            xs <- X[I, , drop = FALSE]
            sum(dist(xs)/nrow(xs))
        }, 0))
    }
    logW <- E.logW <- SE.sim <- numeric(K.max)
    if (verbose) 
        cat("Clustering k = 1,2,..., K.max (= ", K.max, "): .. ", 
            sep = "")
    for (k in 1:K.max) logW[k] <- log(W.k(x, dist_mat, k))
    if (verbose) 
        cat("done\n")
    xs <- scale(x, center = TRUE, scale = FALSE)
    m.x <- rep(attr(xs, "scaled:center"), each = n)
    V.sx <- svd(xs)$v
    rng.x1 <- apply(xs %*% V.sx, 2, range)
    logWks <- matrix(0, B, K.max)
    if (verbose) 
        cat("Bootstrapping, b = 1,2,..., B (= ", B, ")  [one \".\" per sample]:\n", 
            sep = "")
    for (b in 1:B) {
        z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1], 
            max = M[2]), nn = n)
        z <- tcrossprod(z1, V.sx) + m.x
        for (k in 1:K.max) {
            logWks[b, k] <- log(W.k(z, dist_mat, k))
        }
        if (verbose) 
            cat(".", if (b%%50 == 0) 
                paste(b, "\n"))
    }
    if (verbose && (B%%50 != 0)) 
        cat("", B, "\n")
    E.logW <- colMeans(logWks)
    SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
    structure(class = "clusGap", list(Tab = cbind(logW, E.logW, 
        gap = E.logW - logW, SE.sim), n = n, B = B, FUNcluster = FUNcluster))
}

#hc with distance matrix
clusDistMat = function(hc, k) {
z_ct = cutree(hc,k)
return(list(cluster = z_ct))
}

#pam with distance matrix
pamDistMat = function(dist_matrix, k) {
p = pam(dist_matrix, k, diss = TRUE)
return(list(cluster = p$clustering))
}