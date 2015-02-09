# Dissimilarity Matrix

distance_quant = function(M, euclidean = TRUE, cor = FALSE, abs_cor = FALSE)
{
	if(euclidean)# Euclidean dissimilarity matrix
	{
		return(dist_matrix_e =  dist(M,method="Euclidean"))
	}
	# Correlation & Absolute Correlation distance matrix
	if(cor)
	{
		cor_matrix = cor(t(M))
		dist_matrix_cor = 1 - cor_matrix
		return (dist_matrix_cor = as.dist(dist_matrix_cor))
	}
	if(abs_cor)
	{
		cor_matrix = cor(t(M))
		dist_matrix_cor_abs = 1 - abs(cor_matrix)
		return (dist_matrix_cor_abs = as.dist(dist_matrix_cor_abs))
	}
}

distance_ts = function(M, cc = TRUE, dtw = FALSE)
{
	if(cc)# Cross-correlation
	{
		return(dist_matrix_cc = cc(M))
	}
	if(dtw)# DTW(Distance Time Warping) dissimilarity matrix
	{
		return(dist_matrix_dtw =  dist(M,method="DTW"))
	}
}

distance_qual = function(M, correlation = TRUE, pearson = FALSE)
{
	if(correlation)# Correlation
	{
		return(dist_matrix_c = dist(M,method="correlation"))
	}
	if(pearson)# Pearson
	{
		return(dist_matrix_p =  dist(M,method="Pearson"))
	}
}

distance_bin = function(M, binary = TRUE, jaccard = FALSE)
{
	if(binary)# Binary
	{
		return(dist_matrix_bin = dist(M,method="binary"))
	}
	if(jaccard)# Jaccard
	{
		return(dist_matrix_j =  dist(M,method="Jaccard"))
	}
}
