# Cross-Correlation
# input = observationsXtime matrix

cc = function(M)
{
	dis_mat=matrix(0,dim(M)[1],dim(M)[1])	
	for(i in 1:dim(M)[1])
	{
		for(j in 1:dim(M)[1])
		{
			if(i!=j)
			{
				dis_mat[i,j] =  max(c(ccf(M[i,], M[j,], type = "correlation", plot = FALSE)$acf))
			}
		}
	}
	dis_mat = zero_dis(M, dis_mat)
	return(as.dist(dis_mat))
}

# this function takes a sales matrix(productXtime) & a dissimilarity matrix as input
# it codes all the prodcts having zero sales as similar and all other products as dissimilar wrt to products having zero sales
zero_dis = function (M, dis_mat)
{
	rowI = c()
	rowS = rowSums(M)
	
	# tag indices with row sum zero i.e. zero sales
	for(i in 1:dim(M)[1]){
	if(rowS[i] == 0){
		rowI = cbind(rowI, i)
		}
	}
	
	for(i in rowI){
		for(j in rowI){
			if(i!=j)
			dis_mat[i,j] = 0	
		}
	}
	
	dis_mat[is.nan(dis_mat)]= 1
	dis_mat[is.na(dis_mat)]= 1
	return(dis_mat)
}
