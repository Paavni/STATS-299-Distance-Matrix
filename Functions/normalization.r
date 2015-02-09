normalization = function(M, scale = FALSE, one_zero = TRUE)	
{
	if(one_zero)
	{
		return ((M-min(M))/(max(M)-min(M)))
	}
	if(scale)
	{
		return (scale(M))
	}
}