# this function takes a dataframe and a categorical variable as input 
# it returns the dataframe with the dummy variable for all the categories added to the dataframe
dummy = function(dataframe, variable, name){
	for(t in unique(variable)) {
		dataframe[paste(name,t,sep="")] <- factor(ifelse(variable==t,1,0))
	}
	return (list(dataframe=dataframe))
}