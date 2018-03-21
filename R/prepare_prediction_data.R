#' prepare_prediction_data
#'
#' @param object lassovar estimation object
#' @param fc.data model data.frame
#'
#' @return model data.frame augmented with lag columns
#'

prepare_prediction_data <- function(object, fc.data){
	
	dd <- dim(as.matrix(fc.data))
	
	var_dim <- ifelse(1 %in% dd, nrow(coef(object)), nrow(coef(object))-1)
	
	if(!("date" %in% colnames(fc.data))){
		fc.data <- cbind.data.frame(data.frame(date = 1:nrow(fc.data)), fc.data)	
	}
	
	if(var_dim > ncol(fc.data)){
		data_for_prediction <- fc.data %>% 
			do({
				init_data <- .
				out_data <- init_data
				colnames(out_data)[-1] <- paste0("1L_",colnames(init_data)[-1])
				for(lag_ in 2:number_of_lags){
					mutation_text <- c("date",sprintf("lag(%s,%d)",colnames(select(init_data,-date)),lag_ - 1))
					mutation_names <- c("date",sprintf("%dL_%s",lag_,colnames(select(init_data,-date))))
					mutation_text <- setNames(object = as.list(mutation_text), nm = mutation_names)
					out_data <- inner_join(out_data, transmute_(init_data, .dots = mutation_text))
				}
				out_data
			})  
	} else {
		data_for_prediction <- fc.data
	}
	
	data_for_prediction <- data_for_prediction %>% 
		tidyr::drop_na() %>% 
		select(-date) %>% 
		as.matrix()
	
	return(data_for_prediction)
}