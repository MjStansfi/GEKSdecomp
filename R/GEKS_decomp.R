#' Decomposition of GEKS-tornqvist
#'
#' Take smiliar inputs as the GEKS function and return the decomposition
#' on the multilateral scale.
#'
#' @param times vector of the times at which price observations were made. \cr
#' NOTE: \code{times} must be of class Date or numeric.
#' @param price vector of log of prices at the given time
#' @param id vector of distinct identification number of consumer goods
#' @param weight vector of expenditure weights used in the regressions
#' @param custom_time either empty (will assume latest periods) or a vector of length
#' two relating to times compared where [1] is the 'start_time' time and [2] is the 'to' time
#' e.g Comparing contribution start_time "1973-04-01" to "1973-05-01" would be c("1973-04-01", "1973-05-01")
#' @param window_length optional. Single number for length of window for the data that
#' regressions are fit on. Note if window_length is present it assumes custom time is latest two periods
#' in window.
#' @return A dataframe containing the numeric contribution (\code{contrib}) of every price observation
#' in the window, the product of which will (roughly) equal the index movement between the two time periods. \code{p_contrib} is
#' the standardised percentage contribution of the observation, the sum of which will equal 1.
#' @examples
#' library(TPDdecomp)
#'
#' contributions <- with(
#'     turvey,
#'     GEKS_decomp(times = month,
#'                price = price,
#'                id = commodity,
#'                weight = price*quantity,
#'                custom_time = as.Date(c("1973-04-30","1973-05-31")),
#'                window_length = NULL)
#'  )
#' 
#'
#' str(contributions)
#'Classes ‘data.table’ and 'data.frame':	5 obs. of  5 variables:
#'$ from     : Date, format: "1973-04-30" "1973-04-30" "1973-04-30" ...
#'$ to       : Date, format: "1973-05-31" "1973-05-31" "1973-05-31" ...
#'$ id       : Factor w/ 5 levels "Apples","Grapes",..: 1 2 3 4 5
#'$ contrib  : num  1.035 0.998 1.017 1.034 1
#'$ p_contrib: num  0.411 -0.02 0.206 0.403 0
#'
#' @import data.table
#' @export
GEKS_decomp <- function(times,
                        price,
                        id,
                        weight,
                        custom_time = c(),
                        window_length = NULL){
  
  #Check input variables are as required
  c(times, price, id, weight, custom_time, window_length) %=%
    check_inputs (times = times,
                  price = price,
                  id = id,
                  weight = weight,
                  custom_time = custom_time,
                  window_length = window_length)
  
  #Check if custom_times are in the times vector
  if(length(custom_time)!=0 & !all(as.character(custom_time) %in% as.character(times))){
    stop("custom_time is not in times?")
  }
  
  prices_df <- data.table(times = times,
                          price = price,
                          weight = weight,
                          id = as.character(id),
                          key = "times")
  
  # It is essential that the data frame is sorted by date and ID
  setorderv(prices_df, c("times", "id"), c(1, 1))
  
  all_times <- unique(prices_df$times)
 
  if(length(custom_time)!=0){
    
    #Find position of end of window based on custom_time[2]
    end_time <- all_times[which(custom_time[2]==all_times)]
    #Find position of start of window based on custom_time[1]
    start_time <- all_times[which(custom_time[1]==all_times)]
    
  }else{
    end_time <-  all_times[length(all_times)]
    start_time <- all_times[length(all_times)-1]
    
    custom_time <- c(start_time,end_time) #to append later
  }
  cat("Comparing change",as.character(custom_time[1]) ,"to",as.character(custom_time[2]),'\n')
  
  #If the input vectors contain more time periods than the window length
  #we will remove what wouldn't be included in the GEKS calculation
  if(!is.null(window_length)){

    #return times of interest based off end_time index
    times_of_interest <- all_times[(which(all_times==end_time)-window_length):which(all_times==end_time)]
    
    #check there is enough data for the window
    if (any(is.na(times_of_interest))){
      stop("There are NA's in the times of interest. If you have a custom time",
           " and a window length, make sure there is enough back data start_time custom_time[2]")
    }
    # Filter data.table by times of interest
    prices_df <- prices_df[times %in% times_of_interest]
  }

  #Create seperate times and id index variable
  prices_df[,"times_index":=as.numeric(as.factor(times))]
  
  #Convert start and end time to numeric index
  start_time <- prices_df$times_index[which(prices_df$times==start_time)][1]
  end_time <- prices_df$times_index[which(prices_df$times==end_time)][1]
  
  data_window_a <- prices_df[times_index==start_time]
  
  data_window_b <- prices_df[times_index==end_time]
  
  commodities <- unique(prices_df[,id])

  item <- vector("list",length(commodities))
  
  comm_index <- 0
  for(choice in commodities){
    
    comm_index <- comm_index+1
    
    cat(comm_index,"/",length(commodities),'\r')
    
    a_dot <- get_weight_dot(prices_df,choice,start_time)
    b_dot <- get_weight_dot(prices_df,choice,end_time)
    
    p_vector <- c()

    for(i in unique(prices_df$times_index)){
      
      data_window_c <- prices_df[times_index==i]
      price <- data_window_c[id == choice, (price)]
      
      if(length(price)==0){ #When t price doesn't exist
        
        temp_p <- 1
        
      }else  if(a_dot==0){ #Case where price doesn't exist in period a
        
        price_b <- prices_df[times_index==end_time & id == choice, (price)]
        
        commodities_b <- data_window_b[id%in%data_window_c$id,id]
        
        item_weight_b <- get_single_weight(data_window_c,data_window_b,commodities_b,choice,i,end_time)

        temp_p <- (price_b/price)^(item_weight_b/length(unique(prices_df$times_index)))
        
      }else if(b_dot==0){ #Case where price doesn't exist in period b
        
        price_a <- prices_df[times_index==start_time & id == choice, (price)]

        commodities_a <- data_window_b[id%in%data_window_c$id,id]
        
        item_weight_a <- get_single_weight(data_window_c,data_window_a,commodities_a,choice,i,start_time)
        
        temp_p <- (price/price_a)^(item_weight_a/length(unique(prices_df$times_index)))
        
      }else{ #Case where price exists in period a and b
        
        commodities_a <- data_window_a[id%in%data_window_c$id,id]
        
        commodities_b <- data_window_b[id%in%data_window_c$id,id]
        
        item_weight_a <- get_single_weight(data_window_c,data_window_a,commodities_a,choice,i,start_time)
        
        item_weight_b <- get_single_weight(data_window_c,data_window_b,commodities_b,choice,i,end_time)
        
        temp_p <- price^((item_weight_a-item_weight_b)/(length(unique(prices_df$times_index))))
        
      }
      
      p_vector <- c(p_vector,
                    temp_p)
    }
    
    if(a_dot==0 | b_dot==0){ # Can never be both
      
      contrib <- prod(p_vector)
      
    }else{
      
      p_prod <- prod(p_vector)
      
      price_a <- prices_df[times_index==start_time & id == choice, (price)]
      
      price_b <- prices_df[times_index==end_time & id == choice, (price)]
      
      
      if(length(price_a)==0){ #ie doesn't exist
        price_a <- 1
      }
      
      if(length(price_b)==0){ #ie doesn't exist
        price_b <- 1
      }
      
      contrib <- (
        (price_b^(b_dot))/
          (price_a^(a_dot))
      )*p_prod
      
    }
    
    item[[comm_index]] <- data.frame(id=choice,contrib = contrib)
    
  }
  
  item <- rbindlist(item)
  
  item[,"p_contrib" := percentage_contrib(contrib)]
  
  item[,"from":=custom_time[1]]
  
  item[,"to":=custom_time[2]]
  
  setcolorder(item,c("from","to","id","contrib","p_contrib"))
  
  return(item)
}


#' Get weight between times t and period of interest
#'
#' @param data_window_a vector of the times at which price observations were made. \cr
#' NOTE: \code{times} must be of class Date or numeric.
#' @param data_window_b vector of log of prices at the given time
#' @param all_id vector of distinct identification number of consumer goods
#' @param id_of_interest vector of expenditure weights used in the regressions
#' @param period_a either empty (will assume latest periods) or a vector of length
#' two relating to times compared where [1] is the 'start_time' time and [2] is the 'to' time
#' e.g Comparing contribution start_time "1973-04-01" to "1973-05-01" would be c("1973-04-01", "1973-05-01")
#' @param period_b optional. Single number for length of window for the data that
#' regressions are fit on. Note if window_length is present it assumes custom time is latest two periods
#' in window.
#' @import data.table
get_single_weight <- function(data_window_a,
                                   data_window_b,
                                   all_id,
                                   id_of_interest,
                                   period_a,
                                   period_b){
  
  if(is.na(match(id_of_interest,data_window_b$id))){
    
    return(0) #Its not part of the matched sample between that period and any other so w=0
    
  }else{
    
    total_exp_a <- sum(data_window_a[id%in%all_id,weight]) 
    total_exp_b <- sum(data_window_b[id%in%all_id,weight])
    
    exp_a <- sum(data_window_a[id==id_of_interest,weight]) 
    exp_b <- sum(data_window_b[id==id_of_interest,weight]) 
    
    wgt <- 0.5*((exp_a/total_exp_a) + (exp_b/total_exp_b))
    
    return(wgt)
  }
}

#' Get weight between all times t and period of interest
#'
#' @param data_window vector of the times at which price observations were made. \cr
#' NOTE: \code{times} must be of class Date or numeric.
#' @param id_of_interest vector of log of prices at the given time
#' @param period vector of distinct identification number of consumer goods
#' @import data.table
get_weight_dot <- function(data_window,id_of_interest,period){
  
  data_window_b <- data_window[times_index==period]  
  
  w <- vector(mode = "numeric",length=length(unique(data_window$times_index)))
  
  for(i in unique(data_window$times_index)){
    
    data_window_a <- data_window[times_index==i] 
    all_id <- data_window_b[id%in%data_window_a$id,id]
    temp_w <- get_single_weight(data_window_a,data_window_b,all_id,id_of_interest,i,period)
    
    w[i] <- temp_w
    
  }
  
  wgt_dot <- (1/(length(w)))*sum(w)
  
  return(wgt_dot)
}
