#GOOD
get_single_weight_comm <- function(data_window_a,
                                   data_window_b,
                                   all_id,
                                   id,
                                   period_a,
                                   period_b){
  
  if(is.na(match(id,data_window_b$commodity))){
    
    return(0) #Its not part of the matched sample between that period and any other so w=0
    
  }else{
    
    total_exp_a <- sum(data_window_a[commodity%in%all_id,weight]) 
    total_exp_b <- sum(data_window_b[commodity%in%all_id,weight])
  
    exp_a <- sum(data_window_a[commodity==id,weight]) 
    exp_b <- sum(data_window_b[commodity==id,weight]) 
    
    wgt <- 0.5*((exp_a/total_exp_a) + (exp_b/total_exp_b))
    
    return(wgt)
  }
}

get_weight_dot_comm <- function(data_window,id,period){
  
  data_window_b <- data_window[month_n==period]  
  
  w <- vector(mode = "numeric",length=length(unique(data_window$month_n)))
  
  for(i in unique(data_window$month_n)){
    
    data_window_a <- data_window[month_n==i] 
    all_id <- data_window_b[commodity%in%data_window_a$commodity,commodity]
    temp_w <- get_single_weight_comm(data_window_a,data_window_b,all_id,id,i,period)
    
    w[i] <- temp_w
    
  }
  
  wgt_dot <- (1/(length(w)))*sum(w)
  
  return(wgt_dot)
}

GEKS_contrib <- function(data_window,from,to){
  
  data_window_a <- data_window[month_n==from]
  
  data_window_b <- data_window[month_n==to]
  
  commodities <- unique(data_window[,commodity])

  item <- vector("list",length(commodities))
  
  #For each choice calculate its contribution
  comm_index <- 0
  for(choice in commodities){
    
    cat("Choice:",choice,'\n')
    comm_index <- comm_index+1
    a_dot <- get_weight_dot_comm(data_window,choice,from)
    b_dot <- get_weight_dot_comm(data_window,choice,to)
    
    p_vector <- c()
    # i <- 30
    for(i in unique(data_window$month_n)){
      
      data_window_c <- data_window[month_n==i]
      price <- data_window_c[commodity == choice, (price)]
      
      if(length(price)==0){
        temp_p <- 1
      }else  if(a_dot==0){ #Case where price doesn't exist in period a
        
        price_b <- data_window[month_n==to & commodity == choice, (price)]
        
        commodities_b <- data_window_b[commodity%in%data_window_c$commodity,commodity]
        
        item_weight_b <- get_single_weight_comm(data_window_c,data_window_b,commodities_b,choice,i,to)
        
        temp_p <- (price_b/price)^(item_weight_b/length(unique(data_window$month_n)))
        
      }else if(b_dot==0){ #Case where price doesn't exist in period b
        
        price_a <- data_window[month_n==from & commodity == choice, (price)]

        commodities_a <- data_window_b[commodity%in%data_window_c$commodity,commodity]
        
        item_weight_a <- get_single_weight_comm(data_window_c,data_window_a,commodities_a,choice,i,from)
        
        temp_p <- (price/price_a)^(item_weight_a/length(unique(data_window$month_n)))
        
      }else{ #Case where price exists in period a and b
        
        commodities_a <- data_window_a[commodity%in%data_window_c$commodity,commodity]
        
        commodities_b <- data_window_b[commodity%in%data_window_c$commodity,commodity]
        
        item_weight_a <- get_single_weight_comm(data_window_c,data_window_a,commodities_a,choice,i,from)
        
        item_weight_b <- get_single_weight_comm(data_window_c,data_window_b,commodities_b,choice,i,to)
        
        temp_p <- price^((item_weight_a-item_weight_b)/(length(unique(data_window$month_n))))
        
      }
      
      p_vector <- c(p_vector,
                    temp_p)
    }
    
    if(a_dot==0 | b_dot==0){ # Can never be both
      
      contrib <- prod(p_vector)
      
    }else{
      
      p_prod <- prod(p_vector)
      
      price_a <- data_window[month_n==from & commodity == choice, (price)]
      
      price_b <- data_window[month_n==to & commodity == choice, (price)]
      
      
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
  
  return(rbindlist(item))
}

