
class_to_df <- function(class_obj) {
  # Create an empty data.frame
  classall_df <- tibble()
  not_found_df <- tibble()
  # Iterate over the list of taxa
  for (taxon in names(class_obj)) {
    # Check if the taxon has classification information
    if (!is.null(nrow(class_obj[[taxon]]))) {
      # Extract the classification data.frame for the taxon
      df <- class_obj[[taxon]]
      
      # Add a column to the data.frame indicating the taxon name
      df$Taxon <- taxon
      
      # Bind the current data.frame to the overall data.frame
      classall_df <- bind_rows(classall_df, df)
    } else {
      df <- tibble(Taxon = taxon)
      not_found_df <- bind_rows(  not_found_df, df)
    }
  }
  
  # Pivot the data.frame to have unique 'rank' values as separate columns
  classall_df <- classall_df %>% select(-id )%>%
    pivot_wider(names_from = rank, values_from = name)
  
  return(list(classall_df,not_found_df))
}

#
# Replace into dtot, the values of result$class
#
replace_nodes <- function(dtot,result,field){
  sapply( 1:nrow(dtot), function(i) {
    res_gen <- dtot$resource[i]
    rep_cla <- result %>% filter(Taxon == res_gen) 
    if(nrow(rep_cla)==1){
      print(paste(dtot$resource[i],rep_cla[field] ))
      dtot$resource[i] <<- unlist(rep_cla[field])
    }
    res_gen <- dtot$consumer[i]
    rep_cla <- result %>% filter(Taxon == res_gen) 
    if(nrow(rep_cla)==1){
      print(paste(rep_cla[field], dtot$consumer[i] ))
      dtot$consumer[i] <<- unlist(rep_cla[field])
    }
    
  })
  return(dtot)
}
