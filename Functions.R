# FUNCTIONS
# asterisk()

# Create asterisk significance function
# Returns number of asterisks depending on the p-value
asterisk = function(p){
  result = ""
  if (p < 0.001){
    result = "***"
  } else if (p < 0.01){
    result = "**"
  } else if (p < 0.05){
    result = "*"
  } else if (p < 0.1){
    result = "·"    
  }
  return(result)
}
