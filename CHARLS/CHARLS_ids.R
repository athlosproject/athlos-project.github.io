# CHARLS_ids.R


# According to CHARLS documentation:

# The IDs (individualID, householdID and communityID) can be matched with there counterparts
# in the baseline sample. The community IDs are the same, but the household IDs
# are different, the householdID in the baseline is an 9 digit number, but in this wave, it
# changed, now it is an 10 digit number, the 10th digit is an indicator for splitting household
# due to divorce, you can safely adjust the householdID in baseline sample to 10 digit
# by adding ”0” in the 10th digit, then the modified householdID is comparable to that in
# the second wave. As the householdID changed in the second wave, the inidividualID also
# changed, now it is an 12 digit number, where the last two digit is the identifier for individuals
# within households which is comparable to those in baseline individualID, so you
# can simply use the adjusted householdID with the last two digit of individualID as madeup
# numbers to generate the new individualID which is comparable to that in the second
# wave.

# The next function adds a ”0” in the 10th character of each string given (so the 10th digit if the string is an integer). It is applied to wave 1 id's to make them comparable to that in
# the first wave id's.

# ids is a vector of strings, all of length 11

CHARLS_ids <- function(ids){
  # Length of the vector ids
  n <- length(ids)
  # Build a matrix where each row corresponds to an element of ids
  ids_s <- matrix(unlist(strsplit(ids, "")), byrow = TRUE, nrow=n, ncol = 11)
  # Add a column with zeroes in the 10th position.
  ids_s2 <- cbind(ids_s[,1:9], rep(0,n), ids_s[,10:11])

  # Build a vector with zeroes in the 10th position of the elements of ids
  newids <- paste(ids_s2[,1],ids_s2[,2], ids_s2[,3], ids_s2[,4], 
                    ids_s2[,5], ids_s2[,6], ids_s2[,7], ids_s2[,8], 
                    ids_s2[,9], ids_s2[,10], ids_s2[,11],ids_s2[,12], sep = "")
  
  as.character(newids)
}

