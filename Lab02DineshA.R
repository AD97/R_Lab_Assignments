# Aadhithya Dinesh
# MIS 545 Section 02
# Lab02DineshA.R
# This R application will calculate the extended price for a product ordered 
# by a user.

# This function is used to display the details of the current lab and the list 
# of valid item IDs.
DisplayTitle <- function(){
  print("Lab 02 - Product Lookup")
  print("Programmer: Aadhithya Dinesh")
  print("____________________________")
}

# This function is used to find the price of the given item ID passed as 
# parameter.
LookupPrice <- function(itemIDParameter){
  itemPriceVector <- c(189.99, 131.99, 56.99, 245.99, 28.99)
  itemPriceLookUp <- 0
  for(i in 1:length(itemPriceVector)){
    if(itemIDParameter == itemIDVector[i]){
      itemPriceLookUp <- itemPriceVector[i]
      break                                   # break the loop if match found
    }
  }
  return(itemPriceLookUp)                     # return the price found 
}

# Calling the DisplayTitle() function
DisplayTitle()

# Assign the values to the itemID vector
itemIDVector <- c("AY4", "TE3", "TE4", "GH6","QZ4")

# Loop control variable set to Y as user needs a prompt at least once
loopControl <- "Y"                         
while(loopControl == "Y" && itemPriceLookUp == 0) {
# printing valid item IDs with proper formatting
  print(paste0("Valid item IDs are: ", paste0(
        itemIDVector[1:length(itemIDVector)-1], collapse =" "), 
        " and ", paste0(itemIDVector[length(itemIDVector)]))
       )
  itemID <- readline("Please enter the item ID: ")  
  itemPrice <- LookupPrice(itemID)  # to lookup price of the entered itemID
  if(itemPrice == 0) {
    print("Invalid unit ID -- please try again.") # if returned price is 0 
                                                  # then display invalid unit ID                   
  } else {
    print(paste0("The unit price is $",itemPrice))
    quantity <- strtoi(readline("Please enter the number of units desired: "))
    print(paste0("The extended price is $",itemPrice*quantity)) # print extended
  }                                                             # price  
  loopControl <- readline("Would you like to enter another product (Y or N)? "
                         ) # ask user if he/she wants to enter another item ID
}
