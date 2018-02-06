x = 1:9 ##creates a test vector

Benford = function(x, control){ ##creates a function with 2 inputs
  digit = substr(x, 1, 1) ##subsets the input to extract the first digit
  freq = table(digit) ##creates a table indicating the frequency with which each first digit occurs in the given vector
  m = freq/sum(freq) ## creates a variable equal to the proportional frequency of each first digit in the vector 
  for (i in 1:9){ ## a for loop that runs the following equations with the numbers 1 through 9
    Leemis = max(m - log10(1+1/i)) ## creates a variable for the output of the Leemis equation
    ChoVec = (m - log10(1+1/i))^2 ## creates a variable to input into the Cho Gains equation for simplicity
    Cho_Gains = sqrt(sum(ChoVec)) ## creates a variable for the output of the Cho Gains equation
  }

  if (control == 1){ 
    print.noquote(freq)
    return(Leemis)
  }
  
  if (control == 2){
    print.noquote(freq)
    return(Cho_Gains)
  }
  else{
    print.noquote(freq)
    return(c(Leemis,Cho_Gains)) ## an if else statement that allows you to tell the function which outputs to return based on the control input and prints the table containing the full digit distribution no matter what
  }
}

Benford(1:9, 3) ## test



print.benfords = function(x, control){ ## same base as the Benfords function
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = as.data.frame(max(m - log10(1+1/i))) ## classifies Leemis as a data frame in order to easily add labels
    ChoVec = (m - log10(1+1/i))^2
    Cho_Gains = as.data.frame(sqrt(sum(ChoVec))) ## classifies Leemis as a data frame in order to easily add labels
    colnames(Cho_Gains) = c("Cho_Gains") ## labels output of Cho Gains for clarity
    colnames(Leemis) = c("Leemis") ## labels output of Leemis for clarity
    if (max(Leemis) <= 0.851){
      print.noquote("Leemis: cannot reject the null")
    }else if (0.967 >= Leemis & Leemis > 0.851){
      print.noquote("Leemis: *")
    }else if (1.212 >= Leemis & Leemis > 0.967){
      print.noquote("Leemis: **")
    }else{
      print.noquote("Leemis: ***") ## an if else statement which prints the relevant number of asterisk's for the appropriate confidence interval for Leemis 
    }
    if (Cho_Gains <= 1.212){
      print.noquote("Cho Gains: cannot reject the null")
    }else if (1.330 >= Cho_Gains & Cho_Gains > 1.212){
      print.noquote("Cho Gains: *")
    }else if (1.569 >= Cho_Gains & Cho_Gains > 1.330){
      print.noquote("Cho Gains: **")
    }else{
      print.noquote("Cho Gains: ***") ## an if else statement which prints the relevant number of asterisk's for the appropriate confidence interval for Cho Gains 
    }
  if (control == 1){
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(Leemis)
  }else if (control == 2){
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(Cho_Gains)
  }else{
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(c(Leemis, Cho_Gains)) ## added command to print a legend to the output if else statement
  }
}
}

print.benfords(1:9, 0) ## check

getwd() ## checks working directory location
setwd("/Users/emilygarner/Documents/School/Second Sem/R/Problem Sets/PS2") ## sets working directory location for printing a csv file

capture.benfords = function(x, control){ ## function with 2 inputs to match print.benfords
capture.output(print.benfords(x, control), file = "Capture_Benfords.csv") ## commands function to capture the output of print.benfords with your chosen inputs and create a csv entitled "Capture_Benfords"
}

capture.benfords(1:9, 3) ## check