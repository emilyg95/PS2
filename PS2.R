Benford = function(x, control){
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = max(m - log10(1+1/i))
    ChoVec = (m - log10(1+1/i))^2
    Cho_Gains = sqrt(sum(ChoVec))
  }
  
  if (control == 1){
    return(Leemis)
  }
  
  if (control == 2){
    return(Cho_Gains)
  }
  else{
    return(c(Leemis,Cho_Gains))
  }
}

Benford(1:9, 3)



print.benfords = function(x, control){
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = as.data.frame(max(m - log10(1+1/i)))
    ChoVec = (m - log10(1+1/i))^2
    Cho_Gains = as.data.frame(sqrt(sum(ChoVec)))
    colnames(Cho_Gains) = c("Cho_Gains")
    colnames(Leemis) = c("Leemis")
    if (max(Leemis) <= 0.851){
      print.noquote("Leemis: cannot reject the null")
    }else if (0.967 >= max(Leemis) & max(Leemis) > 0.851){
      print.noquote("Leemis: *")
    }else if (1.212 >= max(Leemis) & max(Leemis) > 0.967){
      print.noquote("Leemis: **")
    }else{
      print.noquote("Leemis: ***")
    }
    if ((Cho_Gains) <= 1.212){
      print.noquote("Cho Gains: cannot reject the null")
    }else if (1.330 >= (Cho_Gains) & (Cho_Gains) > 1.212){
      print.noquote("Cho Gains: *")
    }else if (1.569 >= (Cho_Gains) & (Cho_Gains) > 1.330){
      print.noquote("Cho Gains: **")
    }else{
      print.noquote("Cho Gains: ***")
    }
  if (control == 1){
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(Leemis)
  }else if (control == 2){
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(Cho_Gains)
  }else{
    print.noquote("Significance Levels; a = 0.1*, a = 0.05**, a = 0.01***")
    return(c(Leemis, Cho_Gains))
  }
}
}


print.benfords(1:9, 1)
View(unlist(print.benfords(1:9, 3)))

getwd()
setwd("/Users/emilygarner/Documents/School/Second Sem/R/Problem Sets/PS2")

capture.benfords = function(x, control){
capture.output(print.benfords(x, control), file = "Capture_Benfords.csv")
}

capture.benfords(1:9, 3)