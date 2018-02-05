Benford = function(x, control){
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = m - log10(1+1/i)
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

Benford(1:9, 1)

print.benfords = function(x, control){
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = as.data.frame(m - log10(1+1/i), responseName = "Leemis", stringsAsFactors = F)
    ChoVec = (m - log10(1+1/i))^2
    Cho_Gains = as.data.frame(sqrt(sum(ChoVec)), responseName = "Cho_Gains")
    colnames(Cho_Gains) = c("Cho_Gains")
    names(Leemis) = c("Last_Digits", "Leemis")
    Stars_L = symnum(Leemis, cutpoints = c(0, 0.851,  0.967, 1.212, 100),
                    symbols = c("*","**","***"," "))
    Signif_L = cbind(Leemis, Stars_L)
    Stars_C = symnum(Cho_Gains, cutpoints = c(0, 1.212,  1.330, 1.212, 1.569),
                     symbols = c("*","**","***"," "))
    Signif_C = cbind(Cho_Gains, Stars_C)
  }

  if (control == 1){
    return(Signif_L)
  }
  
  if (control == 2){
    return(Signif_C)
  }
  else{
    return(c(Signif_L,Signif_C))
  }
}

print.benfords(1:9, 3)




symp <- symnum(Leemis, corr = FALSE,
               cutpoints = c(0.851,  0.967, 1.212),
               symbols = c("***","**","*","."," "))

  print.benfords(4:9, 3)
x = 1:9










names(print.benfords(x,2))

print.benfords = function(x, control){
  digit = substr(x, 1, 1)
  freq = table(digit)
  m = freq/sum(freq)
  for (i in 1:9){
    Leemis = as.matrix(m - log10(1+1/i), responseName = "Leemis", stringsAsFactors = F)
    ChoVec = (m - log10(1+1/i))^2
    Cho_Gains = as.data.frame(sqrt(sum(ChoVec)))
  }
  
  names(Leemis) = c("Digit", "Leemis")
  
  if (control == 1){
    return(Leemis)
    colnames(print.benfords) = c("First Digit", "Leemis")
  }
  
  if (control == 2){
    return(Cho_Gains)
    names(print.benfords) = c("Cho Gains")
  }
  else{
    return(c(Leemis,Cho_Gains))
    colnames(print.benfords) = c("First Digit", "Leemis", "Cho Gains")
  }
}
