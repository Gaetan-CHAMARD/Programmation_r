# Exercice 2.1.1

brutToNet1 <- function(salary) {
  if (!is.numeric(salary)){
    return("ERORR: type not expected")
  }
   return (salary * (1-0.22)) 
}
# Tests
print(brutToNet1(4000))
 # [1]1560
print(brutToNet1("2000"))
 # [1]"ERROR: type not expected"

# Exercice 2.1.2

brutToNet2 <- function(salary, contract) {
  if (!is.numeric(salary)){
    return("ERROR: salary not numeric")
  }
  if(contract != "cadre" & contract != "non-cadre"){
    return("ERROR: contract unknow")
  }
  if(contract == "cadre"){
    return(salary*(1-0.225-0.075))
  }
  if (contract == "non-cadre"){
    return(salary*(1-0.22-0.075))
  }
}
# Tests
print(brutToNet2(2000, "cadre"))
print(brutToNet2(2000, "non-cadre"))
print(brutToNet2("2000", "cadre"))
print(brutToNet2(2000, "employé"))

# Exercice 2.1.3

brutToNet3 <- function(salary, contract, rate= 0.075, time = 100){
 if (!is.numeric(salary)) | !is.numeric(rate) | !is.numeric(time)){
   return("ERROR: salary, rate and time must be numeric")
 }
 if (contract != "cadre" & contract != "non-cadre"){
   return("ERROR: contratc unknown")
 }
 if (rate < 0 | rate > 1 | time < 0 | time > 100){
   return("ERROR: rate and time must be in range(0,100")
 }
 if (contract == "cadre"){
   net_before_tax <- salary * (1-0.225)* time
   net_after_tax <- net_before_tax * (1-rate)
 }
 if (contract == "non-cadre"){
   net_before_tax <- salary * (1-0.22)* time
   net_after_tax <- net_before_tax * (1-rate)
 }
 return(list("net_before_tax" = net_before_tax, "net_after_tax" = net_after_tax))
}
#Tests
print(brutToNet3(2000,"cadre") )