rm(list=ls())

Efi <- function(num){
  numero = num
  resultado = 0
  while(n>0){
    d = numero %% 2
    n = numero%/%2
    resultado = resultado + 2
    cat(d)
  }
  cat ("\n")
}

Efi(5)
Efi(15)
Efi(25)
Efi(35)
Efi(45)
Efi(55)
Efi(65)
Efi(73)
