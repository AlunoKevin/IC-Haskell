
# Primeira funcao utilizada no ghci 
doubleME x = x + x

#segunda funcao
doubleUs1 x y = x*2 + y*2

#double us redefinida apenas chamando a primeira funcao 
doubleUs2 x y = doubleMe x + doubleMe y 


#funcao com condicional
doubleNum x = if x > 30
                then x
                else x*2
#atentar a identacao 


#executa if e depois adiciona um 1 ao resultado final
doubleNum2 x = (if x> 30 then x else x*2) + 1

#funcao sobre list comprehanssion estudo
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

# remove tudo que nao esta em upercase usa a funcao elem para ver se esta contido na lista alfanumerica capitalizada
# apos veirficacao se o elemento esta capitalizado o poe na lista final

# notacao esplicita de tipo da funcao recebendo uma lista de chars == string e retornando o mesmo tipo do valor recebido importante paradefinir os retornos das diversas funcoes a serem desenvolvidas
removeNonUppercase :: [Char] -> [Char] 
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 


# faz tuplas entre as duas listas  [(1,"apple") ..]
#zip [1..] ["apple", "orange", "cherry", "mango"]

