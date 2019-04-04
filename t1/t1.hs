import Data.Char
import Data.String
import Data.List


--Exercicio 1 - Crie uma função isVowel :: Char -> Bool que verifique se um caracter é uma vogal ou não.

isVowel :: Char -> Bool
isVowel a = elem a "aeiou"

--Exercicio 2 - Escreva uma função addComma, que adicione uma vírgula no final de cada string contida numa lista.

addComma :: [String] -> [String]
addComma a = map (++",") a


--Exercicio 3 - Crie uma função htmlListItems :: [String] -> [String], que receba uma lista de strings e retorne outra lista contendo as strings formatadas como itens de lista em HTML. Resolva este exercício COM e SEM funções anônimas (lambda). Exemplo de uso da função:

--htmlListItems :: [String] -> [String]
--htmlListItems a = map ("<LI>"++) a

--Exercicio 4 - Defina uma função que receba uma string e produza outra retirando as vogais, conforme os exemplos abaixo. Resolva este exercício COM e SEM funções anônimas (lambda).

semVogais :: String -> String
semVogais "" = ""
semVogais (x:xs)
    | elem x "aeiou" = semVogais xs
    | otherwise      = x : semVogais xs


--Exercicio 5 - Defina uma função que receba uma string, possivelmente contendo espaços, e que retorne outra string substituindo os demais caracteres por '-', mas mantendo os espaços. Resolva este exercício COM e SEM funções anônimas (lambda). Exemplos:
removeSpace :: String -> String
removeSpace a = filter (/= ' ') a

removSpace :: String -> String
removSpace x = (\a -> filter (/= ' ')a)x

--Exercicio 6 - Escreva uma função firstName :: String -> String que, dado o nome completo de uma pessoa, obtenha seu primeiro nome. Suponha que cada parte do nome seja separada por um espaço e que não existam espaços no início ou fim do nome. Dica: estude funções pré-definidas em Haskell (List operations -> Sublists) em http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#g:18. Exemplos de uso da função:

firstName :: String -> String
firstName x = takeWhile(/= ' ') x


--Exercicio 7 - Escreva uma função isInt :: String -> Bool que verifique se uma dada string só contém dígitos de 0 a 9. Exemplos:

isInt :: String -> Bool
isInt x = all isNumber x


--Exercicio 8 - Escreva uma função lastName :: String -> String que, dado o nome completo de uma pessoa, obtenha seu último sobrenome. Suponha que cada parte do nome seja separada por um espaço e que não existam espaços no início ou fim do nome. Exemplos de uso da função:
lastName :: String -> String
lastName x = last (words x)


--Exercicio 9 - Escreva uma função userName :: String -> String que, dado o nome completo de uma pessoa, crie um nome de usuário (login) da pessoa, formado por: primeira letra do nome seguida do sobrenome, tudo em minúsculas. Dica: estude as funções pré-definidas no módulo Data.Char, para manipulação de maiúsculas e minúsculas. Você precisará carregar este módulo usando import Data.Char no interpretador ou no início do arquivo do programa.
userName :: String -> String
userName x =  map toLower ([head (firstName x)] ++ (lastName x))


--Exercicio 10 - Escreva uma função encodeName :: String -> String que substitua vogais em uma string, conforme o esquema a seguir: a = 4, e = 3, i = 2, o = 1, u = 0.
encodeName :: String -> String
encodeName x = map (\c -> if c=='a' then '4' else if c=='e' then '3' else if c=='i' then '2' else if c=='o' then '1' else if c=='u' then '0' else c)x


--Exercicio 11 - Escreva uma função betterEncodeName :: String -> String que substitua vogais em uma string, conforme este esquema: a = 4, e = 3, i = 1, o = 0, u = 00. Exemplos de uso da função:
betterEncodeName :: String -> String
betterEncodeName x = map (\c -> if c=='a' then '4' else if c=='e' then '3' else if c=='i' then '1' else if c=='o' then '0' else if c=='u' then '0' else c)x

--Exercicio 12 - Dada uma lista de strings, produzir outra lista com strings de 10 caracteres, usando o seguinte esquema: strings de entrada com mais de 10 caracteres são truncadas, strings com até 10 caracteres são completadas com '.' até ficarem com 10 caracteres. Exemplo:

listString :: [String] -> [String]
listString x = map (\c -> if (length) c <= 10 then c ++ take (10 -(length) c) (cycle ".") else c)x





















