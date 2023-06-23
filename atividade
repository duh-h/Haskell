{-
1. What are the types of the following values?
a. [’a’, ’b’, ’c’]= 
b. (’a’, ’b’, ’c’)= 
c. [(False, ’O’), (True,’1’)]= 
d. ([False, True], [’0’,’1’])= 
e. [tail, init, reverse]= 

a.Uma lista de caracteres [char]
b.uam tupla de caracteres (char,char,char)
c.uma lista de tuplas (bool,char)
d.uma tupla de listas ([bool],[char])
e.uma lista de funçoes que retonam uma lista




2. Write down definitions that have the following types; it does not matter what
a. the definitions actually do as long as they are type correct.
b. bools :: [Bool]
c. nums :: [[Int]]
d. add :: Int -> Int -> Int -> Int
e. copy :: a -> (a,a)
f. apply :: (a -> b) -> a -> b


b. Uma lista de bool
c. uma lista de lista de numeros inteiros
d. uma funcao que recebe 3 numeros inteiros e retorna 1 numero inteiro
e.uma funçao que recebe uma valor e retorna uma tupla de valores
f.uma funçoa que recebe uma funçao e um valor e retorna um valor



3. What are the types of the following functions?
a. second xs = head (tail xs)
b. swap (x,y) = (y,x)
c. pair x y = (x,y)
d. double x = x*2
e. palindrome xs = reverse xs == xs
f. twice f x = f (f x)
Hint: take care to include the necessary class constraints in the types if the
functions are defined using overloaded operators.

a. [a]-> a
b. (a,b)->(b,a)
c. a -> b -> (a,b)
d. Num => a -> a -> a
e. [a] -> bool
f. (a -> a) -> a -> (a -> a)






4. Using library functions, define a function halve :: [a] -> ([a],[a]) that splits an evenlengthed list into two halves. For example:
> halve [1,2,3,4,5,6]
([1,2,3],[4,5,6])
-}

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs


{-
5. Define a function third :: [a] -> a that returns the third element in a list that contains 
at least this many elements using:
a. head and tail;
b. list indexing !!;
c. pattern matching.
-}


third :: [a] -> a
third xs = head(tail(tail xs))

--ghci> third [1,2,3,4]
--ghci> 3

third2 :: [a] -> a
third2 xs = xs !! 2

--ghci> third2 [1,2,3,4]
--ghci> 3

third3 :: [a] -> a
third3 (_:_:x:_) = x
third3 [_,_] = error "lista nao tem terceiro elemento"
third3 [_] = error "lista nao tem terceiro elemento"
third3 [] = error "lista nao tem terceiro elemento"

--ghci> third3 [1,2,3,4]
--ghci> 3


{-
6. The Luhn algorithm is used to check bank card numbers for simple errors such as 
mistyping a digit, and proceeds as follows:
• consider each digit as a separate number;
• moving left, double every other number from the second last;
• subtract 9 from each number that is now greater than 9;
• add all the resulting numbers together;
• if the total is divisible by 10, the card number is valid.
Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the 
result is greater than 9. For example:
> luhnDouble 3
6
> luhnDouble 6
3
Using luhnDouble and the integer remainder function mod, define a function
luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank card number is 
valid. For example:
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False

--}

lunhDouble :: Int -> Int
lunhDouble x  
    | x*2 > 9 = x*2-9
    | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn _ b c d =
    let total = lunhDouble b + lunhDouble c + lunhDouble d
    in total `mod` 10 == 0 

--ghci> luhn 4 7 8 3
--False
--ghci> luhn 1 7 8 4
--True




{--
7. Using a list comprehension, give an expression that calculates the sum 
1
2 + 2
2 + . . . + 1002 of the first one hundred integer squares.
-}

somaQuadrado :: Int
somaQuadrado = sum [x ^ 2 | x <- [1..100]]

--ghci> somaQuadrado
--338350

{-

8. Suppose that a coordinate grid of size m ˆ n is given by the list of all pairs (x, y_q of 
integers such that 0 ≤ x ≤ m and 0 ≤ y ≤ n. Using a list comprehension, define a function 
grid :: Int -> Int -> [(Int, Int)] that returns a coordinate grid of a given size. For example:
> grid 1 2
[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]
--}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--ghci> grid 2 2
--[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]



{--
9. Using a list comprehension and the function grid above, define a function
square :: Int -> [(Int, Int)] that returns a coordinate square of size n, excluding the 
diagonal from (0, 0) to (n, n). For example:
> square 2
[(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)]
--}

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

--ghci> square 3
--[(0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)]



{--
10. In a similar way to the function length, show how the library function
replicate :: Int -> a -> [a] that produces a list of identical elements can be defined using 
a list comprehension. For example:
> replicate 3 True
[True, True, True]
--}

replicate2 :: Int -> a -> [a]
replicate2 b a = [a | _ <- [1..b]]

--replicate2 5 True
--[True,True,True,True,True]



{--
11. A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation
x
2 + y
2 = z
2
. Using a list comprehension with three generators, define a function
pyths :: Int -> [(Int,Int,Int)] that returns the list of all such triples whose components 
are at most a given limit. For example:
> pyths 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
--}

pyths :: Int -> [(Int,Int,Int)]
pyths a = [(x,y,z) | x <- [1..a], y <- [1..a], z <- [1..a] , (x^2 + y^2) == z^2]

--ghci> pyths 15
--[(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(9,12,15),(12,5,13),(12,9,15)]




{--
12. A positive integer is perfect if it equals the sum of all of its factors, excluding the 
number itself. Using a list comprehension and the function factors, define a function 
perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit. For 
example:
> perfects 500
[6,28,496]
--}

perfects :: Int -> [Int]
perfects a = [x | x <- [1..a], sum[y | y <- [1..x-1], x `mod` y == 0 ] == x]

--ghci> perfects 800
--[6,28,496]




{--
13. Define a recursive function sumdown :: Int -> Int that returns the sum of the nonnegative integers from a given value down to zero. For example, sumdown 3 should 
return the result 3+2+1+0 = 6.
--}

sumdown :: Int -> Int
sumdown a = auxSumDown a 0

auxSumDown :: Int -> Int -> Int
auxSumDown a b 
    | a == 0 = b
    | otherwise = auxSumDown (a-1) (b+a)

--ghci> sumdown 4
--10



{--
14. Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s 
algorithm for calculating the greatest common divisor of two nonnegative integers: if 
the two numbers are equal, this number is the result; otherwise, the smaller number is 
subtracted from the larger, and the same process is then repeated. For example:
> euclid 6 27
3
--}

euclid :: Int -> Int -> Int
euclid a b 
    | a > b = euclid (a-b) b
    | a < b = euclid a (b-a)
    | otherwise = a

--ghci> euclid 6 36
--6



{--
15. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two 
sorted lists to give a single sorted list. For example:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
--}

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = auxmerge xs ys []

auxmerge :: Ord a => [a] -> [a] -> [a] -> [a]
auxmerge xs [] zs = zs ++ xs
auxmerge [] ys zs = zs ++ ys
auxmerge (x:xs) (y:ys) zs 
    | x <= y = auxmerge xs (y:ys) (zs ++ [x])
    | otherwise = auxmerge (x:xs) ys (zs ++ [y])

--ghci> merge [2,5,6] [1,3,4]
--[1,2,3,4,5,6]




{--
16. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge 
sort , in which the empty list and singleton lists are already sorted, and any other list is 
sorted by merging together the two lists that result from sorting the two halves of the 
list separately.
(*) Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves 
whose lengths differ by at most one.
--}

--Usando o halve da questao 4
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x]=[x]
msort xs = 
    let (primeiraLista, segundaLista) = halve xs
    in merge (msort primeiraLista) (msort segundaLista) 

--ghci> msort [9,8,7,6,5,4,3,1,2]
--[1,2,3,4,5,6,7,8,9]



{-
17. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its 
two argument functions to successive elements in a list, in turn about order. For 
example:
> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]
--}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap a b (x:xs) = a x : altMap b a xs

--ghci> altMap (+10) (+100) [0,1,2,3,4]
--[10,101,12,103,14]




{--
18. Using altMap, define a function luhn :: [Int] -> Bool that implements the Luhn 
algorithm from the exercise 6 for bank card numbers of any length.
-}
