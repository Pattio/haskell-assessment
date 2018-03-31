{- 
    Edvinas Byla | CS3518 Haskell In Course Assessment
    
    To run required and additional tests for all the problems
    write runTests in ghci terminal
-}


-- Simplest implementation using Haskell provided elem function --
inlist :: (Eq t) => [t] -> t -> Bool
inlist xs target = elem target xs

inlistTests = do
    putStrLn("\nRunning inlist tests")
    putStrLn("\ninlist [2,3,2,4,7,9] 7")
    print(inlist [2,3,2,4,7,9] 7)
    putStrLn("\ninlist [2..100] 101")
    print(inlist [2..100] 101)
    putStrLn("\ninlist [2..] 101")
    print(inlist [2..] 101)
    putStrLn("\ninlist ['a', 'b', 'c', 'd'] 'e'")
    print(inlist ['a', 'b', 'c', 'd'] 'e')
    putStrLn("\ninlist [1.11, 2.22, 3.33] 2.22")
    print(inlist [1.11, 2.22, 3.33] 2.22)

{- 
    Filter list to only have elements we are looking for (target),
    then use the length function to get the size of that list, then compare
    that size to 1, if it's equal 1 it will return true, else false


    It won't work with the last example. List in the last example is 
    infinite, because of Haskell lazy evaluation and to make sure 
    that number occurs only once in list you need to go through 
    all list, which is impossible as the list is infinite

-}
exactlyonce :: (Eq t) => [t] -> t -> Bool
exactlyonce xs target = (length $ filter (==target) xs) == 1

exactlyonceTests = do
    putStrLn("\nRunning exactlyonce tests")
    putStrLn("\nexactlyonce [2,3,2,4,3] 3")
    print(exactlyonce [2,3,2,4,3] 3)
    putStrLn("\nexactlyonce [3..30] 15")
    print(exactlyonce [3..30] 15)
    putStrLn("\nexactlyonce ['a', 'b', 'c', 'd', 'b'] 'b'")
    print(exactlyonce ['a', 'b', 'c', 'd', 'b'] 'b')
    putStrLn("\nexactlyonce [1.11, 2.22, 3.33] 1.11")
    print(exactlyonce [1.11, 2.22, 3.33] 1.11)


{- 
    Filter both lists to only have 1's, compare them, if they have the
    same number of 1's they will be equal and return true otherwise
    it will return false
-}
equalones :: (Eq t, Num t) => [t] -> [t] -> Bool
equalones xs ys = (filter (==1) xs) == (filter (==1) ys)

equalonesTests = do
    putStrLn("\nRunning equalones tests")
    putStrLn("\nequalones [1,2,0] [3,5,1,1]")
    print(equalones [1,2,0] [3,5,1,1])
    putStrLn("\nequalones [1,0] [0]")
    print(equalones [1,0] [0])
    putStrLn("\nequalones [1,0,0,1] [0,1,1,0]")
    print(equalones [1,0,0,1] [0,1,1,0])
    putStrLn("\nequalones [1.1, 1.2, 1.3] [1.4, 1.5, 1]")
    print(equalones [1.1, 1.2, 1.3] [1.4, 1.5, 1])
    putStrLn("\nequalones [] []")
    print(equalones [] [])


{- 
    Map trough list and apply a lamba function to each of the elements.
    List argument is skipped, because function is almost point free
-}
replacenew :: Num t => t -> [t] -> [t]
replacenew x = map (\element -> (x-element)^2)

replacenewTests = do
    putStrLn("\nRunning replacenew tests")
    putStrLn("\nreplacenew 2 [3,6,9]")
    print(replacenew 2 [3,6,9])
    putStrLn("\nreplacenew 0 [3,6,9]")
    print(replacenew 0 [3,6,9])
    putStrLn("\nreplacenew 1.11 [3,6,9]")
    print(replacenew 1.11 [3,6,9])


{- 
    Fold the list by multiplying accumulator to the sum of the inner list.
    List argument is skipped, because function is point free
-}
addthemup :: (Num a, Foldable t1, Foldable t2) => t1 (t2 a) -> a
addthemup x = foldl (\acc xs -> acc * sum xs) 1 x

addthemupTests = do
    putStrLn("\nRunning addthemup tests")
    putStrLn("\naddthemup [[1,3],[3,7]]")
    print(addthemup [[1,3],[3,7]])
    putStrLn("\naddthemup [[1,2,3],[9]]")
    print(addthemup [[1,2,3],[9]])
    putStrLn("\naddthemup [[1,2],[]]")
    print(addthemup [[1,2],[]])
    putStrLn("\naddthemup [[1,2],[1,3],[4,5,7],[2]]")
    print(addthemup [[1,2],[1,3],[4,5,7],[2]])
    putStrLn("\naddthemup [[1.1,2.2],[1.1,3.3],[4.4,5.5,7.7],[2.2]]")
    print(addthemup [[1.1,2.2],[1.1,3.3],[4.4,5.5,7.7],[2.2]])
    putStrLn("\naddthemup [[-1],[-2],[-3, -4],[-5]]")
    print(addthemup [[-1],[-2],[-3, -4],[-5]])


{- 
    First, create  a generic argument genericNumber and add few constrains 
    i.e it should be equatable (Eq) and it should be a number (Num)
    those constrains are required so we can decrease x by 1 and also 
    compare x to 0. For function we give the most generic type t, i.e.
    it can take anything of type t and return anything of same type.
    Argument y is also of that type as it is passed to the function. 
    Also function returns value of type t.


    Function implementation is quite simple first check if x is equal zero
    if yes, then return y, if not then apply f to y, decrease x by 1 and 
    call repeatnew recursively.
-}

repeatnew :: (Eq genericNumber, Num genericNumber) => (t -> t) -> genericNumber -> t -> t
repeatnew _ 0 y = y
repeatnew f x y = repeatnew f (x-1) (f y)


repeatnewTests = do
    putStrLn("\nRunning repeatnew tests")
    let square y = y * y
    putStrLn("\nrepeatnew square 1 2")
    print(repeatnew square 1 2)
    putStrLn("\nrepeatnew square 2 2")
    print(repeatnew square 2 2)
    putStrLn("\nrepeatnew square 4 1")
    print(repeatnew square 4 1)
    putStrLn("\nrepeatnew square 10 0.999")
    print(repeatnew square 10 0.999)
    putStrLn("\nrepeatnew tail 3 ['a', 'b', 'c', 'd']")
    putStrLn(repeatnew tail 3 ['a', 'b', 'c', 'd'])


{- 
    First check if the list is valid, i.e has more than 2 elements if yes, 
    then reverse list and check if 3rd element is equal to 1
-}
antepenultimate1 :: (Eq t, Num t) => [t] -> Bool
antepenultimate1 xs = length xs > 2 && (reverse xs !! 2) == 1


antepenultimate1Tests = do
    putStrLn("\nRunning antepenultimate1 tests")
    putStrLn("\nantepenultimate1 []")
    print(antepenultimate1 [])
    putStrLn("\nantepenultimate1 [1,0,1,1,0]")
    print(antepenultimate1 [1,0,1,1,0])
    putStrLn("\nantepenultimate1 [1,0,0,1,1]")
    print(antepenultimate1 [1,0,0,1,1])
    putStrLn("\nantepenultimate1 [1,0,1,0,1,0]")
    print(antepenultimate1 [1,0,1,0,1,0])
    putStrLn("\nantepenultimate1 [0,0,0,1,0,0]")
    print(antepenultimate1 [0,0,0,1,0,0])

{- 
    First of all check if list is empty, if yes return False
    Then check if 2 first elements of list are equal to 1
    If yes then return True, if not third variant of function 
    is called which skips first parameter and calls function 
    recursively with the rest of the list
-}

sequenceones :: (Eq a, Num a) => [a] -> Bool
sequenceones [] = False
sequenceones (1:1:_) = True
sequenceones (_:xs) = sequenceones xs

sequenceonesTests = do
    putStrLn("\nRunning sequenceones tests")
    putStrLn("\nsequenceones []")
    print(sequenceones [])
    putStrLn("\nsequenceones [1,0,1,1,0]")
    print(sequenceones [1,0,1,1,0])
    putStrLn("\nsequenceones [1,0,0,1,0,1]")
    print(sequenceones [1,0,0,1,0,1])
    putStrLn("\nsequenceones [1,0,1,0,1]")
    print(sequenceones [1,0,1,0,1])
    putStrLn("\nsequenceones [0,0,1,1,0]")
    print(sequenceones [0,0,1,1,0])
    putStrLn("\nsequenceones [1..20]")
    print(sequenceones [1..20])


runTests = do
    inlistTests
    exactlyonceTests
    equalonesTests
    replacenewTests
    addthemupTests
    repeatnewTests
    antepenultimate1Tests
    sequenceonesTests