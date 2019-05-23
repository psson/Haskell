doubleMe x = x + x

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]


removeNonUppercase st = [ c | c <- st , c `elem` ['A'..'Z']]

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "No match"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1)(x2,y2) = (x1+x2,y1+y2)

add3DVectors :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
add3DVectors (x1,y1,z1)(x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs