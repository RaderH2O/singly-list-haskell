module Main where
data SinglyList a = LeftEnd a (SinglyList a) |
                    Middle a (SinglyList a)  |
                    RightEnd a

instance Show a => Show (SinglyList a) where
  show (LeftEnd a b)    = "LeftEnd " ++ (show a) ++ ", " ++ (show b)
  show (Middle a b)     = "Middle " ++ (show a) ++ ", " ++ (show b)
  show (RightEnd a)     = "RightEnd " ++ (show a)

nextElement :: SinglyList a -> SinglyList a
nextElement (LeftEnd a (Middle b c)) = LeftEnd b c
nextElement (LeftEnd a (RightEnd b)) = RightEnd b
nextElement (Middle _ a) = a
nextElement a = a

listToSinglyMiddle :: [a] -> SinglyList a
listToSinglyMiddle [x]       = RightEnd x
listToSinglyMiddle (x:xs)    = e1
  where e1 = Middle x e2
        e2 = listToSinglyMiddle xs

listToSingly :: [a] -> SinglyList a
listToSingly (x:xs) = e1
  where e1 = LeftEnd x $ listToSinglyMiddle xs

singlyToList :: SinglyList a -> [a]
singlyToList (LeftEnd a b)      = a : singlyToList b
singlyToList (Middle a b)       = a : singlyToList b
singlyToList (RightEnd a)       = [a]

(.+) :: SinglyList a -> [a] -> SinglyList a
(.+) sl l = listToSingly $ singlyToList sl ++ l

values :: SinglyList Int
values = let e1 = LeftEnd  1 e2
             e2 = Middle   2 e3
             e3 = Middle   3 e4
             e4 = Middle   4 e5
             e5 = RightEnd 5
         in e1

main = undefined
