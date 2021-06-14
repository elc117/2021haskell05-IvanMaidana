-- Prática 05 de Haskell
-- Nome: Ivan Maidana da Silveira
--import System.Random (randomRIO)
--1 
bmi :: Float -> Float -> String
bmi peso altura =
    let imc = peso/altura^2   
    in if imc <= 18.5 then "ABAIXO" else
       if imc >= 30.0 then "ACIMA" else "NORMAL"

--2
bmi' :: Float -> Float -> String
bmi' peso altura  
    | imc <= 18.5 = "ABAIXO"  
    | imc >= 30.0 = "ACIMA"  
    | otherwise   = "NORMAL"  
    where imc = peso / altura ^ 2 

--3
cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]


--4
--anD :: Int -> Int -> Bool
--anD x y
--   |(trnsfor x == True && trnsfor y == True) =  True
--   |(trnsfor x == False && trnsfor y == False) =  True
--   |otherwise = False

--trnsfor :: Int -> Bool
--trnsfor num
--   |num == 0 = False
--   |otherwise = True

--andTable :: [(Bool, Bool, Bool)]
--andTable = [(trnsfor x, trnsfor y, (anD x y)) | x <- randomRIO(0,--1::Int) , y <- randomRIO(0,1::Int)]