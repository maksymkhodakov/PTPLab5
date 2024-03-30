module Main where

-- Task1
-- f9
maybeSqrt :: Double -> Maybe Double
maybeSqrt x =
  let value = x - (1/38)
  in if value < 0 then Nothing else Just (sqrt value)

-- f0
maybeSqrtExpression :: Double -> Maybe Double
maybeSqrtExpression x =
  let value = x^2 - logBase 10 38
  in if value < 0 then Nothing else Just (sqrt value)

-- f8
maybeLog10 :: Double -> Maybe Double
maybeLog10 x =
  let value = x - (1/38)
  in if value <= 0 then Nothing else Just (logBase 10 value)

-- Task2
-- do нотація
superpositionDo :: Double -> Maybe Double
superpositionDo x = do
  result1 <- maybeLog10 x
  result2 <- maybeSqrtExpression result1
  maybeSqrt result2

-- без do нотації
superpositionBind :: Double -> Maybe Double
superpositionBind x = maybeLog10 x >>= maybeSqrtExpression >>= maybeSqrt

-- Task3
-- Функція для обчислення lg(x - (1/n))
maybeLog :: Double -> Double -> Maybe Double
maybeLog x n =
  let value = x - (1/n)
  in if value <= 0 then Nothing else Just (logBase 10 value)


-- Task4
superpositionDoTask4 :: Double -> Maybe Double
superpositionDoTask4 x = do
  firstArg <- maybeSqrt x
  secondArg <- maybeSqrtExpression x
  maybeLog firstArg secondArg

superpositionBindTask4 :: Double -> Maybe Double
superpositionBindTask4 x =
  maybeSqrt x >>= \firstArg ->
  maybeSqrtExpression x >>= \secondArg ->
  maybeLog firstArg secondArg


main :: IO ()
main = do
  print $ "f9:"
  -- Тестування з додатнім числом, що дає додатній результат під коренем
  print $ maybeSqrt 2
  -- Тестування з числом, що призводить до від'ємного результату під коренем
  print $ maybeSqrt (-1)
  -- Тестування з межовим значенням (1/38), що має давати 0 під коренем
  print $ maybeSqrt (1/38)
  print $ "f0:"
  -- Приклад виклику функції з позитивним результатом
  print $ maybeSqrtExpression 10
  -- Приклад виклику функції з результатом, який дає від'ємне число під коренем
  print $ maybeSqrtExpression 1
  -- Виклик функції зі значенням, що може бути близьким до межі від'ємного під корінним виразом
  print $ maybeSqrtExpression 2
  putStrLn "f8:"
  -- Вхідне значення, що призводить до від'ємного аргументу логарифму
  print $ maybeLog10 1
  -- Граничне значення, має повернути Nothing
  print $ maybeLog10 (1/38)
  -- Позитивне значення, що дає додатній аргумент логарифму
  print $ maybeLog10 2

  print $ "Task2"
  -- Позитивний тест, має повернути Just
  putStrLn "Тест 1 (очікується Just):"
  print $ superpositionDo 100
  print $ superpositionBind 100

  -- Тест, який призводить до Nothing на етапі u3
  putStrLn "\nТест 2 (очікується Nothing на u3):"
  print $ superpositionDo (1/38)
  print $ superpositionBind (1/38)

  -- Тест, який може призвести до помилки на етапі u2
  putStrLn "\nТест 3 (можливе Nothing на u2):"
  print $ superpositionDo 1
  print $ superpositionBind 1

  -- Тест, який проходить u3 і u2, але призводить до помилки на u1
  putStrLn "\nТест 4 (можливе Nothing на u1):"
  print $ superpositionDo 0.5
  print $ superpositionBind 0.5

  print $ "Task3"
  -- Позитивний тест
  putStrLn "Позитивний тест (очікується Just):"
  print $ maybeLog 10 2 -- n=2, x=10

  -- Позитивний тест
  putStrLn "Позитивний тест (очікується Just):"
  print $ maybeLog 10.8 7.5

  -- Граничний випадок
  putStrLn "\nГраничний випадок (очікується 0.0):"
  print $ maybeLog 1.5 2 -- n=2, x=1.5 (1/2 = 0.5, x - (1/2) = 1)

  -- Від'ємний випадок
  putStrLn "\nВід'ємний випадок (очікується Nothing):"
  print $ maybeLog 0.5 2 -- n=2, x=0.5

  print $ "Task4"
  -- Тест 1: x = 20, обидва аргументи повернуть Just значення
  putStrLn "Тест 1 (очікується Just):"
  print $ superpositionDoTask4 20
  print $ superpositionBindTask4 20

  -- Тест 2: x дуже малий, maybeSqrt поверне Nothing
  putStrLn "\nТест 2 (очікується Nothing через maybeSqrt):"
  print $ superpositionDoTask4 0.02
  print $ superpositionBindTask4 0.02

  -- Тест 3: x = 0.5, можливо, що maybeSqrtExpression поверне Nothing
  putStrLn "\nТест 3 (очікується Nothing через maybeSqrtExpression):"
  print $ superpositionDoTask4 0.5
  print $ superpositionBindTask4 0.5

  -- Тест 4: x дуже малий, обидва аргументи повернуть Nothing
  putStrLn "\nТест 4 (очікується Nothing, обидва аргументи):"
  print $ superpositionDoTask4 0.001
  print $ superpositionBindTask4 0.001
