--        Muhammad Zeeshan Akram
--        19100254
--        
--        README         --
--        Avreage and varience functions are taken from internet.

-- ========================================================================================================================== --


--
--                                                          ASSIGNMENT 1
--
--      A common type of text alignment in print media is "justification", where the spaces between words, are stretched or
--      compressed to align both the left and right ends of each line of text. In this problem we'll be implementing a text
--      justification function for a monospaced terminal output (i.e. fixed width font where every letter has the same width).
--
--      Alignment is achieved by inserting blanks and hyphenating the words. For example, given a text:
--
--              "He who controls the past controls the future. He who controls the present controls the past."
--
--      we want to be able to align it like this (to a width of say, 15 columns):
--
--              He who controls
--              the  past cont-
--              rols  the futu-
--              re. He  who co-
--              ntrols the pre-
--              sent   controls
--              the past.
--


-- ========================================================================================================================== --


import Data.List
import Data.Char
import Data.Int

text1 = "He who controls the past controls the future. He who controls the present controls the past."
text2 = "A creative man is motivated by the desire to achieve, not by the desire to beat others."


-- ========================================================================================================================== --


-- ========================================================= PART 1 ========================================================= --


--
-- Define a function that splits a list of words into two lists, such that the first list does not exceed a given line width.
-- The function should take an integer and a list of words as input, and return a pair of lists.
-- Make sure that spaces between words are counted in the line width.
--
-- Example:
--    splitLine ["A", "creative", "man"] 12   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 11   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 10   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 9    ==>   (["A"], ["creative", "man"])
--


splitLine :: [String] -> Int -> ([String], [String])
splitLine  [] k = ([],[])
splitLine  (x:xs) 0 = ([],x:xs)

splitLine  (x:xs) k 
     |  k < (length x)  = ([],(x:xs))
     | otherwise = ((x:a),b)
      where (a,b) = splitLine xs (k-length(x)-1)





-- Function definition here






-- ========================================================= PART 2 ========================================================= --


--
-- To be able to align the lines nicely. we have to be able to hyphenate long words. Although there are rules for hyphenation
-- for each language, we will take a simpler approach here and assume that there is a list of words and their proper hyphenation.
-- For example:

enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]


--
-- Define a function that splits a list of words into two lists in 'different ways'. The first list should not exceed a given
-- line width, and may include a hyphenated part of a word at the end. You can use the splitLine function and then attempt
-- to breakup the next word with a given list of hyphenation rules. Include a breakup option in the output only if the line
-- width constraint is satisfied.
-- The function should take a hyphenation map, an integer line width and a list of words as input. Return pairs of lists as
-- in part 1.
--
-- Example:
--    lineBreaks enHyp 12 ["He", "who", "controls."]   ==>   [(["He","who"], ["controls."]), (["He","who","co-"], ["ntrols."]), (["He","who","cont-"], ["rols."])]
--
-- Make sure that words from the list are hyphenated even when they have a trailing punctuation (e.g. "controls.")
--
-- You might find 'map', 'find', 'isAlpha' and 'filter' useful.
--

breakup :: [String] -> String-> [[String]]
breakup [] s = []
breakup (x:xs) s | xs == [] = [[x++s]]
breakup (x:xs) s =[[x++"-"]++ [concat xs++s]]++ breakup ([x++(head xs)]++tail xs) s


splitLine' :: Int -> [String] -> ([String], [String])
splitLine' k line = splitLine line k 

lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]
lineBreaks enHyp k [] = []
lineBreaks enHyp k line  |  k >= (foldl (+) 0 (map length line))+(length line) -1 = [splitLine line k]
                         | filter ((==( filter isAlpha (head(snd(splitLine line k)))  )).fst) enHyp /= [] = 

     let sL = splitLine line k 
         headElem  =  head(snd(sL)) 
         tailElems =  tail(snd(sL)) 
         lenOfFst  = foldl (+) (length (fst sL)) (map length (fst sL))
         breakElems = (filter (\x -> (length (head x) <= k-lenOfFst)) (breakup (snd (head(filter ((==( filter isAlpha headElem  )).fst) enHyp))) (filter (not.isAlpha) headElem) )) ++ [[headElem]]
     in 
   --  map (splitLine' k) (map (fst (sL)++) (breakup (snd (head(filter ((==( filter isAlpha headElem  )).fst) enHyp))) (filter (not.isAlpha) headElem) ))

     map (splitLine' k) (map (fst (sL)++) (map (++tailElems) breakElems))
                         | otherwise = [splitLine line k]



-- ========================================================= PART 3 ========================================================= --


--
-- Define a function that inserts a given number of blanks (spaces) into a list of strings and outputs a list of all possible
-- insertions. Only insert blanks between strings and not at the beginning or end of the list (if there are less than two
-- strings in the list then return nothing). Remove duplicate lists from the output.
-- The function should take the number of blanks and the the list of strings as input and return a lists of strings.
--
-- Example:
--    blankInsertions 2 ["A", "creative", "man"]   ==>   [["A", " ", " ", "creative", "man"], ["A", " ", "creative", " ", "man"], ["A", "creative", " ", " ", "man"]]
--
-- Use let/in/where to make the code readable
--
init' :: [String] -> [String]
init'  x =init (filter (/=" ") x)

insertAfter ::[String] -> String -> [String]
insertAfter [] s = []
insertAfter (x:xs) s  | s == x = [x]++[" "]++xs
                      | otherwise = x:insertAfter xs s

insertComp :: [[String]]-> [[String]]
insertComp  [] = []
insertComp (x:xs) = (map (insertAfter x) (init' x)) ++ insertComp xs


insert' :: Int -> [[String]]-> [[String]]
insert' 0 x = x
insert' k x | k < 0 = x
insert' k x =  insert' (k-1) (insertComp x)

-----------------------------
removeDuplicate :: [[String]]-> [[String]]
removeDuplicate [] = []
removeDuplicate (x:xs) | (x `elem` xs) = removeDuplicate xs
                       | otherwise = x : removeDuplicate xs




ankInsertions :: Int -> [String] -> [[String]]
ankInsertions k x = removeDuplicate (insert' k [x])





-- Function definition here



-- ========================================================= PART 4 ========================================================= --


--
-- Define a function to score a list of strings based on four factors:
--
--    blankCost: The cost of introducing each blank in the list
--    blankProxCost: The cost of having blanks close to each other
--    blankUnevenCost: The cost of having blanks spread unevenly
--    hypCost: The st of hyphenating the last word in the list
--
-- The cost of a list of strings is computed simply as the weighted sum of the individual costs. The blankProxCost weight equals
-- the length of the list minus the average distance between blanks (0 if there are no blanks). The blankUnevenCost weight is
-- the variance of the distances between blanks.
--
-- The function should take a list of strings and return the line cost as a double
--
-- Exampl0e:
--    lineCost ["He", " ", " ", "who", "controls"]
--        ==>   blankCost * 2.0 + blankProxCost * (5 - average(1, 0, 2)) + blankUnevenCost * variance(1, 0, 2) + hypCost * 0.0
--        ==>   blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666...
--
-- Use let/in/where to make the code readable
--


---- Do not modify these in the submission ----
blankCost = 1.0
blankProxCost = 1.0
blankUnevenCost = 1.0
hypCost = 1.0
-----------------------------------------------


blankDist :: [String] -> Int -> [Int]
blankDist []  k = [k]
blankDist x _ | find (==" ") x == Nothing = [0] 
blankDist (x:xs) k | x == " " = k : blankDist xs 0
                   | otherwise  =  blankDist xs (k+1)


---------------------------------------------------------------
----------------------- Taken From internet--------------------
mean :: (Floating a) => [a] -> a
mean xs = sum xs / (fromIntegral . length) xs

var :: (Eq a,Floating a) => [a] -> a
var [] = 0
var (x:xs) | xs == [] = 0
var xs = (sum $ map (\x -> (x - m)^2) xs) / (fromIntegral (length xs)-1)
    where
      m = mean xs
-------------------------------------------------------------------
-------------------------------------------------------------------


lineCost :: [String] -> Double

lineCost x =
 let doubleList =  map fromIntegral (blankDist x 0)
 in 
 blankCost * fromIntegral(length (filter (==" ") x)) + ( ((fromIntegral (length x)) - mean (doubleList)) * blankProxCost)  + (var (doubleList) * blankUnevenCost ) 


-- Function definition here



-- ========================================================= PART 5 ========================================================= --

--str = ["He", "who", "controls"]
--
-- Define a function that returns the best line break in a list of words given a cost function, a hyphenation map and the maximum
-- line width (the best line break is the one that minimizes the line cost of the broken list).
-- The function should take a cost function, a hyphenation map, the maximum line width and the list of strings to split and return
-- a pair of lists of strings as in part 1.
--
-- Example:
--    bestLineBreak lineCost enHyp 12 ["He", "who", "controls"]   ==>   (["He", "who", "cont-"], ["rols"])
--
-- Use let/in/where to make the code readable
--

-- lbFst => linebreak first (extract first from line break tuple)

zipFstSnd :: [[String]] -> [[[String]]] -> [[([String],[String])]]
zipFstSnd [] _ = []
zipFstSnd _ [] = []
zipFstSnd (x:xs) (y:ys)  = (map ((\x y -> (y,x)) x) y) : zipFstSnd xs ys


bestLineBreak :: ([String] -> Double) -> [(String, [String])] -> Int -> [String] -> ([String], [String])
bestLineBreak f enHyp k str =
 let 
  lbFst = map fst (lineBreaks enHyp k str)
  lbSnd = map snd (lineBreaks enHyp k str)
  
  lengthDiff =map (+ (k+1)) (map (*(-1)) (zipWith (+) (map length (map concat lbFst))  (map length lbFst)))


  allLists = (concat (zipFstSnd lbSnd (zipWith ($) (map ankInsertions lengthDiff) lbFst)))
  minLineCost = minimum(map (\(x,y) -> f x) allLists)
  in 
   head (filter (\ (x,y) -> f x == minLineCost ) allLists)

-- Function definition here


--
-- Finally define a function that justifies a given text into a list of lines satisfying a given width constraint.
-- The function should take a cost function, hyphenation map, maximum line width, and a text string as input and return a list of
-- strings.
--
-- 'justifyText lineCost enHyp 15 text1' should give you the example at the start of the assignment.
--
-- You might find the words and unwords functions useful.
--

rmExtraSpace :: [String] -> [String]
rmExtraSpace [] = []
rmExtraSpace (x:xs) | xs == [] = [x]
rmExtraSpace (x:xs) | (head xs) == " " || x == " " = concat [x++(head xs)] : rmExtraSpace (tail xs)
rmExtraSpace (x:xs) | otherwise =  x : (head xs) : rmExtraSpace (tail xs)




helper :: ([String] -> Double) -> [(String, [String])] -> Int -> [String] -> [String]
helper f enHyp k [] = []
helper f enHyp k str | k >= (foldl (+) 0 (map length str))+(length str) -1  = str ++ ["\n"]

helper f enHyp k str = a ++["\n"]++ helper f enHyp k b
       where (a,b) = bestLineBreak f enHyp k (str)

justifyText :: ([String] -> Double) -> [(String, [String])] -> Int -> String -> [String]
justifyText f enHyp k str = rmExtraSpace (helper lineCost enHyp k (words str))


-- Function definition here





