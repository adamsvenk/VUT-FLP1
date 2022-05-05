{-
    File: Main.hs
    Project: VUT-FIT FLP Functional project (simplify-bkg)
    Author: Adam Švenk (xsvenk00)
    Version: 1.0
    Date: 2022-04-10
-}

{- 
    Import modules
-}
module Main (main) where
import System.IO (hGetContents, openFile, IOMode (ReadMode))
import GHC.IO.Handle.FD ( stdin )
import System.Environment (getArgs)
import Data.List (nub, sort)

{-
    Definition of the rule and the rules data types
-}
type Rule = (Char, [Char])
type Rules = [Rule]

{- Modify terms of both types (non-terminal characters and terminal characters) to correct printable format
    Input : Set of non-terminal characters or terminal characters
    Output : [non-terminal/terminal character],[non-terminal/terminal character],...
-}
allTermTypesString :: [Char] -> [Char]
allTermTypesString [] = "\n"
allTermTypesString [x] = x : "\n"
allTermTypesString (x:xs) = x : ',' : allTermTypesString xs

{- Modify rules to correct printable format
    Input : Set of rules
    Output : [non-terminal characters]->[non-terminal + terminals characters]
-}
rulesString :: Rules -> [Char]
rulesString [] = ""
rulesString ((x,a):xs) = x : "->" ++ a ++ "\n" ++ rulesString xs

{- Concatenate all elements of context-free grammar to correct printable format
    Input : Context-free grammar
    Output : [non-terminal characters]
             [terminal characters]
             [start symbol]
             [rule 1]
             [rule 2]
             .
             .
             .
             [rule n]
-}

{- Modify context-free grammar to printable format
    Input : Context-free grammar
    Output : Context-free grammar in the printable format
-}
cfgString :: CFG -> [Char]
cfgString inputGrammar = printNonTerms ++ printTerms ++ printStartSymbol ++ "\n" ++ printRules
    where
        printNonTerms = allTermTypesString (sort (nonterminals inputGrammar))
        printTerms = allTermTypesString (sort (terminals inputGrammar))
        printStartSymbol = [startSymbol inputGrammar]
        printRules = rulesString (rules inputGrammar)

{-
    Definition of the context-free grammar data type
-}
data CFG = CFG {nonterminals :: [Char],
                terminals :: [Char],
                startSymbol :: Char,
                rules :: Rules
}

{-
    Definition of the program parameters data type
-}
data Params = Params {
    option :: Integer,
    file :: [Char]
}

{- A multiline comment
    which can continue for many lines
-}
main :: IO ()
main = do
    args <- System.Environment.getArgs

    let params = processArgs args

    inputHandle <-
        if file params == ""
        then return GHC.IO.Handle.FD.stdin
        else System.IO.openFile (file params) System.IO.ReadMode

    inputContent <- System.IO.hGetContents inputHandle

    let inputLines = lines inputContent
    putStr (getSimplifyOuput params inputLines)

{- Get correct printable output based on the 
    Input : Program parameters
    Input : Set of all input strings
    Output : Printable simplified output
    Note : If the program parameters are wrong, print an error
-}
getSimplifyOuput :: Params -> [[Char]] -> [Char]
getSimplifyOuput (Params 0 _) inputGrammar = cfgString (parseGrammar inputGrammar)
getSimplifyOuput (Params 1 _) inputGrammar = cfgString (simplify1stStep (parseGrammar inputGrammar))
getSimplifyOuput (Params 2 _) inputGrammar = cfgString (simplify2ndStep (simplify1stStep (parseGrammar inputGrammar)))
getSimplifyOuput _ _ = error "Error: Wrong option"

{- Create the grammar based on the second step of the Algorithm 4.3
   Remove all terminal and non-terminal characters, which are unreachable
    Input : Context-free grammar from the first step of the Algorithm 4.3
    Output : Context-free grammar from the second step of the Algorithm 4.3
-}
simplify2ndStep :: CFG -> CFG
simplify2ndStep inputGrammar = CFG simplifyNonTerms terms sSymbol filterRules
    where
        simplifyNonTerms = nub (optimiseAllTerms inputGrammar [sSymbol] `intersAllElems` nonterminals inputGrammar)
        terms = nub (optimiseAllTerms inputGrammar [sSymbol] `intersAllElems` terminals inputGrammar)
        sSymbol = startSymbol inputGrammar
        filterRules = getNewRules (rules inputGrammar) (simplifyNonTerms ++ terms ++ [lookForEps (rules inputGrammar)])

{- Implementation of the Algorithm 4.2 based from the course VUT-FIT-TIN study text
    Input : Context-free grammar
    Input : Set of terminal and non-terminal characters Vi (for more details view study text for the course VUT-FIT-TIN)
    Output : New set of terminal and non-terminal characters
-}
optimiseAllTerms :: CFG -> [Char] -> [Char]
optimiseAllTerms inputGrammar previous =
    if previous == optimiseAllTermsStep (filterRRulesOut (rules inputGrammar) previous) previous `unifyAllElems` previous
    then previous
    else optimiseAllTerms inputGrammar (optimiseAllTermsStep (filterRRulesOut (rules inputGrammar) previous) previous `unifyAllElems` previous)

{- One iteration of the Algorithm 4.2
    Input : Rules
    Input : Set of terminal and non-terminal characters
    Output : Set of terminal and non-terminal characters
-}
optimiseAllTermsStep :: Rules -> [Char] -> [Char]
optimiseAllTermsStep [] _ = []
optimiseAllTermsStep ((rl,rr):rs) allTerms =
    if rl `elem` allTerms
    then optimiseAllTermsStep rs allTerms ++ rr
    else optimiseAllTermsStep rs allTerms

{- Check if second set contains at leats one of the elements from the first set
    Input : Set 1
    Input : Set 2
    Output : Boolean
-}
atLeastOneElem :: Eq a => [a] -> [a] -> Bool
atLeastOneElem [] _ = False
atLeastOneElem _ [] = False
atLeastOneElem (x:xs) y = x `elem` y || atLeastOneElem xs y

{- Create rules, that contain only allowed characters of the left side
    Input : Rules
    Input : Set of terminal and non-terminal characters
    Output : New set of rules
-}
filterRRulesOut :: Rules -> [Char] -> Rules
filterRRulesOut [] _ = []
filterRRulesOut (r@(rl,__):rs) allTerms = ([r | allTerms `atLeastOneElem` [rl]]) ++ filterRRulesOut rs allTerms

{- Process all program arguments
    Input : Program arguments
    Output : Program parameters including [option] and [file]
-}
processArgs :: [[Char]] -> Params
processArgs [] = error "Error: Wrong program parameters"
processArgs [argOption] = Params (processOption argOption) ""
processArgs [argOption, argFileName] = Params (processOption argOption) argFileName
processArgs _ = Params (-1) ""

{- Process program option argument
    Input : [option] program argument
    Option : option identifier based on the program argument
-}
processOption :: [Char] -> Integer
processOption argOption = case argOption of
    "-i" -> 0 -- Print context-free grammar from internal representation
    "-1" -> 1 -- Print context-free grammar after 1st step of the Algorithm 4.3
    "-2" -> 2 -- Print context-free grammar after 2nd step of the Algorithm 4.3
    _ -> -1 -- Unknown option

{- Parse one rule of the context-free grammar
    Input : Rule in the string form
    Output : Rule in correct form
    Note : If the rule is not formally correct, print an error
-}
parseRule :: [Char] -> Rule
parseRule (n:'-':'>':nt) =
    if nt /= []
    then (n, nt)
    else error "Error: Rule is in a wrong format"
parseRule _ = error "Error: Rule is in a wrong format"

{- Parse rules of the context-free grammar
    Input : Rules in the string form
    Output : Rules in correct form
-}
parseRules :: [[Char]] -> Rules
parseRules [] = []
parseRules [x] = [parseRule x]
parseRules (x:xs) = parseRule x : parseRules xs

{- Parse non-terminal characters of the context-free grammar
    Input : Non-terminal characters in the string form
    Output : Non-terminal characters in correct form
    Note : If one of the non-terminal characters is not formally correct, print an error
-}
parseNonTerms :: [Char] -> [Char]
parseNonTerms [x] = [validateNonTerm x]
parseNonTerms (x:',':xs) = x : parseNonTerms xs
parseNonTerms _ = error "Error: Non-terminals are in a wrong format"

{- Parse terminal characters of the context-free grammar
    Input : Terminal characters in the string form
    Output : Terminal characters in correct form
    Note : If one of the terminal characters is not formally correct, print an error
-}
parseTerms :: [Char] -> [Char]
parseTerms [] = []
parseTerms [x] = [validateTerm x]
parseTerms (x:',':xs) = x : parseTerms xs
parseTerms _ = error "Error: Terminals are in a wrong format"

{- Parse start symbol of the context-free grammar
    Input : Start symbol in the string form
    Output : Start symbol as non-terminal character
    Note : If the start symbol is not non-terminal, print an error
-}
parseStartSymbol :: [Char] -> Char
parseStartSymbol [x] = validateNonTerm x
parseStartSymbol _ = error "Error: Start symbol is not correct"

{- Parse context-free grammar
    Input : Context-free grammar in the string form
    Ouput : Parsed context-free grammar
-}
parseGrammar :: [[Char]] -> CFG
parseGrammar (n:t:d:rs) = CFG (parseNonTerms n) (parseTerms t) (parseStartSymbol d) (parseRules rs)
parseGrammar _ = error "Error: Input is in a wrong format"

{- Create intersection of two sets
    Input : Set 1
    Input : Set 2
    Output : Set 1 & Set 2 intersected together
-}
intersAllElems :: Eq a => [a] -> [a] -> [a]
intersAllElems [] _ = []
intersAllElems xs ys = filter (`elem` xs) ys

{- Merge two sets without creating duplicates
    Input : Set 1
    Input : Set 2
    Output : Set 1 & Set 2 combined together without any duplicates
-}
unifyAllElems :: Eq a => [a] -> [a] -> [a]
unifyAllElems [] y = y
unifyAllElems (x:xs) y =
    if x `elem` y
    then unifyAllElems xs y
    else unifyAllElems xs y++[x]

{- Validate input non-terminal charater
    Input : Non-terminal character
    Output : Non-terminal character
    Note : If the character is not from the [A-Z] set, print an error
-}
validateNonTerm :: Char -> Char
validateNonTerm '|' = error "Error: Non-terminal is not valid"
validateNonTerm x =
    if x `elem` ['A'..'Z']
    then x
    else validateNonTerm '|'

{- Validate input terminal charater
    Input : Terminal character
    Output : Terminal character
    Note : If the character is not from the [a-z] set, print an error
-}
validateTerm :: Char -> Char
validateTerm '|' = error "Error: Terminal is not valid"
validateTerm x =
    if x `elem` ['a'..'z']
    then x
    else validateTerm '|'

{- Create the grammar based on the first step of the Algorithm 4.3
   Remove all non-terminal characters from the input grammar, which do not generate terminal strings.
    Input : Context-free grammar
    Output : Context-free grammar from the first step of the Algorithm 4.3
-}
simplify1stStep :: CFG -> CFG
simplify1stStep inputGrammar = CFG simplifyNonTerms terms sSymbol filterRules
    where
        simplifyNonTerms = removeNonTerms inputGrammar [] `unifyAllElems` [startSymbol inputGrammar]
        terms = terminals inputGrammar
        sSymbol = startSymbol inputGrammar
        filterRules = getNewRules (rules inputGrammar) (simplifyNonTerms ++ terms ++ [lookForEps (rules inputGrammar)])

{- Implementation of the Algorithm 4.1 based from the course VUT-FIT-TIN study text
    Input : Context-free grammar
    Input : Set of non-terminal characters Ni (for more details view study text for the course VUT-FIT-TIN)
    Output : New set of non-terminal characters
-}
removeNonTerms :: CFG -> [Char] -> [Char]
removeNonTerms inputGrammar previous =
    if previous == getNewNonTermFromRules (rules inputGrammar) (terminals inputGrammar ++ previous ++ [lookForEps (rules inputGrammar)])
    then previous
    else removeNonTerms inputGrammar (getNewNonTermFromRules (rules inputGrammar) (terminals inputGrammar ++ previous ++ [lookForEps (rules inputGrammar)]))

{- Check if all elements of one set are elements of the other set (superset)
    Input : Set
    Input : Superset
    Output : Boolean [True/False] based on the condition
-}
areAllElementsOf :: [Char] -> [Char] -> Bool
areAllElementsOf [] _ = True
areAllElementsOf _ [] = False
areAllElementsOf (x:xs) superSet = x `elem` superSet && xs `areAllElementsOf` superSet

{- Implementation of the Algorithm 4.1 based from the course VUT-FIT-TIN study text
    Input : Set of rules
    Input : Set of all terminal characters (non-terminal characters and terminal characters)
    Output : New set of non-terminal characters
-}
getNewNonTermFromRules :: Rules -> [Char] -> [Char]
getNewNonTermFromRules [] _ = []
getNewNonTermFromRules ((rl,rr):rs) allTerms =
    if rr `areAllElementsOf` allTerms
    then getNewNonTermFromRules rs allTerms `unifyAllElems` [rl]
    else getNewNonTermFromRules rs allTerms

{- Create new set of rules that contains only allowed characters
    Input : Set of rules
    Input : Set of all allowed characters
    Output : New set of rules
-}
getNewRules :: Rules -> [Char] -> Rules
getNewRules [] _ = []
getNewRules (r@(rl,rr):rs) allTerms = ([r | (rl : rr) `areAllElementsOf` allTerms]) ++ getNewRules rs allTerms

{- Check if the rules contains one with epsilon (#) character
    Input : Set of rules
    Output : Boolean
-}
lookForEps :: Rules -> Char
lookForEps [] = '\0'
lookForEps ((_,rr):rs) = if '#' `elem` rr then '#' else lookForEps rs