module Naive
    (greedyCompress
    ,SubstringOccurences
    ,substring
    ,substringLen
    ,soCount
    ,getSubstrings) where

import Data.List (foldl', sortBy, isPrefixOf)

-- stripPrefix isn't supported in hugs (used for development while on a
-- trip) -> reimplement it, using the isPrefixOf function that's actually
-- there.
stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix prefix x = if (prefix `isPrefixOf` x)
    then Just $ drop (length prefix) x
    else Nothing

-- import Data.List.NonEmpty -- didn't work for me, so I copy-pasted instead
-- and implemented pushNonEmptyList
-- 
-- data NonEmpty a
--     = NonEmpty { neHead :: a
--                , neTail :: [a]
--                }
--     deriving (Eq, Show)
-- 
-- -- | Semantic function for NonEmpty
-- nonEmptyToList :: NonEmpty a -> [a]
-- nonEmptyToList ne = neHead ne : neTail ne
-- 
-- pushNonEmptyList :: a -> NonEmpty a -> NonEmpty a
-- pushNonEmptyList x (NonEmpty h t) = NonEmpty x (h:t)

data SubstringOccurences
   = SubstringOccurences {
        soParent :: String
      , soFrom :: Int
      , soTo :: Int
      , soOccursTemp :: [Int]
      , soCount :: Int
     }
     deriving (Eq, Show)

-- preferred constructor
makeSubstringOccurences :: String -> Int -> Int -> SubstringOccurences
makeSubstringOccurences parent from to = SubstringOccurences parent from to [from] 0

theSEED = SubstringOccurences "" 0 0 [] 0

parentString :: SubstringOccurences -> String
parentString = soParent

substringFrom :: SubstringOccurences -> Int
substringFrom = soFrom

substringTo :: SubstringOccurences -> Int
substringTo = soTo

substringOccurs :: SubstringOccurences -> [Int]
substringOccurs = soOccursTemp

substringLen :: SubstringOccurences -> Int
substringLen x = (substringTo x) - (substringFrom x)

substring :: SubstringOccurences -> String
substring x = Prelude.take (to - from) (Prelude.drop from str)
        where to = substringTo x
              from = substringFrom x
              str = parentString x

pushOccurence :: Int -> SubstringOccurences -> SubstringOccurences
pushOccurence x (SubstringOccurences str from to occurs count) = SubstringOccurences str from to (x:occurs) count

-- UNSAFE: what if list is empty?
getFirstOccurence = head . substringOccurs

consolidateSubstringOccurences x = if null occurs
                                    then x
                                    else SubstringOccurences parent from to [] (count + (length occurs))
                                where (SubstringOccurences parent from to occurs count) = x

isNotOverlapping :: Int -> SubstringOccurences -> Bool
isNotOverlapping _ so | null (substringOccurs so) = True
isNotOverlapping from so = ((from + (substringLen so)) <= (getFirstOccurence so))

isOccurenceTail :: String -> Int -> SubstringOccurences -> Bool
-- expensive case: this occurence not seen yet at all in this parent
-- string, so need to do full string compare
isOccurenceTail parent from so | null (soOccursTemp so) = string_equal parent (substring so) (from + 1) (substringLen so)
-- cheap case: this occurence has already been seen in this parent.
-- If indices line up, there's a match
isOccurenceTail parent from so = (from + 1) == (getFirstOccurence so)

maybesToList :: [Maybe a] -> [a]
maybesToList [] = []
maybesToList ((Just x):xs) = x:maybesToList xs
maybesToList (Nothing:xs) = maybesToList xs

string_equal :: String -> String -> Int -> Int -> Bool
string_equal s1 s2 s1_start s2_len = string_equal_1 (drop s1_start s1) s2 s2_len

string_equal_1 :: String -> String -> Int -> Bool
string_equal_1 [] _ leng = leng < 1
string_equal_1 _ [] leng = if leng < 1
                              then True
                              else error "Ohoh"
string_equal_1 (x:xs) (y:ys) leng = (leng < 1) || ((x == y) && (string_equal_1 xs ys (leng - 1)))

isNewOccurence :: String -> Int -> SubstringOccurences -> Bool
isNewOccurence parent from so = isNotOverlapping from so &&
                                (string_equal parent (substring so) from len)
                    where len = (substringLen so)

updateOccurences :: String -> Int -> SubstringOccurences -> (Maybe SubstringOccurences, Maybe SubstringOccurences)
updateOccurences parent from so = (updated_so, new_so)
                            where updated_so = if isNewOccurence parent from so
                                                  then Just (pushOccurence from so)
                                                  else Nothing
                                  new_so = if isOccurenceTail parent from so
                                              then Just $ makeSubstringOccurences parent from (from + 1 + substringLen so)
                                              else Nothing

updateOccurencesList :: String -> Int -> [SubstringOccurences] -> [SubstringOccurences]
updateOccurencesList parent from so_list = foldl' (generateUpdatedThenMerge parent from) so_list so_list

generateUpdatedThenMerge :: String -> Int -> [SubstringOccurences] -> SubstringOccurences -> [SubstringOccurences]
generateUpdatedThenMerge parent from so_list so = mergeUpdated (updateOccurences parent from so) so_list

mergeUpdated :: (Maybe SubstringOccurences, Maybe SubstringOccurences) -> [SubstringOccurences] -> [SubstringOccurences]
mergeUpdated (Nothing, Nothing) so_list = so_list
mergeUpdated (Nothing, Just x) so_list = if findSubstringOccurencesInList x so_list
                                            then so_list -- no need to insert, same will be picked up sooner or later
                                            else x:so_list
mergeUpdated (Just x, Nothing) so_list = replaceSubstringOccurencesInList x so_list
mergeUpdated (Just x, Just y) so_list = (mergeUpdated (Nothing, Just y)) . (mergeUpdated (Just x, Nothing)) $ so_list

eqSubstringOccurencesInList :: SubstringOccurences -> SubstringOccurences -> Bool
eqSubstringOccurencesInList x y = (substring x) == (substring y)

findSubstringOccurencesInList :: SubstringOccurences -> [SubstringOccurences] -> Bool
findSubstringOccurencesInList so [] = False
findSubstringOccurencesInList so (x:xs) = (eqSubstringOccurencesInList so x) || (findSubstringOccurencesInList so xs)

replaceSubstringOccurencesInList :: SubstringOccurences -> [SubstringOccurences] -> [SubstringOccurences]
replaceSubstringOccurencesInList x (y:ys) = if (eqSubstringOccurencesInList x y)
                                               then x:ys
                                               else y:(replaceSubstringOccurencesInList x ys)

countDown :: Int -> [Int]
countDown start = map (start -) [0..start]

getSubstrings_1 :: String -> [SubstringOccurences] -> [SubstringOccurences]
-- get all substrings and their occurences in one string
getSubstrings_1 str x = map consolidateSubstringOccurences $ foldl' (\ acc x -> updateOccurencesList str x acc) x (countDown ((length str) - 1))

getSubstrings :: [String] -> [SubstringOccurences]
-- get all substrings and their occurences in the list of strings
getSubstrings = foldl' (\acc x -> getSubstrings_1 x acc) [theSEED]

compressionGain :: SubstringOccurences -> Int
-- gain obtained by reserving substring + 1 bytes in memory to represent it using 1 byte only
--
-- This assumes that only the given substring is replaced by a one byte reference.
--
-- Consider these two substrings:
-- "aa" and "aaaa" used to compress "aaaaaabaaaabaaaaaa"
-- "aa" is seen 8 times, so the gain would be computed as 2 * 8 - 8 - 3 = 5
-- and "aaaa" is seen 3 times, so the gain would be computed as 4 * 3 - 3 - 5 = 4
-- But actually, "aaaaaabaaaabaaaaaa" can be written is "\xFE\xFFb\xFEb\xFE\xFF" (7 bytes),
-- storing "aa" in \xFF (3 bytes) and "aaaa" as "\xFF\xFF" in \xFE (3
-- bytes), for a total gain of 18 - 7 - 3 - 3 = 5 bytes which doesn't
-- match the expected 9 bytes gain from the naive computations above:
-- (1) compressing "aa" delivered less gain because there were less
--     occurences after replacing "aaaa"
-- (2) OTOH, compressing "aaaa" delivered a bit more gain than expected
--     because we could store the substring using the "aa" substring
compressionGain so = substringLen so * soCount so -- initial space taken
                     - soCount so -- deduct the 1 byte each to point to the substring
                     - (substringLen so + 1) -- deduct space necessary to store substring once

orderCompressionCandidatesBy :: Ord a => (t -> a) -> t -> t -> Ordering
orderCompressionCandidatesBy metric a b
  | x == y = EQ
  | x > y = GT
  | otherwise = LT
  where x = metric a
        y = metric b

betterCompressionCandidate :: SubstringOccurences -> SubstringOccurences -> Ordering
-- order 2 substring occurences according to their (naive) expected compression gain
betterCompressionCandidate = orderCompressionCandidatesBy $ negate . compressionGain

getCompressionCandidates = sortBy betterCompressionCandidate . filter (\x -> compressionGain x > 0) . getSubstrings

-- from http://haskell.1045720.n5.nabble.com/Newbie-Replacing-substring-td3113520.html
-- NB: if old==[], this generates an infinite list of new!
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace old new ys
    Just ys' -> new ++ replace old new ys'

firstEltNotInOther :: [SubstringOccurences] -> [SubstringOccurences] -> Maybe SubstringOccurences
firstEltNotInOther (x:xs) [] = Just x
firstEltNotInOther [] _ = Nothing
firstEltNotInOther (x:xs) other
  | findSubstringOccurencesInList x other = firstEltNotInOther xs other
  | otherwise = Just x

doReplace :: [String] -> (SubstringOccurences, String) -> [String]
doReplace strings (substr, slot) = map (replace (substring substr) slot) strings

execCompression :: [(SubstringOccurences, String)] -> [String] -> [String]
execCompression substitutions strings = foldl' doReplace strings substitutions
-- Test execCompression:
-- let s1 = "aaaaaabaaaabaaaaaa"
--     s2 = "baobab"
--  in execCompression [(makeSubstringOccurences s1 0 4, "4"), (makeSubstringOccurences s2 0 2, "B"), (makeSubstringOccurences s1 0 2, "2")] [s1, s2]
-- should return ["42b4b42","BoBb"]

-- ! do not compress the substring occurence with itself !
execCompressionOnSlots :: [(SubstringOccurences, String)] -> [String]
execCompressionOnSlots substitutions = e1 substitutions [] (map (substring . fst) substitutions)
-- Test execCompressionOnSlots:
-- let s1 = "aaaaaabaaaabaaaaaa"
--     s2 = "baobab"
--  in execCompressionOnSlots [(makeSubstringOccurences s1 0 4, "4"), (makeSubstringOccurences s2 0 2, "B"), (makeSubstringOccurences s1 0 2, "2")]
-- should return ["22", "ba", "aa"]
-- Notice how "aaaa" is represented by "22", using the compression of "aa"

e1 :: [(SubstringOccurences, String)] -> [String] -> [String] -> [String]
e1 (subst:substs) out_before (out:outs) = e1 substs (out:doReplace out_before subst) (doReplace outs subst)
e1 [] out_before [] = reverse out_before
e1 [] _ outs = error "Should never happen, there were more strings than substitutions"

selectNewCandidate :: [(SubstringOccurences, String)] -> [String] -> Maybe SubstringOccurences
selectNewCandidate substitutions strings = firstEltNotInOther (getCompressionCandidates
                                                                ((execCompressionOnSlots substitutions) ++
                                                                 (execCompression substitutions strings)))
                                                              (map fst substitutions)

-- selectCandidates :: [String] -> [(SubstringOccurences, String)] -> [String] -> Int -> ([(String, String)], [String])
-- selectCandidates slots substitutions strings n
--   | n == 0 = (drop slot_count strings, zip slots $ take slot_count strings)
--   | otherwise = selectCandidates slots new_subst (execCompression new_subst new_substring strings slots_taken) (n - 1)
--   where slot_count = length slots
--         slots_taken = slot_count - n                                                         
--         new_substring = firstEltNotInOther (getCompressionCandidates strings)
--                                            (map snd substitutions)
--         new_subst = zip (map fst substitutions ++ [new_substring]) slots

selectCandidates :: [String] -> [(SubstringOccurences, String)] -> [String] -> [(SubstringOccurences, String)]
selectCandidates (slot:slots) substitutions strings = case selectNewCandidate substitutions strings of
  Just substringOccurences -> selectCandidates slots (substitutions ++ [(substringOccurences, slot)]) strings
  Nothing -> substitutions
selectCandidates [] substitutions strings = substitutions

greedyCompress :: [String] -> [String] -> ([String], [(String, String)], Int)
greedyCompress orig_strings slots = (compressed_strings,
                                     compressed_slots,
                                     gain)
  where substitutions = selectCandidates slots [] orig_strings
        compressed_strings = execCompression substitutions orig_strings
        compressed_slots = zip slots $ execCompressionOnSlots substitutions
        totalLength ss = foldl' (+) 0 . map length $ ss
        gain = totalLength orig_strings -
               totalLength compressed_strings -
               (totalLength . map snd $ compressed_slots) -
               length compressed_slots
-- main = do
--     putStrLn (show $ greedyCompress strings ["1", "2", "3", "4", "5", "6", "7"])
--   where strings = ["aaaaaabaaaabaaaaaa", "aabaaobaabaa", "Hello world", "Hellenic", "Helen", "Hell's Angels", "Angelina", "Anger MAnagement"]

-- main = do
--   hSlot <- openFile "slots.txt" ReadMode
--   hSetEncoding hSlot latin1
--   c <- hGetContents hSlot
--   let slots = lines c
--   hStrings <- openFile "strings.txt" ReadMode
--   hSetEncoding hStrings latin1
--   c <- hGetContents hStrings
--   let strings = lines c
--   print (map length [strings, slots])
--   --print (greedyCompress strings slots)
--   print (take 5 (getSubstrings strings))
--   hClose hSlot
--   hClose hStrings
