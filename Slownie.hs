{-Piotr Maślankowski
  Pracownia nr 4
  Metody programowania 2015/16
  Moduł Slownie -}

module Slownie (Rodzaj(..), Waluta(..), slownie) where

import Data.List
import Data.Char

data Rodzaj = Meski | Zenski | Nijaki deriving Show
data Wersja = Male | Srednie | Wielkie deriving Show --wersja odmiany - małe: jeden, srednie: dwa - piec, duze: > piec oraz zero

data Waluta = Waluta {
  mianownik_poj :: String,
  mianownik_mn :: String,
  dopelniacz_mn :: String,
  rodzaj :: Rodzaj
} deriving Show

--rozwijamy liczbę na cyfry, wykładniki i ogony
rozwinLiczbe :: Integer -> [(Int, Int, Integer)]
rozwinLiczbe n = zip3 cyfry wykladniki sufiksy where
  cyfry = map (digitToInt) $ show n
  wykladniki = reverse [0..k] where k = genericLength cyfry - 1
  sufiksy = map (read . map (intToDigit) ) $ tails cyfry

--funkcja tworzy listę par: (n_i, k) - oznaczenia jak w treści zadania, taką, że n = n_k * 1000^k + ... + n_0
tworzPary :: Integer -> Integer -> [(Integer, Integer)]
tworzPary n wykl = zip ns $ reverse [0..k] where
  ns = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` wykl, x `div` wykl)) n
  k = genericLength ns - 1

wypiszMalaLiczbe :: Integer -> Rodzaj -> String
wypiszMalaLiczbe n rodz = let dwa Zenski = "dwie "
                              dwa _ = "dwa "

                              male = ["", "jeden ", dwa rodz, "trzy ", "cztery ", "pięć ",
                                    "sześć ", "siedem ", "osiem ", "dziewięć ", "dziesięć ",
                                    "jedenaście ", "dwanaście ", "trzynaście ", "czternaście ",
                                    "piętnaście ", "szesnaście ", "siedemnaście ", "osiemnaście ",
                                    "dziewiętnaście ", "dwadzieścia "]
                              nazwy = [[],
                                       ["", "dziesięć", "dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt",
                                        "sześćdziesiąt", "siedemdziesiąt", "osiemdziesiąt", "dziewięćdziesiąt"],
                                       ["", "sto", "dwieście", "trzysta", "czterysta", "pięćset", "sześćset", "siedemset", "osiemset", "dziewięćset"]]
                          in foldr (\(a,b,c) acc -> if c < 20 then (male !! (fromIntegral c))
                                    else nazwy !! b !! a ++ " " ++ acc ) [] $ rozwinLiczbe n

--znajdujemy nazwę dla dużych liczb
znajdzNazwe :: Integer -> Wersja -> String
znajdzNazwe n wer | n == 0 = ""
                  | n == 1 = odmienSufiks "tysiąc" wer
                  | k < 10 = ["", "mi", "bi", "try", "kwadry", "kwinty", "seksty", "septy", "okty", "noni"] !! (fromIntegral k) ++ (odmienSufiks suf wer)
                  | otherwise = let przedrostki = [ ["", "un", "do", "tri", "kwatuor", "kwin", "seks", "septen", "okto", "nowem"],
                                                    ["", "decy", "wicy", "trycy", "kwadragi", "kwintagi", "seksginty", "septagi", "oktagi", "nonagi"],
                                                    ["", "centy", "ducenty", "trycenty", "kwadryge", "kwinge", "sescenty", "septynge", "oktynge", "nonge"]]
                                    pary = tworzPary k 10
                                in foldl (\acc x -> (przedrostki !! (fromIntegral $ snd x)) !! (fromIntegral $ fst x) ++ acc) [] pary ++ (odmienSufiks suf wer)
                  where k = n `div` 2
                        suf = if n `mod` 2 == 0 then "lion" else "liard"

odmienSufiks :: String -> Wersja -> String
odmienSufiks suf wer | suf == "tysiąc" = case wer of
                                            Male -> "tysiąc "
                                            Srednie -> "tysiące "
                                            Wielkie -> "tysięcy "
                     | otherwise = suf ++ case wer of
                                            Male -> " "
                                            Srednie -> "y "
                                            Wielkie -> "ów "

wyznaczWersje :: Integer -> Wersja
wyznaczWersje n | n == 1 = Male
                | (n `elem` [5..19]) || (n `mod` 100) `elem` [5..19] || ((n `mod` 10) `elem` (0:1:[5..9])) = Wielkie
                | otherwise {-(n `mod` 10) `elem` [2..4]-} = Srednie

wyznaczWalute :: Integer -> Waluta -> String
wyznaczWalute n wal | n == 1 = mianownik_poj wal
                    | (n `mod` 100) `elem` 0:1:[5..20] || (n `mod` 10) `elem` 0:1:[5..9] = dopelniacz_mn wal
                    | otherwise = mianownik_mn wal

tlumacz :: [(Integer, Integer)] -> Rodzaj -> String
tlumacz ls rodz = concatMap (\(a,b) ->
                                  if a /= 0 then
                                    if a == 1 && b /= 0 then
                                      (znajdzNazwe b $ wyznaczWersje a)
                                    else
                                      wypiszMalaLiczbe a (wyznaczRodzaj b) ++ (znajdzNazwe b $ wyznaczWersje a)
                                  else
                                    "") ls
                    where wyznaczRodzaj x = if x == 0 then rodz else Meski

slownie :: Waluta -> Integer -> String
slownie wal n | n == 0 = "zero " ++ dopelniacz_mn wal
              | n < 0 = "minus " ++ aux (abs n) wal
              | otherwise = aux n wal
              where
                aux n wal | n == 0 = "zero " ++ dopelniacz_mn wal
                          | n == 1 = "jeden " ++ mianownik_poj wal
                          | otherwise = let tmp = tlumacz (tworzPary n 1000) $ rodzaj wal
                                        in tmp ++ (wyznaczWalute n wal)
