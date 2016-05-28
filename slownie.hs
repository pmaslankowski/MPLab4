{-Piotr Maślankowski
  Pracownia nr 4
  Metody programowania 2015/16}
  Program slownie
-}
  
import System.Environment
import Slownie
import Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ wypiszWynik (args !! 1) ((read (args !! 0)) :: Integer)

wypiszWynik :: String -> Integer -> String
wypiszWynik wal n = slownie (exec $ Map.lookup wal waluty) n
  where
  exec :: Maybe a -> a
  exec (Just x) = x
  waluty = fromList [("AUD", Waluta "dolar australijski" "dolary australijskie" "dolarów australijskich" Meski),
                    ("BGN", Waluta "lew" "lewy" "lewów" Meski),
                    ("BRL", Waluta "real" "reale" "reali" Meski),
                    ("BYR", Waluta "rubel białoruski" "ruble białoruskie" "rubli białoruskich" Meski),
                    ("CAD", Waluta "dolar kanadyjski" "dolary kanadyjskie" "dolarów kanadyjskich" Meski),
                    ("CHF", Waluta "frank szwajcarski" "franki szwajcarskie" "franków szwajcarskich" Meski),
                    ("CNY", Waluta "yuan renminbi" "yuany renminbi" "yuanów renminbi" Meski),
                    ("CZK", Waluta "korona czeska" "korony czeskie" "koron czeskich" Zenski),
                    ("DKK", Waluta "korona duńska" "korony duńskie" "koron duńskich" Zenski),
                    ("EUR", Waluta "euro" "euro" "euro" Nijaki),
                    ("GBP", Waluta "funt szterling" "funty szterlingi" "funtów szterlingów" Meski),
                    ("HKD", Waluta "dolar Hongkongu" "dolary Hongkongu" "dolarów Hongkongu" Meski),
                    ("HRK", Waluta "kuna" "kuny" "kun" Zenski),
                    ("HUF", Waluta "forint" "forinty" "forintów" Meski),
                    ("IDR", Waluta "rupia indonezyjska" "rupie indonezyjskie" "rupii indonezyjskich" Zenski),
                    ("ISK", Waluta "korona islandzka" "korony islandzkie" "koron islandzkich" Zenski),
                    ("JPY", Waluta "jen" "jeny" "jenów" Meski),
                    ("KRW", Waluta "won południowokoreański" "wony południowokoreańskie" "wonów południowokoreańskich" Meski),
                    ("MXN", Waluta "peso meksykańskie" "peso meksykańskie" "peso meksykańskich" Nijaki),
                    ("MYR", Waluta "ringgit" "ringgity" "ringgitów" Meski),
                    ("NOK", Waluta "korona norweska" "korony norweskie" "koron norweskich" Zenski),
                    ("NZD", Waluta "dolar nowozelandzki" "dolary nowozelandzkie" "dolarów nowozelandzkich" Meski),
                    ("PHP", Waluta "peso filipińskie" "peso filipińskie" "peso filipińskich" Nijaki),
                    ("PLN", Waluta "złoty" "złote" "złotych" Meski),
                    ("RON", Waluta "lej rumuński" "leje rumuńskie" "lei rumuńskich" Meski),
                    ("RUB", Waluta "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Meski),
                    ("SDR", Waluta "specjalne prawo ciągnienia" "specjalne prawa ciągnienia" "specjalnych praw ciągnienia" Nijaki),
                    ("SEK", Waluta "korona szwedzka" "korony szwedzkie" "koron szwedzkich" Zenski),
                    ("SGD", Waluta "dolar singapurski" "dolary singapurskie" "dolarów singapurskich" Meski),
                    ("THB", Waluta "bat" "baty" "batów" Meski),
                    ("TRY", Waluta "lira turecka" "liry tureckie" "lir tureckich" Zenski),
                    ("UAH", Waluta "hrywna" "hrywny" "hrywien" Zenski),
                    ("USD", Waluta "dolar amerykański" "dolary amerykańskie" "dolarów amerykańskich" Meski),
                    ("ZAR", Waluta "rand" "randy" "randów" Meski)]
