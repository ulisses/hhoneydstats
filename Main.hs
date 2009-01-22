{-#OPTIONS -fno-monomorphism-restriction -XParallelListComp#-}
module Main where

import Data.Array.Vector

import Control.Monad.State
import Control.Monad.Instances
import IO
import Char
import Data.List
import Mpi
import Parser
import GetCountry

import System.IO.Unsafe

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Simple
import qualified Graphics.Rendering.Cairo as C
import Data.Accessor

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import Control.Concurrent
import Control.Parallel

import System.Random

blue = Color 0 0 1

class GetInfo info where
        getIPDst :: info -> IP
        getIPSrc :: info -> IP
        getPortDst :: info -> Port
        getPortSrc :: info -> Port
        getOS :: info -> OS
        getYear :: info -> Integer
        getMonth :: info -> Int
        getDay :: info -> Int
        getHour :: info -> Int
        getMin :: info -> Int
        getData :: info -> DataT
        getCountry :: info -> String

instance GetInfo Info where
        getIPDst (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ipd
        getIPDst (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ipd
        getIPDst (Simple dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (FA dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (RA dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (IC dataT tCPUDP ips ipd code type_ size) = ipd
        getIPDst (PAos dataT tCPUDP ips ps ipd pd s5 os) = ipd
        getIPDst (PA dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (Ros dataT tCPUDP ips ps ipd pd s5 os) = ipd
        getIPDst (Aos dataT tCPUDP ips ps ipd pd s5 os) = ipd
        getIPDst (A dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (Sos dataT tCPUDP ips ps ipd pd s5 os) = ipd
        getIPDst (S dataT tCPUDP ips ps ipd pd s5) = ipd
        getIPDst (Ssos dataT tCPUDP ips ps ipd pd os) = ipd
        getIPDst (Ss dataT tCPUDP ips ps ipd pd) = ipd
        getIPDst (Es dataT tCPUDP ips ps ipd pd in_ out) = ipd
        getIPDst (R dataT tCPUDP ips ps ipd pd s5) = ipd

        getIPSrc (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ips
        getIPSrc (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ips
        getIPSrc (Simple dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (RA dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (FA dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (IC dataT tCPUDP ips ipd code type_ size) = ips
        getIPSrc (PAos dataT tCPUDP ips ps ipd pd s5 os) = ips
        getIPSrc (PA dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (Ros dataT tCPUDP ips ps ipd pd s5 os) = ips
        getIPSrc (Aos dataT tCPUDP ips ps ipd pd s5 os) = ips
        getIPSrc (A dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (Sos dataT tCPUDP ips ps ipd pd s5 os) = ips
        getIPSrc (S dataT tCPUDP ips ps ipd pd s5) = ips
        getIPSrc (Ssos dataT tCPUDP ips ps ipd pd os) = ips
        getIPSrc (Ss dataT tCPUDP ips ps ipd pd) = ips
        getIPSrc (Es dataT tCPUDP ips ps ipd pd in_ out) = ips
        getIPSrc (R dataT tCPUDP ips ps ipd pd s5) = ips

        getPortDst (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = pd
        getPortDst (FAos dataT tCPUDP ips ps ipd pd s5 os_) = pd
        getPortDst (Simple dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (RA dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (FA dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (IC dataT tCPUDP ips ipd code type_ size) = NOPORT
        getPortDst (PAos dataT tCPUDP ips ps ipd pd s5 os) = pd
        getPortDst (PA dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (Ros dataT tCPUDP ips ps ipd pd s5 os) = pd
        getPortDst (Aos dataT tCPUDP ips ps ipd pd s5 os) = pd
        getPortDst (A dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (Sos dataT tCPUDP ips ps ipd pd s5 os) = pd
        getPortDst (S dataT tCPUDP ips ps ipd pd s5) = pd
        getPortDst (Ssos dataT tCPUDP ips ps ipd pd os) = pd
        getPortDst (Ss dataT tCPUDP ips ps ipd pd) = pd
        getPortDst (Es dataT tCPUDP ips ps ipd pd in_ out) = pd
        getPortDst (R dataT tCPUDP ips ps ipd pd s5) = pd

        getPortSrc (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ps
        getPortSrc (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ps
        getPortSrc (Simple dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (RA dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (FA dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (IC dataT tCPUDP ips ipd code type_ size) = NOPORT
        getPortSrc (PAos dataT tCPUDP ips ps ipd pd s5 os) = ps
        getPortSrc (PA dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (Ros dataT tCPUDP ips ps ipd pd s5 os) = ps
        getPortSrc (Aos dataT tCPUDP ips ps ipd pd s5 os) = ps
        getPortSrc (A dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (Sos dataT tCPUDP ips ps ipd pd s5 os) = ps
        getPortSrc (S dataT tCPUDP ips ps ipd pd s5) = ps
        getPortSrc (Ssos dataT tCPUDP ips ps ipd pd os) = ps
        getPortSrc (Ss dataT tCPUDP ips ps ipd pd) = ps
        getPortSrc (Es dataT tCPUDP ips ps ipd pd in_ out) = ps
        getPortSrc (R dataT tCPUDP ips ps ipd pd s5) = ps

        getOS (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = os_
        getOS (FAos dataT tCPUDP ips ps ipd pd s5 os_) = os_
        getOS (Simple dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (RA dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (FA dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (IC dataT tCPUDP ips ipd code type_ size) = NOOS
        getOS (PAos dataT tCPUDP ips ps ipd pd s5 os) = os
        getOS (PA dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (Ros dataT tCPUDP ips ps ipd pd s5 os) = os
        getOS (Aos dataT tCPUDP ips ps ipd pd s5 os) = os
        getOS (A dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (Sos dataT tCPUDP ips ps ipd pd s5 os) = os
        getOS (S dataT tCPUDP ips ps ipd pd s5) = NOOS
        getOS (Ssos dataT tCPUDP ips ps ipd pd os) = os
        getOS (Ss dataT tCPUDP ips ps ipd pd) = NOOS
        getOS (Es dataT tCPUDP ips ps ipd pd in_ out) = NOOS
        getOS (R dataT tCPUDP ips ps ipd pd s5) = NOOS

        getYear (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Simple dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (RA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (FA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (IC dataT tCPUDP ips ipd code type_ size) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (PAos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (PA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Ros dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Aos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (A dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Sos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (S dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Ssos dataT tCPUDP ips ps ipd pd os) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Ss dataT tCPUDP ips ps ipd pd) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (Es dataT tCPUDP ips ps ipd pd in_ out) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT
        getYear (R dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> a) . toGregorian . localDay) dataT

        getMonth (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Simple dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (RA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (FA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (IC dataT tCPUDP ips ipd code type_ size) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (PAos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (PA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Ros dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Aos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (A dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Sos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (S dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Ssos dataT tCPUDP ips ps ipd pd os) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Ss dataT tCPUDP ips ps ipd pd) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (Es dataT tCPUDP ips ps ipd pd in_ out) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT
        getMonth (R dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> b) . toGregorian . localDay) dataT

        getDay (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (FAos dataT tCPUDP ips ps ipd pd s5 os_) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Simple dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (RA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (FA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (IC dataT tCPUDP ips ipd code type_ size) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (PAos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (PA dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Ros dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Aos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (A dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Sos dataT tCPUDP ips ps ipd pd s5 os) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (S dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Ssos dataT tCPUDP ips ps ipd pd os) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Ss dataT tCPUDP ips ps ipd pd) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (Es dataT tCPUDP ips ps ipd pd in_ out) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT
        getDay (R dataT tCPUDP ips ps ipd pd s5) = ((\(a,b,c) -> c) . toGregorian . localDay) dataT

        getHour (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = (todHour . localTimeOfDay) dataT
        getHour (FAos dataT tCPUDP ips ps ipd pd s5 os_) = (todHour . localTimeOfDay) dataT
        getHour (Simple dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (RA dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (FA dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (IC dataT tCPUDP ips ipd code type_ size) = (todHour . localTimeOfDay) dataT
        getHour (PAos dataT tCPUDP ips ps ipd pd s5 os) = (todHour . localTimeOfDay) dataT
        getHour (PA dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (Ros dataT tCPUDP ips ps ipd pd s5 os) = (todHour . localTimeOfDay) dataT
        getHour (Aos dataT tCPUDP ips ps ipd pd s5 os) = (todHour . localTimeOfDay) dataT
        getHour (A dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (Sos dataT tCPUDP ips ps ipd pd s5 os) = (todHour . localTimeOfDay) dataT
        getHour (S dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT
        getHour (Ssos dataT tCPUDP ips ps ipd pd os) = (todHour . localTimeOfDay) dataT
        getHour (Ss dataT tCPUDP ips ps ipd pd) = (todHour . localTimeOfDay) dataT
        getHour (Es dataT tCPUDP ips ps ipd pd in_ out) = (todHour . localTimeOfDay) dataT
        getHour (R dataT tCPUDP ips ps ipd pd s5) = (todHour . localTimeOfDay) dataT

        getMin (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = (todMin . localTimeOfDay) dataT
        getMin (FAos dataT tCPUDP ips ps ipd pd s5 os_) = (todMin . localTimeOfDay) dataT
        getMin (Simple dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (RA dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (FA dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (IC dataT tCPUDP ips ipd code type_ size) = (todMin . localTimeOfDay) dataT
        getMin (PAos dataT tCPUDP ips ps ipd pd s5 os) = (todMin . localTimeOfDay) dataT
        getMin (PA dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (Ros dataT tCPUDP ips ps ipd pd s5 os) = (todMin . localTimeOfDay) dataT
        getMin (Aos dataT tCPUDP ips ps ipd pd s5 os) = (todMin . localTimeOfDay) dataT
        getMin (A dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (Sos dataT tCPUDP ips ps ipd pd s5 os) = (todMin . localTimeOfDay) dataT
        getMin (S dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT
        getMin (Ssos dataT tCPUDP ips ps ipd pd os) = (todMin . localTimeOfDay) dataT
        getMin (Ss dataT tCPUDP ips ps ipd pd) = (todMin . localTimeOfDay) dataT
        getMin (Es dataT tCPUDP ips ps ipd pd in_ out) = (todMin . localTimeOfDay) dataT
        getMin (R dataT tCPUDP ips ps ipd pd s5) = (todMin . localTimeOfDay) dataT

        getData (FPAos dataT tCPUDP ips ps ipd pd s5 os_) = dataT
        getData (FAos dataT tCPUDP ips ps ipd pd s5 os_) = dataT
        getData (Simple dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (RA dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (FA dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (IC dataT tCPUDP ips ipd code type_ size) = dataT
        getData (PAos dataT tCPUDP ips ps ipd pd s5 os) = dataT
        getData (PA dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (Ros dataT tCPUDP ips ps ipd pd s5 os) = dataT
        getData (Aos dataT tCPUDP ips ps ipd pd s5 os) = dataT
        getData (A dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (Sos dataT tCPUDP ips ps ipd pd s5 os) = dataT
        getData (S dataT tCPUDP ips ps ipd pd s5) = dataT
        getData (Ssos dataT tCPUDP ips ps ipd pd os) = dataT
        getData (Ss dataT tCPUDP ips ps ipd pd) = dataT
        getData (Es dataT tCPUDP ips ps ipd pd in_ out) = dataT
        getData (R dataT tCPUDP ips ps ipd pd s5) = dataT

        getCountry (FPAos dataT tCPUDP (IP ips) ps ipd pd s5 os_) = unsafePerformIO $ getCountryByIP ips
        getCountry (FAos dataT tCPUDP (IP ips) ps ipd pd s5 os_) = unsafePerformIO $ getCountryByIP ips
        getCountry (Simple dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (FA dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (RA dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (IC dataT tCPUDP (IP ips) ipd code type_ size) = unsafePerformIO $ getCountryByIP ips
        getCountry (PAos dataT tCPUDP (IP ips) ps ipd pd s5 os) = unsafePerformIO $ getCountryByIP ips
        getCountry (PA dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (Ros dataT tCPUDP (IP ips) ps ipd pd s5 os) = unsafePerformIO $ getCountryByIP ips
        getCountry (Aos dataT tCPUDP (IP ips) ps ipd pd s5 os) = unsafePerformIO $ getCountryByIP ips
        getCountry (A dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (Sos dataT tCPUDP (IP ips) ps ipd pd s5 os) = unsafePerformIO $ getCountryByIP ips
        getCountry (S dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips
        getCountry (Ssos dataT tCPUDP (IP ips) ps ipd pd os) = unsafePerformIO $ getCountryByIP ips
        getCountry (Ss dataT tCPUDP (IP ips) ps ipd pd) = unsafePerformIO $ getCountryByIP ips
        getCountry (Es dataT tCPUDP (IP ips) ps ipd pd in_ out) = unsafePerformIO $ getCountryByIP ips
        getCountry (R dataT tCPUDP (IP ips) ps ipd pd s5) = unsafePerformIO $ getCountryByIP ips

randList :: [a] -> IO [a]
randList ll =
    do lst <- mapM rrand [0..length ll - 1]
       return $ mix ll (nub lst ++ ([0..length ll - 1]\\lst))
    where rrand top = getStdRandom $ randomR (0,top)
          mix l = foldr (\i t -> l!!i : t) []

mkTitlePort y m d = "Ports of day " ++ show y ++ "-" ++ show m ++ "-" ++  show d
mkTitleIP y m d = "IP's of day " ++ show y ++ "-" ++ show m ++ "-" ++  show d
mkTitleOS y m d = "OS of day " ++ show y ++ "-" ++ show m ++ "-" ++  show d
mkTitleC y m d = "Countries of day " ++ show y ++ "-" ++ show m ++ "-" ++  show d

prepareDataImage j i l = [ (os, length lip) | (os,lip) <- getJByI  j i l ]

{-main = do
    l <- readFile "honeyd.log">>= return . lexer . lines
    (y,m,d) <- return . toGregorian . utctDay =<< getCurrentTime-}
--     let today = (filterByData y m d l)
--     saveImagePieAs "portDay.png" (mkTitlePort y m d)  $ printTop $ prepareDataImage getPortDst getIPSrc today
--     saveImagePieAs "ipDay.png" (mkTitleIP y m d) $ printTop $ getBy getIPSrc today
--     saveImagePieAs "osDay.png" (mkTitleOS y m d) $ printTop $ prepareDataImage getOS getIPSrc today
--     saveImagePieAs "countryDay.png" (mkTitleC y m d) $ printsTop $ prepareDataImage getCountry getIPSrc today

{-    let month = (filterByMonth y m l)
    saveImagePieAs "portMonth.png" "Ports last Month"  $ printTop $ prepareDataImage getPortDst getIPSrc month-}
--     saveImagePieAs "ipMonth.png" "IP last Month" $ printTop $ getBy getIPSrc) month
--     saveImagePieAs "osMonth.png" "OS last Month" $ printTop $ getBy getOS) month
--     (saveImagePieAs "portDia.png" "Ports" . top10 . printf . getBy getPortDst) (filterByData y m d l)

--     writeFile "a" $ foldr (\(ip,lp) t -> show ip ++ " " ++ show lp ++ "\n" ++ t) [] $ getJByI getIPSrc getPortDst l
--     (saveImagePieAs "ip.png" "IP" . take 10 . printf . getBy getIPSrc) l
--     (saveImagePieAs "day.png" "IP" . take 10 . printf . getBy getData) l
--     (saveImagePieAs "os.png" "OS" . take 10 . printf . getBy getOS) l
--     return ()

-- ./program -r honeyd.log -pie portDay.png -date 2008 12 10

-- **
kk = readFile "honeyd.log" >>= return . filterByPortDst (Port 25) . lexer . lines >>= return . unlines . map show >>= writeFile "bla"
-- **


{-main = do
    l <- readFile "honeyd.log" >>= return . lexer . lines
    --(y,m,d) <- return . toGregorian . utctDay =<< getCurrentTime
    let (y,m,d) = (2008,12,18)
    let today = (filterByData y m d l)
    --saveImagePieAs "cDay.png" (mkTitleC y m d)  $ printTop $ prepareDataImage getCountry getIPSrc today
    saveImagePieAs "portDay18.png" (mkTitlePort y m d)  $ printTop $ prepareDataImage getPortDst getIPSrc today-}
    --saveImagePieAs "ipDay.png" (mkTitleIP y m d)  $ printTop $ getBy getIPSrc today
    --saveImagePieAs "osDay.png" (mkTitleOS y m d) $ printTop $ prepareDataImage getOS getIPSrc today
    --saveImagePieAs "countryDay.png" (mkTitleC 2008 12 10) $ printsTop $ prepareDataImage getCountry getIPSrc today
    --let month = (filterByMonth y m l)
    --saveImagePieAs "portMonth.png" ("Ports in Month " ++ show m)  $ printTop $ prepareDataImage getPortDst getIPSrc month
    --saveImagePieAs "osMonth.png" "OS last Month" $ printTop $ getBy getOS month
    --saveImagePieAs "ipMonth.png" "IP hit's - Month December" $ printTop $ getBy getIPSrc month
    --saveImagePieAs "ip_.png" "IP" $ take 10 $ printf $ getBy getIPSrc l

-- ./program -r honeyd.log -os -lines os.png -date all

--main = runPort
logFile = "honeyd.log"

--main = runPortN 80

--main = runIPSrcN "82.155.123.90"
--main = runIPSrcPorts "82.155.123.90"

-- ler: http://permalink.gmane.org/gmane.comp.lang.haskell.cafe/50340
main = runPortNIPSrc 25
--main = print . sumU . mapU (+7) $ zipWithU (*)(enumFromToU 1 (4000000 :: Int)) (enumFromToU 2 (4000001 :: Int))

-- hit's by host for port n
runPortNIPSrc n = readFile logFile >>= run . filterByPortDst (Port n) . lexer . lines
    where run l = renderIP l >>= \x -> renderableToPNGFile x 650 650 $ "port-" ++ show n ++ "-ips.png"


-- hit's by port for host n
runIPSrcPorts n = readFile logFile >>= run . filterByIPSrc (IP n) . lexer . lines
    where run l = renderPort l >>= \x -> renderableToPNGFile x 650 650 $ "ip-" ++ n ++ "-ports.png"


runIPSrcN n = readFile logFile >>= run . filterByIPSrc (IP n) . lexer . lines
    where run l = renderIP l >>= \x -> renderableToPNGFile x 650 650 $ "ip-" ++ n ++ ".png"

runPortN n = readFile logFile >>= run . filterByPortDst (Port n) . lexer . lines
    where run l = renderPort l >>= \x -> renderableToPNGFile x 650 650 $ "port-" ++ show n ++ ".png"


runOS = readFile logFile >>= run . lexer . lines
    where run l = renderOS l >>= \x -> renderableToPNGFile x 650 650 "os.png"

renderOS prices = return (toRenderable layout)
  where
    plotLinesValues [] = []
    plotLinesValues [(y,m,d,n)] = let (y1',m1',d1') = incDay y m d
                        in [[point y m d n, point y1' m1' d1' 0.1]]
    plotLinesValues (c:cs) = plotLinesValues' c cs
            where plotLinesValues' (y,m,d,n) [] = [[point y m d n]]
                  plotLinesValues' (y1,m1,d1,n1) ((y,m,d,n):cs)
                                | abs (diffDays (day y1 m1 d1) (day y m d)) > 1
                                        = let (y1',m1',d1') = incDay y1 m1 d1
                                          in [[point y1 m1 d1 n1, point y1' m1' d1' n1]] ++ plotLinesValues' (y,m,d,n) cs
                                | otherwise = [[point y1 m1 d1 n1, point y m d n]] ++ plotLinesValues' (y,m,d,n) cs
                  day yyyy mm dd = fromGregorian (fromIntegral yyyy) mm dd
                  incDay' yyyy mm dd = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)

    point y1 m1 d1 n1 = Point (date d1 m1 y1) n1

    incDay yyyy mm dd = let newDay = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)
                            yyyy' = ((\(a,_,_) -> a) . toGregorian) newDay
                            mm' = ((\(_,b,_) -> b) . toGregorian) newDay
                            dd' = ((\(_,_,c) -> c) . toGregorian) newDay
                        in (yyyy',mm',dd')

    pointFilterAll lip l = (map (foldr (\a@((x,y,w,p):_) b -> (x,y,w,fromIntegral $ length a,p) : b) []
                            . groupBy (\(a,b,c,_) (e,f,g,_) -> a==e&&b==f&&c==g)
                            . sort)
                            . groupBy (\(a,b,c,p) (e,f,g,p2) -> p == p2) . sortBy (\(y,m,d,p) (y2,m2,d2,p2) -> compare p p2) . pointFilter lip) l
            where pointFilter [] _ = []
                  pointFilter (c:cs) l = [ (fromInteger $ getYear r, getMonth r, getDay r, c) | r <- filterBy getOS c l ]
                                      ++ pointFilter cs l
    renderPortPoints lineStyle l lip colors = [
                (show port, HA_Bottom,VA_Left, toPlot $ defaultPlotLines {
                                    plot_lines_style = lineStyle color,
                                    plot_lines_values = plotLinesValues l'
                                })
            | (port, l') <- map agrupa $ pointFilterAll lip l | color <- colors]
            where agrupa l@((_,_,_,_,p):_) = (p, map dropPort l)
                  dropPort (y,m,d,n,p) = (y,m,d,n)

    ports = getAllPortDst prices
    getAllPortDst = filter (/=NOOS) . map snd . getAllBy getOS
    genColor [] = return []
    genColor (_:cs) =
            do r <- randomRIO (0.0,1.0)
               g <- randomRIO (0.0,1.0)
               b <- randomRIO (0.0,1.0)
               c <- genColor cs
               return $ Color r g b : c

    lineStyle c = (plot_lines_style defaultPlotLines) {
                   line_width=3 * 1.0,
                   line_color = c
                  }

    layout = defaultLayout1 {
        layout1_title = "Hit's by Operative System",
        layout1_horizontal_axes = linkedAxes (autoTimeAxis defaultAxis),
        layout1_vertical_axes = linkedAxes (autoScaledLogAxis defaultAxis),
        layout1_legend = Just (defaultLegendStyle {
                   legend_label_style=(legend_label_style defaultLegendStyle) {
                        font_size = 8
                   },
                   legend_margin = 5,
                   legend_plot_size = 8
                } ),
        layout1_plots = renderPortPoints lineStyle prices ports (unsafePerformIO $ genColor [1..length ports])
    }

-----------------------------------------------------------------------------------------------------------------------------------------------

runCountry = readFile logFile >>= run . lexer . lines
    where run l = renderCountry l >>= \x -> renderableToPNGFile x 650 650 "country.png"

renderCountry prices = return (toRenderable layout)
  where
    plotLinesValues [] = []
    plotLinesValues [(y,m,d,n)] = let (y1',m1',d1') = incDay y m d
                        in [[point y m d n, point y1' m1' d1' 0.1]]
    plotLinesValues (c:cs) = plotLinesValues' c cs
            where plotLinesValues' (y,m,d,n) [] = [[point y m d n]]
                  plotLinesValues' (y1,m1,d1,n1) ((y,m,d,n):cs)
                                | abs (diffDays (day y1 m1 d1) (day y m d)) > 1
                                        = let (y1',m1',d1') = incDay y1 m1 d1
                                          in [[point y1 m1 d1 n1, point y1' m1' d1' n1]] ++ plotLinesValues' (y,m,d,n) cs
                                | otherwise = [[point y1 m1 d1 n1, point y m d n]] ++ plotLinesValues' (y,m,d,n) cs
                  day yyyy mm dd = fromGregorian (fromIntegral yyyy) mm dd
                  incDay' yyyy mm dd = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)

    point y1 m1 d1 n1 = Point (date d1 m1 y1) n1

    incDay yyyy mm dd = let newDay = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)
                            yyyy' = ((\(a,_,_) -> a) . toGregorian ) newDay
                            mm' = ((\(_,b,_) -> b) . toGregorian ) newDay
                            dd' = ((\(_,_,c) -> c) . toGregorian ) newDay
                        in (yyyy',mm',dd')

    pointFilterAll lip l = (map (foldr (\a@((x,y,w,p):_) b -> (x,y,w,fromIntegral $ length a,p) : b) []
                            . groupBy (\(a,b,c,_) (e,f,g,_) -> a==e&&b==f&&c==g)
                            . sort)
                            . groupBy (\(a,b,c,p) (e,f,g,p2) -> p == p2) . sortBy (\(y,m,d,p) (y2,m2,d2,p2) -> compare p p2) . pointFilter lip) l
            where pointFilter [] _ = []
                  pointFilter (c:cs) l = [ (fromInteger $ getYear r, getMonth r, getDay r, c) | r <- filterBy getCountry c l ]
                                      ++ pointFilter cs l
    renderPortPoints lineStyle l lip colors = [
                ("Country " ++ show port, HA_Bottom,VA_Left, toPlot $ defaultPlotLines {
                                    plot_lines_style = lineStyle color,
                                    plot_lines_values = plotLinesValues l'
                                })
            | (port, l') <- map agrupa $ pointFilterAll lip l | color <- colors]
            where agrupa l@((_,_,_,_,p):_) = (p, map dropPort l)
                  dropPort (y,m,d,n,p) = (y,m,d,n)

    ports = getAllPortDst prices
    getAllPortDst = map snd . getAllBy getCountry
    genColor [] = return []
    genColor (_:cs) =
            do r <- randomRIO (0.0,1.0)
               g <- randomRIO (0.0,1.0)
               b <- randomRIO (0.0,1.0)
               c <- genColor cs
               return $ Color r g b : c

    lineStyle c = (plot_lines_style defaultPlotLines) {
                   line_width=3 * 1.0,
                   line_color = c
                  }

    layout = defaultLayout1 {
        layout1_title = "Hit's by Country",
        layout1_horizontal_axes = linkedAxes (autoTimeAxis defaultAxis),
        layout1_vertical_axes = linkedAxes (autoScaledLogAxis defaultAxis),
        layout1_legend = Just (defaultLegendStyle {
                   legend_label_style=(legend_label_style defaultLegendStyle) {
                        font_size = 8
                   },
                   legend_margin = 5,
                   legend_plot_size = 8
                } ),
        layout1_plots = renderPortPoints lineStyle prices ports (unsafePerformIO $ genColor [1..length ports])
    }

-----------------------------------------------------------------------------------------------------------------------------------------------

runIP = readFile logFile >>= run . lexer . lines
    where run l = renderIP l >>= \x -> renderableToPNGFile x 650 650 "ip.png"

renderIP prices = return (toRenderable layout)
  where
    plotLinesValues [] = []
    plotLinesValues [(y,m,d,n)] = let (y1',m1',d1') = incDay y m d
                        in [[point y m d n, point y1' m1' d1' 0.1]]
    plotLinesValues (c:cs) = plotLinesValues' c cs
            where plotLinesValues' (y,m,d,n) [] = [[point y m d n]]
                  plotLinesValues' (y1,m1,d1,n1) ((y,m,d,n):cs)
                                | abs (diffDays (day y1 m1 d1) (day y m d)) > 1
                                        = let (y1',m1',d1') = incDay y1 m1 d1
                                          in [[point y1 m1 d1 n1, point y1' m1' d1' n1]] ++ plotLinesValues' (y,m,d,n) cs
                                | otherwise = [[point y1 m1 d1 n1, point y m d n]] ++ plotLinesValues' (y,m,d,n) cs
                  day yyyy mm dd = fromGregorian (fromIntegral yyyy) mm dd
                  incDay' yyyy mm dd = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)

    point y1 m1 d1 n1 = Point (date d1 m1 y1) n1

    incDay yyyy mm dd = let newDay = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)
                            yyyy' = ((\(a,_,_) -> a) . toGregorian ) newDay
                            mm' = ((\(_,b,_) -> b) . toGregorian ) newDay
                            dd' = ((\(_,_,c) -> c) . toGregorian ) newDay
                        in (yyyy',mm',dd')

    pointFilterAll lip l = (map (foldr (\a@((x,y,w,p):_) b -> (x,y,w,fromIntegral $ length a,p) : b) []
                            . groupBy (\(a,b,c,_) (e,f,g,_) -> a==e&&b==f&&c==g)
                            . sort)
                            . groupBy (\(a,b,c,p) (e,f,g,p2) -> p == p2) . sortBy (\(y,m,d,p) (y2,m2,d2,p2) -> compare p p2) . pointFilter lip) l
            where pointFilter [] _ = []
                  pointFilter (c:cs) l = [ (fromInteger $ getYear r, getMonth r, getDay r, c) | r <- filterBy getIPSrc c l ]
                                      ++ pointFilter cs l
    renderPortPoints lineStyle l lip colors = [
                ("IP " ++ show port, HA_Bottom,VA_Left, toPlot $ defaultPlotLines {
                                    plot_lines_style = lineStyle color,
                                    plot_lines_values = plotLinesValues l'
                                })
            | (port, l') <- map agrupa $ pointFilterAll lip l | color <- colors]
            where agrupa l@((_,_,_,_,p):_) = (p, map dropPort l)
                  dropPort (y,m,d,n,p) = (y,m,d,n)

    ports = getAllPortDst prices
    getAllPortDst = map snd . getAllBy getIPSrc
    genColor [] = return []
    genColor (_:cs) =
            do r <- randomRIO (0.0,1.0)
               g <- randomRIO (0.0,1.0)
               b <- randomRIO (0.0,1.0)
               c <- genColor cs
               return $ Color r g b : c

    lineStyle c = (plot_lines_style defaultPlotLines) {
                   line_width=3 * 1.0,
                   line_color = c
                  }

    layout = defaultLayout1 {
        layout1_title = "Hit's by IP",
        layout1_horizontal_axes = linkedAxes (autoTimeAxis defaultAxis),
        layout1_vertical_axes = linkedAxes (autoScaledLogAxis defaultAxis),
        layout1_legend = Just (defaultLegendStyle {
                   legend_label_style=(legend_label_style defaultLegendStyle) {
                        font_size = 8
                   },
                   legend_margin = 5,
                   legend_plot_size = 8
                } ),
        layout1_plots = renderPortPoints lineStyle prices ports (unsafePerformIO $ genColor [1..length ports])
    }

-----------------------------------------------------------------------------------------------------------------------------------------------

runPort = readFile logFile >>= run . lexer . lines
    where run l = renderPort l >>= \x -> renderableToPNGFile x 650 650 "ports.png"

renderPort prices = return (toRenderable layout)
  where
    plotLinesValues [] = []
    plotLinesValues [(y,m,d,n)] = let (y1',m1',d1') = incDay y m d
                        in [[point y m d n, point y1' m1' d1' 0.1]]
    plotLinesValues (c:cs) = plotLinesValues' c cs
            where plotLinesValues' (y,m,d,n) [] = [[point y m d n]]
                  plotLinesValues' (y1,m1,d1,n1) ((y,m,d,n):cs)
                                | abs (diffDays (day y1 m1 d1) (day y m d)) > 1
                                        = let (y1',m1',d1') = incDay y1 m1 d1
                                          in [[point y1 m1 d1 n1, point y1' m1' d1' n1]] ++ plotLinesValues' (y,m,d,n) cs
                                | otherwise = [[point y1 m1 d1 n1, point y m d n]] ++ plotLinesValues' (y,m,d,n) cs
                  day yyyy mm dd = fromGregorian (fromIntegral yyyy) mm dd
                  incDay' yyyy mm dd = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)

    point y1 m1 d1 n1 = Point (date d1 m1 y1) n1

    incDay yyyy mm dd = let newDay = addDays 1 (fromGregorian (fromIntegral yyyy) mm dd)
                            yyyy' = ((\(a,_,_) -> a) . toGregorian ) newDay
                            mm' = ((\(_,b,_) -> b) . toGregorian ) newDay
                            dd' = ((\(_,_,c) -> c) . toGregorian ) newDay
                        in (yyyy',mm',dd')

    pointFilterAll lip l = (map (foldr (\a@((x,y,w,p):_) b -> (x,y,w,fromIntegral $ length a,p) : b) []
                            . groupBy (\(a,b,c,_) (e,f,g,_) -> a==e&&b==f&&c==g)
                            . sort)
                            . groupBy (\(a,b,c,p) (e,f,g,p2) -> p == p2) . sortBy (\(y,m,d,p) (y2,m2,d2,p2) -> compare p p2) . pointFilter lip) l
            where pointFilter [] _ = []
                  pointFilter (c:cs) l = [ (fromInteger $ getYear r, getMonth r, getDay r, c) | r <- filterBy getPortDst c l ]
                                      ++ pointFilter cs l
    renderPortPoints lineStyle l lip colors = [
                ("Port " ++ show port, HA_Bottom,VA_Left, toPlot $ defaultPlotLines {
                                    plot_lines_style = lineStyle color,
                                    plot_lines_values = plotLinesValues l'
                                })
            | (port, l') <- map agrupa $ pointFilterAll lip l | color <- colors]
            where agrupa l@((_,_,_,_,p):_) = (p, map dropPort l)
                  dropPort (y,m,d,n,p) = (y,m,d,n)

    ports = getAllPortDst prices
    getAllPortDst = filter (/=NOPORT) . map snd . getAllBy getPortDst
    genColor [] = return []
    genColor (_:cs) =
            do r <- randomRIO (0.0,1.0)
               g <- randomRIO (0.0,1.0)
               b <- randomRIO (0.0,1.0)
               c <- genColor cs
               return $ Color r g b : c

    lineStyle c = (plot_lines_style defaultPlotLines){
                   line_width=3 * 1.0,
                   line_color = c
                  }

    layout = defaultLayout1 {
        layout1_title = "Hit's by port",
        layout1_horizontal_axes = linkedAxes' (autoTimeAxis defaultAxis),
        layout1_vertical_axes = linkedAxes' (autoScaledLogAxis defaultAxis),
        layout1_legend = Just (defaultLegendStyle {
                   legend_label_style=(legend_label_style defaultLegendStyle) {
                        font_size = 8
                   },
                   legend_margin = 5,
                   legend_plot_size = 8
                } ),
        layout1_plots = renderPortPoints lineStyle prices ports (unsafePerformIO $ genColor [1..length ports])
    }

-----------------------------------------------------------------------------------------------------------------------------------------------

test2 prices = return (toRenderable layout)
  where
    lineStyle c = (plot_lines_style defaultPlotLines){
                   line_width=3 * 1.0,
                   line_color = c
                  }

    price1 = defaultPlotLines {
        plot_lines_style = lineStyle blue,
        plot_lines_values = [[ Point (date d m y) v | (y,m,d,v) <- prices]]
    }
    layout = defaultLayout1 {
        layout1_title="Price History",

       layout1_horizontal_axes=linkedAxes' (autoTimeAxis defaultAxis),
        layout1_vertical_axes=linkedAxes (autoScaledLogAxis defaultAxis),

        layout1_plots = [("porta 44111", HA_Bottom,VA_Left,(toPlot price1))]
    }

date dd mm yyyy = doubleFromLocalTime (LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight)

{-
main :: IO()
main = readFile "honeyd.log" >>= return . lexer . lines >>= evalStateT main_kernel-}

main_kernel :: StateT [Info] IO ()
main_kernel = do
        l <- get
        lift $ putStrLn "Got file!"
        lift $ writeFile "a" $ foldr (\(ip,lp) t -> show ip ++ " " ++ show lp ++ "\n" ++ t) [] $ getJByI getIPSrc getPortDst l
{-        (lift . saveImagePieAs "port.png" "Ports" . take 10 . printf . getBy getPortDst) l
        (lift . saveImagePieAs "ip.png" "IP" . take 10 . printf . getBy getIPSrc) l
        (lift . saveImagePieAs "day.png" "IP" . take 10 . printf . getBy getData) l
        (lift . saveImagePieAs "os.png" "OS" . take 10 . printf . getBy getOS) l-}
        return ()

test = ["2008-12-31-14:09:55.3667 tcp(6) S 82.155.66.151 52866 192.168.1.50 139",                  -- 2 Ss
        "2008-12-07-17:08:28.9826 tcp(6) - 82.155.66.152 55753 192.168.1.50 139: 40 R",            -- 3 R
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 139",                  -- 4 Ss
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 21 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 21 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 440 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 440 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-11-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-13-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-08-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 440 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 440 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-23-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 440 [Linux 2.6 .1-7]", -- 1 Ssos
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-17:08:53.4858 tcp(6) S 82.155.66.153 56423 192.168.1.50 440",                  -- 4 Ss
        "2008-12-07-14:07:39.6507 tcp(6) S 193.136.19.149 46694 192.168.1.50 23 [Linux 2.6 .1-7]"  -- 1 Ssos
       ]

-- fazer um filtro generico que receba coisas dete genero: getOS >< getIPSrc

fff l = [ ip | (ip,n) <- getBy getIPSrc l]

--filterByIPSrc :: String -> [Info] -> [Info]
filterByIPSrc ip l = filter ((ip==). getIPSrc) l

filterBy f src l = filter ((src==). f) l

filterByMonth :: Integer -> Int -> [Info] -> [Info]
filterByMonth _ _ [] = []
filterByMonth y m (c:cs) | getYear c == y
                        && getMonth c == m = c : filterByMonth y m cs
                         | (getMonth c > m && getYear c > y) = []
                         |otherwise = filterByMonth y m cs

filterByData :: Integer -> Int -> Int -> [Info] -> [Info]
filterByData _ _ _ [] = []
filterByData y m d (c:cs) | getDay c == d
                         && getYear c == y
                         && getMonth c == m = c : filterByData y m d cs
                          | (getMonth c > m && getYear c > y && getDay c > d) = []
                          |otherwise = filterByData y m d cs

--filterByPortDst :: Port -> [a] -> [a]
filterByPortDst _ [] = []
filterByPortDst p (h:t) | getPortDst h == p = h : filterByPortDst p t
                        | otherwise = filterByPortDst p t

getBy f = reverse . sortBy (\a b -> compare (snd a) (snd b)) . foldr (\a b -> (head a,length a):b) [] . group . sort . map f

-- ordenacao por data... :D
genial = putStrLn $ unlines $ map show $ sortBy (\x y -> compare (getData x) (getData y)) (lexer test )
-- tentar (comprare . getData >< getData) :D

--pensar_ = groupBy (\(a,b) (c,d) -> b == d) . sortBy (\(a,b) (c,d) -> compare b d) . uncurry (zipWith ($) . map (curry (getIPSrc >< getOS)))

getAllByIPPort = reverse . sort . foldr (\a b -> (head a, fromIntegral $ length a):b) [] . group . sort . map getIPSrc

getAllBy f   = reverse . sort . foldr (\a b -> (length a, head a):b) [] . group . sort . map f
getTop10By f = top10 . reverse . sort . foldr (\a b -> (length a, head a):b) [] . group . sort . map f

---------------------------------------------------------------------------------------------------------
printTop = printf . top10
printsTop = printfs . top10
printf = foldr (\(a,b) t -> (show a ++ " (" ++ show b ++ ")", b) : t)  []
printfs = foldr (\(a,b) t -> (a ++ " (" ++ show b ++ ")", b) : t)  []
top10 = take 10

-- render image
---------------------------------------------------------------------------------------------------------
saveImagePieAs :: FilePath -> String -> [(String,Int)] -> IO()
saveImagePieAs name title l = renderableToPNGFile (toRenderable layout) 650 650 name
        where layout = defaultPieLayout {
                           pie_title = title,
                           pie_plot = defaultPieChart {
                                 pie_data = [ defaultPieItem{pitem_value=fromIntegral v,pitem_label=s,pitem_offset=0} | (s,v) <- l ]
                           }
                       }

---------------------------------------------------------------------------------------------------------

-- CATAS, ANAS e HYLOS
---------------------------------------------------------------------------------------------------------
getJByI :: (Eq a, Eq b) => (Info -> a) -> (Info -> b) -> [Info] -> [(a,[b])]
getJByI i j = hylo transforme (expandAndGet i j)

expandAndGet i j [] = Left()
expandAndGet i j (c:cs) = Right((i c, j c), cs)

transforme (Left()) = []
transforme (Right((key,val),l)) =
    case lookup key l of
        Nothing   -> (key,[val]) : l
        (Just lk) -> (key, nub (val : lk)) : filter ((key/=) . fst) l

---------------------------------------------------------------------------------------------------------
hylo f g = cata f . ana g

cata f = f . rec (cata f) . out
    where out [] = Left()
          out (h:t) = Right(h,t)
ana g = inc . rec (ana g) . g
    where inc = either (const []) (uncurry (:))

rec f = id -|- id >< f

        --(lift . putStrLn . unlines . printf . getTop10By getIPSrc) l
        --(lift . putStrLn . unlines . printf . getAllBy getOS) l
        --(lift . putStrLn . unlines . sortBy (\x y -> compare (getData x) (getData y))) l
        --(lift . putStrLn . unlines . map show . getJByI getOS getData) l
        --(lift . putStrLn . unlines . map show . filterByData 2008 12 20) l
        --(lift . saveImagePieAs "os.png" "TESTEEEE" . take 10 . printf . getBy getOS) l
        --(lift . print . sortBy (\x y -> compare (getData x) (getData y))) l


-- main = gg2

{-gg1 = readFile "honeyd.log" >>= grafico . lexer . lines
grafico l = test2 (c l)  >>= \x -> renderableToPNGFile x 650 650 "as"
c = foldr (\a@((x,y,z):_) b -> (x,y,z,fromIntegral $ length a):b) [] . group . g1
    where g1 [] = []
          g1 (c:cs) = (fromInteger $ getYear c, getMonth c, getDay c) : g1 cs

gg2 = readFile "honeyd.log" >>= grafico3 . lexer . lines
grafico3 l = test2 (c2 l)  >>= \x -> renderableToPNGFile x 650 650 "as2"
c2 = foldr (\a@((x,y,w):_) b -> (x,y,w,fromIntegral $ length a) : b) [] . groupBy (\(a,b,c) (e,f,g) -> a==e&&b==f&&c==g) . sort .  g1

g1 l = [ (fromInteger $ getYear c, getMonth c, getDay c) | c <- filterBy getPortDst (Port 137) l ]
g2 l = [ (fromInteger $ getYear c, getMonth c, getDay c) | c <- filterByIPSrc (IP "200.46.177.4") l ]
-}
