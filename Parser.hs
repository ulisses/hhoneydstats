module Parser where

import Char
import Data.List
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

data Info = FAos DataT TCPUDP IP     Port   IP     Port   String OS
          | FA DataT TCPUDP IP     Port   IP     Port   String
          | FPAos DataT TCPUDP IP     Port   IP     Port   String OS
          | RA DataT TCPUDP IP     Port   IP     Port   String
          | IC DataT TCPUDP IP IP Code Type Size
          | PAos DataT TCPUDP IP Port IP Port String OS
          |   PA DataT TCPUDP IP     Port   IP     Port   String
          |  Ros DataT TCPUDP IP Port IP Port String OS
          |    R DataT TCPUDP IP     Port   IP     Port   String
          |  Aos DataT TCPUDP IP Port IP Port String OS
          |    A DataT TCPUDP IP     Port   IP     Port   String
          |  Sos DataT TCPUDP IP     Port   IP     Port   String OS
          | Simple DataT TCPUDP IP Port IP Port String
          |    S DataT TCPUDP IP     Port   IP     Port   String
          | Ssos DataT TCPUDP IP Port IP Port OS
          |   Ss DataT TCPUDP IP Port IP Port
          |   Es DataT TCPUDP IP Port IP Port In Out
          | NULL
        deriving (Eq,Ord)
-- 2008-12-17-01:11:47.6209 tcp(6) - 82.154.64.174 34507 192.168.1.50 445: 40 RA
-- 2008-12-14-09:04:38.2678 icmp(1) - 80.236.5.27 192.168.1.50: 3(13): 56
-- 2008-12-19-18:25:03.3130 tcp(6) - 124.8.74.33 1806 192.168.1.50 25: 70 FPA [Windows XP SP1]
-- 2008-12-08-04:19:41.3059 tcp(6) - 82.155.123.90 58437 192.168.1.50 445: 40 FA
-- 2008-12-09-21:05:51.7703 tcp(6) - 82.155.57.245 2313 192.168.1.50 445: 158 PA [Windows XP SP1]
-- 2008-12-07-22:24:50.2923 tcp(6) - 193.136.19.149 43069 192.168.1.50 25: 53 PA
-- 2008-12-07-20:59:29.6910 tcp(6) - 88.175.73.149 4332 192.168.1.50 139: 40 R [Windows XP SP1]
-- 2008-12-09-11:57:04.2741 tcp(6) - 82.155.137.139 1230 192.168.1.50 445: 40 A [Windows XP SP1]
-- 2008-12-07-18:35:28.4232 tcp(6) - 82.155.7.176 2794 192.168.1.50 445: 40 A
-- 2008-12-07-15:34:49.0070 tcp(6) - 82.155.116.238 3578 192.168.1.50 23: 60 S [Linux 2.6 .1-7]
-- 2008-12-15-22:59:03.4039 udp(17) - 192.168.1.254 67 255.255.255.255 68: 298
-- 2008-12-07-16:06:29.2781 tcp(6) - 124.207.41.198 48804 192.168.1.50 23: 40 S
-- 2008-12-09-06:31:59.3902 tcp(6) S 88.44.123.210 3637 192.168.1.50 139 [Windows XP SP1]
-- 2008-12-07-16:06:28.9343 tcp(6) S 82.155.0.49 22617 192.168.1.50 139
-- 2008-12-09-11:41:25.6859 tcp(6) E 82.155.1.160 4399 192.168.1.50 445: 0 0
-- 2008-12-07-16:20:04.5996 tcp(6) - 82.155.122.18 61582 192.168.1.50 139: 40 R

instance Show Info where
    show (FAos d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                               ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " FA [" ++ show os ++ "]"
    show (FA d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                         ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " FA"
    show (FPAos d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                               ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " FPA [" ++ show os ++ "]"
    show (RA d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                         ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " RA"
    show (IC d tui ip1 ip2 code ty size) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show ip2
                                        ++ ": " ++ code ++ "(" ++ ty ++ "): " ++ size
    show (PAos d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " PA [" ++ show os ++ "]"
    show (PA d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " PA"
    show (Ros d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " R [" ++ show os ++ "]"
    show (R d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1 ++ " "
                                        ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " R"
    show (Aos d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " A [" ++ show os ++ "]"
    show (A d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1 ++ " "
                                        ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " FA"
    show (Sos d tui ip1 port1 ip2 port2 s os) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " S [" ++ show os ++ "]"
    show (Simple d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s
    show (S d tui ip1 port1 ip2 port2 s) = show d ++ " " ++ show tui ++ " - " ++ show ip1 ++ " " ++ show port1 ++ " "
                                        ++ show ip2 ++ " " ++ show port2 ++ ": " ++ s ++ " S"
    show (Ssos d tui ip1 port1 ip2 port2 os) = show d ++ " " ++ show tui ++ " S " ++ show ip1 ++ " " ++ show port1 ++ " "
                                        ++ show ip2 ++ " " ++ show port2 ++ " " ++ " [" ++ show os ++ "]"
    show (Ss d tui ip1 port1 ip2 port2) = show d ++ " " ++ show tui ++ " S " ++ show ip1 ++ " " ++ show port1
                                        ++ " " ++ show ip2 ++ " " ++ show port2
    show (Es d tui ip1 port1 ip2 port2 in_ out) = show d ++ " " ++ show tui ++ " E " ++ show ip1 ++ " " ++ show port1 ++ " "
                                        ++ show ip2 ++ " " ++ show port2 ++ ": " ++ show in_ ++ " " ++ show out

data OS = OS String | NOOS deriving (Eq,Ord)
instance Show OS where
        show (OS s) = s
        show NOOS = "Unknown"
data In = In Int deriving (Eq,Ord)
instance Show In where
        show (In n) = show n
data Out = Out Int deriving (Eq,Ord)
instance Show Out where
        show (Out n) = show n
data Port = Port Int | NOPORT deriving (Eq,Ord)
instance Show Port where
        show (Port n) = show n
        show NOPORT = []
data IP = IP String deriving (Eq,Ord)
instance Show IP where
        show (IP s) = s
data TCPUDP = TCP String | UDP String |ICMP String deriving (Eq,Ord)
instance Show TCPUDP where
        show (TCP s) = "tcp(" ++ s ++ ")"
        show (UDP s) = "udp(" ++ s ++ ")"
        show (ICMP s) = "icmp(" ++ s ++ ")"
type DataT = LocalTime
data Token = TokenVar String | TokenIP String | TokenData String deriving (Show,Eq,Ord)
type Code = String
type Type = String
type Size = String
type Ano = String
type Mes = String
type Dia = String
type Hora = String
type Minuto = String
type Segundo = String
type Centesimo = String

-- main = getContents >>= putStrLn . unlines . map show  . filter (/=NULL) . map lexer . lines

lexer = filter (/=NULL) . map lexer_

lexer_ s | ("FA" `isInfixOf` s) && (last s == ']') = lexerFAos_ s
         | ("FA" `isInfixOf` s) && (last s == 'A') = lexerFA_ s
         | ("icmp" `isInfixOf` s) && ((length $ filter (==':') s) == 4) = lexerICMP_ s
         | ("FPA" `isInfixOf` s) && (last s == ']') && ((length $ filter (==':') s) == 3) = lexerFPAos_ s
         | ("PA" `isInfixOf` s) && (last s == ']') && ((length $ filter (==':') s) == 3) = lexerPAos_ s
         | ("RA" `isInfixOf` s) && (last s == 'A') && ((length $ filter (==':') s) == 3) = lexerRA_ s
         | ("PA" `isInfixOf` s) && (last s == 'A') = lexerPA_ s
         | ("R" `isInfixOf` s)  && (last s == ']') && ((length $ filter (==':') s) == 3) = lexerRos_ s
         | ("A" `isInfixOf` s)  && (last s == ']') && ((length $ filter (==':') s) == 3) = lexerAos_ s
         | ("A" `isInfixOf` s)  && (last s == 'A') && ((length $ filter (==':') s) == 3) = lexerA_ s
         | ("S" `isInfixOf` s)  && (last s == ']') && ((length $ filter (==':') s) == 3) = lexerSos_ s
         | ("S" `isInfixOf` s)  && (last s == 'S') = lexerS_ s
         | ("S" `isInfixOf` s)  && (last s == ']') && ((length $ filter (==':') s) == 2) = lexerSsos_ s
         | ("S" `isInfixOf` s) = lexerSs_ s
         | ("E" `isInfixOf` s) = lexerEs_ s
         | ("R" `isInfixOf` s) = lexerR_ s
         | ("----" `isInfixOf` s) = NULL
         | ((length $ filter (==':') s) == 3) = lexers_ s
         | otherwise = error $ "!Parsing error => [ " ++ s ++ " ]"

lexerFPAos_ s = let ((a,_):_) = lexerFPAos s in a
lexerRA_ s = let ((a,_):_) = lexerRA s in a
lexers_ s = let ((a,_):_) = lexers s in a
lexerFAos_ s = let ((a,_):_) = lexerFAos s in a
lexerFA_ s = let ((a,_):_) = lexerFA s in a
lexerICMP_ s = let ((a,_):_) = lexerICMP s in a
lexerPAos_ s = let ((a,_):_) = lexerPAos s in a
lexerPA_ s = let ((a,_):_) = lexerPA s in a
lexerRos_ s = let ((a,_):_) = lexerRos s in a
lexerAos_ s = let ((a,_):_) = lexerAos s in a
lexerA_ s = let ((a,_):_) = lexerA s in a
lexerSos_ s = let ((a,_):_) = lexerSos s in a
lexerS_ s = let ((a,_):_) = lexerS s in a
lexerSsos_ s = let ((a,_):_) = lexerSsos s in a
lexerSs_ s = let ((a,_):_) = lexerSs s in a
lexerEs_ s = let ((a,_):_) = lexerEs s in a
lexerR_ s = let ((a,_):_) = lexerR s in a

-- 2008-12-17-01:11:47.6209 tcp(6) - 82.154.64.174 34507 192.168.1.50 445: 40 RA
lexerRA :: ReadS Info
lexerRA s = [ (RA dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
              (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
              (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
              (":",p8) <- lex p7, (s5,p9) <- lex p8, ("RA",res) <- lex p9
            ]

-- 2008-12-14-09:04:38.2678 icmp(1) - 80.236.5.27 192.168.1.50: 3(13): 56
lexerICMP :: ReadS Info
lexerICMP s = [ (IC dataT tCPUDP ips ipd code type_ size,res) | (dataT,p1) <- lexData s,
                (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
                (ipd,p5) <- lexIP $ tail $ p4, (":", p6) <- lex p5, (code,p7) <- lex p6, ("(",p8) <- lex p7,
                (type_,p9) <- lex p8, (")",p10) <- lex p9, (size,res) <- lex $ tail $ p10
              ]

-- 2008-12-19-18:25:03.3130 tcp(6) - 124.8.74.33 1806 192.168.1.50 25: 70 FPA [Windows XP SP1]
lexerFPAos :: ReadS Info
lexerFPAos s = [ (FPAos dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
                 (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
                 (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
                 (":",p8) <- lex p7, (s5,p9) <- lex p8, ("FPA",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
               ]

-- 2008-12-14-09:54:27.6131 tcp(6) - 168.167.152.228 58274 192.168.1.50 445: 52 FA [Windows XP SP1]
lexerFAos :: ReadS Info
lexerFAos s = [ (FAos dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
                (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
                (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
                (":",p8) <- lex p7, (s5,p9) <- lex p8, ("FA",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
              ]

-- 2008-12-08-04:19:41.3059 tcp(6) - 82.155.123.90 58437 192.168.1.50 445: 40 FA
lexerFA :: ReadS Info
lexerFA s = [ (FA dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
              (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
              (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
              (":",p8) <- lex p7, (s5,p9) <- lex p8, ("FA",res) <- lex p9
            ]

-- 2008-12-09-21:05:51.7703 tcp(6) - 82.155.57.245 2313 192.168.1.50 445: 158 PA [Windows XP SP1]
lexerPAos :: ReadS Info
lexerPAos s = [ (PAos dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
                (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
                (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
                (":",p8) <- lex p7, (s5,p9) <- lex p8, ("PA",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
              ]

-- 2008-12-07-22:24:50.2923 tcp(6) - 193.136.19.149 43069 192.168.1.50 25: 53 PA
lexerPA :: ReadS Info
lexerPA s = [ (PA dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
              (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
              (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
              (":",p8) <- lex p7, (s5,p9) <- lex p8, ("PA",res) <- lex p9
            ]

-- 2008-12-07-20:59:29.6910 tcp(6) - 88.175.73.149 4332 192.168.1.50 139: 40 R [Windows XP SP1]
lexerRos :: ReadS Info
lexerRos s = [ (Ros dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
               (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
               (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
               (":",p8) <- lex p7, (s5,p9) <- lex p8, ("R",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
             ]

-- 2008-12-09-11:57:04.2741 tcp(6) - 82.155.137.139 1230 192.168.1.50 445: 40 A [Windows XP SP1]
lexerAos :: ReadS Info
lexerAos s = [ (Aos dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
               (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
               (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
               (":",p8) <- lex p7, (s5,p9) <- lex p8, ("A",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
             ]

-- 2008-12-07-18:35:28.4232 tcp(6) - 82.155.7.176 2794 192.168.1.50 445: 40 A
lexerA :: ReadS Info
lexerA s = [ (A dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
             (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
             (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
             (":",p8) <- lex p7, (s5,p9) <- lex p8, ("A",res) <- lex p9
           ]

-- 2008-12-07-15:34:49.0070 tcp(6) - 82.155.116.238 3578 192.168.1.50 23: 60 S [Linux 2.6 .1-7]
lexerSos :: ReadS Info
lexerSos s = [ (Sos dataT tCPUDP ips ps ipd pd s5 os_,res) | (dataT,p1) <- lexData s,
               (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
               (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
               (":",p8) <- lex p7, (s5,p9) <- lex p8, ("S",p10) <- lex p9, ("[",p11) <- lex p10, (os_,res) <- lexOS p11
             ]


-- 2008-12-15-22:59:03.4039 udp(17) - 192.168.1.254 67 255.255.255.255 68: 298
lexers :: ReadS Info
lexers s = [ (Simple dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
             (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
             (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
             (":",p8) <- lex p7, (s5,res) <- lex p8
           ]

-- 2008-12-07-16:06:29.2781 tcp(6) - 124.207.41.198 48804 192.168.1.50 23: 40 S
lexerS :: ReadS Info
lexerS s = [ (S dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
             (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
             (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
             (":",p8) <- lex p7, (s5,p9) <- lex p8, ("S",res) <- lex p9
           ]

-- 2008-12-09-06:31:59.3902 tcp(6) S 88.44.123.210 3637 192.168.1.50 139 [Windows XP SP1]
lexerSsos :: ReadS Info
lexerSsos s = [ (Ssos dataT tCPUDP ips ps ipd pd os_, res) | (dataT,p1) <- lexData s,
                (tCPUDP, p2) <- lexTCPUDP p1, ("S",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
                (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
                ("[",p8) <- lex p7, (os_,res) <- lexOS p8
              ]

-- 2008-12-07-16:06:28.9343 tcp(6) S 82.155.0.49 22617 192.168.1.50 139
lexerSs :: ReadS Info
lexerSs s = [ (Ss dataT tCPUDP ips ps ipd pd, res) | (dataT,p1) <- lexData s,
              (tCPUDP, p2) <- lexTCPUDP p1, ("S",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
              (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,res) <- lexPort $ tail $ p6
            ]

-- 2008-12-09-11:41:25.6859 tcp(6) E 82.155.1.160 4399 192.168.1.50 445: 0 0
lexerEs :: ReadS Info
lexerEs s = [ (Es dataT tCPUDP ips ps ipd pd in_ out, res) | (dataT,p1) <- lexData s,
              (tCPUDP, p2) <- lexTCPUDP p1, ("E",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
              (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
              (":",p8) <- lex p7, (in_,p9) <- lexIn $ tail $ p8,(out,res) <- lexOut $ tail $ p9
            ]

-- 2008-12-07-16:20:04.5996 tcp(6) - 82.155.122.18 61582 192.168.1.50 139: 40 R
lexerR :: ReadS Info
lexerR s = [ (R dataT tCPUDP ips ps ipd pd s5,res) | (dataT,p1) <- lexData s,
             (tCPUDP, p2) <- lexTCPUDP p1, ("-",p3) <- lex p2, (ips,p4) <- lexIP $ tail $ p3,
             (ps,p5) <- lexPort $ tail $ p4, (ipd,p6) <- lexIP $ tail $ p5, (pd,p7) <- lexPort $ tail $ p6,
             (":",p8) <- lex p7, (s5,p9) <- lex p8, ("R",res) <- lex p9
           ]

lexOS = (:[]) . (\(a,b) -> (OS a,b)) . span (/=']')
lexIn = (:[]) . (\(a,b) -> (In (read a :: Int),b)) . span isDigit
lexOut = (:[]) . (\(a,b) -> (Out (read a :: Int),b)) . span isDigit
lexPort = (:[]) . (\(a,b) -> (Port (read a :: Int),b)) . span isDigit
lexIP = (:[]) . (\(a,b) -> (IP a,b)) . span isIP
lexTCPUDP s = [ (decideTCPUDP tu n,res) | (tu,p1) <- lex s, ("(",p2) <- lex p1,
                (n,p3) <- lex p2, (")",res) <- lex p3
              ]

lexData s = let (data_,res) = span (/=' ') s
                (data',_) = span (/='.') data_
            in [(time data',res)]


isIP c = isDigit c || c == '.'
isData c = isDigit c || c == ':' || c == '-' || c == '.'
isTCPUDP c = isAlphaNum c || c == '(' || c == ')'
decideTCPUDP s =
             case s of
                 "tcp" -> TCP
                 "udp" -> UDP
                 "icmp" -> ICMP

time :: String -> LocalTime
time = readTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S"
