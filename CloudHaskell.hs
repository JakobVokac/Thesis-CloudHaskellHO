--  Copyright (c) 2019. Jakob Vokac
--  and University of Groningen.
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR PROFITS; # OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON  ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF  THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Misty.CloudHaskell where

import Misty.Process
import Misty.Types
import Misty.Channel

import Data.Char
import qualified Data.List as List

import Debug.Trace

import Misty.Latex

-- TODO: context passing -- works!
-- TODO: initialzing channels -- works!
-- TODO: channel passing from misty on non-enclosured abstractions.
-- TODO: implement binary operations


-- Used for tracing the result (for debugging)
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x






------------ -------- Functions for identifying and returning Booleans instead of channels -----------------------------

haskellIsBoolChannel :: String -> Bool
haskellIsBoolChannel str = ("True" `List.isPrefixOf` str) || ("False" `List.isPrefixOf` str)

haskellReturnBoolPrefix :: String -> String
haskellReturnBoolPrefix str
  | "True" `List.isPrefixOf` str = "True"
  | "False" `List.isPrefixOf` str = "False"
  | otherwise = traceThis "-- Error: Non bool recognized as bool"

haskellCheckForBool :: String -> String
haskellCheckForBool u = if (haskellIsBoolChannel u) then "" else u

------------------------------------------------------------------------------------------------------------------------



--------------------------- ------- Functions for printing channels names ----------------------------------------------

separateInitDigits :: String -> String
separateInitDigits (x:xs) = if isDigit x
                            then
                              x:(separateInitDigits xs)
                            else
                              ('_':x:xs)
separateInitDigits _ = ""

haskellChannelName :: String -> String
haskellChannelName n = if (ifProp (Ch n))
                         then
                           n
                         else
                           (removeNonAlphaNumUnderscore . reverse . separateInitDigits . reverse) $ n

removeNonAlphaNumUnderscore :: String -> String
removeNonAlphaNumUnderscore n = [x | x <- n, isAlphaNum x || x == '_']

haskellPrintEnvName :: ChannelN -> String
haskellPrintEnvName (Ch n) = printName n
haskellPrintEnvName (ChCmpl n) = printName n

haskellPrintName :: ChannelN -> String
haskellPrintName (Ch n) = haskellChannelName n
haskellPrintName (ChCmpl n) = haskellChannelName n

haskellPrintNameS :: [ChannelN] -> String
haskellPrintNameS (n:ns) = haskellPrintName n

haskellGetNameS :: [ChannelN] -> [String]
haskellGetNameS ns = [haskellPrintName n | n <- ns]

haskellPrintVarN :: VarN -> String
haskellPrintVarN v = removeNonAlphaNumUnderscore $ show v

haskellPrintVarNS :: [VarN] -> String
haskellPrintVarNS (v:vs) = removeNonAlphaNumUnderscore $ show v

------------------------------------------------------------------------------------------------------------------------



----------------------------- ------ Functions for getting channel names -----------------------------------------------

haskellGetParams :: [PProc] -> [String]
haskellGetParams pl = concat [haskellGetTrioParams x | x <- pl]

haskellGetRestrParams :: [(ChannelN, MinM)] -> [PProc] -> [String]
haskellGetRestrParams nts pl = (haskellGetParams pl) List.\\ (concat (map (\x -> [x ++ "R", x ++ "S"]) [haskellPrintName x | (x,y) <- nts]))

haskellGetTrioParams :: PProc -> [String]
haskellGetTrioParams (PRecv u x t p) =
  if (ifProp u)
    then
      case p of PEnd -> [(haskellPrintName u) ++ "R"]
                (PApp v' u') -> let appVarS = map (\x -> haskellCheckForBool x) $ zipWith (++) (haskellGetNameS u') (map (\x -> [head x]) $ haskellUnclosureTypeAppVarS t) in
                  ([(haskellPrintName u) ++ "R"] ++ haskellClosureArgs [(SAbstr v')] ++ appVarS)
                (PAppVar x' u') -> let appVarS = map (\x -> haskellCheckForBool x) $ zipWith (++) (haskellGetNameS u') (map (\x -> [head x]) $ haskellUnclosureTypeAppVarS t) in
                  ([(haskellPrintName u) ++ "R"] ++ appVarS)
                (PRecv u2 x2 t2 (PSend u3 v3 p3)) -> [(haskellPrintName u) ++ "R", (haskellPrintName u2) ++ "R", (haskellPrintName u3) ++ "S"]
                (PSend u2 v2 (PSend u3 v3 p3)) -> [(haskellPrintName u) ++ "R", (haskellPrintName u2) ++ "S", (haskellPrintName u3) ++ "S"] ++ (haskellClosureArgs v2)
                (PComp _ ) -> ["PRecvPCompError"]
                (PRestr nts p) -> ["PRecvPRestrError"]
                _ -> ["PRecvError"]
    else
      []

haskellGetTrioParams (PRestr nts (PComp pl)) = haskellGetRestrParams nts pl
haskellGetTrioParams PEnd = ["PEndError"]
haskellGetTrioParams (PApp x u) = ["PAppError"]
haskellGetTrioParams (PAppVar x u) = ["PAppVarError"]
haskellGetTrioParams (PSend u v p) = [(haskellPrintName u) ++ "S"]
haskellGetTrioParams (PComp _ ) = ["PCompError"]
haskellGetTrioParams _ = ["NonExhError"]

------------------------------------------------------------------------------------------------------------------------



---------------------- Functions for printing process spawning (spawnLocal $ ...) --------------------------------------

haskellSpawnProcessS :: [PProc] -> String
haskellSpawnProcessS pl = concat [ (haskellSpawnProcess p) ++ "\n" | p <- pl ]

haskellSpawnProcess :: PProc -> String
haskellSpawnProcess (PRecv u x t p) =
  if (ifProp u)
    then
      case p of PEnd -> "  spawnLocal $ trioEnd" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ "R"
                (PApp v' u') ->  let appVarS = List.intercalate " " $ map (\x -> haskellCheckForBool x) $ zipWith (++) (haskellGetNameS u') (map (\x -> [head x]) $ haskellUnclosureTypeAppVarS t ) in
                  "  spawnLocal $ trioApp" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ "R " ++ (concat $ map (\x -> x ++ " ") (haskellClosureArgs [(SAbstr v')])) ++ appVarS
                (PAppVar x' u') -> let appVarS = List.intercalate " " $ map (\x -> haskellCheckForBool x) $ zipWith (++) (haskellGetNameS u') (map (\x -> [head x]) $ haskellUnclosureTypeAppVarS t ) in
                  "  spawnLocal $ trioAppVar" ++ (haskellPrintName u) ++ " "  ++ (haskellPrintName u) ++ "R " ++ appVarS
                (PRecv u2 x2 t2 (PSend u3 v3 p3)) -> "  spawnLocal $ trioRecv" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ "R " ++ (haskellPrintName u2) ++ "R " ++ (haskellPrintName u3) ++ "S"
                (PSend u2 v2 (PSend u3 v3 p3)) -> "  spawnLocal $ trioSend" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ "R " ++ (haskellPrintName u2) ++ "S " ++ (haskellPrintName u3) ++ "S " ++ (concat $ map (\x -> x ++ " ") (haskellClosureArgs v2))
                (PComp _ ) -> "  ! Pcomp after Receive, invalid trio !"
                (PRestr nts p) -> "  ! PRestr after Receive, invalid trio !"
                _ -> "Error haskellTrios (PRecv u x t p); case of p \n"
    else
      "Error haskellTrios (PRecv u x t p) \n\n\n"

haskellSpawnProcess (PRestr nts (PComp pl)) = "  spawnLocal $ " ++ "restr" ++ (haskellPrintName $ fst $ head nts) ++ " " ++ (concat [x ++ " " | x <- (haskellGetRestrParams nts pl)])
haskellSpawnProcess PEnd = "  ! PEnd without Receive, invalid trio !"
haskellSpawnProcess (PApp x u) = "  ! PApp without Receive, invalid trio !"
haskellSpawnProcess (PAppVar x u) = "  ! PAppVar without Receive, invalid trio !"
haskellSpawnProcess (PSend u v p) = "  spawnLocal $ trioStart" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ "S"
haskellSpawnProcess (PComp _ ) = "  ! Double PComp after Restr, invalid syntax !"
haskellSpawnProcess _ = "Unimplemented haskellSpawnProcess case"

------------------------------------------------------------------------------------------------------------------------



------------------------------------------- Master process functions ---------------------------------------------------

haskellSpawnChannelsMaster :: [(ChannelN, ST)] -> [PProc] -> String
haskellSpawnChannelsMaster env pl = concat [ "  (" ++ n ++ "S," ++ n ++ "R) <- newChan \n" | n <- env2 ]
                           where
                             env2 = List.nub $ map init $ List.intersectBy (\x y -> y `List.isPrefixOf` x) (haskellGetParams pl) envNames
                             envNames = [ haskellPrintName n | (n,st) <- env, not . haskellIsBoolChannel $ haskellPrintName n ]

haskellSpawnPropagatorsMaster :: [(ChannelN, MinM)] -> [PProc] -> String
haskellSpawnPropagatorsMaster nts pl = concat [ "  (" ++ n ++ "S," ++ n ++ "R) <- newChan \n" | n <- props ]
                                 where
                                   props = (map (\(x,y) -> haskellPrintName x) nts) `List.intersect`
                                           (map (\x -> if (length x) > 0 then init x else x) (haskellGetParams pl))

haskellMaster :: PProc -> [(ChannelN, ST)] -> String
haskellMaster (PRestr nts (PComp pl)) env = "master :: Process ()" ++ "\n" ++
                                            "master = do" ++ "\n" ++
                                            "" ++ (haskellSpawnChannelsMaster env pl) ++ "\n" ++
                                            "" ++ (haskellSpawnPropagatorsMaster nts pl) ++ "\n" ++
                                            "" ++ (haskellSpawnProcessS pl) ++ "\n" ++
                                            (haskellPrintHoldProcess "master") ++ "\n\n\n" ++
                                            (haskellTrios (PComp pl))

------------------------------------------------------------------------------------------------------------------------



------------------------------------------------- Trio functions -------------------------------------------------------

haskellTrios :: PProc -> String
haskellTrios (PRecv u x t p) =
  if (ifProp u)
    then
      case p of PEnd -> degenEndTrio (PRecv u x t p)
                (PApp v' u') -> degenAppTrio (PRecv u x t p)
                (PAppVar x' u') -> degenAppVarTrio (PRecv u x t p)
                (PRecv u' x' t' p') -> recvTrio (PRecv u x t p)
                (PSend u' v' p') -> sendTrio (PRecv u x t p)
                _ -> "Error haskellTrios (PRecv u x t p); case of p \n\n\n"
    else
      "Error haskellTrios (PRecv u x t p) \n\n\n"

haskellTrios (PSend u v p) =
  if (ifProp u)
    then
      degenStartTrio (PSend u v p)
    else
      "Error haskellTrios (PSend u v p) \n\n\n"

haskellTrios (PApp v u) = "Error haskellTrios (PApp v u) \n\n\n"
haskellTrios (PAppVar x u) = "Error haskellTrios (PAppVar x u) \n\n\n"
haskellTrios (PComp (p:ps)) = haskellTrios p ++ "\n" ++ haskellTrios (PComp ps)
haskellTrios (PComp _ ) = ""

haskellTrios (PRestr nts (PComp pl)) =
  "restr" ++ (haskellPrintName $ fst $ head nts) ++ " " ++ (concat [x ++ " " | x <- (haskellGetRestrParams nts pl)]) ++ " = do" ++ "\n" ++
  "" ++ restrictions ++ "\n" ++
  "" ++ (haskellSpawnProcessS pl) ++ "\n" ++
  (haskellPrintHoldProcess ("restr" ++ (haskellPrintName $ fst $ head nts))) ++ "\n\n\n" ++
  (haskellTrios (PComp pl))
  where
    restrictions = concat [ "  (" ++ (haskellPrintName n) ++ "S," ++ (haskellPrintName n) ++ "R) <- newChan \n" | (n,st) <- nts ]

haskellTrios PEnd = "Error haskellTrios PEnd \n\n\n"
haskellTrios _ = "Error haskellTrios; unimplemented PProc type\n\n\n"


degenStartTrio (PSend u v p) =
  name ++ " = do" ++ "\n" ++
  "  " ++ "sendChan " ++ (haskellPrintName u) ++ " ([] :: Context)" ++ "\n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n"
  where
    name = "trioStart" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u)


degenEndTrio (PRecv u x t PEnd) =
  name ++ " = do" ++ "\n" ++
  "  " ++ "_ <- receiveChan " ++ (haskellPrintName u) ++ "\n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n"
  where
    name = "trioEnd" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u)


degenAppTrio (PRecv u x t (PApp v' u')) =
  name ++ " = do" ++ "\n" ++
  "  " ++ "_" ++ " <- receiveChan " ++ (haskellPrintName u) ++ "\n" ++
  "  " ++ abstraction ++ " " ++ printVars ++ "\n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n" ++ declaration
  where
    name = "trioApp" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ " " ++
           (concat $ map (\x -> x ++ " ") (haskellClosureArgs [(SAbstr v')])) ++
           (List.intercalate " " $ map (\x -> haskellCheckForBool x) vars)
    vars = zipWith (++) ( haskellGetNameS u') $ (map (\x -> [head x])  $ haskellUnclosureTypeAppS v')
    printVars = case length ( vars ) of
      0 -> "--Error empty PApp channel input list"
      1 -> (\x -> if haskellIsBoolChannel x then (haskellReturnBoolPrefix x) else x ) $ head vars
      _ -> "(" ++ (List.intercalate "," $ map (\x -> if haskellIsBoolChannel x then (haskellReturnBoolPrefix x) else x ) vars) ++ ")"
    (abstraction, declaration) = haskellTriosS2 v' ((haskellPrintName u) ++
                                 (if (length u') > 0 then haskellPrintName $ head u' else "empty"))
-- u' empty for some reason, decomposition doesn't return any, checked with latex output


degenAppVarTrio (PRecv u x t (PAppVar x' u')) =
  name ++ " = do"  ++ "\n" ++
  "  " ++ "ct <- receiveChan " ++ (haskellPrintName u)  ++ " :: Process Context\n" ++
  "  " ++ "Right proc <- return $ unclosure rtable $ (decode " ++ "(snd $ head ct)" ++
  " :: Closure (" ++ printTypes ++ " -> Process ()))"  ++ "\n" ++
  "  " ++ "proc " ++ printVars ++ "\n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n"
  where
    name = "trioAppVar" ++ (haskellPrintName u) ++ " " ++ (haskellPrintName u) ++ " " ++
           (List.intercalate " " $ map (\x -> haskellCheckForBool x) vars)
    vars = zipWith (++) (haskellGetNameS u') (map (\x -> [head x]) $ haskellUnclosureTypeAppVarS t )
    printVars = case length ( vars ) of
      1 -> (\x -> if haskellIsBoolChannel x then (haskellReturnBoolPrefix x) else x ) $ head vars
      _ -> "(" ++ (List.intercalate "," $
           map (\x -> if haskellIsBoolChannel x then (haskellReturnBoolPrefix x) else x ) vars) ++ ")"
    printTypes = case length ( vars ) of
      1 -> head $ haskellUnclosureTypeAppVarS t
      _ -> "(" ++ (List.intercalate "," $ haskellUnclosureTypeAppVarS t) ++ ")"


sendTrio (PRecv u1 x1 t1 (PSend u2 v2 (PSend u3 v3 p3))) =
  name ++ " = do" ++ "\n" ++
  "  " ++ "ct" ++ " <- receiveChan " ++ (haskellPrintName u1) ++ " :: Process Context\n" ++
  "  " ++ "sendChan " ++ (haskellPrintName u2) ++ " " ++ ct1 ++ "\n" ++
  "  " ++ "sendChan " ++ (haskellPrintName u3) ++ " " ++ ct2 ++ "\n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n" ++ absDecl
  where
    name = "trioSend" ++ (haskellPrintName u1) ++ " " ++ (haskellPrintName u1) ++ " " ++
           (haskellPrintName u2) ++ " " ++ (haskellPrintName u3) ++ " " ++
           (concat $ map (\x -> x ++ " ") (haskellClosureArgs v2))
    (absCl, absDecl) = haskellTriosS v2 ((haskellPrintName u1) ++ (haskellPrintName u2) ++ (haskellPrintName u3))
    ct1 = case v2 of ((SAbstr _):_) -> absCl
                     ((SVar _):_) -> " $ ctGet ct " ++ (show v2)
                     _ -> " ([] :: Context)"
    ct2 = if absCl == "var" then " $ ctSub ct " ++ (show v2) else " ct"


recvTrio (PRecv u1 x1 t1 (PRecv u2 x2 t2 (PSend u3 v3 p3))) =
  name ++ " = do" ++ "\n" ++
  "  " ++ "ct <- receiveChan " ++ (haskellPrintName u1) ++ " :: Process Context\n" ++
  "  " ++ "abstr <- receiveChan " ++ (haskellPrintName u2) ++ " :: Process ByteString\n" ++
  "  " ++ "sendChan " ++ (haskellPrintName u3) ++ " $ ct ++ " ++ ct2 ++ " \n" ++
  "  say $ \"End " ++ name ++ "\"" ++ "\n\n"
  where
    name = "trioRecv" ++ (haskellPrintName u1) ++ " " ++ (haskellPrintName u1) ++ " " ++
           (haskellPrintName u2) ++ " " ++ (haskellPrintName u3)
    ct2 = "[(" ++ (show $ head x2) ++ ",abstr)]"

------------------------------------------------------------------------------------------------------------------------




------------------------------------------ Type definition functions -----------------------------------------------------

haskellUnclosureTypeAppS :: PAbstr -> [String]
haskellUnclosureTypeAppS (PAbstr channels types proc) = map unclosureMinM types


haskellUnclosureTypeAppVarS :: [MinU] -> [String]
haskellUnclosureTypeAppVarS ms = concat [unclosureMinU2 m | m <- ms]


haskellUnclosureTypeAppVar :: [MinU] -> String
haskellUnclosureTypeAppVar (x:_) = unclosureMinU x
haskellUnclosureTypeAppVar _ = "|!Typing error, abstraction must have input!|"


haskellUnclosureType :: [MinU] -> String
haskellUnclosureType (x:xs) = "(" ++ (unclosureMinU x) ++ (haskellUnclosureType2 xs)


haskellUnclosureType2 :: [MinU] -> String
haskellUnclosureType2 (x:xs) = ", " ++ (unclosureMinU x) ++ (haskellUnclosureType2 xs)
haskellUnclosureType2 _ = ")"


unclosureMinU :: MinU -> String
unclosureMinU (MinUSh x) = (unclosureMinMs x)
unclosureMinU (MinULin x) = (unclosureMinMs x)


unclosureMinU2 :: MinU -> [String]
unclosureMinU2 (MinUSh xs) = map unclosureMinM xs
unclosureMinU2 (MinULin xs) = map unclosureMinM xs


unclosureMinMs :: [MinM] -> String
unclosureMinMs (x:xs) = (unclosureMinM x) ++ (unclosureMinMs2 xs)


unclosureMinMs2 :: [MinM] -> String
unclosureMinMs2 (x:xs) = ", " ++ (unclosureMinM x) ++ (unclosureMinMs2 xs)
unclosureMinMs2 _ = ""


unclosureMinM :: MinM -> String
unclosureMinM (MinSend _) = "SendPort ByteString"
unclosureMinM (MinRecv _) = "ReceivePort ByteString"
unclosureMinM (MinEnd) = "Bool"
unclosureMinM (MinBool) = "!MinBool unimplemented!"
unclosureMinM _ = "!!Error, unimplemented MinM type!!"

------------------------------------------------------------------------------------------------------------------------



-------------------------------------------- Abstraction functions -----------------------------------------------------

haskellTriosS :: [PSendee] -> String -> (String, String)
haskellTriosS ((SAbstr v):_) name = haskellTriosV v ("closedAbstr" ++ name)
haskellTriosS ((SVar x):_) name = ("var", "")
haskellTriosS _ name = ("()","")


haskellTriosS2 :: PAbstr -> String -> (String, String)
haskellTriosS2 (PAbstr channels types proc) name = haskellTriosV2 (PAbstr channels types proc) ("abstr" ++ name)


haskellTriosV :: PAbstr -> String -> (String, String)
haskellTriosV (PAbstr channels types proc) name =
  ("(encode $ " ++ name ++ "Closure " ++ printHaskellClosureArgs ++ " )" , haskellAbstraction proc name $ haskellGetNameS channels )
  where
    clArgs = (haskellClosureArgs [(SAbstr (PAbstr channels types proc))])
    printHaskellClosureArgs = case length clArgs of 0 -> ""
                                                    1 -> concat clArgs
                                                    2 -> "(" ++ (tail (concat $ map (\x -> x ++ ", ") clArgs)) ++ ")"


haskellTriosV2 :: PAbstr -> String -> (String, String)
haskellTriosV2 (PAbstr channels types proc) name =
  (name ++ " " ++ printHaskellClosureArgs, haskellAbstraction proc name $ haskellGetNameS channels)
  where
    clArgs = (haskellClosureArgs [(SAbstr (PAbstr channels types proc))])
    printHaskellClosureArgs = case length clArgs of 0 -> ""
                                                    1 -> concat clArgs
                                                    2 -> "(" ++ (tail (concat $ map (\x -> x ++ ", ") clArgs)) ++ ")"


haskellAbstraction :: PProc -> String -> [String] -> String
haskellAbstraction (PComp pl) name inChannels =
  name ++ closedParamsPrint ++ inChannelsPrint ++ " = do" ++ "\n" ++
  concat ["  (" ++ x ++ "S," ++ x ++ "R) <- newChan \n" | x <- filterPropagators pl] ++ "\n" ++
  haskellSpawnProcessS pl ++ "\n" ++
  (concat ["  say $ show (" ++ x ++ " :: Bool) \n" | x <- inChannelsTemp2, not (("R" `List.isSuffixOf` x) || ("S" `List.isSuffixOf` x))]) ++
  (haskellPrintHoldProcess name) ++ "\n\n" ++
  (haskellTrios (PComp pl)) ++ "\n\n" ++
  (if ("closed" `List.isPrefixOf` name) then closurePrint else "") ++ "\n\n"
  where
    params = haskellGetParams pl
    inChannelsTemp = params `List.intersect` (concat $ map (\x -> [x ++ "R",x ++ "S"]) inChannels)
    inChannelsTemp2 = (inChannels List.\\ (map init inChannelsTemp)) ++ inChannelsTemp
    closedParams = [x | x <- params, (if (x /= "") then (not . ifProp) (Ch x) else False) ] List.\\ inChannelsTemp2
    closedParamsPrint = case (length closedParams) of 0 -> " "
                                                      1 -> " " ++ (head closedParams) ++ " "
                                                      _ -> " (" ++ (List.intercalate "," closedParams) ++ ") "
    inChannelsPrint = case (length inChannelsTemp2) of 1 -> " " ++ (head inChannelsTemp2) ++ " "
                                                       _ -> " (" ++ (List.intercalate "," inChannelsTemp2) ++ ") "
    closurePrint = case (length closedParams) of 0 -> name ++ "Static = staticLabel \"$" ++ name ++ "\"\n" ++
                                                      name ++ "Closure" ++ " = staticClosure " ++ name ++ "Static"
                                                 _ -> name ++ "Static = staticLabel \"$" ++ name ++ "\"\n" ++
                                                      "" ++ name ++ "Params = staticLabel \"$" ++ name ++ "Params\"\n" ++
                                                      name ++ "Closure params" ++ " = closure decoder (encode params)" ++ "\n" ++
                                                      "  where\n" ++
                                                      "    decoder = " ++ name ++ "Static `staticCompose` " ++ name ++ "Params"


filterPropagators :: [PProc] -> [String]
filterPropagators pl = List.nub [init x | x <- haskellGetParams pl, (if (x /= "") then ifProp (Ch x) else False)]


haskellClosureArgs :: [PSendee] -> [String]
haskellClosureArgs ((SAbstr (PAbstr channels types (PComp proc))):_) =
  [x | x <- haskellGetParams proc, if (x /= "") then (not . ifProp) (Ch x) else False, x /= (inChannel ++ "S"), x /= (inChannel ++ "R")]
  where
    inChannel = haskellPrintName $ head channels
haskellClosureArgs ((SVar x):_) = []
haskellClosureArgs _ = []


haskellGatherClosures :: PProc -> [(String, Int)]
haskellGatherClosures (PRestr nts pl) = haskellGatherClosures pl
haskellGatherClosures (PComp pl) = concat $ map haskellGatherClosures pl
haskellGatherClosures (PRecv u1 x1 t1 (PSend u2 v2@((SAbstr (PAbstr ns ts p)):_) (PSend u3 v3 p3))) =
  [("closedAbstr" ++ (haskellPrintName u1) ++ (haskellPrintName u2) ++
  (haskellPrintName u3), length $ haskellClosureArgs v2)] ++ (haskellGatherClosures p)
haskellGatherClosures _ = []

------------------------------------------------------------------------------------------------------------------------



------- Functions for imports, context, environment printing, rtable, main and the process holding hack ----------------

haskellPrintHoldProcess :: String -> String
haskellPrintHoldProcess name =  "  -- Keeping process running \n" ++
                                "  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ()) \n" ++
                                "  say $ \"process " ++ name ++ " finished and waiting\" \n" ++
                                "  _ <- receiveChan rFake \n" ++
                                "  -- \n" ++
                                "  say $ \"End\""


haskellRTable :: PProc -> String
haskellRTable proc =  "rtable :: RemoteTable" ++ "\n" ++
                      "rtable = " ++ (concat closures3) ++
                      "          $ initRemoteTable"
                      where
                        closures = haskellGatherClosures proc
                        closures2 = [". registerStatic \"$" ++ x ++ "\" (toDynamic " ++ x ++ ")\n" ++ (haskellRTableParams x i) | (x,i) <- closures]
                        closures3 = [(tail (head closures2))] ++ (map (\x -> "          " ++ x) $ tail closures2)


haskellRTableParams :: String -> Int -> String
haskellRTableParams name 0 = ""
haskellRTableParams name 1 = "          . registerStatic \"$" ++ name ++
                             "Params\" (toDynamic (decode :: (ByteString -> SendPort ByteString)))\n"
haskellRTableParams name x = "          . registerStatic \"$" ++ name ++
                             "Params\" (toDynamic (decode :: (ByteString -> (SendPort ByteString" ++
                             (concat (take (x-1) $ repeat ",SendPort ByteString")) ++ "))))\n"


haskellMain :: String
haskellMain = "main :: IO () \n" ++
              "main = do \n" ++
              "  Right transport <- createTransport \"127.0.0.1\" \"8108\"  (\\port' -> (\"127.0.0.1\" , port')) defaultTCPParameters \n" ++
              "  node <- newLocalNode transport rtable \n" ++
              "  runProcess node master \n"


haskellPrintImports :: String
haskellPrintImports = "{-# LANGUAGE TemplateHaskell #-} \n" ++
                      "import System.Environment (getArgs) \n" ++
                      "import Data.Map \n" ++
                      "import Data.ByteString.Lazy hiding (head, elem) \n" ++
                      "import Data.Binary \n" ++
                      "import Data.Rank1Dynamic \n" ++
                      "import Control.Distributed.Static hiding (initRemoteTable) \n" ++
                      "import Control.Distributed.Process \n" ++
                      "import Control.Distributed.Process.Node \n" ++
                      "import Control.Distributed.Process.Closure \n" ++
                      "import Network.Transport.TCP (createTransport, defaultTCPParameters) \n"


haskellPrintContextFunctions :: String
haskellPrintContextFunctions = "type Context = [(String, ByteString)] \n\n" ++
                               "ctGet :: Context -> [String] -> Context \n" ++
                               "ctGet ct strs = toList $ filterWithKey (\\k _ -> k `elem` strs) (fromList ct) \n\n" ++
                               "ctSub :: Context -> [String] -> Context \n" ++
                               "ctSub ct strs = toList $ filterWithKey (\\k _ -> not (k `elem` strs)) (fromList ct) \n\n"


haskellprintEnv :: [(ChannelN, ST)] -> String
haskellprintEnv xs = concat [(haskellPrintEnvName x) ++ "\n" | (x,y) <- xs]

------------------------------------------------------------------------------------------------------------------------



------------- Main function, is used in the Misty module for printing the entire Cloud Haskell program -----------------

getHaskellContent :: PProc -> [(ChannelN, ST)] -> String
getHaskellContent process env = haskellPrintImports ++ "\n\n" ++ haskellPrintContextFunctions ++ "\n\n" ++
                                (haskellMaster process env) ++ "\n\n" ++ rtable ++ "\n\n" ++ haskellMain
                                where
                                  rtable = if (haskellGatherClosures process) == []
                                            then
                                              "rtable = initRemoteTable\n"
                                            else
                                              (haskellRTable process)

------------------------------------------------------------------------------------------------------------------------