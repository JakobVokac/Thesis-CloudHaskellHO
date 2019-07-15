-------------------------------- Cloud Haskell output code ------------------------------------------------



removeNonAlphaNum :: String -> String
removeNonAlphaNum n = [x | x <- n, isAlphaNum x]

haskellPrintName :: ChannelN -> String
haskellPrintName (Ch n) = removeNonAlphaNum n
haskellPrintName (ChCmpl n) = removeNonAlphaNum n

haskellPrintNameS :: [ChannelN] -> String
haskellPrintNameS ((Ch n):ns) = removeNonAlphaNum n
haskellPrintNameS ((ChCmpl n):ns) = removeNonAlphaNum n

haskellPrintVarN :: VarN -> String
haskellPrintVarN v = show v

haskellPrintVarNS :: [VarN] -> String
haskellPrintVarNS (v:vs) = show v

haskellTrios :: PProc -> String
haskellTrios (PRecv u x t p) = if (ifProp u)
                                then
                                  case p of PEnd -> degenEndTrio (PRecv u x t p)
                                            (PAppVar x' u') -> degenAppVarTrio (PRecv u x t p)
                                            (PRecv u' x' t' p') -> recvTrio (PRecv u x t p)
                                            (PSend u' v' p') -> sendTrio (PRecv u x t p)
                                            _ -> "Error haskellTrios (PRecv u x t p); case of p \n\n\n"
                                else
                                  "Error haskellTrios (PRecv u x t p) \n\n\n"

haskellTrios (PSend u v p) = if (ifProp u)
                              then
                                degenStartTrio (PSend u v p)
                              else
                                "Error haskellTrios (PSend u v p) \n\n\n"

haskellTrios (PApp v u) = "Error haskellTrios (PApp v u) \n\n\n"
haskellTrios (PAppVar x u) = "Error haskellTrios (PAppVar x u) \n\n\n"
haskellTrios (PComp (p:ps)) = haskellTrios p ++ "\n -- next proc \n" ++ haskellTrios (PComp ps)
haskellTrios (PComp _ ) = "-- End of program" 
haskellTrios (PRestr nts p) = "TODO haskellTrios (PRestr nts p) \n\n\n" ++ haskellTrios p
haskellTrios PEnd = "Error haskellTrios PEnd \n\n\n"
haskellTrios _ = "Error haskellTrios; unimplemented PProc type\n\n\n"

degenStartTrio (PSend u v p) = "trioStart :: SendPort () -> Process ()" ++ "\n" ++
                               "trioStart " ++ (haskellPrintName u) ++ " = do" ++ "\n" ++
                               "  " ++ "sendChan " ++ (haskellPrintName u) ++ " ()" ++ "\n\n"

degenEndTrio (PRecv u x t PEnd) = "trioEnd :: ReceivePort () -> Process ()" ++ "\n" ++
                                  "trioEnd " ++ (haskellPrintName u) ++ " = do" ++ "\n" ++
                                  "  " ++ "_ <- receiveChan " ++ (haskellPrintName u) ++ "\n\n"

degenAppVarTrio (PRecv u x t (PAppVar x' u')) = "trioApp :: ReceivePort ProcClosure -> |!fixType!| -> Process ()" ++ "\n" ++
                                                "trioApp " ++ (haskellPrintName u) ++ " " ++ (haskellPrintNameS u') ++ " = do"  ++ "\n" ++
                                                "  " ++ x' ++ " <- receiveChan " ++ (haskellPrintName u)  ++ "\n" ++
                                                "  " ++ "Right proc <- return $ unclosure rtable $ (decode " ++ x' ++ " :: Closure (|!fixType!| -> Process ()))"  ++ "\n" ++
                                                "  " ++ "proc " ++ (haskellPrintNameS u') ++ "\n\n"

sendTrio (PRecv u1 x1 t1 (PSend u2 v2 (PSend u3 v3 p3))) = "trioSend :: ReceivePort () -> SendPort a -> SendPort () -> Process ()" ++ "\n" ++
                                                           "trioSend " ++ (haskellPrintName u1) ++ " " ++ (haskellPrintName u2) ++ " " ++ (haskellPrintName u3) ++ " = do" ++ "\n" ++
                                                           "  " ++ "_ <- receiveChan " ++ (haskellPrintName u1) ++ "\n" ++
                                                           "  " ++ "sendChan " ++ (haskellPrintName u2) ++ " " ++ (haskellTriosS v2) ++ "\n" ++
                                                           "  " ++ "sendChan " ++ (haskellPrintName u3) ++ " ()" ++ "\n\n" 

recvTrio (PRecv u1 x1 t1 (PRecv u2 x2 t2 (PSend u3 v3 p3))) = "trioRecv :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()" ++ "\n" ++
                                                              "trioRecv " ++ (haskellPrintName u1) ++ " " ++ (haskellPrintName u2) ++ " " ++ (haskellPrintName u3) ++ " = do" ++ "\n" ++
                                                              "  " ++ "_ <- receiveChan " ++ (haskellPrintName u1) ++ "\n" ++
                                                              "  " ++ (haskellPrintVarNS x2) ++ " <- receiveChan " ++ (haskellPrintName u2) ++ "\n" ++
                                                              "  " ++ "sendChan " ++ (haskellPrintName u3) ++ " " ++ (haskellPrintVarNS x2) ++ "\n\n"


haskellTriosS :: PSendee -> (String, String)
haskellTriosS _ = "{Abstraction or value, TODO}"
haskellTriosS (SAbstr v) = haskellTriosV v
haskellTriosS (SVar x) = (x, "")

haskellTriosV :: PAbstr -> (String, String)
haskellTriosV v = ("TODO abstraction name","TODO abstraction closure")

getHaskellContent :: PProc -> String
getHaskellContent process = "\n\n" ++ haskellTrios process 



-----------------------------------------------------------------------------------------------------------

