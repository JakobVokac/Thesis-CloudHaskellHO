

Error haskellTrios (PRestr nts p) 


trioStart :: SendPort () -> Process ()
trioStart c_{1} = do
  sendChan c_{1} ()


 -- next proc 
trioRecv :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
trioRecv c_{1} u_{1} c_{2} = do
  _ <- receiveChan c_{1}
  "x" <- receiveChan u_{1}
  sendChan c_{2} "x"


 -- next proc 
trioSend :: ReceivePort () -> SendPort a -> SendPort () -> Process ()
trioSend c_{2} u_{2} c_{3} = do
  _ <- receiveChan c_{2}
  sendChan u_{2} {Abstraction or value, TODO}
  sendChan c_{3} ()


 -- next proc 
trioEnd :: ReceivePort () -> Process ()
trioEnd c_{3} = do
  _ <- receiveChan c_{3}


 -- next proc 
-- End of program