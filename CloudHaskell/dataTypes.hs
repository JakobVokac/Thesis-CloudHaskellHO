

data Env = Number Int Env | Word String Env

manipEnv :: Env -> Env
manipEnv (Number a) = Number (a + 10)
manipEnv (Word a) = Word (a ++ "a")

