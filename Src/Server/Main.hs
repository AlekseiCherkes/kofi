import Server
import Loggers
import MessageHandler

main = withLoggers $ \_ -> do
  runServer handleMessage

