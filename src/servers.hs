import Data.ByteString.Char8
import Network.DNS

main = do
    print "Hi"
    let hostname = Data.ByteString.Char8.pack "www.epicgames.com"
    rs <- makeResolvSeed defaultResolvConf
    withResolver rs $ \resolver -> lookupA resolver hostname
