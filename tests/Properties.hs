import FoundationDB

main :: IO ()
main = withFoundationDB (return ())
