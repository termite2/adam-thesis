import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake/"} $ do
    want ["thesis.pdf"]

    "thesis.pdf" %> \out -> do
        need ["thesis.tex"]
        cmd "pdflatex" ["thesis.tex"]

