import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake/"} $ do
    want ["thesis.pdf"]

    "thesis.pdf" %> \out -> do
        files <- getDirectoryFiles "diagrams" ["//*.hs"]
        need $ ["thesis.tex", "background.tex", "game.tex", "intro.tex", "solving.tex", "syntcomp.tex", "userguided.tex"] ++ map (\x -> "diagrams" </> x -<.> "pdf") files
        cmd "pdflatex" ["thesis.tex"]

    "diagrams/*.pdf" %> \out -> do
        let nd = out -<.> "hs"
        need $ [nd]
        cmd "runhaskell" [nd, "-o", out, "-w 200"]

    phony "clean" $ do
        putNormal "Cleaning files in .shake"
        removeFilesAfter ".shake" ["//*"]
