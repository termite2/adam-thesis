import System.Environment 
import Control.Monad

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildDir = "build"

latexSources = ["thesis.tex", "background.tex", "game.tex", "intro.tex", "solving.tex", "syntcomp.tex", "userguided.tex", "extra.bib"]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake/"} $ do
    want ["thesis.pdf"]

    "thesis.pdf" %> \out -> do
        files <- getDirectoryFiles "diagrams" ["//*.hs"]
        bibs  <- getDirectoryFiles "bibtex"   ["//*.bib"]
        let diagramFiles = map (\x -> buildDir </> "diagrams" </> x -<.> "pdf") files
            bibFiles     = map ("bibtex" </>) bibs

        need $ latexSources ++ diagramFiles ++ bibFiles

        env' <- liftIO getEnvironment
        let env = Env $ ("TEXINPUTS", "build:") : env'

        () <- cmd "pdflatex" env ["-output-directory=" ++ buildDir, "thesis.tex"]
        () <- cmd "bibtex"   [buildDir </> "thesis.aux"]
        () <- cmd "pdflatex" env ["-output-directory=" ++ buildDir, "thesis.tex"]
        () <- cmd "pdflatex" env ["-output-directory=" ++ buildDir, "thesis.tex"]
        cmd "cp" [buildDir </> "thesis.pdf", "thesis.pdf"]

    (buildDir </> "diagrams" </> "*.pdf") %> \out -> do
        let nd = dropDirectory1 out -<.> "hs"
        need $ [nd]
        cmd "runhaskell" [nd, "-o", out, "-w 200"]

    phony "clean" $ do
        putNormal $ "Cleaning files in .shake and " ++ buildDir
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter buildDir ["//*"]
