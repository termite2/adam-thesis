import System.Environment 
import Control.Monad

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildDir = "build"

latexSources  = ["thesis.tex", "background.tex", "game.tex", "intro.tex", "solving.tex", "syntcomp.tex", "userguided.tex", "i2c.tex", "conclusions.tex"]
tslRefSources = ["body.tex"]
bibSources    = ["extra.bib"]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake/", shakeThreads=4} $ do
    want ["thesis.pdf"]

    "thesis.pdf" %> \out -> do
        files <- getDirectoryFiles "diagrams" ["//*.hs"]
        bibs  <- getDirectoryFiles "bibtex"   ["//*.bib"]
        tsls  <- getDirectoryFiles "i2c"      ["//*.tsl"]

        let diagramFiles = map (\x -> buildDir </> "diagrams" </> x -<.> "pdf") files
            bibFiles     = map ("bibtex" </>) bibs

        need $ map ("sources" </>) latexSources ++ map ("tsl_reference" </>) tslRefSources ++ bibSources ++ diagramFiles ++ bibFiles ++ map ("i2c" </>) tsls

        env' <- liftIO getEnvironment
        let env = Env $ ("TEXINPUTS", "sources:build:") : env'

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
