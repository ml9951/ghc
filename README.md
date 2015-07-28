The Glasgow Haskell Compiler with Partial Abort STM
============================
This is a fork of GHC that adds a new STM monad which is able to 
partially abort transactions.  Below are the steps necessary to
get everything checked out and built:


    git clone --recursive git://git.haskell.org/ghc.git
    cd ghc
    git remote add pstm-remote https://github.com/ml9951/ghc.git
    git fetch pstm-remote
    git checkout pstm
    cp mk/build.mk.sample mk/build.mk #Uncomment the following line: "BuildFlavour = quick"
    perl boot
    ./configure
    make         # can also say 'make -jX' for X number of jobs
    make install	 
    
Contributors & Acknowledgements
===============================

GHC in its current form wouldn't exist without the hard work of
[its many contributors] [12]. Over time, it has grown to include the
efforts and research of many institutions, highly talented people, and
groups from around the world. We'd like to thank them all, and invite
you to join!

  [1]:  http://www.haskell.org/ghc/            "www.haskell.org/ghc/"
  [2]:  http://ghc.haskell.org/trac/ghc    "ghc.haskell.org/trac/ghc"
  [3]:  http://ghc.haskell.org/trac/ghc/wiki/Building
          "ghc.haskell.org/trac/ghc/wiki/Building"
  [4]:  http://www.haskell.org/happy/          "www.haskell.org/happy/"
  [5]:  http://www.haskell.org/alex/           "www.haskell.org/alex/"
  [6]:  http://www.haskell.org/haddock/        "www.haskell.org/haddock/"
  [7]: https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#GettingaGHCrepositoryfromGitHub
          "https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#GettingaGHCrepositoryfromGitHub"
  [8]:  http://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
          "http://ghc.haskell.org/trac/ghc/wiki/Building/Preparation"
  [9]:  http://www.haskell.org/cabal/          "http://www.haskell.org/cabal/"
  [10]: http://ghc.haskell.org/trac/ghc/
          "http://ghc.haskell.org/trac/ghc/"
  [11]: http://www.haskell.org/pipermail/glasgow-haskell-users/
          "http://www.haskell.org/pipermail/glasgow-haskell-users/"
  [12]: http://ghc.haskell.org/trac/ghc/wiki/TeamGHC
          "http://ghc.haskell.org/trac/ghc/wiki/TeamGHC"
