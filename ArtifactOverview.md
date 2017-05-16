Artifact accompanying ICFP paper __Normalization by Evaluation for Sized Dependent Types__

Sit -- Checking dependent types with sized natural numbers
==========================================================

The __Sit__ (size-irrelevant types) checker is a prototype coming with the ICFP 2017 paper
__Normalization by Evaluation for Sized Dependent Types__ by Abel, Vezzosi, and Winterhalter.
It is a proof of concept that we can have a dependently typed language with sized types
where type checking is decidable and termination is instrumented by sizes.
The main new feature is the shape-irrelevant quantifier
`..(i : Size) -> T` to be used in types of types, distinguished from
irrelevant size quantification `..(i : Size) -> T` to be used in types of programs.

Installation with docker
------------------------

If you have a `docker` installation, you can get and test `Sit` by the command on the shell:
```shell
docker run theowinterhalter/icfp-sit
```
You should see something like this:
```
Unable to find image 'theowinterhalter/icfp-sit:latest' locally
latest: Pulling from theowinterhalter/icfp-sit
cd0a524342ef: ... Pull complete
dd5f938bc637: ... Pull complete
cb90bb83bdab: ... Pull complete
a1e5cf9ae1e4: ... Pull complete
12fbf6ef4b99: ... Pull complete
b6a6d86e767f: ... Pull complete
6dc027e33bb0: ... Pull complete
Digest: sha256:9f1ff6ab7cfdc0de82df61fac6a0b6cbd34d62de7a8e5596d4217a5a4fa95772
Status: Downloaded newer image for theowinterhalter/icfp-sit:latest
Checking Eq : forall (A : Set) (a b : A) -> Set1
Checking Eq = \ A a b -> (P : A -> Set) -> P a -> P b
...
```

Installation from Hackage
-------------------------

If you have `ghc >= 7.8` and `cabal >= 1.24`, you can install `Sit` from `hackage.haskell.org`:
```shell
cabal update
cabal install Sit
cd $HOME/.cabal/share/x86_64-linux-ghc-7.10.3/Sit-0.2017.5.2/test
Sit.bin Test.agda
```
This assumes that your `.cabal/bin` is in the `PATH`.
You also have to adjust the path in the `cd` command above to your system.

Installation from the supplied tarball
--------------------------------------

With GHC, you can also install from the supplied tarball:
```shell
tar xzf Sit-0.2017.2.26.tar.gz
cd Sit-0.2017.2.26
make
```

Programming with Sit
--------------------

The syntax is summarized on the github page
https://github.com/andreasabel/Sit
but to get started, look at the file
`test/Test.agda`
coming with Sit.
Note that the syntax is a subset of the Agda syntax,
you can compare the result of type checking with Sit to Agda's answer.
(You need to get the latest stable version of Agda from github.com).
The predefined types and combinators of Sit are defined in
`test/Base.agda`.

The type checker is called via
```
Sit.bin <myfile>.agda
```

Evaluating the artifact
-----------------------

Reading the supplied examples, understand the difference between ordinary quantification,
irrelevant quantification (`.(i : Size)`) and shape-irrelevant quantification
(`..(i : Size)`).

Check your own examples.

Read the source code on
https://github.com/andreasabel/Sit
or
https://hackage.haskell.org/package/Sit
.
