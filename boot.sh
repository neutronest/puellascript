#!/bin/sh

export TOP=$PWD/.boot
export GHC_LIBDIR=`ghc --print-libdir`
export PHC=`which phc`

rm -rf $TOP
mkdir -p $TOP/package.conf.d

cd boot-libraries

cp $GHC_LIBDIR/package.conf.d/rts.conf $TOP/package.conf.d/rts.conf
ghc-pkg --package-db $TOP/package.conf.d recache

ghc ghc-prim/Setup.hs
cd ghc-prim
./Setup configure --builddir=$TOP/dist/ghc-prim --package-db=$TOP/package.conf.d --prefix $TOP --with-ghc=$PHC
./Setup build --builddir=$TOP/dist/ghc-prim
./Setup install --builddir=$TOP/dist/ghc-prim
sed -i -e 's,^exposed-modules:,exposed-modules: GHC.Prim,' $TOP/package.conf.d/ghc-prim-*.conf
ghc-pkg --package-db $TOP/package.conf.d recache
cd ..

ghc integer-gmp/Setup.hs
cd integer-gmp
./Setup configure --builddir=$TOP/dist/integer-gmp --package-db=$TOP/package.conf.d --prefix $TOP --with-ghc=$PHC
./Setup build --builddir=$TOP/dist/integer-gmp
./Setup install --builddir=$TOP/dist/integer-gmp
cd ..

ghc base/Setup.hs
cd base
./Setup configure --builddir=$TOP/dist/base --package-db=$TOP/package.conf.d --prefix $TOP --with-ghc=$PHC -finteger-gmp
./Setup build --builddir=$TOP/dist/base
./Setup install --builddir=$TOP/dist/base
cd ..
