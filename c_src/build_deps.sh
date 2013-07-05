#!/bin/sh

set -e

ROOT="$PWD"

HIREDIS_VSN="v0.11.0"

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# Changed "make" to $MAKE

case "$1" in
    clean)
		# rm ./c_src/hiredis
		rm -rf $ROOT/c_src/hiredis
        ;;

    get-deps)
		#clone hiredis to ./c_src/hiredis from git@github.com:redis/hiredis.git
		cd c_src

        git clone git@github.com:redis/hiredis.git
        cd hiredis
        git checkout $HIREDIS_VSN
        
        cd $ROOT
        ;;

    *)
        #build hiredis and install lib to ./priv/hiredis
		#export LDFLAGS="$LDFLAGS -L $ROOT/c_src/hiredis -llibhiredis"
		export CFLAGS="$CFLAGS -arch x86_64 -m64"
		cd $ROOT/c_src/hiredis && make

		# rm -rf $ROOT/priv/hiredis
		# mkdir -p $ROOT/priv/hiredis
		# cp $ROOT/c_src/hiredis/*.h $ROOT/priv/hiredis
		# cp $ROOT/c_src/hiredis/*.o $ROOT/priv/hiredis
		# cp $ROOT/c_src/hiredis/*.a $ROOT/priv/hiredis
		# cp $ROOT/c_src/hiredis/*.dylib $ROOT/priv/hiredis
		
		cd $ROOT
        ;;
esac
