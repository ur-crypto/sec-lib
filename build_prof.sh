#!/bin/bash
stack install --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -threaded" --no-system-ghc
