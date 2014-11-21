#!/bin/bash

export LD_LIBRARY_PATH=./libs
exec ./dist/build/find-hackage/find-hackage
