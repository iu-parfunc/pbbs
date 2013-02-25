#!/bin/bash

set -e

echo "Fetching raw binary blobs."

mkdir -p data/

cd data/

wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/angelTriangles.bz2
wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/chr22.dna.bz2
wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/dragonTriangles.bz2
wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/etext99.bz2
wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/happyTriangles.bz2
wget --no-clobber http://cs.indiana.edu/~rrnewton/files/pbbs_datasets/wikisamp.xml.bz2
