#!/usr/bin/env bash
#
# Download MNIST digits from Yann LeCun's website.
set -euo pipefail

mkdir -p ./mnist-data

# training images
curl http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz | \
    gunzip --stdout > \
    ./mnist-data/train-images-idx3-ubyte
# training labels
curl http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz | \
    gunzip --stdout > \
    ./mnist-data/train-labels-idx1-ubyte
# test set images
curl http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz | \
    gunzip --stdout > \
    ./mnist-data/t10k-images-idx3-ubyte
# test set labels
curl http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz | \
    gunzip --stdout > \
    ./mnist-data/t10k-images-idx1-ubyte
