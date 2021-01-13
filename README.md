# Learning Project: LeNet 5 Digit Classification

Working through a re-creation of LeNet 5 from scratch, without using existing
deep learning packages.

Data and paper link: http://yann.lecun.com/exdb/mnist/

Build:

``` sh
./download-mnist.sh  # download the MNIST images to ./mnist-data
cabal run
```

## Progress so far

Work-in-progress:
  - [x] Loading MNIST images and label sets from raw binary
  - [x] Convolution layer forward evaluation
  - [x] Convolution layer back-propagation
  - [ ] Numerical test of gradient for convolution layer
  - [ ] Sigmoidal activation function
  - [ ] Sub-sampling layer forward evaluation
  - [ ] Sub-sampling layer back-propagation
  - [ ] Fully-connected layer forward evaluation
  - [ ] Fully-connected layer back-propagation
  - [ ] RBF units

