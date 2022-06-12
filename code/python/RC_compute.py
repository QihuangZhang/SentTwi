  
# -*- coding: utf-8 -*-
#
# This file is for the RC implementation of the prediction of time series data
#
# Copyright Nils Schaetti <nils.schaetti@unine.ch>


# Imports
## RC compute
import torch
from echotorch.datasets.NARMADataset import NARMADataset
from echotorch.datasets.CreateDataset import CreateDataset
import echotorch.nn.reservoir as etrs
import echotorch.utils
import echotorch.utils.matrix_generation as mg
from torch.autograd import Variable
from torch.utils.data import DataLoader
import numpy as np
import matplotlib.pyplot as plt

## Data analysis
import pandas as pd

## optimization
import echotorch.utils.optimization as optim
from itertools import product
from tqdm import tqdm

## Load the data

## os.chdir('N:\\Work\\waterloo\\2020\\SentTwi\\')
infectcase = pd.read_csv('output/Infected_Cases.csv')
tweetcounts = pd.read_csv('output/Tweets.csv')

# Length of training samples
train_sample_length = 7 * len(infectcase.index)

# Length of test samples
test_sample_length = len(infectcase.index)

# How many training/test samples
# n_train_samples = 1
# n_test_samples = 1

# Batch size (how many sample processed at the same time?)
batch_size = 1

# Reservoir hyper-parameters
spectral_radius = 1.07
leaky_rate = 0.9261
input_dim = 1
reservoir_size = 200
connectivity = 0.1954
ridge_param = 0.00000409
input_scaling = 0.9252
bias_scaling = 0.079079

# Predicted/target plot length
plot_length = 200

# Use CUDA?
use_cuda = True
# use_cuda = torch.cuda.is_available() if use_cuda else False

# Manual seed initialisation
np.random.seed(1)
torch.manual_seed(1)

# Parameters ranges
param_ranges = dict()
param_ranges['spectral_radius'] = np.arange(0, 1.1, 0.5)
param_ranges['leaky_rate'] = np.arange(0.1, 0.2, 0.5)
param_ranges['reservoir_size'] = np.arange(50, 410, 100)
param_ranges['connectivity'] = np.arange(0.1, 1.1, 0.5)
param_ranges['ridge_param'] = np.logspace(-7, 2, base=10, num=5)
param_ranges['input_scaling'] = np.arange(0.1, 1.1, 0.5)
param_ranges['bias_scaling'] = np.arange(0.0, 1.0, 0.5)

parameter_population = (
            dict(zip(param_ranges.keys(), values)) for values in product(*param_ranges.values())
        )

### parameter_population = [dict(zip(param_ranges.keys(), values)) for values in product(*param_ranges.values())]

### Cross-validation

CViNRMSEmin = 99999

for r, param_individual in enumerate(tqdm(parameter_population)):
    print(r)

    ### Get parameter values
    spectral_radius = param_individual.get("spectral_radius")
    leaky_rate = param_individual.get("leaky_rate")
    input_dim = 1
    reservoir_size = 200
    connectivity = param_individual.get("connectivity")
    ridge_param = param_individual.get("ridge_param")
    input_scaling = param_individual.get("input_scaling")
    bias_scaling = param_individual.get("bias_scaling")

    # Manual seed initialisation
np.random.seed(1)
torch.manual_seed(1)

indexcity = np.arange(3, 11, 1).tolist()
cityMSE = list()
cityNRMSE = list()


## Create the predictor

# Internal matrix
w_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    spetral_radius=spectral_radius
)

# Input weights
win_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    scale=input_scaling,
    apply_spectral_radius=False
)

# Bias vector
wbias_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    scale=bias_scaling,
    apply_spectral_radius=False
)

# Create a Leaky-integrated ESN,
# with least-square training algo.
# esn = etrs.ESN(
esn = etrs.LiESN(
    input_dim=input_dim,
    hidden_dim=reservoir_size,
    output_dim=1,
    leaky_rate=leaky_rate,
    learning_algo='inv',
    w_generator=w_generator,
    win_generator=win_generator,
    wbias_generator=wbias_generator,
    ridge_param=ridge_param
)

# Transfer in the GPU if possible
if use_cuda:
    esn.cuda()
# end if



for city_i in indexcity:
indexcityindex = indexcity[:] 
indexcityindex.pop(city_i-3)

train_tensor = CreateDataset(infectcase.iloc[:,indexcityindex], tweetcounts.iloc[:,indexcityindex])
test_tensor = CreateDataset(infectcase.iloc[:,city_i], tweetcounts.iloc[:,city_i])

# Data loader
trainloader = DataLoader(train_tensor, batch_size=batch_size, shuffle=False, num_workers=2)
testloader = DataLoader(test_tensor, batch_size=batch_size, shuffle=False, num_workers=2)

# For each batch
for data in trainloader:
    # Inputs and outputs
    inputs, targets = data
    # Transform data to Variables
    inputs, targets = Variable(inputs), Variable(targets)
    if use_cuda: inputs, targets = inputs.cuda(), targets.cuda()
    # ESN need inputs and targets
    esn(inputs, targets)
# end for

# Now we finalize the training by
# computing the output matrix Wout.
esn.finalize()

# Get the first sample in test set,
# and transform it to Variable.
dataiter = iter(testloader)
test_u, test_y = dataiter.next()
test_u, test_y = Variable(test_u), Variable(test_y)
if use_cuda: test_u, test_y = test_u.cuda(), test_y.cuda()

# Make a prediction with our trained ESN
y_predicted = esn(test_u)

# Print test MSE and NRMSE
cityMSE.append(echotorch.utils.mse(y_predicted.data, test_y.data))
cityNRMSE.append(echotorch.utils.nrmse(y_predicted.data, test_y.data))
# end for

CViNRMSE = sum(cityNRMSE)/len(cityNRMSE)

if CViNRMSE < CViNRMSEmin:
    param_best = param_individual
    CViNRMSEmin = CViNRMSE
# end if
# end for


# load dataset
# narma10_train_dataset = NARMADataset(train_sample_length, n_train_samples, system_order=10)
# narma10_test_dataset = NARMADataset(test_sample_length, n_test_samples, system_order=10)


# Internal matrix
w_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    spetral_radius=spectral_radius
)

# Input weights
win_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    scale=input_scaling,
    apply_spectral_radius=False
)

# Bias vector
wbias_generator = echotorch.utils.matrix_generation.NormalMatrixGenerator(
    connectivity=connectivity,
    scale=bias_scaling,
    apply_spectral_radius=False
)

# Create a Leaky-integrated ESN,
# with least-square training algo.
# esn = etrs.ESN(
esn = etrs.LiESN(
    input_dim=input_dim,
    hidden_dim=reservoir_size,
    output_dim=1,
    leaky_rate=leaky_rate,
    learning_algo='inv',
    w_generator=w_generator,
    win_generator=win_generator,
    wbias_generator=wbias_generator,
    ridge_param=ridge_param
)

# Transfer in the GPU if possible
if use_cuda:
    esn.cuda()
# end if




Causal_tensor = CreateDataset(infectcase.iloc[:,3:11], tweetcounts.iloc[:,3:11])
test_tensor = CreateDataset(infectcase.iloc[:,10], tweetcounts.iloc[:,10])

# Data loader
trainloader = DataLoader(train_tensor, batch_size=batch_size, shuffle=False, num_workers=2)
testloader = DataLoader(test_tensor, batch_size=batch_size, shuffle=False, num_workers=2)




# For each batch
for data in trainloader:
    # Inputs and outputs
    inputs, targets = data

    # Transform data to Variables
    inputs, targets = Variable(inputs), Variable(targets)
    if use_cuda: inputs, targets = inputs.cuda(), targets.cuda()

    # ESN need inputs and targets
    esn(inputs, targets)
# end for

# Now we finalize the training by
# computing the output matrix Wout.
esn.finalize()

# Get the first sample in training set,
# and transform it to Variable.
dataiter = iter(trainloader)
train_u, train_y = dataiter.next()
train_u, train_y = Variable(train_u), Variable(train_y)
if use_cuda: train_u, train_y = train_u.cuda(), train_y.cuda()

# Make a prediction with our trained ESN
y_predicted = esn(train_u)

# Print training MSE and NRMSE
print(u"Train MSE: {}".format(echotorch.utils.mse(y_predicted.data, train_y.data)))
print(u"Test NRMSE: {}".format(echotorch.utils.nrmse(y_predicted.data, train_y.data)))
print(u"")

# Get the first sample in test set,
# and transform it to Variable.
dataiter = iter(testloader)
test_u, test_y = dataiter.next()
test_u, test_y = Variable(test_u), Variable(test_y)
if use_cuda: test_u, test_y = test_u.cuda(), test_y.cuda()

# Make a prediction with our trained ESN
y_predicted = esn(test_u)

# Print test MSE and NRMSE
print(u"Test MSE: {}".format(echotorch.utils.mse(y_predicted.data, test_y.data)))
print(u"Test NRMSE: {}".format(echotorch.utils.nrmse(y_predicted.data, test_y.data)))
print(u"")

# Show target and predicted
plt.plot(test_y[0, :plot_length, 0].data, 'r')
plt.plot(y_predicted[0, :plot_length, 0].data, 'b')
plt.show()