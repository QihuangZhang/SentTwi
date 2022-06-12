# Imports

import torch
import numpy as np
import pandas as pd
import echotorch.utils.matrix_generation as mg
from torch.autograd import Variable
from torch.utils.data.dataloader import DataLoader
import echotorch.nn.reservoir as etrs
from echotorch.datasets.CreateDataset import CreateDataset

class scantau:
    """
    Scan the correlation between y and yhat for different time lag. The prediction
    is based on reservoir computing
    Parameters
    ----------
    data : data frame of date (rows) by city (columns).
    label : data frame of date (rows) by city (columns). label should be
        in the same dimention as data
    dtype : str, numpy.dtype, or ExtensionDtype, optional
        Data type for the output Series. If not specified, this will be
        inferred from `data`.
        See the :ref:`user guide <basics.dtypes>` for more usages.
    param_best : dict
        The best parameter.
    """
    # ----------------------------------------------------------------------
    # Constructors

    def __init__(
        self, data, label, param_best, span=30
    ):
        self.taus = np.arange(-span, span+1, 1)
        self.data = data
        self.label = label
        self.param_best = param_best
        self.ncity = len(data.columns)-3
        self.nT = len(data)

        self.batch_size = 1

        # Generate results
        self.inputs, self.outputs = self.get_rho()
    # end __init__
       

    def get_rho(self):
        """
        Execute the tau scan
        :return:
        """

        rho = list()
        rhofull = list()
        taurecord = list()
        use_cuda = False

        for r, tau in enumerate(self.taus):
            print(r)
            ### Get parameter values
            spectral_radius = self.param_best.get("spectral_radius")
            leaky_rate = self.param_best.get("leaky_rate")
            input_dim = 1
            reservoir_size = self.param_best.get("reservoir_size")
            connectivity = self.param_best.get("connectivity")
            ridge_param = self.param_best.get("ridge_param")
            input_scaling = self.param_best.get("input_scaling")
            bias_scaling = self.param_best.get("bias_scaling")
            # Manual seed initialisation
            np.random.seed(1)
            torch.manual_seed(1)
            ## Create the predictor
            # Internal matrix
            w_generator = mg.NormalMatrixGenerator(
                connectivity=connectivity,
                spetral_radius=spectral_radius
            )
            # Input weights
            win_generator = mg.NormalMatrixGenerator(
                connectivity=connectivity,
                scale=input_scaling,
                apply_spectral_radius=False
            )
            # Bias vector
            wbias_generator = mg.NormalMatrixGenerator(
                connectivity=connectivity,
                scale=bias_scaling,
                apply_spectral_radius=False
            )
            indexcity = np.arange(3, 3+ self.ncity, 1).tolist()
            y = pd.Series(dtype=float) 
            yhat = pd.Series(dtype=float) 
            rhoi = list()
            for city_i in indexcity:
                indexcityindex = indexcity[:] 
                indexcityindex.pop(city_i-3)
                if tau>=0:
                    train_tensor = CreateDataset(self.data.iloc[tau:self.nT,indexcityindex], self.label.iloc[:(self.nT-tau),indexcityindex])
                    test_tensor = CreateDataset(self.data.iloc[tau:self.nT:,city_i], self.label.iloc[:(self.nT-tau),city_i])
                else:
                    train_tensor = CreateDataset(self.data.iloc[:self.nT+tau,indexcityindex], self.label.iloc[(-tau):self.nT,indexcityindex])
                    test_tensor = CreateDataset(self.data.iloc[:self.nT+tau,city_i], self.label.iloc[(-tau):self.nT,city_i])
                # end ifelse
                # Data loader
                trainloader = DataLoader(train_tensor, batch_size=self.batch_size, shuffle=False, num_workers=2)
                testloader = DataLoader(test_tensor, batch_size=self.batch_size, shuffle=False, num_workers=2)
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
                npy_predicted = pd.Series(y_predicted.numpy().reshape(y_predicted.size()[1]))
                npy = pd.Series(test_y.numpy().reshape(test_y.size()[1]))
                y = y.append(npy)
                yhat = yhat.append(npy_predicted)
                rhoi.append(npy.corr(npy_predicted))
            # end for
            yhat = yhat.reset_index(drop=True)
            y = y.reset_index(drop=True)
            rho.append(sum(rhoi)/len(rhoi))
            rhofull.append(y.corr(yhat))
            taurecord.append(tau)
        # end for
        return taurecord, rhofull
    # end get_rho

# end scantau




