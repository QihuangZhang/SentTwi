import torch
import numpy as np
import pandas as pd
import echotorch.utils.matrix_generation as mg
from torch.autograd import Variable
import echotorch.utils
from torch.utils.data.dataloader import DataLoader
import echotorch.nn.reservoir as etrs
from echotorch.datasets.CreateDataset import CreateDataset
from itertools import product

class crossvalid:
    """
    Configure the optimal choice parameter combinations
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
            self, data, label, param_ranges
        ):
            self.data = data
            self.label = label
            self.parameter_population = (
                dict(zip(param_ranges.keys(), values)) for values in product(*param_ranges.values())
                )
            self.ncity = len(data.columns)-3
            self.nT = len(data)

            self.batch_size = 2

            # Generate results
            self.outputs = self.cross_validate()
     # end __init__
       

    def cross_validate(self):
        """
        Execute the cross_validate
        :return:
        """

        use_cuda = False
        batch_size = 1

        CViNRMSEmin = 99999

        for r, param_individual in enumerate(self.parameter_population):
            print(r)
            ### Get parameter values
            spectral_radius = param_individual.get("spectral_radius")
            leaky_rate = param_individual.get("leaky_rate")
            input_dim = 1
            reservoir_size = param_individual.get("reservoir_size")
            connectivity = param_individual.get("connectivity")
            ridge_param = param_individual.get("ridge_param")
            input_scaling = param_individual.get("input_scaling")
            bias_scaling = param_individual.get("bias_scaling")
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
            cityMSE = list()
            cityNRMSE = list()
            for city_i in indexcity:
                indexcityindex = indexcity[:] 
                indexcityindex.pop(city_i-3)
                train_tensor = CreateDataset(self.data.iloc[:,indexcityindex], self.label.iloc[:,indexcityindex])
                test_tensor = CreateDataset(self.data.iloc[:,city_i], self.label.iloc[:,city_i])
                # Data loader
                trainloader = DataLoader(train_tensor, batch_size=batch_size, shuffle=False, num_workers=1)
                testloader = DataLoader(test_tensor, batch_size=batch_size, shuffle=False, num_workers=1)
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
                # For each batch
                for data in trainloader:
                    # Inputs and outputs
                    inputs, targets = data
                    # Transform data to Variables
                    inputs, targets = Variable(inputs), Variable(targets)
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

        return param_best
    # end cross_validate

# end scantau



