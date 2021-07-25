import streamlit as st
import pandas as pd
import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import Flatten
from keras.layers import Dropout
import keras as keras
# import plotly.express as px
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import acf,pacf
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.arima_model import ARIMA
import statsmodels.api as sm
from keras.preprocessing.sequence import TimeseriesGenerator
from sklearn.preprocessing import MinMaxScaler
from keras.layers import Bidirectional
import warnings                                  # `do not disturbe` mode
warnings.filterwarnings ('ignore')


st.title('Time Series Forecasting Using LSTM')
# @st.cache
rad=st.sidebar.radio('Time series forecast',['Data ahead Forecasting','Observed vs Predicted','Mulite step forecast','Grid search','MAPE based choosing best Model'])

st.write("IMPORT DATA")
data_load_state = st.text('Loading data...')
data=st.file_uploader('Upload here',type=['CSV','xlsx','sav'])
if data is not None:
    data=pd.read_csv(data)
data_load_state.text("Done! (using st.cache)")
data['StartYear']=st.sidebar.selectbox('Start Year:', data['Year'])
data['EndYear']=st.sidebar.selectbox('End Year:', data['Year'])
mask = (data['Year'] > data['StartYear']) & (data['Year'] <= data['EndYear'])
data=data.loc[mask]
data['Year'] = pd.to_datetime(data['Year'])
Make_choice=st.sidebar.selectbox('Select your columns:', data.columns)
df=data[Make_choice].tolist()

if rad=='Data ahead Forecasting':
    scaler=MinMaxScaler(feature_range=(0,1))
    df1=scaler.fit_transform(np.array(df).reshape(-1,1))
    train=df1
    X_train, y_train = np.array(train), np.array(train)
    X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))
    periods_input = st.number_input('Number of input:',min_value = 1, max_value = 365)
    n_input = periods_input
    n_features = 1
    regressor = Sequential()
    regressor.add(Bidirectional(LSTM(units = 100, return_sequences = True,activation='tanh' ,recurrent_activation='sigmoid',input_shape = (X_train.shape[1], 1))))
    regressor.add(Dropout(0.02))
    regressor.add(LSTM(units = 15, return_sequences = True))
    regressor.add(Dropout(0.02))
    regressor.add(LSTM(units = 25, return_sequences = True))
    regressor.add(Dropout(0.02))
    regressor.add(LSTM(units = 35))
    regressor.add(Dropout(0.02))
    regressor.add(Dense(units = 1,activation='linear'))
    optimizer = keras.optimizers.Adam(learning_rate=0.001)
    regressor.compile(optimizer = optimizer, loss = 'mean_squared_error')
    regressor.fit(X_train, y_train, epochs = 50, batch_size =len(train),shuffle=False)
    pd.set_option('display.float_format', '{:.2f}'.format)
    train_predicted=regressor.predict(X_train,verbose=0)
    train_predicted1=scaler.inverse_transform(train_predicted)
    df_Model_fitted=pd.DataFrame(train_predicted1)
    pred_list = []
    batch = train[-n_input:].reshape((1, n_input, n_features))
    for i in range(n_input):
        pred_list.append(regressor.predict(batch)[0])
        batch = np.append(batch[:,1:,:],[[pred_list[i]]],axis=1)
    pred_list1=scaler.inverse_transform(pred_list)
    df_forecast=pd.DataFrame(pred_list1)
    df_predicted=df_Model_fitted.append(df_forecast)
    path=st.text_input('Enter data freq:')
    df_predicted['Year']=pd.date_range(start=min(data['Year']), periods=len(df_predicted), freq=path)
    df_Observed=data[Make_choice].tolist()
    df_Observed=pd.DataFrame(df_Observed)
    n_input=periods_input
    NA_toObservedData=np.repeat(np.nan,n_input)
    NA_toObservedData=pd.DataFrame(NA_toObservedData)
    df_Observed=df_Observed.append(NA_toObservedData)
    df_Observed['Year']=pd.date_range(start=min(data['Year']), periods=len(df_predicted), freq=path)
    Final_df1=pd.merge(df_Observed,df_predicted,on='Year')
    Final_df1.columns=['Observed','Year','Predicted']
    LSTM_Model_FinalDF=Final_df1.reindex(columns=['Year','Observed','Predicted'])

    st.write(LSTM_Model_FinalDF)
    plt.plot(data['Year'],data[Make_choice])
    st.pyplot()
if rad=='Observed vs Predicted':
    import numpy as numpy
    import numpy as np
    def create_dataset(dataset, look_back=1):
        dataX, dataY = [], []
        for i in range(len(dataset)-look_back-1):
            a = dataset[i:(i+look_back), 0]
            dataX.append(a)
            dataY.append(dataset[i + look_back, 0])
        return numpy.array(dataX), numpy.array(dataY)

    numpy.random.seed(7)
    scaler = MinMaxScaler(feature_range=(0, 1))
    dataset = scaler.fit_transform(np.array(df).reshape(-1,1))
    periods_input = st.number_input('Traing time for the model:',min_value = 1, max_value = 100)
    periods_input=periods_input/100
    train_size = int(len(dataset)*periods_input)
    test_size = len(dataset) - train_size
    train, test = dataset[0:train_size,:], dataset[train_size:len(dataset),:]
    look_back = st.number_input('Look back:',min_value = 1, max_value = 12)
    trainX, trainY = create_dataset(train, look_back)
    # st.write(len(trainX),len(trainY))
    testX, testY = create_dataset(test, look_back)
    # # reshape input to be [samples, time steps, features]
    trainX = numpy.reshape(trainX, (trainX.shape[0], 1, trainX.shape[1]))
    #
    testX = numpy.reshape(testX, (testX.shape[0], 1, testX.shape[1]))
    model = Sequential()
    model = Sequential()
    model.add(LSTM(50, input_shape=(1, look_back)))
    model.add(Dense(1))
    model.compile(loss='mean_squared_error', optimizer='adam')
    model.fit(trainX, trainY, epochs=100, batch_size=1, verbose=2)
    trainPredict = model.predict(trainX)
    #
    testPredict = model.predict(testX)
# invert predictions
    trainPredict = scaler.inverse_transform(trainPredict)
    trainY = scaler.inverse_transform([trainY])
    testPredict = scaler.inverse_transform(testPredict)
    testY = scaler.inverse_transform([testY])

    #

    import numpy as np

    def mean_absolute_percentage_error(y_true, y_pred):
        y_true, y_pred = np.array(y_true), np.array(y_pred)
        return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

    # calculate root mean squared error
    y_true=trainY
    y_pred=trainPredict
    trainScore = mean_absolute_percentage_error(y_true, y_pred)
    print('Train Score: %.2f RMSE' % (trainScore))
    testScore = mean_absolute_percentage_error(testY, testPredict)
    print('Test Score: %.2f RMSE' % (testScore))
    # shift train predictions for plotting
    trainPredictPlot = numpy.empty_like(dataset)
    trainPredictPlot[:, :] = numpy.nan
    trainPredictPlot[look_back:len(trainPredict)+look_back, :] = trainPredict
    # shift test predictions for plotting
    testPredictPlot = numpy.empty_like(dataset)
    testPredictPlot[:, :] = numpy.nan
    testPredictPlot[len(trainPredict)+(look_back*2)+1:len(dataset)-1, :] = testPredict
    # plot baseline and predictions
    plt.plot(scaler.inverse_transform(dataset))
    plt.plot(trainPredictPlot)
    plt.plot(testPredictPlot)
    st.pyplot()
if rad=='Mulite step forecast':
    ##
    from pandas import DataFrame
    from pandas import Series
    from pandas import concat
    from pandas import read_csv
    from pandas import datetime
    from sklearn.metrics import mean_squared_error
    from sklearn.preprocessing import MinMaxScaler
    from keras.models import Sequential
    from keras.layers import Dense
    from keras.layers import LSTM
    from math import sqrt
    from matplotlib import pyplot
    from numpy import array

    # date-time parsing function for loading the dataset
    def parser(x):
    	return datetime.strptime('190'+x, '%Y-%m')

    # convert time series into supervised learning problem
    def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
    	n_vars = 1 if type(data) is list else data.shape[1]
    	df = DataFrame(data)
    	cols, names = list(), list()
    	# input sequence (t-n, ... t-1)
    	for i in range(n_in, 0, -1):
    		cols.append(df.shift(i))
    		names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
    	# forecast sequence (t, t+1, ... t+n)
    	for i in range(0, n_out):
    		cols.append(df.shift(-i))
    		if i == 0:
    			names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
    		else:
    			names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
    	# put it all together
    	agg = concat(cols, axis=1)
    	agg.columns = names
    	# drop rows with NaN values
    	if dropnan:
    		agg.dropna(inplace=True)
    	return agg

    # create a differenced series
    def difference(dataset, interval=1):
    	diff = list()
    	for i in range(interval, len(dataset)):
    		value = dataset[i] - dataset[i - interval]
    		diff.append(value)
    	return Series(diff)

    # transform series into train and test sets for supervised learning
    def prepare_data(series, n_test, n_lag, n_seq):
    	# extract raw values
    	raw_values = series
    	# transform data to be stationary
    	diff_series = difference(raw_values, 1)
    	diff_values = diff_series.values
    	diff_values = diff_values.reshape(len(diff_values), 1)
    	# rescale values to -1, 1
    	scaler = MinMaxScaler(feature_range=(-1, 1))
    	scaled_values = scaler.fit_transform(diff_values)
    	scaled_values = scaled_values.reshape(len(scaled_values), 1)
    	# transform into supervised learning problem X, y
    	supervised = series_to_supervised(scaled_values, n_lag, n_seq)
    	supervised_values = supervised.values
    	# split into train and test sets
    	train, test = supervised_values[0:-n_test], supervised_values[-n_test:]
    	return scaler, train, test

    # fit an LSTM network to training data
    def fit_lstm(train, n_lag, n_seq, n_batch, nb_epoch, n_neurons):
    	# reshape training into [samples, timesteps, features]
    	X, y = train[:, 0:n_lag], train[:, n_lag:]
    	X = X.reshape(X.shape[0], 1, X.shape[1])
    	# design network
    	model = Sequential()
    	model.add(LSTM(n_neurons, batch_input_shape=(n_batch, X.shape[1], X.shape[2]), stateful=True))
    	model.add(Dense(y.shape[1]))
    	model.compile(loss='mean_squared_error', optimizer='adam')
    	# fit network
    	for i in range(nb_epoch):
    		model.fit(X, y, epochs=1, batch_size=n_batch, verbose=0, shuffle=False)
    		model.reset_states()
    	return model

    # make one forecast with an LSTM,
    def forecast_lstm(model, X, n_batch):
    	# reshape input pattern to [samples, timesteps, features]
    	X = X.reshape(1, 1, len(X))
    	# make forecast
    	forecast = model.predict(X, batch_size=n_batch)
    	# convert to array
    	return [x for x in forecast[0, :]]

    # evaluate the persistence model
    def make_forecasts(model, n_batch, train, test, n_lag, n_seq):
    	forecasts = list()
    	for i in range(len(test)):
    		X, y = test[i, 0:n_lag], test[i, n_lag:]
    		# make forecast
    		forecast = forecast_lstm(model, X, n_batch)
    		# store the forecast
    		forecasts.append(forecast)
    	return forecasts

    # invert differenced forecast
    def inverse_difference(last_ob, forecast):
    	# invert first forecast
    	inverted = list()
    	inverted.append(forecast[0] + last_ob)
    	# propagate difference forecast using inverted first value
    	for i in range(1, len(forecast)):
    		inverted.append(forecast[i] + inverted[i-1])
    	return inverted

    # inverse data transform on forecasts
    def inverse_transform(series, forecasts, scaler, n_test):
    	inverted = list()
    	for i in range(len(forecasts)):
    		# create array from forecast
    		forecast = array(forecasts[i])
    		forecast = forecast.reshape(1, len(forecast))
    		# invert scaling
    		inv_scale = scaler.inverse_transform(forecast)
    		inv_scale = inv_scale[0, :]
    		# invert differencing
    		index = len(series) - n_test + i - 1
    		last_ob = series
    		inv_diff = inverse_difference(last_ob, inv_scale)
    		# store
    		inverted.append(inv_diff)
    	return inverted
    import numpy as np

    # MAPE
    def mean_absolute_percentage_error(y_true, y_pred):
        y_true, y_pred = np.array(y_true), np.array(y_pred)
        return np.mean(np.abs((y_true - y_pred) / y_true)) * 100


    # evaluate the RMSE for each forecast time step
    def evaluate_forecasts(test, forecasts, n_lag, n_seq):
    	for i in range(n_seq):
    		actual = [row[i] for row in test]
    		predicted = [forecast[i] for forecast in forecasts]
    		MAPE = mean_absolute_percentage_error(actual, predicted)
    		print('t+%d MAPE: %f' % ((i+1), MAPE))

    # plot the forecasts in the context of the original dataset
    def plot_forecasts(series, forecasts, n_test):
    	# plot the entire dataset in blue
    	pyplot.plot(series)
    	# plot the forecasts in red
    	for i in range(len(forecasts)):
    		off_s = len(series) - n_test + i - 1
    		off_e = off_s + len(forecasts[i]) + 1
    		xaxis = [x for x in range(off_s, off_e)]
    		yaxis = [series[off_s]] + forecasts[i]
    		pyplot.plot(xaxis, yaxis, color='red')
    	# show the plot
    	st.pyplot()
    if __name__=='__main__':
        n_lag = st.number_input('Number of LAG:',min_value = 1, max_value = 365)
        n_seq = st.number_input('Number of sequence:',min_value = 1, max_value = 365)
        n_test = st.number_input('Number of Test cases:',min_value = 1, max_value = 365)
        n_epochs = st.number_input('Number of Epochs:',min_value = 1, max_value = 2500)
        n_batch = st.number_input('Number of batch:',min_value = 1, max_value = 1)
        n_neurons = st.number_input('Number of Neurons:',min_value = 1, max_value = 10)
        # prepare data
        series=df
        scaler, train, test = prepare_data(series, n_test, n_lag, n_seq)
        # fit model
        model = fit_lstm(train, n_lag, n_seq, n_batch, n_epochs, n_neurons)
        # make forecasts
        forecasts = make_forecasts(model, n_batch, train, test, n_lag, n_seq)
        # inverse transform forecasts and test
        forecasts = inverse_transform(series, forecasts, scaler, n_test+2)
        actual = [row[n_lag:] for row in test]
        actual = inverse_transform(series, actual, scaler, n_test+2)
        # evaluate forecasts
        evaluate_forecasts(actual, forecasts, n_lag, n_seq)
        # plot forecasts
        plot_forecasts(series, forecasts, n_test+2)
if rad=='Grid search':
    from math import sqrt
    from numpy import array
    from numpy import mean
    from pandas import DataFrame
    from pandas import concat
    from pandas import read_csv
    from sklearn.metrics import mean_squared_error
    from keras.models import Sequential
    from keras.layers import Dense
    from keras.layers import LSTM
    import numpy as np
    # split a univariate dataset into train/test sets
    def train_test_split(data, n_test):
    	return data[:-n_test], data[-n_test:]

    # transform list into supervised learning format
    def series_to_supervised(data, n_in=1, n_out=1):
    	df = DataFrame(data)
    	cols = list()
    	# input sequence (t-n, ... t-1)
    	for i in range(n_in, 0, -1):
    		cols.append(df.shift(i))
    	# forecast sequence (t, t+1, ... t+n)
    	for i in range(0, n_out):
    		cols.append(df.shift(-i))
    	# put it all together
    	agg = concat(cols, axis=1)
    	# drop rows with NaN values
    	agg.dropna(inplace=True)
    	return agg.values

    # root mean squared error or rmse
    # def measure_rmse(actual, predicted):
    # 	return sqrt(mean_squared_error(actual, predicted))

    # difference dataset
    def difference(data, order):
    	return [data[i] - data[i - order] for i in range(order, len(data))]

    # fit a model
    def model_fit(train, config):
    	# unpack config
    	n_input, n_nodes, n_epochs, n_batch, n_diff = config
    	# prepare data
    	if n_diff > 0:
    		train = difference(train, n_diff)
    	# transform series into supervised format
    	data = series_to_supervised(train, n_in=n_input)
    	# separate inputs and outputs
    	train_x, train_y = data[:, :-1], data[:, -1]
    	# reshape input data into [samples, timesteps, features]
    	n_features = 1
    	train_x = train_x.reshape((train_x.shape[0], train_x.shape[1], n_features))
    	# define model
    	model = Sequential()
    	model.add(LSTM(n_nodes, activation='relu', input_shape=(n_input, n_features)))
    	model.add(Dense(n_nodes, activation='relu'))
    	model.add(Dense(1))
    	model.compile(loss='mse', optimizer='adam')
    	# fit model
    	model.fit(train_x, train_y, epochs=n_epochs, batch_size=n_batch, verbose=0)
    	return model

    # forecast with the fit model
    def model_predict(model, history, config):
    	# unpack config
    	n_input, _, _, _, n_diff = config
    	# prepare data
    	correction = 0.0
    	if n_diff > 0:
    		correction = history[-n_diff]
    		history = difference(history, n_diff)
    	# reshape sample into [samples, timesteps, features]
    	x_input = array(history[-n_input:]).reshape((1, n_input, 1))
    	# forecast
    	yhat = model.predict(x_input, verbose=0)
    	return correction + yhat[0]

    def MAPE(test,predictions):
        mape = np.mean(np.abs((test - predictions)/test))*100
        return mape
    # walk-forward validation for univariate data
    def walk_forward_validation(data, n_test, cfg):
    	predictions = list()
    	# split dataset
    	train, test = train_test_split(data, n_test)
    	# fit model
    	model = model_fit(train, cfg)
    	# seed history with training dataset
    	history = [x for x in train]
    	# step over each time-step in the test set
    	for i in range(len(test)):
    		# fit model and make forecast for history
    		yhat = model_predict(model, history, cfg)
    		# store forecast in list of predictions
    		predictions.append(yhat)
    		# add actual observation to history for the next loop
    		history.append(test[i])
    	# estimate prediction error
    	error = MAPE(test, predictions)
    	print(' > %.3f' % error)
    	return error

    # score a model, return None on failure
    def repeat_evaluate(data, config, n_test, n_repeats=10):
    	# convert config to a key
    	key = str(config)
    	# fit and evaluate the model n times
    	scores = [walk_forward_validation(data, n_test, config) for _ in range(n_repeats)]
    	# summarize score
    	result = mean(scores)
    	print('> Model[%s] %.3f' % (key, result))
    	return (key, result)

    # grid search configs
    def grid_search(data, cfg_list, n_test):
    	# evaluate configs
    	scores = [repeat_evaluate(data, cfg, n_test) for cfg in cfg_list]
    	# sort configs by error, asc
    	scores.sort(key=lambda tup: tup[1])
    	return scores

    # create a list of configs to try
    def model_configs():
    	# define scope of configs
    	n_input = [12]
    	n_nodes = [500]
    	n_epochs = [150]
    	n_batch = [1, 150]
    	n_diff = [12]
    	# create configs
    	configs = list()
    	for i in n_input:
    		for j in n_nodes:
    			for k in n_epochs:
    				for l in n_batch:
    					for m in n_diff:
    						cfg = [i, j, k, l, m]
    						configs.append(cfg)
    	print('Total configs: %d' % len(configs))
    	return configs
    # if__name__=='__main__':

    # define dataset
    # series = df
    data = df
    # data split
    n_test = st.number_input('Enter the Number of test',min_value=1,max_value=365)
    # model configs
    cfg_list = model_configs()
    # grid search
    scores = grid_search(data, cfg_list, n_test)
    st.write(scores)
    # list top 10 configs
    # for cfg, error in scores[:3]:
    # 	st.write(cfg, error)
if rad=='MAPE based choosing best Model':
     st.write('WIP')
