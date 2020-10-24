trainingtarget <- as.numeric(point.df[, (vv+1)])-1
testtarget <- as.numeric(point.dft[, (vv+1)])-1

traininLabels<-to_categorical(trainingtarget)
testLabels<-to_categorical(testtarget)

#### Hyperparameter flag
FLAGS <- flags(
  flag_numeric('dropout_1', 0.2, 'First dropout'),
  flag_numeric('dropout_2', 0.2, 'Second dropout'),
  flag_numeric('dropout_3', 0.1, 'Third dropout'),
  flag_numeric('dropout_4', 0.1, 'Forth dropout'),
  flag_numeric('dropout_5', 0.1, 'Fifth dropout'),
  flag_numeric('dropout_6', 0.1, 'sixth dropout')
  
  
)

### Define model parameters with 4 hidden layers with 200 neuron
model <- keras_model_sequential()
model %>% 
  # Imput layer
  layer_dense(units = 1000, activation = 'sigmoid',input_shape = c(vv)) %>% 
  layer_dropout(rate = FLAGS$dropout_1,seed = 1) %>% 
  # Hidden layers
  layer_dense(units =1000, activation = 'sigmoid') %>%
  layer_dropout(rate = FLAGS$dropout_2,seed = 1) %>%
  layer_dense(units = 1000, activation = 'sigmoid') %>%
  layer_dropout(rate = FLAGS$dropout_3,seed = 1) %>%
  layer_dense(units =1000, activation = 'sigmoid') %>%
  layer_dropout(rate = FLAGS$dropout_4) %>%
  layer_dense(units = 1000, activation = 'sigmoid') %>%
  layer_dropout(rate = FLAGS$dropout_5) %>%
  layer_dense(units = 1000, activation = 'sigmoid') %>%
  layer_dropout(rate = FLAGS$dropout_6) %>%
  
  # Output layer
  layer_dense(units = 6, activation = 'softmax')
summary(model)

#### Define an optimizer (Stochastic gradient descent optimizer)
#optimizer <- optimizer_sgd(lr=0.01, decay=1e-6, momentum=0.9)

#### Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

####  Fit the model to the data 
history<-model %>% fit(
  training, traininLabels, 
  epochs = 100, 
  batch_size = 100, 
  shuffle = TRUE,
  validation_split = 0.2
)

### Plot history

#plot(history)

#### Evaluate the model
score <- model %>% evaluate(test, testLabels, batch_size = 100)
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

#### Prediction & confusion matrix - test data
class.test <- model %>%
  predict_classes(test, batch_size = 100)
CF_ML_DNN<-table(testtarget,class.test)
CF_ML_DNN

detach(package:keras, unload=TRUE)
detach(package:tfruns, unload=TRUE)
detach(package:tfestimators, unload=TRUE)
