# Model: Create a decision tree model to predict if customer signing a term deposit.

model <- function(input_data,
                  model_type="decision_tree"){
  
  stopifnot(model_type %in% c("decision_tree", "logistic_regression"))
  
  if(model_type=="decision_tree") model_res <- rpart(y ~ ., data = input_data)
  if(model_type=="logistic_regression") model_res <- glm(y ~.,family=binomial(link='logit'),data=input_data)
  model_res
}

# EXAMPLE:
# dt_model<- model(bmarketing, model_type="decision_tree")
# summary(dt_model)


# Model-Plot: 
# We shall implement a function to present a nice representation of the model, 
# e.g. for a decision tree we should plot the tree and respective nodes.

plot_model <- function(model_res){
  rpart.plot(model_res,roundint=FALSE)  
}

# EXAMPLE:
# plot_model(dt_model)
# summary(dt_model)


# Model-Predict: 
# The model prediction function shall return the actual predicted classes from the model.

predict_model <- function(model_res, 
                          input_data){
  predict(model_res, input_data, type = "class")
}

# EXAMPLE:
# predictions <- predict_model(dt_model, bmarketing)

# Model-Performance: 
# The model performance function should calculate the accuracy (or precision) of the model.

test_model_performance <- function(input_model, 
                                   pred){
  ## Compute the accuracy
  a <- mean(pred$y == input_model)
  
  # Lets look at the confusion matrix
  b <- table(input_model, pred$y)  
  
  list(acc=a, conf_mat =b)
}

# EXAMPLE:
# test_model_performance(predictions, 
#                         bmarketing)


