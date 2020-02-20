#### 5.2.2 **Transformation** - 2nd Linear Regression Model


```{r Transformation - log(y), echo=TRUE, message=FALSE, warning=FALSE, fig.align='center'}

airbnb_model_2 <- lm(log(price) ~ room_type + neighbourhood_group + latitude + longitude + number_of_reviews + availability_365 
                     + reviews_per_month + calculated_host_listings_count + minimum_nights, data = airbnb_train)



summary_model_2 <- summary(airbnb_model_2)
mse_2 <- summary_model_2$sigma^2
r_sq_2 <- summary_model_2$r.squared
adj_r_sq_2 <- summary_model_2$adj.r.squared

summary_model_2

cat("Model 2 Observations: \n")
cat("MSE:" , mse_2 , "\n")
cat("R-squared:", r_sq_2, "\n")
cat("Adjusted R-squared:", adj_r_sq_2, "\n")

```

Model 2 Observations: 
  
  * MSE: 0.1020295 
* R-squared: 0.4969679 
* Adjusted R-squared: 0.4966717 

****
  
  Plot of the Transformed Linear Regression Model
```{r Plot the Log Model, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center'}
plot(airbnb_model_2)
```

