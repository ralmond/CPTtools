states <- c("Novice","Intermediate","Advanced")

Language_modal <- list()
Language_exp <- list()

Language_modal$Reading <- matrix(
    c(0.229, 0.025, 0.000, 0.025, 0.445, 0.029, 0.000, 0.040, 0.207),
    3,3, TRUE, list(Actual=states,MAP=states))
Language_modal$Writing <- matrix(
    c(0.163, 0.097, 0.000, 0.053, 0.388, 0.065, 0.000, 0.051, 0.183),
    3,3, TRUE, list(Actual=states,MAP=states))
Language_modal$Speaking <- matrix(
    c(0.243, 0.027, 0.000, 0.044, 0.390, 0.030, 0.000, 0.033, 0.233),
    3,3,TRUE, list(Actual=states,MAP=states))
Language_modal$Listening <- matrix(
    c(0.242, 0.054, 0.000, 0.054, 0.290, 0.059, 0.000, 0.087, 0.214),
    3,3, TRUE, list(Actual=states,MAP=states))


Language_exp$Reading <- matrix(
    c(0.220, 0.034, 0.000, 0.037, 0.413, 0.050, 0.000, 0.049, 0.198),
    3,3, TRUE, list(Actual=states,MAP=states))
Language_exp$Writing <- matrix(
    c(0.162, 0.092, 0.007, 0.091, 0.331, 0.084, 0.003, 0.076, 0.154),
    3,3, TRUE, list(Actual=states,MAP=states))
Language_exp$Speaking <- matrix(
    rep(NA,9),
    3,3, TRUE, list(Actual=states,MAP=states))
Language_exp$Listening <- matrix(
    rep(NA,9),
    3,3, TRUE, list(Actual=states,MAP=states))


