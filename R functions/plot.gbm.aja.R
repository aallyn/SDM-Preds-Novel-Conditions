plot.gbm.aja<- function (x, i.var = 1, i.use, n.sims = 100, n.trees = x$n.trees, continuous.resolution = 100, return.grid = FALSE, type = c("link", "response"), level.plot = TRUE, contour = FALSE, number = 4, overlap = 0.1, col.regions = viridis::viridis, ...) 


{
    if (FALSE) {
        x = mod_fit
        i.var = "oisst_daily"
        i.use = pred_dat_temp$oisst_daily
        n.sims = 100
        return.grid = TRUE
        type = "response"
        n.trees = x$n.trees
        continuous.resolution = 500
    }
    type <- match.arg(type)
    if (all(is.character(i.var))) {
        i <- match(i.var, x$var.names)
        if (any(is.na(i))) {
            stop(
                "Requested variables not found in ", deparse(substitute(x)),
                ": ", i.var[is.na(i)]
            )
        } else {
            i.var <- i
        }
    }
    
    if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
        warning("i.var must be between 1 and ", length(x$var.names))
    }
    if (n.trees > x$n.trees) {
        warning(paste("n.trees exceeds the number of tree(s) in the model: ", 
            x$n.trees, ". Using ", x$n.trees, " tree(s) instead.", 
            sep = ""))
        n.trees <- x$n.trees
    }
    if (length(i.var) > 3) {
        warning("plot.gbm() will only create up to (and including) 3-way ", 
            "interaction plots.\nBeyond that, plot.gbm() will only return ", 
            "the plotting data structure.")
        return.grid <- TRUE
    }
    grid.levels <- vector("list", length(i.var))
    for (i in 1:length(i.var)) {
        if (is.numeric(x$var.levels[[i.var[i]]])) {
            if(is.null(i.use)){
                grid.levels[[i]] <- seq(from = min(x$var.levels[[i.var[i]]]), 
                to = max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
            } else {
                grid.levels[[i]] <- seq(from = min(i.use), 
                to = max(i.use), length = continuous.resolution)
            }
        }
        else {
            grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]], 
                levels = x$var.levels[[i.var[i]]])) - 1
        }
    }
    X <- expand.grid(grid.levels)
    names(X) <- paste("X", 1:length(i.var), sep = "")
    if (is.null(x$num.classes)) {
        x$num.classes <- 1
    }
    names(X)[1:length(i.var)] <- x$var.names[i.var]

    if(n.sims > 1){
        res_col <- ncol(X)+1
        
        for (i in 1:n.sims) {
            y_temp <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)), cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes), i.var = as.integer(i.var - 1), n.trees = as.integer(n.trees), initF = as.double(x$initF), trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type), PACKAGE = "gbm")
            X[, res_col] <- 1 / (1 + exp(-y_temp))

            res_col <- res_col + 1
        }
        
        # Get mean and SD
        X_out <- X %>%
        rowwise() %>%
        mutate(.,
            "Mean" = mean(c_across(V2:V101)),
            "SD" = sd(c_across(V2:V101))
        ) %>% 
        select(., oisst_daily, Mean, SD)
        
        # Return it
        return(X_out)
    } else {
        y_temp <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)), cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes), i.var = as.integer(i.var - 1), n.trees = as.integer(n.trees), initF = as.double(x$initF), trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type), PACKAGE = "gbm")
        X$y <- 1 / (1 + exp(-y_temp))
        return(X)
    }
}




