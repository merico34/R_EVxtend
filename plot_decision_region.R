#plot 2D decision boundary
# plot_decision_region <- function(x,y,model,nb_points_by_axe) {
plot_decision_region <- function(data, outcome_var, nb_points_by_axe, model, ...) {
        # nb_points_by_axe = 100
        
        # if ( is.integer(outcome_var) | is.numeric(outcome_var) ) 
        #         outcome_var<-colnames(data)[outcome_var]
        
        # equation = as.character(attr(model,"predvars"))
        
        # predictors = model$final$xNames
        predictors = model$coefnames
        predictors = intersect(colnames(data),predictors) #keep simple vars
        
        df <- NULL
        df <- data.frame(index=1:nb_points_by_axe) #initialisation en nb de rows
        for (var in predictors) {
                df_tmp <- NULL
                # creer une rangée de valeurs possibles pour chaque predictor
                # assign(paste0(var,'_min'), trunc(min(data[,var]))-1)
                # assign(paste0(var,'_max'), trunc(max(data[,var]))+1)
                assign(paste0(var,'_seq'), seq(
                        from=trunc(min(data[,var]))-1
                        ,to=trunc(max(data[,var]))+1
                        ,length.out = nb_points_by_axe
                        ))
                df_tmp <- data.frame(get(paste0(var,'_seq')))
                colnames(df_tmp)[1]<- var
                df <- cbind(df,df_tmp)
        }
        df <- df[,-1]
        # x1d_min = trunc(min(x[,1]))-1
        # x1d_max = trunc(max(x[,1]))+1
        # x2d_min = trunc(min(x[,2]))-1
        # x2d_max = trunc(max(x[,2]))+1
        # x1_seq=seq(from=x1d_min, to=x1d_max, length.out = nb_points_by_axe)
        # x2_seq=seq(from=x2d_min, to=x2d_max, length.out = nb_points_by_axe)
        
        x_grid = expand.grid(df)
        
        probs <- tryCatch({
                preds <- predict(model, newdata=x_grid
                                 , type="prob"
                )
                matrix(preds[,1], nb_points_by_axe, nb_points_by_axe)
        }, warning = function(e) {
                preds <- predict(model, newdata=x_grid
                                 # , type="prob"
                )
                message("probs generation warning")
                return(matrix(preds, nb_points_by_axe, nb_points_by_axe))
        })
        
        # preds <- predict(model, newdata=x_grid
        #                  # , type="prob"
        #                  )
        # probs <- matrix(preds[,1], nb_points_by_axe, nb_points_by_axe)
        
        par(mar=rep(2,4),pty="s")
        plot(data[,1],data[,2], type = "n")  # setting up coord. system
        # contour(x1_seq, x2_seq, probs, levels=0.5, labels="", xlab="", ylab=""
        #         , axes=FALSE)
        x1_seq = unique(x_grid[,1])
        x2_seq = unique(x_grid[,2])
        contour(x1_seq, x2_seq, probs, levels=0.5, labels="", xlab="", ylab=""
                , axes=FALSE)
        # if (length(model$finalModel$y) == 0)
        #         outc <- model$finalModel$learn$y else
        #                 outc <- model$finalModel$y
        
        outc = data[,which(colnames(data)==outcome_var)]

        points(data[,1],data[,2], col=ifelse(outc==1, "coral", "cornflowerblue"))
        points(x_grid, pch=".", cex=1, col=ifelse(probs<0.5, "coral", "cornflowerblue"))
        box()
}