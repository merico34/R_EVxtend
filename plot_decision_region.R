#plot 2D decision boundary
plot_decision_region <- function(x,y,model,nb_points_by_axe) {
        nb_points_by_axe = 100
        x1d_min = trunc(min(x[,1]))-1
        x1d_max = trunc(max(x[,1]))+1
        x2d_min = trunc(min(x[,2]))-1
        x2d_max = trunc(max(x[,2]))+1
        x1_seq=seq(from=x1d_min, to=x1d_max, length.out = nb_points_by_axe)
        x2_seq=seq(from=x2d_min, to=x2d_max, length.out = nb_points_by_axe)
        
        x_grid = expand.grid(x1=x1_seq
                             ,x2=x2_seq
        )
        mod15 <- predict(model, newdata=x_grid, type="prob")
        prob15 <- matrix(mod15, nb_points_by_axe, nb_points_by_axe)
        par(mar=rep(2,4),pty="s")
        plot(x, type = "n")  # setting up coord. system
        # plot(-5:5,-5:5, type = "n")
        contour(x1_seq, x2_seq, prob15, levels=0.5, labels="", xlab="", ylab=""
                , axes=FALSE)
        points(x, col=ifelse(y==1, "coral", "cornflowerblue"))
        points(x_grid, pch=".", cex=1, col=ifelse(prob15<0.5, "coral", "cornflowerblue"))
        box()
}