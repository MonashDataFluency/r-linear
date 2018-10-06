
line <- function(x1,y1,x2,y2) lines(c(x1,x2),c(y1,y2))

red <- "#ee0000"

btext <- function(x,y,...) {
    a <- 0.02
    for(i in -2:2)
        for(j in -2:2)
            text(x+a*i,y+a*j,col="white",...)
    text(x,y,...)
}

begin <- function() {
    par(mar=c(0,0,0,0))
    plot(0,type='n',axes=FALSE,ann=FALSE, 
         xlim=c(-1,6), ylim=c(-1,6), asp=1)

    arrows(-1,0,5,0)
    text(5,0, adj=c(-0.5,0.5), "y[1]")
    arrows(0,-1,0,5)
    text(0,5, adj=c(0.5,-0.5), "y[2]")
}

the_truth <- function() {
    points(3.5,3.5, pch=21,bg="black")
    btext(3.5,3.5, adj=c(1,0.5), "The unknown truth  ")
}

one_exp <- function() {
    points(3,5, pch=21,bg="white")
}

many_exp <- function() {
    s <- 0.6
    n <- 500
    set.seed(123)
    points(c(rnorm(n,mean=3.5,sd=s), 3), c(rnorm(n,mean=3.5,sd=s), 5), pch=21,bg="white")
    btext(3.5, 1.5, "Possible experiment outcomes")
}


to_predict <- function() {
    line(-1,-1, 5,5)
    text(0,0, srt=45, adj=c(0.2,0), "Subspace of possible predictions")

    arrows(4,4, 3,5, length=0.1)
    line(3.8,3.8, 3.6,4.0)
    line(3.6,4.0, 3.8,4.2)
    text(3.5, 4.5, adj=c(0.5,-1.5),srt=-45, expression(hat(epsilon)))
    text(3.5,4.5, adj=c(0.5,-0.5),cex=0.75,srt=-45, "residuals(fit)")

    points(3,5, pch=21,bg="white")
    text(3,5, adj=c(1.5,-0.5), expression(y))
    points(4,4, pch=21,bg="grey")
    text(4,4, adj=c(0,1.25), expression(Chi * hat(beta)))
    text(4,4, adj=c(0,3.75), " predict(fit)")
}

to_test <- function() {
    arrows(0,0, 3,5, length=0.1, col=red)
    text(1.5, 2.5, adj=c(0.5, -0.5),cex=0.75,srt=60,col=red, "residuals(fit0)")
    text(1.5, 2.5, adj=c(0.5, -1.5),srt=60,col=red, expression(hat(epsilon)[0]))

    points(0,0, pch=21,bg="black")
    text(0,0, adj=c(-0,1.1), " H0 restricted\n  to this point")

    line(-1,-1, 5,5)
    text(2,2, srt=45, adj=c(0.5,0), "H1 can predict anything on this line")

    arrows(4,4, 3,5, length=0.1, col=red)
    line(3.8,3.8, 3.6,4.0)
    line(3.6,4.0, 3.8,4.2)
    text(3.5, 4.5, adj=c(0.5,-1.5),srt=-45,col=red, expression(hat(epsilon)[1]))
    text(3.5,4.5, adj=c(0.5,-0.5),cex=0.75,srt=-45,col=red, "residuals(fit1)")

    points(3,5, pch=21,bg="white")
    text(3,5, adj=c(1.5,-0.5), expression(y))
    points(4,4, pch=21,bg="grey")
    #text(4,4, adj=c(0,1.25), expression(Chi * hat(beta)[1]))
    #text(4,4, adj=c(0,3.75), " predict(fit1)")
}




