simulate.lme.data<-function (object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), 
                             m2, method = c("REML", "ML"), niterEM = c(40, 200), useGen, 
                             ...) 
{
  if (inherits(nsim, "lm") || inherits(nsim, "lme")) 
    stop("order of arguments in 'simulate.lme' has changed to conform with generic in R-2.2.0", 
         domain = NA)
  getResults1 <- function(conLin, nIter, pdClass, REML, ssq, 
                          p, pp1) {
    unlist(.C(nlme:::mixed_combined, as.double(conLin$Xy), as.integer(unlist(conLin$dims)), 
              double(ssq), as.integer(nIter), as.integer(pdClass), 
              as.integer(REML), logLik = double(1), R0 = double(pp1), 
              lRSS = double(1), info = integer(1), sigma = as.double(conLin$sigma))[c("info", 
                                                                                      "logLik")])
  }
  getResults2 <- function(conLin, reSt, REML, control) {
    lmeSt <- lmeStruct(reStruct = reStruct(reSt, REML = REML))
    attr(lmeSt, "conLin") <- conLin
    lmeSt <- Initialize(lmeSt, data = NULL, groups = NULL, 
                        control = control)
    attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
    aMs <- nlminb(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
                                                            lmePars), control = list(iter.max = control$msMaxIter, 
                                                                                     eval.max = control$msMaxEval, trace = control$msVerbose))
    c(info = aMs$flags[1], logLik = -aMs$value)
  }
  if (!exists(".Random.seed", envir = .GlobalEnv)) 
    runif(1)
  RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
  set.seed(seed)
  if (inherits(object, "lme")) {
    fit1 <- object
    object <- as.list(object$call[-1])
  }
  else {
    object <- as.list(match.call(lme, substitute(object))[-1])
    fit1 <- do.call(lme, object)
  }
  if (length(fit1$modelStruct) > 1) 
    stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
  reSt1 <- fit1$modelStruct$reStruct
  condL1 <- do.call(createConLin, object)
  pdClass1 <- vapply(reSt1, data.class, "")
  pdClass1 <- match(pdClass1, c("pdSymm", "pdDiag", "pdIdent", 
                                "pdCompSymm", "pdLogChol"), 0) - 1
  control1 <- lmeControl()
  if (!is.null(object$control)) {
    control1[names(object$control)] <- object$control
  }
  control1$niterEM <- niterEM[1]
  sig <- fit1$sigma
  DeltaInv <- pdMatrix(reSt1, factor = TRUE)
  for (i in names(DeltaInv)) {
    DeltaInv[[i]] <- sig * DeltaInv[[i]]
  }
  if (missing(useGen)) {
    useGen <- any(pdClass1 == -1)
  }
  nullD <- condL1$dims
  N <- nullD$N
  Q <- nullD$Q
  p1 <- nullD$ncol[Q + 1]
  pp11 <- p1 * (p1 + 1)
  ycol1 <- sum(nullD$ncol)
  qvec <- nullD$qvec[1:Q]
  ssq1 <- sum(qvec^2)
  csq1 <- cumsum(c(1, qvec[-Q]))
  csq2 <- cumsum(qvec)
  ngrp <- nullD$ngrps
  base <- condL1$Xy[, ycol1 - (nullD$ncol[Q + 1]:1), drop = FALSE] %*% 
    fixef(fit1)
  ind <- lapply(1:Q, function(i) rep(1:ngrp[i], nullD$ZXlen[[i]]))
  if (ML <- !is.na(match("ML", method))) 
    nML <- array(0, c(nsim, 2), list(1:nsim, c("info", "logLik")))
  if (REML <- !is.na(match("REML", method))) 
    nREML <- array(0, c(nsim, 2), list(1:nsim, c("info", 
                                                 "logLik")))
  if ((ALT <- !missing(m2))) {
    if (inherits(m2, "lme")) {
      fit2 <- m2
      m2 <- as.list(m2$call[-1])
    }
    else {
      m2 <- as.list(match.call(lme, substitute(m2))[-1])
      if (is.null(m2$random)) {
        m2$random <- asOneSidedFormula(object$fixed[-2])
      }
      aux <- object
      aux[names(m2)] <- m2
      m2 <- aux
      fit2 <- do.call(lme, m2)
    }
    if (length(fit2$modelStruct) > 1) {
      stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
    }
    condL2 <- do.call(createConLin, m2)
    reSt2 <- fit2$modelStruct$reStruct
    control2 <- lmeControl()
    if (!is.null(m2$control)) {
      control2[names(m2$control)] <- m2$control
    }
    control2$niterEM <- niterEM[2]
    pdClass2 <- vapply(fit2$modelStruct$reStruct, data.class, 
                       "")
    pdClass2 <- match(pdClass2, c("pdSymm", "pdDiag", "pdIdent", 
                                  "pdCompSymm", "pdLogChol"), 0) - 1
    useGen <- useGen || any(pdClass2 == -1)
    altD <- condL2$dims
    ssq2 <- sum((altD$qvec[1:altD$Q])^2)
    p2 <- altD$ncol[altD$Q + 1]
    pp12 <- p2 * (p2 + 1)
    ycol2 <- sum(altD$ncol)
    if (ML) 
      aML <- nML
    if (REML) 
      aREML <- nREML
  }
  ian_data_save<-NULL #store outputs
  for (i in 1:nsim) {
    base2 <- base + rnorm(N, sd = sig)
    for (j in 1:Q) {
      base2 <- base2 + ((array(rnorm(ngrp[j] * qvec[j]), 
                               c(ngrp[j], qvec[j]), list(1:ngrp[j], NULL)) %*% 
                           DeltaInv[[j]])[ind[[j]], , drop = FALSE] * condL1$Xy[, 
                                                                                csq1[j]:csq2[j], drop = FALSE]) %*% rep(1, qvec[j])
    }
    condL1$Xy[, ycol1] <- base2
    if (REML) {
      nREML[i, ] <- if (useGen) 
        getResults2(condL1, reSt1, TRUE, control1)
      else getResults1(condL1, niterEM[1], pdClass1, TRUE, 
                       ssq1, p1, pp11)
    }
    if (ML) {
      nML[i, ] <- if (useGen) 
        getResults2(condL1, reSt1, FALSE, control1)
      else getResults1(condL1, niterEM[1], pdClass1, FALSE, 
                       ssq1, p1, pp11)
    }
    if (ALT) {
      condL2$Xy[, ycol2] <- base2
      if (REML) {
        aREML[i, ] <- if (useGen) 
          getResults2(condL2, reSt2, TRUE, control2)
        else getResults1(condL2, niterEM[2], pdClass2, 
                         TRUE, ssq2, p2, pp12)
      }
      if (ML) {
        aML[i, ] <- if (useGen) 
          getResults2(condL2, reSt2, FALSE, control2)
        else getResults1(condL2, niterEM[2], pdClass2, 
                         FALSE, ssq2, p2, pp12)
      }
    }
  
  ian_data_save<-cbind(ian_data_save,base2)  
  }
  v.null <- v.alt <- list()
  if (ML) {
    nML[, "logLik"] <- nML[, "logLik"] + N * (log(N) - (1 + 
                                                          log(2 * pi)))/2
    v.null$ML <- nML
    if (ALT) {
      aML[, "logLik"] <- aML[, "logLik"] + N * (log(N) - 
                                                  (1 + log(2 * pi)))/2
      v.alt$ML <- aML
    }
  }
  if (REML) {
    nREML[, "logLik"] <- nREML[, "logLik"] + (N - p1) * 
      (log(N - p1) - (1 + log(2 * pi)))/2
    v.null$REML <- nREML
    if (ALT) {
      aREML[, "logLik"] <- aREML[, "logLik"] + (N - p2) * 
        (log(N - p2) - (1 + log(2 * pi)))/2
      v.alt$REML <- aREML
    }
  }
  df <- p1 + length(coef(reSt1)) + 1
  if (ALT) 
    df <- abs(df - (p2 + length(coef(reSt2)) + 1))
  structure(if (ALT && (ML || REML)) 
    list(null = v.null, alt = v.alt,data=ian_data_save)
    else list(null = v.null,data=ian_data_save), class = "simulate.lme", call = match.call(), 
    seed = seed, df = df, useGen = useGen)
  #return(base2)
}


#

createConLin <-
  function(fixed, data = sys.frame(sys.parent()),
           random = pdSymm(eval(as.call(fixed[-2]))), ...)
  {
    if(!inherits(fixed, "formula") || length(fixed) != 3)
      stop("\nfixed-effects model must be a formula of the form \"resp ~ pred\"")
    REML <- FALSE
    reSt <- reStruct(random, REML = REML, data = NULL)
    groups <- getGroupsFormula(reSt)
    if(is.null(groups)) {
      if(inherits(data, "groupedData")) {
        groups <- getGroupsFormula(data)
        groupsL <- rev(getGroupsFormula(data,
                                        asList = TRUE))
        Q <- length(groupsL)
        if(length(reSt) != Q) {		# may need to repeat reSt
          if(length(reSt) != 1) {
            stop("incompatible lengths for 'random' and grouping factors")
          }
          auxForm <-
            eval(parse(text = paste("~", deparse(formula(random)[[2]]), "|",
                                    deparse(groups[[2]]))))
          reSt <- reStruct(auxForm, REML = REML, data = NULL)
        }
        else {
          names(reSt) <- names(groupsL)
        }
      }
      else {
        stop("'data' must inherit from \"groupedData\" class if 'random' does not define groups")
      }
    }
    ## create an lme structure containing the random effects model
    lmeSt <- lmeStruct(reStruct = reSt)
    ## extract a data frame with enough information to evaluate
    ## fixed, groups, reStruct, corStruct, and varStruct
    dataMix <-
      model.frame(formula = asOneFormula(formula(lmeSt), fixed, groups),
                  data = data, drop.unused.levels = TRUE)
    origOrder <- row.names(dataMix)	# preserve the original order
    ## sort the model.frame by groups and get the matrices and parameters
    ## used in the estimation procedures
    grps <- getGroups(dataMix, eval(parse(text = paste("~1",
                                                       deparse(groups[[2]]), sep = "|"))))
    ## ordering data by groups
    if(inherits(grps, "factor")) {	# single level
      ##"order" treats a single named argument peculiarly so must split this off
      ord <- order(grps)
      grps <- data.frame(grps)
      row.names(grps) <- origOrder
      names(grps) <- as.character(deparse((groups[[2]])))
    }
    else {
      ord <- do.call(order, grps)
      ## making group levels unique
      for(i in 2:ncol(grps)) {
        grps[, i] <-
          as.factor(paste(as.character(grps[, i - 1]),
                          as.character(grps[, i    ]), sep = "/"))
      }
    }
    grps <- grps[ord,  , drop = FALSE]
    dataMix <- dataMix[ord,  , drop = FALSE]
    ##    revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order
    ## obtaining basic model matrices
    N <- nrow(grps)
    Z <- model.matrix(reSt, dataMix)
    ncols <- attr(Z, "ncols")
    Names(lmeSt$reStruct) <- attr(Z, "nams")
    ## keeping the contrasts for later use in predict
    contr <- attr(Z, "contr")
    X <- model.frame(fixed, dataMix)
    auxContr <- lapply(X, function(el)
      if(inherits(el, "factor")) contrasts(el))
    contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
    contr <- contr[!vapply(contr, is.null, NA)]
    X <- model.matrix(fixed, X)
    y <- eval(fixed[[2]], dataMix)
    ncols <- c(ncols, dim(X)[2], 1)
    ## Q <- ncol(grps)
    ## creating the condensed linear model :
    list(Xy = array(c(Z, X, y), c(N, sum(ncols)),
                    list(row.names(dataMix),
                         c(colnames(Z), colnames(X), deparse(fixed[[2]])))),
         dims = nlme:::MEdims(grps, ncols), logLik = 0,
         sigma = 0) # <- no "fixed Sigma" yet
  }

