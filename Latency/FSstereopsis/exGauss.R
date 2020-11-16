# Title     : TODO
# Objective : TODO
# Created by: senrisuo
# Created on: 2020/11/15

# Created by @kanri_ninjin
# http://noucobi.com/neuro/RTanalysis/S3.html

rexGauss <- function(n, mu=300, sigma=50, tau=50) {
	return(rnorm(n, mu, sigma) + rexp(n, 1/tau))
}

dexGauss <- function(x, mu=300, sigma=50, tau=50, log=FALSE, limit=10^-4) {
	if (tau < limit) {
		d <- dnorm(x, mean=mu, sd=sigma, log=TRUE)
	} else {
		d <- log(1 / tau) - (x - mu) / tau + (sigma^2) / (2 * (tau^2)) +
			pnorm((x - mu) / sigma - sigma / tau, 0, 1, log.p=TRUE)
	}
	if (!log) {
		d <- exp(d)
	}
	return(d)
}

fitexGauss <- function(
	rt, # A vector of RTs to analyze distribution
	type = c("ml", "chisq", "ks"), # Fitting procedure
	probs = seq(0.1, 0.9, by=0.2), # Quantiles for chisq procedure
	warn = FALSE, # Whether to print warnings in optim
	maxit = 1000, # Maximal number of iteration in optim
	qmax = 100, # Maximal number of quantiles in Q-Q plot
	... # Additional arguments for optim
) {
	type <- type[1]
	rt <- rt[!is.na(rt)]
	nrt <- length(rt)
	arg <- list(...)
	arg$control$maxit <- maxit

	# 1st - 3rd cumulants
	k <- rep(NA, 3)
	k[1] <- mean(rt)
	k[2] <- var(rt)
	k[3] <- mean((rt - k[1])^3)

	# Estimates of ex-Gaussian parameters by moment statistics
	momfit <- c(mu=NA, sigma=NA, tau=NA)
	momfit[3] <- ifelse(k[3] > 0, (k[3]/2)^(1/3), 10^(-5))
	momfit[1] <- k[1] - momfit[3]
	v <- k[2] - momfit[3]^2
	momfit[2] <- ifelse(v > 0, sqrt(v), 10^(-5))

	# Definition of function to minimize in optim
	if (type == "ml") {
		# Sign-inverted sum of prob density (in log scale)
		fn <- function(THETA) {
			THETA <- exp(THETA)
			return(-sum(dexGauss(rt, THETA[1], THETA[2], THETA[3], TRUE)))
		}
	} else if (type == "chisq") {
		# Chi-squared statistics in contingency table
		n_lev <- 1:(length(probs) + 1)
		n_pred <- diff(c(0, probs, 1)) * nrt
		fn <- function(THETA) {
			THETA <- exp(THETA)
			q <- c(-Inf, qexGauss(probs, THETA[1], THETA[2], THETA[3]), Inf)
			n_obs <- table(cut(rt, breaks=q, labels=n_lev))
			return(sum(((n_obs - n_pred) ^ 2) / n_pred))
		}
	} else if (type == "ks") {
		# Y-axis discrepancy in cumulative prob dist
		rt_sorted <- sort(rt)
		p <- seq(1 / nrt, by=1 / nrt, length.out=nrt)
		fn <- function(THETA) {
			THETA <- exp(THETA)
			return(max(abs(pexGauss(rt_sorted, THETA[1], THETA[2], THETA[3]) - p)))
		}
	}
	optold <- options(warn=ifelse(warn, 0, -1))
	init <- momfit
	init <- log(init)
	loglikefit <- optim(par=init, fn=fn, arg)
	param <- loglikefit$par
	param <- exp(param)
	options(optold)

	if (FALSE) { # To check the failure in moment estimates
		if (any(momfit == 10^(-5))) {
			cat("fitexGauss: Moment estimate for sigma or tau became 0\n")
			print(matrix(c(momfit, param), byrow=TRUE, nrow=2,
				dimnames=list(c("mom","loglike"), c("mu","sigma","tau"))))
		}
	}

	rtnvalue <- list(mom=momfit, loglike=param,
		optimrslt=loglikefit[-1], convergence=loglikefit$convergence, rt=rt)
	class(rtnvalue) <- "htest.exGauss"

	# Calculate r^2 in Q-Q plot
	if (nrt > (qmax+1)) {
		# qmax-tiles should be separated by qmax+1 points
		# Require qmax+3 seps to eliminate lower and upper limits
		#   which are -Inf and Inf in the theoretical CDF
		obs <- quantile(rt, probs=seq(0, 1, length.out=qmax+3))
	} else {
		obs <- sort(rt)
	}
	l <- length(obs)
	pred <- qexGauss(seq(0, 1, length.out=l),
		param["mu"], param["sigma"], param["tau"])
	obs <- obs[-c(1, l)]
	pred <- pred[-c(1, l)]

	rtnvalue$lmrslt <- lm(obs~pred)
	s <- summary(rtnvalue$lmrslt)
	rtnvalue$r.squared <- s$r.squared

	# Kolmogorov-Smirnov Test
	rtnvalue$ksrslt <- suppressWarnings(ks.test(rt, pexGauss,
		mu=param["mu"], sigma=param["sigma"], tau=param["tau"]))

	return(rtnvalue)
}

pexGauss <- function(q, mu=300, sigma=50, tau=50,
	lower.tail=TRUE, log.p=FALSE) {
	mayinf <- -(q-mu)/tau
	if (any(is.infinite(exp(mayinf)))) {
		# Practically equal to Gaussian distribution
		#   when tau is very small
		p <- pnorm((q-mu)/sigma)
	} else {
		p <- pnorm((q-mu)/sigma) - exp(mayinf +
			(sigma^2)/(2*(tau^2)) + log(pnorm((q-mu)/sigma - sigma/tau)))
	}
	if (!lower.tail) {
		p <- 1 - p
	}
	if (log.p) {
		p <- log(p)
	}
	return(p)
}

qexGauss <- function(p, mu=300, sigma=50, tau=50,
	lower.tail=TRUE, log.p=FALSE) {
	if (log.p) {
		p <- exp(p)
	}
	if (!lower.tail) {
		p <- 1 - p
	}
	if (any(c(p < 0, p > 1))) {
		stop("qexGauss: <p> must be from 0 to 1")
	}
	pfunc <- function(x) {
		pexGauss(x, mu=mu, sigma=sigma, tau=tau)
	}
	m <- mu + tau
	bin <- ifelse(p > pfunc(m), 1, -1) * mean(c(sigma, tau))
	q <- mapply(FUN=function(p_tmp, b_tmp) {
		if (p_tmp == 0) {
			return(-Inf)
		} else if (p_tmp == 1) {
			return(Inf)
		}
		cri <- m
		while (sign(pfunc(cri) - p_tmp) == sign(pfunc(cri + b_tmp) - p_tmp)) {
			cri <- cri + b_tmp
		}
		func <- function(r){ pfunc(r) - p_tmp }
		return(uniroot(func, sort(c(cri, cri + b_tmp)))$root)
	}, p, bin)
	return(q)
}

plot.htest.exGauss <- function(
	x,
	y = NA,
	main = "",
	xlab = "Time [ms]",
	ylab = ifelse(freq, "Frequency", "Density"),
	border = "black",
	ylim = NULL,
	freq = TRUE,
	breaks = 30,
	plot = TRUE,
	...
) {
	rt  <- x$rt
	fit <- x$loglike

	# Draw histogram
	histrslt <- hist(rt, breaks=breaks, plot=plot,
		border=border,
		main=main, xlab=xlab, ylab=ylab,
		ylim=ylim, freq=freq)

	if (x$optimrslt$convergence == 0) {
		# Density function with fitted exGauss parameters
		# xlim <- range(histrslt$breaks)
		xlim <- par("usr")[1:2]
		x <- seq(xlim[1], xlim[2], length.out=100)
		y <- dexGauss(x, fit["mu"], fit["sigma"], fit["tau"])

		if (!freq) { # Density plot
			h <- 1
		} else { # Frequency plot
			h <- max(histrslt$counts) / max(histrslt$density)
		}
		lines(x, y * h, ...)
	}

	invisible()
}

