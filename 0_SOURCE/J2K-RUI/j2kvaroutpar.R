
#FIX ME: I should be much less restrictive:
# If there are three elements:
# check theirs names, if they don't match, suppose the right order and send a warning.
# if there are no names, suppose the right order with a warning
# If there are less than three element: check their names
as.j2koutvar <- function(x){
	if (!is.list(x)) stop("x must be a list that can be coerced to a 'j2koutvar' object.")
	mandatoryelements = c("misc", "liter", "reach")
	nx = names(x)
	mnx = match(mandatoryelements, nx)
	namnx = which(is.na(mnx))
	if (length(namnx)>0L){
		if (length(namnx) == length(mandatoryelements)){
			stop("Elements ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " are missing.")
		} else if (length(namnx) == 1L) {
			warning("Element ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " is missing.")
		} else {
			warning("Elements ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " are missing.")
		}
		y = x[mnx[-namnx]]
	} else {
		y = x[mnx]
	}
	class(y) = append(class(y), "j2koutvar")
	return(y)
}

summary.j2koutvar = function(x){
	ans = unlist(lapply(x, length))
	class(ans) = "summary.j2koutvar"
	return(ans)
}

print.summary.j2koutvar = function(x){
	cat("Miscellaneous variables: ", x[["misc"]], "\n")
	cat("Liter variables:         ", x[["liter"]], "\n")
	cat("Reach variables:         ", x[["reach"]], "\n")
}

print.j2koutvar = function(x){
	novartxt = "<no variables>\n"
	cat("Miscellaneous variables: \n")
	if (!is.null(x[["misc"]])) print(x[["misc"]]) else cat(novartxt)
	cat("Liter variables: \n")
	if (!is.null(x[["liter"]])) print(x[["liter"]]) else cat(novartxt)
	cat("Reach variables: \n")
	if (!is.null(x[["reach"]])) print(x[["reach"]]) else cat(novartxt)
}


`+.j2koutvar` = function(e1, e2){
	ov = as.j2koutvar(e1)
	nov = as.j2koutvar(e2)
	class(ov) = NULL
	class(nov) = NULL
	m = c(ov[['misc']], nov[['misc']])
	l = c(ov[['liter']], nov[['liter']])
	r = c(ov[['reach']], nov[['reach']])
	misc = unique(m)
	liter = unique(l)
	reach = unique(r)
	if (!identical(misc, m)) warning("Duplicate(s) in 'misc' removed.")
	if (!identical(liter, l)) warning("Duplicate(s) in 'liter' removed.")
	if (!identical(reach, r)) warning("Duplicate(s) in 'reach' removed.")
	ov = list("misc"=misc, "liter"=liter, "reach"=reach)
	class(ov) = class(e1)
	return(ov)
}

# ===================================================================
# sub-catchment output parameter object 

as.j2koutpar <- function(x){
	if (!is.list(x)) stop("x must be a list that can coerced to a 'j2koutpar' object.")
	mandatoryelements = c("misc", "liter")
	nx = names(x)
	mnx = match(mandatoryelements, nx)
	namnx = which(is.na(mnx))
	if (length(namnx)>0L){
		if (length(namnx) == length(mandatoryelements)){
			stop("Elements ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " are missing.")
		} else if (length(namnx) == 1L) {
			warning("Element ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " is missing.")
		} else {
			warning("Elements ", paste(paste0("'", mandatoryelements[namnx], "'"), collapse=", "), " are missing.")
		}
		y = x[mnx[-namnx]]
	} else {
		y = x[mnx]
	}
	class(y) = append(class(y), "j2koutpar")
	return(y)
}


summary.j2koutpar = function(x){
	ans = unlist(lapply(x, length))
	class(ans) = "summary.j2koutpar"
	return(ans)
}

print.summary.j2koutpar = function(x){
	cat("Miscellaneous parameters: ", x[["misc"]], "\n")
	cat("Liter parameters:         ", x[["liter"]], "\n")
}


print.j2koutpar = function(x){
	novartxt = "<no parameters>\n"
	cat("Miscellaneous parameters: \n")
	if (!is.null(x[["misc"]])) print(x[["misc"]]) else cat(novartxt)
	cat("Liter parameters: \n")
	if (!is.null(x[["liter"]])) print(x[["liter"]]) else cat(novartxt)
}


`+.j2koutpar` = function(e1, e2){
	ov = as.j2koutpar(e1)
	nov = as.j2koutpar(e2)
	class(ov) = NULL
	class(nov) = NULL
	m = c(ov[['misc']], nov[['misc']])
	l = c(ov[['liter']], nov[['liter']])
	misc = unique(m)
	liter = unique(l)
	if (!identical(misc, m)) warning("Duplicate(s) in 'misc' removed.")
	if (!identical(liter, l)) warning("Duplicate(s) in 'liter' removed.")
	ov = list("misc"=misc, "liter"=liter)
	class(ov) = class(e1)
	return(ov)
}


# outvariables = list( "liter"=c(paste("liter", 1:10, sep = "_")), "misc"=c("misc"),"reach"=c("reach"))

# x = as.j2koutvar(outvariables)
# summary(x)

# y = list("liter"=paste("liter", 10:15, sep = "_"), "misc"=paste("misc", 11:15, sep = "_"),"reach"=paste("reach", 11:15, sep = "_"))

# y = as.j2koutvar(y)


# y+x


# x
# x








