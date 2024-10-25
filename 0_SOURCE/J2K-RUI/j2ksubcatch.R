
# TO COMMENT
# create subcatchmentS ==============================================
# given ids (vector), reaches (vector) and waterheds (list) and ...
# create a list of 'j2ksubcatch' objects
# The additional elements that can be provided in .dots. seem unecessary 
# as they will make the 'as.j2ksubcatch' function return an error.
j2kCreateSubcatchments = function(ids, reaches, watersheds, ...){
	# Test length of the mandatory arguments
	n = length(ids)
	if (n != length(reaches) | n != length(watersheds)) stop("'ids', 'reaches' and 'watersheds' arguments must have the same length.")
	if (!is.atomic(ids) | !is.atomic(reaches)) stop("'ids' and 'reaches' must be numeric vectors.")
	if (!is.list(watersheds)) stop("'watersheds' must be a list of numeric vectors.")
	# Test whether the .dots. arguments are named or not, and if they are named, test length
	args = list(...)
	if (length(args)>0){
		na = names(args)
		if (is.null(na)){
			warning("Optional argument provided in .dots. must be named! Unnamed arguments will be ignored.")
			args = NULL
		}else if (length(which(na==""))>0){
			warning("Optional argument provided in .dots. must be named! Unnamed arguments will be ignored.")
			args = args[-which(na=="")]
		}
	}else{args=NULL}
	if (!is.null(args)){
		la = unique(unlist(lapply(args, length)))
		if (length(la)!=1) stop("Optional argument provided in .dots. must have the same length and match the length of 'ids', 'reaches' and 'watersheds'.")
		if (la != n) stop("Optional argument provided in .dots. must have the same length than 'ids', 'reaches' and 'watersheds'.")
		# get the type of the various optional arguments
		ta = c()
		for (k in 1:length(args)){
			types = c(is.atomic(args[[k]]), is.list(args[[k]]))
			if (sum(types)!=1) stop("Optional argument '", names(args)[k], "' must either be a vector or a list.")
			ta[k] = which(types) # gives 1 for a vector, 2 for a list
		}
	}
	# if all these test were successful
	subcatchs = list()
	for (k in 1:n) {
		subcatch = list("id"=ids[k], "reach"=reaches[k], "watersheds"=watersheds[[k]])
		if (!is.null(args)){
			for (i in 1:length(args)){
				if (ta[i]==1) subcatch[[names(args)[i]]] = args[[i]][k]
				if (ta[i]==2) subcatch[[names(args)[i]]] = args[[i]][[k]]
			}
		}
		
		subcatchs[[k]] = subcatch
	}
	subcatchs = as.j2ksubcatch(subcatchs)
	return(subcatchs)
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

as.j2ksubcatch <- function(object)
{
	if (is.null(object)) {
		ans = list()
	} else {
		n = length(object)
		objnames = names(object)
		elemnames = c("id", "reach", "watersheds")
		# check lengths of elements in 'object'
		objelemlength = unique(unlist(lapply(object, function(e) length(e))))
		if (length(objelemlength) > 1L)
			stop("All elements in 'object' must have the same length of 3 for 'id', 'reach' and 'watersheds'.")
		if (objelemlength != 3)
			stop("All elements in 'object' must have a length of 3 for 'id', 'reach' and 'watersheds'.")
		# check names, send a warning if not all of them are named the same way
		objelemnames = unique(lapply(object, function(e) names(e)))
		if (length(objelemnames) > 1L)
			stop("All elements in 'object' must have the same names: c(\"id\", \"reach\", \"watersheds\")")
		objelemnames = objelemnames[[1]]
		if (!identical(objelemnames, elemnames))
			stop("All elements in 'object' must have the same names: c(\"id\", \"reach\", \"watersheds\")")
		# check content: 'id' and 'reach' must both be single values, 'watersheds' must be atomic
		lengthid = sum(unlist(lapply(object, function(e) length(e[["id"]])!=1)))
		if (lengthid != 0L) stop("Sub-elements 'id' must all have a length of 1")
		lengthreach = sum(unlist(lapply(object, function(e) length(e[["reach"]])!=1)))
		if (lengthreach != 0L) stop("Sub-elements 'reach' must all have a length of 1")
		atomicwatersheds = sum(unlist(lapply(object, function(e) !is.atomic(e[["watersheds"]]))))
		if (atomicwatersheds != 0L) stop("Sub-elements 'watersheds' must all be atomic vectors.")
		# check for duplicates in 'id'
		ids = unlist(lapply(object, function(e) e[["id"]]))
		dupids = duplicated(ids)
		if (sum(dupids) != 0L) stop("Duplicated id(s) found: ", paste(ids[dupids], collapse=", ")) # could only send a warning and remove these duplicated elements?
		# now, the 'j2ksubcatch' can be created
		ans = object
	}
	class(ans) = append(class(ans), "j2ksubcatch")
	return(ans)
}

print.j2ksubcatch = function(object, nwatersheds=6L){
	.nc = function(x, n=2L) {
		sapply(x, function(e) {
			if (nchar(e) < n) paste0(paste(rep(" ", n - nchar(e)), collapse=""), e)
			else e
		})
	}
	n = length(object)
	rn = names(object)
	if (n != 0) {
		i = object[, "id"]
		r = object[, "reach"]
		w = object[, "watersheds"]
		nw = unlist(lapply(w, length))
		nh = floor(nwatersheds/3)
		wtxt = unlist(lapply(w, function(e) {
			if (length(e) > nwatersheds) paste0(paste(.nc(e[1:nh]), collapse=", "), ", ..., ", paste(.nc(e[(length(e)-nh+1):length(e)]), collapse=", "))
			else paste(.nc(e), collapse=", ")
		}))
		ans = data.frame("id"=i, "reach"=r, "watersheds"=wtxt)
		rownames(ans) = rn
		print(ans, right=FALSE)
		
	} else {
		cat("<no sub-catchments>\n")
	}
}


`[.j2ksubcatch` = function(object, i, j)
{
	elemnames = c("id", "reach", "watersheds")
	errormessage = "Incorrect index: either [i, j] or [j] is supported.\nSupported values for j are either 1:3 or c('id', 'reach', 'watersheds')"
	mclass = class(object)
	class(object) = NULL
	if (length(object) == 0L) return(NULL)
	if (missing(i) & missing(j)) return(object)
	if (missing(j)) { # no column specified ==> a sub j2ksubcatch is returned
		ans = object[i]
		if (sum(unlist(lapply(ans, is.null)))!=0) {
		  stop("Subscript out of bound")
		}
		class(ans) = mclass
	} else { # column(s) specified, returns either matrix or list
		if (is.numeric(j)) {
			j = as.integer(j)
			s = unique(sign(j))
			if (length(s)!=1L) stop("negative index cannot be mixed with positive index")
			if (s < 0L) {
				j = unique(j)
				j = which(is.na(match(1:3, -j)))
				if (length(j) == 0L) return(NULL)
			}
		}
		if (is.character(j)) j = match(j, elemnames)
		if (sum(j<1 | j>3, na.rm=TRUE) != 0L) stop(errormessage)
		if (sum(is.na(j)) != 0L) stop(errormessage)
		ids = unlist(lapply(object, function(e, i) {e[[i]] }, i=1))
		if (length(j) > 1L) {
			if (3%in%j) {
				warning("Extraction of watersheds is ignored. They cannot be extracted along with reach and/or id.")
				m3 = match(j, 3)
				j = j[which(is.na(m3))]
				if (length(j) == 0L) return(NULL)
			}
			ans = sapply(j, function(i, o) { unlist(lapply(o, function(e, i) e[[i]], i = i)) }, o=object)
			if (is.matrix(ans)) {
				colnames(ans) = elemnames[j]
				rownames(ans) = ids
			}
		} else {
			if (j == 3) {
				ans = lapply(object, function(e, i) {e[[i]] }, i=3)
				names(ans) = ids
			} else {
				ans = sapply(j, function(i, o) { unlist(lapply(o, function(e, i) e[[i]], i = i)) }, o=object)
				if (is.matrix(ans)) {
					colnames(ans) = elemnames[j]
					rownames(ans) = ids
				}
			}
		}
		if (!missing(i)) {
			i = as.integer(i)
			s = unique(sign(i))
			if (length(s)!=1L) stop("negative index cannot be mixed with positive index")
			if (s < 0L) i = -i
			if (is.matrix(ans)) {
				if (s < 0L) ans = ans[-i, ]
				if (s > 0L) ans = ans[i, ]
			} else {
				if (s < 0L) ans = ans[-i]
				if (s > 0L) ans = ans[i]
			}
		}
		if (is.matrix(ans)) {
			if (ncol(ans) == 1L) {
				rn = rownames(ans)
				ans = as.vector(ans)
				names(ans)=rn
			}
		}
	}
	return(ans)
}

`[<-.j2ksubcatch` = function(object, i, j, value)
{
	error_ii = "Incorrect index: either [i, j] or [j] is supported.\nSupported values for j are either 1:3 or c('id', 'reach', 'watersheds')"
	error_rl = "The number of objects to be replaced doesn't match the number of replacement objects"
	memclass = class(object)
	class(object) = NULL
	if (missing(i) & missing(j)) stop(errormessage)
	if (missing(j)) { # behave as a list
		if (length(object[i]) != length(value)) stop(error_rl)
		object[i] = value
	} else {
		if (length(j) > 1L) stop("Only one value for j is supported: 'id', 'reach' or 'watersheds' (1, 2 or 3).")
		if (missing(i)) {
			n = length(object)
			if (n != length(value)) stop(error_rl)
			i = 1:n
		}
		if (j == "watersheds" | j == 3)	for (k in i) object[[k]][[j]] = value[[k]]
		else                            for (k in i) object[[k]][[j]] = value[k]
	}
	object = as.j2ksubcatch(object)
	return(object)
}


`+.j2ksubcatch` = function(e1, e2){
	e2 = as.j2ksubcatch(e2)
	# check for duplicates to avoid errors
	id1 = e1[,"id"]
	id2 = e2[,"id"]
	m12 = match(id2, id1)
	d = which(!is.na(m12))
	if (length(d) != 0L) {
		warning("Duplicated id(s) found and not added: ", paste(m12[d], collapse=", "))
		e2 = e2[-d]
		if (length(e2) == 0L) return(e1)
	}
	# --
	mclass = class(e1)
	class(e1) = NULL
	class(e2) = NULL
	eout = e1
	n = length(eout)
	for (k in 1:length(e2)) eout[[n+k]] = e2[[k]]
	eout = as.j2ksubcatch(eout)
	return(eout)
}


# object = list(
 # list(id=1, reach=1001, watersheds=1:3)
# ,list(id=2, reach=1002, watersheds=2:4)
# ,list(id=3, reach=1003, watersheds=3:4)
# ,list(id=4, reach=1004, watersheds=3:10)
# ,list(id=5, reach=1002, watersheds=2:4)
# )
# object2 = list(
# list(id=6, reach=1003, watersheds=3:4)
# ,list(id=7, reach=1004, watersheds=3:10)
# ,list(id=1, reach=1004, watersheds=3:10)
# ,list(id=8, reach=1003, watersheds=3:4)
# ,list(id=9, reach=1004, watersheds=3:10)
# )

# object = as.j2ksubcatch(object)
# object2 = as.j2ksubcatch(object2)
# object
# object2

# object + object2
# object + object

# object[1, "id"]=9

# object[1:5, "id"]=c(21:25) 
# object
# object[1, "watersheds"]=list(1:length(object)) # should be ok
# object
