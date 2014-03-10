# Create snow socket clusters using SLURM (srun) instead of SSH
# Andrew Raim, Aaron Knister

.logger <- function(msg, ..., writeme = TRUE)
{
	if (writeme)
	{
		dt <- as.character(Sys.time());
		cat(dt,"-", sprintf(msg, ...))
	}
}

# A version of the newSOCKnode function that launches workers using
# the "srun" command. The standard SOCKET method that comes with snow
# uses SSH to launch workers, which is not allowed on our cluster.
slurm.newSOCKnode <- function(rank, ..., options = defaultClusterOptions)
{
	debug <- getClusterOption("debug", options)
	options <- addClusterOptions(options, list(...))

	outfile <- getClusterOption("outfile", options)
	master <- getClusterOption("master", options)
	port <- getClusterOption("port", options)
	manual <- getClusterOption("manual", options)

	## build the local command for starting the worker
	rscript <- getClusterOption("rscript", options)
	snowlib <- getClusterOption("snowlib", options)
	script <- file.path(snowlib, "snow", "RSOCKnode.R")
	env <- paste("MASTER=", master,
				 " PORT=", port,
				 " OUT=", outfile,
				 " SNOWLIB=", snowlib, sep="")
	cmd <- paste(rscript, script, env)

	## add the remote launch command
	cmd <- paste("srun -N1 -n1 --exclusive",cmd)
	.logger("Running cmd: %s \n", cmd, writeme = debug)
	system(cmd, wait = FALSE)

	## need timeout here because of the way internals work
	timeout <- getClusterOption("timeout")
	old <- options(timeout = timeout);
	on.exit(options(old))

	## Need to return a list here, in the same form as the 
	## "cluster" data structure. Note that we're putting "(SLURM)"
	## into the host field, since the hostnames aren't known ahead
	## of time, and it's not really worth the trouble to add it.
	.logger("Opening server-side socket for process %d\n", rank, writeme = debug)
   	con <- socketConnection(port = port, server=TRUE, blocking=TRUE, open="a+b")
	.logger("Process %d started!\n", rank, writeme = debug)
	structure(list(con = con, host = "(SLURM)", rank = rank), class = "SOCKnode")
}

makeSLURMcluster <- function(..., options = defaultClusterOptions)
{
	nprocs <- Sys.getenv("SLURM_NPROCS")
	if (nprocs == "")
	{
		nnodes <- as.integer(Sys.getenv("SLURM_NNODES"))
		ntaskspernode <- as.integer(Sys.getenv("SLURM_NTASKS_PER_NODE"))
		np <- nnodes * ntaskspernode
	}
	else
	{
		np <- as.integer(nprocs)
	}

	options <- addClusterOptions(options, list(...))

	# Add the "debug" option defaulted to FALSE, if the user didn't specify
	# If the user gives TRUE, print extra stuff during cluster setup
	debug <- FALSE
	tryCatch(
		debug <- getClusterOption("debug"),
		error = function(e) { }
	)
	options <- addClusterOptions(options, list(debug = debug))

	info <- Sys.info()[c("nodename", "machine")]
	.logger("Original process running on %s with CPU type %s\n", info[1], info[2], writeme = debug)
	local.hostname <- info[1]

	.logger("About to spawn %d remote slaves for SLURM SOCKET cluster\n", np)
	cl <- vector("list", np)
	for (i in seq(along=cl))
	{
		cl[[i]] <- slurm.newSOCKnode(rank=i, options = options)
	}

	class(cl) <- c("SOCKcluster", "cluster")
	.logger("Remote cluster is constructed\n")
	return(cl)
}

