credentials <-
local({
	.value <- NULL
	function(keypair) {
		if(!missing(keypair)) .value <<- keypair
		else .value
	}
})

