build:
	dune build --profile release 

run-server:
	./_build/default/bin/server.exe

run-client:
	./_build/default/bin/client.exe