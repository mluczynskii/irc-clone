build:
	dune build --profile release 

run-server:
	./_build/default/bin/server.exe 156.17.150.14 9000

run-client:
	./_build/default/bin/client.exe 156.17.150.14 9000