build:
	dune build --profile release 

run-server:
	./_build/default/bin/server.exe 192.168.1.15 9000

run-client:
	./_build/default/bin/client.exe 192.168.1.15 9000