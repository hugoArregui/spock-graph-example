build:

	chicken-spock matchable.scm library.scm graph.scm -o graph.js 

clean:

	rm -f graph.js
