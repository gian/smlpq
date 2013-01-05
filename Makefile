POSTGRES_LIBRARY_PATH=/Library/PostgreSQL/9.2/lib/

all: pq-test

pq-test: pq-test.sml
	mlton -link-opt "-L$(POSTGRES_LIBRARY_PATH) -lpq" pq-test.mlb
