POSTGRES_LIBRARY_PATH=/Library/PostgreSQL/9.2/lib/
SQLITE_LIBRARY_PATH=/usr/local/lib

all: pq-test

pq-test: pq-test.sml
	mlton -link-opt "-L$(POSTGRES_LIBRARY_PATH) -lpq" pq-test.mlb

sqlite: sqlite.sml
	mlton -link-opt "-L$(SQLITE_LIBRARY_PATH) -lsqlite3" sqlite.mlb

sqlite-test: sqlite.sml sqlite-test.sml
	mlton -link-opt "-L$(SQLITE_LIBRARY_PATH) -lsqlite3" sqlite-test.mlb
