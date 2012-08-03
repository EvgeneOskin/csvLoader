csvLoader
======

Instalation
-----------

$ghc --make setup

$./setup configure --prefix=$HOME --user

$./setup build

$./setup install

Runing
------

$./csvLoader -M market_symbol -O path/to/output/dir -S server_name -U user_name -P password 

"market_symbol" is a market symbol abbreviation

"path/to/output/dir" is a path to directory which will contain *.csv files. If path contains " " (space) you should put path into quote ("path").

"server_name" -- now supported only "eoddata"

"user_name" is a eoddata user's name

"password" is a user password