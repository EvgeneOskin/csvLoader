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

$./csvLoader -M markets_symbols -O path/to/output/dir -S server_name -U user_name -P password 

"markets_symbols" is a markets symbols without " " (spaces)
(exemples: [NYSE,NASDAQ,INDEX] or [NYSE]) *type it without [ ]*

"path/to/output/dir" is a path to directory which will contain *.csv files. If path contains " " (space) you should put path into quote ("path").

"server_name" -- now supported only "eoddata"

"user_name" is a eoddata user's name

"password" is a user password

if it'd say
 "csvLoader: user error (FTP:550: These files are only available to Gold or Platinum Members.)"
 try to type

 $./csvLoader -M markets_symbols -O path/to/output/dir -S server_name -U user_name -P password -A account

 "account" is a user account type

 $./csvLoader -M markets_symbols -O path/to/output/dir -S server_name

 user_name and password will be searched in environment variables ("***_USER" and "***_PASSWD",
 where *** is 3-char word, exemple: if server_name is "eodata" user_name and password will be "EOD_USER" "EOD_PASSWD")