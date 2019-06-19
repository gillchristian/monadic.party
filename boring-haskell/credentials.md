```
$ sudo -u postgres psql
psql (11.3 (Ubuntu 11.3-1.pgdg18.04+1))
Type "help" for help.

postgres=# create role "boring-haskell-test" with login password 'password';
CREATE ROLE
postgres=# create database "boring-haskell-test";
CREATE DATABASE
postgres=# grant all privileges on database "boring-haskell-test" to "boring-haskell-test";
GRANT
postgres=#\q
```
