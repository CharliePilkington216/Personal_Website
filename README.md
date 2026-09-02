# Personal website

## Setup

### Database

The project requires a few libraries the rest should be installed at build and are listed in the build dependencies:

* Haskell Servant and Cabal
* C# Blazor WASM
* Postgresql

***

To recreate the database, you can use the .sql files in the documentation, **Ensure you change the passwords** ,and these commands, to create test databases, you may need to change the files slightly and it should be clear what you need to change. If the commands fail to work, you can always copy paste in the sql to psql.

```bash
psql -U postgres -d postgres -f role_creation.sql
```
```bash
psql -U postgres -d authdb -f authdb.sql
```
```bash
psql -U postgres -d portfoliodb -f portfoliodb.sql
```
```bash
psql -U postgres -d inquirydb -f inquirydb.sql
```
```bash
psql -U postgres -d postgres -f api_role_creation.sql
```

### Environment

Once the database has been setup, you can test it is working and the project as a whole if you have made the test databases by creating a .env file with the test.env values and running `cabal test` in the BackEnd directory. To set up the .env file:

* change the test.env db connection strings to use the roles in api_role_creation
* generate the jwt secret by running `openssl rand -hex 32` and copy pasting the output
* set the domain to whatever domain you have
* place the resend API key in along with the emails registered to send via the api and the target email

## Run

You can now run `dotnet run` in FrontEnd and `cabal run` in BackEnd to launch the applications. 

