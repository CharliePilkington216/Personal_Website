-- ---------------------------------------------------------------------
-- Live databases: authdb, portfoliodb, inquirydb
-- ---------------------------------------------------------------------
CREATE ROLE auth_admin      WITH LOGIN PASSWORD 'CHANGE_ME_AUTHDB_PW'      NOSUPERUSER NOCREATEDB NOCREATEROLE;
CREATE ROLE portfolio_admin WITH LOGIN PASSWORD 'CHANGE_ME_PORTFOLIODB_PW' NOSUPERUSER NOCREATEDB NOCREATEROLE;
CREATE ROLE inquiry_admin   WITH LOGIN PASSWORD 'CHANGE_ME_INQUIRYDB_PW'   NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
CREATE DATABASE authdb      OWNER auth_admin;
CREATE DATABASE portfoliodb OWNER portfolio_admin;
CREATE DATABASE inquirydb   OWNER inquiry_admin;
 
-- ---------------------------------------------------------------------
-- Test databases: same schema as above, name prefixed with "test" (uncomment to create)
-- ---------------------------------------------------------------------
--CREATE ROLE testauthdbadmin      WITH LOGIN PASSWORD 'CHANGE_ME_TESTAUTHDB_PW'      NOSUPERUSER NOCREATEDB NOCREATEROLE;
--CREATE ROLE testportfoliodbadmin WITH LOGIN PASSWORD 'CHANGE_ME_TESTPORTFOLIODB_PW' NOSUPERUSER NOCREATEDB NOCREATEROLE;
--CREATE ROLE testinquirydbadmin   WITH LOGIN PASSWORD 'CHANGE_ME_TESTINQUIRYDB_PW'   NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
--CREATE DATABASE testauthdb      OWNER testauthdbadmin;
--CREATE DATABASE testportfoliodb OWNER testportfoliodbadmin;
--CREATE DATABASE testinquirydb   OWNER testinquirydbadmin;
 
-- ---------------------------------------------------------------------
-- Lock down the default PUBLIC schema privileges on each database.
-- By default any role can CREATE in `public` and connect; we strip
-- that so only the owning admin role (and roles it explicitly grants
-- to later) can act on each database. This matches the project's
-- existing "strip PUBLIC default privileges" convention.
-- ---------------------------------------------------------------------
REVOKE ALL ON DATABASE authdb           FROM PUBLIC;
REVOKE ALL ON DATABASE portfoliodb      FROM PUBLIC;
REVOKE ALL ON DATABASE inquirydb        FROM PUBLIC;

--REVOKE ALL ON DATABASE testauthdb       FROM PUBLIC;
--REVOKE ALL ON DATABASE testportfoliodb  FROM PUBLIC;
--REVOKE ALL ON DATABASE testinquirydb    FROM PUBLIC;