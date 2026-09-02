-- =====================================================================
-- authdb — role: auth_api
-- Verifies admin identity, reads/writes session + refresh-token state
-- =====================================================================
 
CREATE ROLE auth_api WITH LOGIN PASSWORD 'CHANGE_ME' NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
GRANT CONNECT ON DATABASE authdb TO auth_api;
GRANT USAGE ON SCHEMA public TO auth_api;
 
-- admins: read-only, used to verify credentials at login
GRANT SELECT ON admins TO auth_api;
 
-- sessions: check validity/expiry, open new sessions, revoke on logout/rotation
GRANT SELECT (session_id, admin_id, revoked, expires_at) ON sessions TO auth_api;
GRANT INSERT (admin_id) ON sessions TO auth_api;
GRANT UPDATE (revoked) ON sessions TO auth_api;
 
-- tokens: check/rotate refresh token hashes, revoke on reuse detection
GRANT SELECT (token_hash, session_id, revoked) ON tokens TO auth_api;
GRANT INSERT (token_hash, session_id) ON tokens TO auth_api;
GRANT UPDATE (revoked) ON tokens TO auth_api;
 
 
-- =====================================================================
-- portfoliodb — role: portfolio_public_api
-- Read-only, unauthenticated portfolio browsing (public routes)
-- =====================================================================
 
CREATE ROLE portfolio_public_api WITH LOGIN PASSWORD 'CHANGE_ME' NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
GRANT CONNECT ON DATABASE portfoliodb TO portfolio_public_api;
GRANT USAGE ON SCHEMA public TO portfolio_public_api;
 
GRANT SELECT (project_id, title, project_link, description, project_date)
  ON projects TO portfolio_public_api;
 
GRANT SELECT ON tags TO portfolio_public_api;
GRANT SELECT ON project_tag_link TO portfolio_public_api;
 
 
-- =====================================================================
-- portfoliodb — role: portfolio_protected_api
-- Admin-scoped project management (create/edit/delete projects + tags)
-- =====================================================================
 
CREATE ROLE portfolio_protected_api WITH LOGIN PASSWORD 'CHANGE_ME' NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
GRANT CONNECT ON DATABASE portfoliodb TO portfolio_protected_api;
GRANT USAGE ON SCHEMA public TO portfolio_protected_api;
 
GRANT SELECT (project_id, title, project_link, description, project_date)
  ON projects TO portfolio_protected_api;
GRANT INSERT (title, project_link, description, project_date)
  ON projects TO portfolio_protected_api;
GRANT UPDATE (title, project_link, description, project_date)
  ON projects TO portfolio_protected_api;
 
-- tags: read, add new tag names, delete tags outright
GRANT SELECT ON tags TO portfolio_protected_api;
GRANT INSERT (name) ON tags TO portfolio_protected_api;
GRANT DELETE ON tags TO portfolio_protected_api;
 
-- project_tag_link: full read/write of the join table
GRANT SELECT, INSERT ON project_tag_link TO portfolio_protected_api;
GRANT DELETE ON project_tag_link TO portfolio_protected_api;
 
 
-- =====================================================================
-- inquirydb — role: inquiry_api
-- Handles inquiry submission + status updates (verify table name!)
-- =====================================================================
 
CREATE ROLE inquiry_api WITH LOGIN PASSWORD 'CHANGE_ME' NOSUPERUSER NOCREATEDB NOCREATEROLE;
 
GRANT CONNECT ON DATABASE inquirydb TO inquiry_api;
GRANT USAGE ON SCHEMA public TO inquiry_api;
 
GRANT SELECT (inquiry_id) ON inquiries TO inquiry_api;
GRANT INSERT (name, email, tutoring, info, status) ON inquiries TO inquiry_api;
GRANT UPDATE (status) ON inquiries TO inquiry_api;
 