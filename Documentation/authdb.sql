CREATE EXTENSION IF NOT EXISTS citext;
 
-- ---------------------------------------------------------------------
-- admins
-- ---------------------------------------------------------------------
CREATE TABLE admins (
    admin_id      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email         CITEXT NOT NULL,
    password_hash TEXT   NOT NULL,
 
    CONSTRAINT admins_email_unique UNIQUE (email)
);
 
-- ---------------------------------------------------------------------
-- sessions
-- ---------------------------------------------------------------------
CREATE TABLE sessions (
    session_id UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
    admin_id   UUID        NOT NULL REFERENCES admins (admin_id) ON DELETE CASCADE,
    revoked    BOOLEAN     NOT NULL DEFAULT FALSE,
    expires_at TIMESTAMPTZ NOT NULL DEFAULT now() + INTERVAL '30 days',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
 
-- Speeds up "all sessions for this admin" lookups (e.g. revoke-all-
-- sessions-on-password-change), since FK columns aren't auto-indexed.
CREATE INDEX idx_sessions_admin_id ON sessions (admin_id);
 
-- ---------------------------------------------------------------------
-- tokens
-- ---------------------------------------------------------------------
CREATE TABLE tokens (
    token_hash TEXT        PRIMARY KEY,
    session_id UUID        NOT NULL REFERENCES sessions (session_id) ON DELETE CASCADE,
    revoked    BOOLEAN     NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
 
-- Requested: index session_id for faster queries (refresh-token
-- rotation/reuse-detection lookups by session, and supports the
-- ON DELETE CASCADE from sessions efficiently).
CREATE INDEX idx_tokens_session_id ON tokens (session_id);
 
-- ---------------------------------------------------------------------
-- Only needed if this script is run as a superuser rather than as
-- the owning admin role. Uncomment and set the right role for the
-- database you're running against (auth_admin / testauth_admin).
-- ---------------------------------------------------------------------
ALTER TABLE admins   OWNER TO auth_admin;
ALTER TABLE sessions OWNER TO auth_admin;
ALTER TABLE tokens   OWNER TO auth_admin;