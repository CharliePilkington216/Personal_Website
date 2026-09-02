CREATE TYPE inquiry_status AS ENUM ('sent', 'pending', 'failed', 'denied');
CREATE TYPE tutoring_type  AS ENUM ('GCSE', 'ALevel', 'Oxbridge', 'other');
 
-- ---------------------------------------------------------------------
-- inquiries
-- ---------------------------------------------------------------------
CREATE TABLE inquiries (
    inquiry_id UUID           PRIMARY KEY DEFAULT gen_random_uuid(),
    name       TEXT           NOT NULL,
    email      TEXT           NOT NULL,
    tutoring   tutoring_type  NOT NULL,
    info       TEXT           NOT NULL,
    status     inquiry_status NOT NULL,
    created_at TIMESTAMPTZ    NOT NULL DEFAULT now()
);
 
-- ---------------------------------------------------------------------
-- Only needed if this script is run as a superuser rather than as
-- the owning admin role.
-- ---------------------------------------------------------------------
ALTER TABLE inquiries OWNER TO inquiry_admin;
ALTER TYPE inquiry_status OWNER TO inquiry_admin;
ALTER TYPE tutoring_type  OWNER TO inquiry_admin;
 