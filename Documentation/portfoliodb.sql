-- ---------------------------------------------------------------------
-- projects
-- ---------------------------------------------------------------------
CREATE TABLE projects (
    project_id   UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
    title        TEXT        NOT NULL,
    project_link TEXT        NOT NULL,
    description  TEXT        NOT NULL,
    project_date TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
 
-- ---------------------------------------------------------------------
-- tags
-- ---------------------------------------------------------------------
CREATE TABLE tags (
    tag_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name   TEXT NOT NULL,
 
    CONSTRAINT tags_name_unique UNIQUE (name)
);
 
-- ---------------------------------------------------------------------
-- project_tag_link (many-to-many join table)
-- ---------------------------------------------------------------------
CREATE TABLE project_tag_link (
    project_id UUID NOT NULL REFERENCES projects (project_id) ON DELETE CASCADE,
    tag_id     UUID NOT NULL REFERENCES tags (tag_id)         ON DELETE CASCADE,
    PRIMARY KEY (project_id, tag_id)
);
 
-- The composite PK (project_id, tag_id) already gives you a fast
-- index for "all tags for a project". Requested: index for faster
-- joins also covers the reverse direction, "all projects for a tag"
-- (e.g. filtering the portfolio by tag), which the PK alone doesn't
-- serve efficiently since tag_id isn't the leading column.
CREATE INDEX idx_project_tag_link_tag_id ON project_tag_link (tag_id);
 
-- ---------------------------------------------------------------------
-- Only needed if this script is run as a superuser rather than as
-- the owning admin role.
-- ---------------------------------------------------------------------
ALTER TABLE projects         OWNER TO portfolio_admin;
ALTER TABLE tags             OWNER TO portfolio_admin;
ALTER TABLE project_tag_link OWNER TO portfolio_admin;