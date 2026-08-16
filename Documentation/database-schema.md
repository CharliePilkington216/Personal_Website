# Database Schema & Access Design

Backing databases for the personal website: authentication, portfolio content, and
contact-form inquiries. Each database is isolated on its own connection with its own
narrowly-scoped roles, so a compromised or buggy service credential can only do the
one thing it was issued for.

Dialect: PostgreSQL 13+ (uses the built-in `gen_random_uuid()`).

## Design conventions

- Every primary key is `UUID DEFAULT gen_random_uuid()`, except join-table composite
  keys, which are made of UUID foreign keys instead of a surrogate id and the token_hash.
- Every database has one `admin` role that owns it (full DDL/DML on everything, current
  and future) and one or more narrow, `LOGIN`-only application roles for services.
- `PUBLIC` is stripped of its default database and schema privileges before any
  application role is created, so nothing is reachable except what's explicitly granted.
- Application roles get column-level grants (`GRANT SELECT (col, ...)`, `UPDATE (col)`)
  wherever a service only needs part of a row, not the whole thing.

## 1. AuthDB

Holds admin login credentials and refresh tokens for the site's admin panel.

### Tables

| Table | Columns |
|---|---|
| `admins` | `admin_id` PK, `email`, `password_hash` |
| `refresh_tokens` | `token_hash` PK (`TEXT`, no default), `admin_id` FK → `admins`, `expires_at`, `revoked`, `created_at` |

> `token_hash` is the one PK in this project that isn't a `UUID DEFAULT
> gen_random_uuid()` — it's `TEXT` with no default, because it holds the hash of the
> raw refresh token (e.g. SHA-256) computed by the application, which doubles as the
> natural lookup key.

### Roles

| Role | admins | refresh_tokens |
|---|---|---|
| `auth_admin` (owner) | full | full |
| `auth_api` | `SELECT(email, password_hash)` | `INSERT`; `UPDATE(revoked)`; `SELECT(token_hash, expires_at)` |
| `auth_cleanup` | none | `SELECT(expires_at, revoked)`; `DELETE` |

`auth_api` backs the login/refresh/logout endpoints and supports exactly three reads
and one write:

```sql
SELECT password_hash FROM admins WHERE email = $1;
SELECT count(*) FROM refresh_tokens WHERE token_hash = $1 AND expires_at > now();
UPDATE refresh_tokens SET revoked = false WHERE token_hash = $1;
INSERT INTO refresh_tokens (...) VALUES (...);   -- issue a new token on login
```

Its column-level grant on `admins` deliberately excludes `admin_id` — it can verify a
password by email but can never read an admin's surrogate id. Its grant on
`refresh_tokens` gives it `token_hash` and `expires_at` (needed for the expiry check
and as the `WHERE` target of the `UPDATE`) but nothing else — it can't read `revoked`,
`admin_id`, or `created_at`, and it cannot delete tokens.

`auth_cleanup` backs a scheduled job that purges expired or revoked tokens. It can see
just enough (`expires_at`, `revoked`) to decide what to delete, and has no access to
`admins` at all — a compromised cleanup credential can't read a password hash.

## 2. PortfolioDB

Holds the projects shown on the site and their tags.

### Tables

| Table | Columns |
|---|---|
| `projects` | `project_id` PK, `title`, `project_link`, `description`, `project_date`, `created_at` |
| `tags` | `tag_id` PK, `name` |
| `project_tag_link` | PK (`project_id` FK, `tag_id` FK) |

### Roles

| Role | projects / tags / project_tag_link |
|---|---|
| `portfolio_admin` (owner) | full |
| `public_api` | `SELECT` |
| `private_api` | `SELECT`, `INSERT`, `UPDATE`, `DELETE` |

`public_api` is what the public site itself queries against — read-only across all
three tables, so the front end can never modify content even if it's compromised.
`private_api` is for the authenticated admin panel and has full CRUD, including
deletes, on all three tables.

## 3. InquiryDB

Holds contact-form submissions.

### Type

```sql
CREATE TYPE inquiry_status AS ENUM ('pending', 'sent', 'failed', 'denied');
```

### Table

| Table | Columns |
|---|---|
| `inquiries` | `inquiry_id` PK, `name`, `email`, `info`, `ip_address`, `status` (`inquiry_status`), `created_at` |

### Roles

| Role | inquiries |
|---|---|
| `inquiry_admin` (owner) | full |
| `inquiry_api` | `INSERT` only |
| `inquiry_cleanup` | `SELECT(created_at)`; `DELETE` |

`inquiry_api` backs the public contact-form endpoint. It's **insert-only** — it can't
read back a single row, including the one it just inserted — so a compromised form
endpoint can be used to spam the table but never to read prior submitters' names,
emails, or messages.

`inquiry_cleanup` backs a retention job (e.g. "delete inquiries older than 90 days").
It can see `created_at` to decide what's stale, and can delete, but can't read `name`,
`email`, `info`, `ip_address`, or `status` for any row.
