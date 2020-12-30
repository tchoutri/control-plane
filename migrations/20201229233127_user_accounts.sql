create table users (
  user_id uuid primary key,
  username text not null,
  display_name text not null,
  password text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null,
  unique(username)
);
