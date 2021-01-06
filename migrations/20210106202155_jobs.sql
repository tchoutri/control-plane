create table jobs (
  id serial primary key,
  payload jsonb not null,
  created_at timestamptz not null,
  run_date timestamptz not null
);

create index if not exists jobs_run_index on jobs
  (run_date);
