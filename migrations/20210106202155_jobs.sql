create type result as enum('OK', 'Failing');

create table jobs (
  id bigserial primary key,
  payload jsonb not null,
  created_at timestamptz not null,
  run_date timestamptz not null,
  locked_at timestamptz,
  attempts integer not null
);

create index if not exists jobs_run_index on jobs (run_date);

create table jobs_results (
  id bigserial primary key,
  job_id bigserial references jobs(id) not null,
  result result not null
);

create unique index job_id_index on jobs_results (job_id);
