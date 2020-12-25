create extension if not exists "uuid-ossp";

create type status as enum('Read', 'Unread');

create table notifications (
  notification_id uuid primary key,
  device text not null,
  title text not null,
  message text not null,
  received_at timestamptz not null,
  status status not null,
  read_at timestamptz,
  constraint consistent_status
    check (
         (status = 'Read'   and read_at is not null)
      or (status = 'Unread' and read_at is null)
    )
);
