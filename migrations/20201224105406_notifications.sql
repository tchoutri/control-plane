create table notifications (
  notification_id uuid primary key,
  device text not null,
  title text not null,
  message text not null,
  received_at timestamptz not null,
  is_read bool not null,
  read_at timestamptz
)


