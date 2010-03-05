{
  application,
  redis,
  [
    {description, "hot shit!"},
    {vsn, "1.0.0"},
    {modules, [redis]},
    {applications, [stdlib, kernel]},
    {registered, [redis]}
  ]
}.
