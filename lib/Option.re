type t('a) = option('a);

let map = (fn, t) =>
  switch (t) {
  | Some(a) => Some(fn(a))
  | None => None
  };

exception None_get;

let get_exn = t =>
  switch (t) {
  | Some(t) => t
  | None => raise(None_get)
  };

let is_none = t =>
  switch (t) {
  | None => true
  | Some(_) => false
  };
