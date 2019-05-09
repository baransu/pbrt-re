let collect = list =>
  list
  |> List.fold_left(
       (acc, element) =>
         switch (element) {
         | Some(e) => [e, ...acc]
         | None => acc
         },
       [],
     )
  |> List.rev;

let min_by = (cmp, list) =>
  list
  |> List.fold_left(
       (current, element) =>
         switch (current) {
         | None => Some(element)
         | Some(current) when cmp(element, current) < 0 => Some(element)
         | _ => current
         },
       None,
     );
