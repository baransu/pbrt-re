type t = {
  x: float,
  y: float,
  z: float,
};

let make = (~x, ~y, ~z) => {x, y, z};

let from_one = value => {x: value, y: value, z: value};

let zero = () => from_one(0.0);

let add = (t, other) => {
  x: t.x +. other.x,
  y: t.y +. other.y,
  z: t.z +. other.z,
};

let (+) = add;

let sub = (t, other) => {
  x: t.x -. other.x,
  y: t.y -. other.y,
  z: t.z -. other.z,
};

let (-) = sub;
