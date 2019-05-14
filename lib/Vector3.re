type t = {
  x: float,
  y: float,
  z: float,
};

let make = (~x, ~y, ~z) => {x, y, z};

let zero = () => {x: 0.0, y: 0.0, z: 0.0};

let down = () => {x: 0.0, y: (-1.0), z: 0.0};

let up = () => {x: 0.0, y: 1.0, z: 0.0};

let forward = () => {x: 0.0, y: 0.0, z: (-1.0)};

let from_one = value => {x: value, y: value, z: value};

let from_point = (t: Point.t): t => {x: t.x, y: t.y, z: t.z};
let to_point = (t: t): Point.t => {x: t.x, y: t.y, z: t.z};

let norm = t => {
  t.x *. t.x +. t.y *. t.y +. t.z *. t.z;
};

let length = t => t |> norm |> sqrt;

let normalize = t => {
  let len = length(t);
  {x: t.x /. len, y: t.y /. len, z: t.z /. len};
};

let dot = (t, other) => {
  t.x *. other.x +. t.y *. other.y +. t.z *. other.z;
};

let cross = (t, other) => {
  x: t.y *. other.z -. t.z *. other.y,
  y: t.z *. other.x -. t.x *. other.z,
  z: t.x *. other.y -. t.y *. other.x,
};

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

let mul = (t, other) => {
  x: t.x *. other.x,
  y: t.y *. other.y,
  z: t.z *. other.z,
};

let ( * ) = mul;

let by_scalar = (other, t) => {
  x: t.x *. other,
  y: t.y *. other,
  z: t.z *. other,
};

let neg = t => {x: -. t.x, y: -. t.y, z: -. t.z};
