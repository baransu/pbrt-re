module Rgb = {
  type t = {
    r: int,
    g: int,
    b: int,
  };
};

type t = {
  r: float,
  g: float,
  b: float,
};

let make = (~r, ~g, ~b) => {r, g, b};

let black = () => make(~r=0.0, ~g=0.0, ~b=0.0);

let to_rgb = (t): Rgb.t => {
  r: t.r *. 255.0 |> int_of_float,
  g: t.g *. 255.0 |> int_of_float,
  b: t.b *. 255.0 |> int_of_float,
};

let mul = (t, other) => {
  r: t.r *. other.r,
  g: t.g *. other.g,
  b: t.b *. other.b,
};

let ( * ) = mul;

let add = (t, other) => {
  r: t.r +. other.r,
  g: t.g +. other.g,
  b: t.b +. other.b,
};

let (+) = add;

let by_scalar = (scalar, t) => {
  r: t.r *. scalar,
  g: t.g *. scalar,
  b: t.b *. scalar,
};

let clamp = t => {
  r: t.r |> min(1.0) |> max(0.0),
  g: t.g |> min(1.0) |> max(0.0),
  b: t.b |> min(1.0) |> max(0.0),
};
