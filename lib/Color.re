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
