module Directional = {
  type t = {
    direction: Vector3.t,
    color: Color.t,
    intensity: float,
  };

  let make = (~color, ~direction, ~intensity) => {
    direction,
    color,
    intensity,
  };
};

module Spherical = {
  type t = {
    position: Point.t,
    color: Color.t,
    intensity: float,
  };

  let make = (~color, ~position, ~intensity) => {color, position, intensity};
};

type t =
  | Directional(Directional.t)
  | Spherical(Spherical.t);

let color =
  fun
  | Directional(l) => l.color
  | Spherical(l) => l.color;

let intensity = (~hit_point) =>
  fun
  | Directional(l) => l.intensity
  | Spherical(l) => l.intensity;

let distance = (~hit_point, t) => 0.0;

let direction_from = (~hit_point, t) => {
  Vector3.down();
};
