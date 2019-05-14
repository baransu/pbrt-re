type t = {
  color: Color.t,
  direction: Vector3.t,
  intensity: float,
};

let make = (~color, ~direction, ~intensity) => {color, direction, intensity};
