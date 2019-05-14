module Ray = {
  type t = {
    origin: Point.t,
    direction: Vector3.t,
  };

  let make = (~origin, ~direction) => {origin, direction};

  let create_prime = (~x, ~y, ~camera: Camera.t) => {
    let x = float_of_int(x);
    let y = float_of_int(y);
    let width = float_of_int(camera.width);
    let height = float_of_int(camera.height);

    let fov_adjustment = tan(FloatExtra.radians(camera.fov) /. 2.0);

    let aspect_ratio = width /. height;

    let sensor_x =
      ((x +. 0.5) /. width *. 2.0 -. 1.0) *. aspect_ratio *. fov_adjustment;

    let sensor_y = (1.0 -. (y +. 0.5) /. height *. 2.0) *. fov_adjustment;

    make(
      ~origin=Point.zero(),
      ~direction=
        Vector3.make(~x=sensor_x, ~y=sensor_y, ~z=-1.0) |> Vector3.normalize,
    );
  };
};
