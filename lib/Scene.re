module Plane = {
  type t = {
    origin: Point.t,
    normal: Vector3.t,
    color: Color.t,
  };

  let make = (~origin, ~normal, ~color) => {origin, normal, color};

  let intersect = (t, ~ray: Rendering.Ray.t) => {
    let denom = Vector3.dot(t.normal, ray.direction);
    if (denom > 1e-6) {
      let v = Point.(t.origin - ray.origin) |> Vector3.from_point;
      let distance = Vector3.dot(v, t.normal) /. denom;
      if (distance >= 0.0) {
        Some(distance);
      } else {
        None;
      };
    } else {
      None;
    };
  };
};

module Sphere = {
  type t = {
    center: Point.t,
    radius: float,
    color: Color.t,
  };

  let make = (~center, ~radius, ~color) => {center, radius, color};

  let intersect = (t, ~ray: Rendering.Ray.t) => {
    let l = Point.(t.center - ray.origin) |> Vector3.from_point;
    let adj = Vector3.dot(l, ray.direction);

    let l2 = Vector3.dot(l, l);
    let d2 = l2 -. adj *. adj;
    let radius2 = t.radius *. t.radius;

    if (d2 > radius2) {
      None;
    } else {
      let thc = sqrt(radius2 -. d2);
      let t0 = adj -. thc;
      let t1 = adj +. thc;
      if (t0 < 0.0 && t1 < 0.0) {
        None;
      } else {
        let distance = t0 < t1 ? t0 : t1;
        Some(distance);
      };
    };
  };
};

module Element = {
  type t =
    | Sphere(Sphere.t)
    | Plane(Plane.t);

  let color =
    fun
    | Sphere(sphere) => sphere.color
    | Plane(plane) => plane.color;

  let intersect = (~ray) =>
    fun
    | Sphere(sphere) => sphere |> Sphere.intersect(~ray)
    | Plane(plane) => plane |> Plane.intersect(~ray);
};

module Intersection = {
  type t = {
    distance: float,
    element: Element.t,
  };

  let make = (~distance, ~element) => {distance, element};
};

type t = {
  camera: Camera.t,
  entities: list(Element.t),
};

let make = (~width, ~height, ~fov, ~background, ~entities) => {
  camera: {
    width,
    height,
    fov,
    background,
  },
  entities,
};

let trace = (scene, ~ray) => {
  scene.entities
  |> List.map(element =>
       element
       |> Element.intersect(~ray)
       |> Option.map(distance => Intersection.make(~distance, ~element))
     )
  |> ListExtra.collect
  |> ListExtra.min_by((a, b) =>
       Intersection.(Float.compare(a.distance, b.distance))
     );
};
