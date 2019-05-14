open Lib;

Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

let render = (scene: Scene.t) => {
  let image = Image.create_rgb(scene.camera.width, scene.camera.height);

  for (y in 0 to scene.camera.height - 1) {
    for (x in 0 to scene.camera.width - 1) {
      let ray = Rendering.Ray.create_prime(x, y, scene.camera);

      let {r, g, b}: Color.Rgb.t =
        (
          switch (scene |> Scene.trace(~ray)) {
          | Some(intersection) =>
            let origin = ray.origin |> Vector3.from_point;
            let direction =
              ray.direction |> Vector3.by_scalar(intersection.distance);

            let hit_point = Vector3.(origin + direction) |> Vector3.to_point;

            let surface_normal =
              intersection.element |> Scene.Element.surface_normal(~hit_point);

            // Logs.app(m =>
            //   m("intersection.distance: %f", intersection.distance)
            // );

            let direction_to_light =
              scene.light.direction |> Vector3.normalize |> Vector3.neg;

            let partial_light_power =
              Vector3.dot(surface_normal, direction_to_light);

            // Logs.app(m => m("partial_light_power: %f", partial_light_power));

            let light_power =
              (partial_light_power |> max(0.0)) *. scene.light.intensity;

            let light_reflected =
              Scene.Element.albedo(intersection.element) /. FloatExtra.pi;

            Color.(
              Scene.Element.color(intersection.element) * scene.light.color
            )
            |> Color.by_scalar(light_power)
            |> Color.by_scalar(light_reflected)
            |> Color.clamp;

          | None => scene.camera.background
          }
        )
        |> Color.to_rgb;

      Image.write_rgb(image, x, y, r, g, b);
    };
  };

  image |> ImageLib.PNG.write_png("test.png");
};

let green = Color.make(~r=0.4, ~g=1.0, ~b=0.4);
let red = Color.make(~r=1.0, ~g=0.0, ~b=0.4);
let blue = Color.make(~r=0.4, ~g=0.4, ~b=1.0);
let white = Color.make(~r=1.0, ~g=1.0, ~b=1.0);

let scene =
  Scene.(
    make(
      ~light=
        Light.make(
          ~color=white,
          ~direction=Vector3.make(~x=-0.25, ~y=-1.0, ~z=-1.0),
          ~intensity=20.0,
        ),
      ~width=800,
      ~height=800,
      ~fov=120.0,
      ~background=Color.make(~r=0.41, ~g=0.85, ~b=1.0),
      ~entities=[
        Element.Plane(
          Plane.make(
            ~albedo=0.18,
            ~origin=Point.make(~x=0.0, ~y=-2.0, ~z=-5.0),
            ~normal=Vector3.down(),
            ~color=Color.make(~r=0.4, ~g=0.4, ~b=0.4),
          ),
        ),
        Element.Sphere(
          Sphere.make(
            ~albedo=0.18,
            ~center=Point.make(~x=-3.0, ~y=4.0, ~z=-10.0),
            ~radius=2.0,
            ~color=red,
          ),
        ),
        Element.Sphere(
          Sphere.make(
            ~albedo=0.18,
            ~center=Point.make(~x=0.0, ~y=0.0, ~z=-10.0),
            ~radius=1.0,
            ~color=green,
          ),
        ),
        Element.Sphere(
          Sphere.make(
            ~albedo=0.18,
            ~center=Point.make(~x=5.0, ~y=0.0, ~z=-10.0),
            ~radius=3.0,
            ~color=blue,
          ),
        ),
      ],
    )
  );

let () = {
  Logs.app(m => m("Starting render"));
  render(scene);
  Logs.app(m => m("Done"));
};
