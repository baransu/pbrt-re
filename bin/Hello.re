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
            scene.lights
            |> List.fold_left(
                 (color, light: Light.t) => {
                   let origin = ray.origin |> Vector3.from_point;
                   let direction =
                     ray.direction |> Vector3.by_scalar(intersection.distance);

                   let hit_point =
                     Vector3.(origin + direction) |> Vector3.to_point;

                   let surface_normal =
                     intersection.element
                     |> Scene.Element.surface_normal(~hit_point);

                   let direction_to_light =
                     light |> Light.direction_from(~hit_point);

                   let shadow_ray =
                     Rendering.Ray.make(
                       ~origin=
                         Vector3.(
                           (hit_point |> from_point)
                           + (surface_normal |> by_scalar(scene.shadow_bias))
                           |> to_point
                         ),
                       ~direction=direction_to_light,
                     );

                   let shadow_intersection =
                     scene |> Scene.trace(~ray=shadow_ray);

                   let in_light =
                     shadow_intersection
                     |> Option.map((intersection: Scene.Intersection.t) => {
                          let light_distance =
                            light |> Light.distance(~hit_point);
                          intersection.distance > light_distance;
                        })
                     |> Option.get_default(true);

                   let light_intensity =
                     if (in_light) {
                       light |> Light.intensity(~hit_point);
                     } else {
                       0.0;
                     };

                   let light_power =
                     (
                       Vector3.dot(surface_normal, direction_to_light)
                       |> max(0.0)
                     )
                     *. light_intensity;

                   let light_reflected =
                     Scene.Element.albedo(intersection.element)
                     /. FloatExtra.pi;

                   let light_color = light |> Light.color;
                   let current_light_color =
                     Color.(
                       Scene.Element.color(intersection.element) * light_color
                     )
                     |> Color.by_scalar(light_power)
                     |> Color.by_scalar(light_reflected);

                   Color.(color + current_light_color);
                 },
                 Color.black(),
               )
            |> Color.clamp

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
      ~lights=[
        Light.(
          Directional(
            Directional.make(
              ~color=white,
              ~direction=Vector3.make(~x=-0.25, ~y=-1.0, ~z=-1.0),
              ~intensity=10.0,
            ),
          )
        ),
        Light.(
          Directional(
            Directional.make(
              ~color=white,
              ~direction=Vector3.make(~x=0.25, ~y=-1.0, ~z=-1.0),
              ~intensity=10.0,
            ),
          )
        ),
      ],
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
