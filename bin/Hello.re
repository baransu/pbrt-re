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
          | Some(intersection) => intersection.element |> Scene.Element.color
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

let scene =
  Scene.(
    make(
      ~width=800,
      ~height=800,
      ~fov=120.0,
      ~background=Color.make(~r=0.41, ~g=0.85, ~b=1.0),
      ~entities=[
        Element.Plane(
          Plane.make(
            ~origin=Point.make(~x=0.0, ~y=-2.0, ~z=-5.0),
            ~normal=Vector3.down(),
            ~color=Color.make(~r=0.4, ~g=0.4, ~b=0.4),
          ),
        ),
        Element.Sphere(
          Sphere.make(
            ~center=Point.make(~x=-3.0, ~y=4.0, ~z=-10.0),
            ~radius=2.0,
            ~color=red,
          ),
        ),
        Element.Sphere(
          Sphere.make(
            ~center=Point.make(~x=0.0, ~y=-2.0, ~z=-10.0),
            ~radius=1.0,
            ~color=green,
          ),
        ),
        Element.Sphere(
          Sphere.make(
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
