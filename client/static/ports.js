// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/

activatePorts = (app, utils, containerSize) => {
  // Inform the Elm app when its container div gets resized.
  window.addEventListener("resize", () =>
    app.ports.resizes.send(containerSize())
  );

  // Create an image object and send it back to the Elm app.
  app.ports.loadImageFile.subscribe(value => {
    utils
      .createImageObject(value.id, value.file)
      .then(image => app.ports.imageLoaded.send(image))
      .catch(error => console.log(error));
  });

  // Read config file as text and send it back to the Elm app.
  app.ports.loadConfigFile.subscribe(file => {
    utils
      .readJsonFile(file)
      .then(fileAsText => app.ports.configLoaded.send(fileAsText))
      .catch(error => console.log(error));
  });

  // Export / save annotations
  app.ports.export.subscribe(value => {
    utils.download(
      JSON.stringify(value),
      "annotations.json",
      "application/json"
    );
  });

  // Pointer capture.
  app.ports.capture.subscribe(event => {
    event.target.setPointerCapture(event.pointerId);
    console.log("captured");
  });
};
