// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/

// Inform the Elm app when its container div gets resized.
window.addEventListener("resize", () =>
  app.ports.resizes.send(containerSize())
);

// Submit annotations
app.ports.export.subscribe(value => {
  // Get the hidden input element with id 'annotation-data'
  let inputElement = document.getElementById("annotation-data");
  inputElement.value = JSON.stringify(value);

  // Click on mturk submit button
  const submitButton = document.getElementById("submitButton");
  submitButton.click();
});
