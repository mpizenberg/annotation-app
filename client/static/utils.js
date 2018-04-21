// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/

const utils = (function() {
  // Download a file through an href temporary DOM element.
  // example use: download( str, "selection.json", "text/plain" )
  function download(data, name, data_type) {
    const a = document.createElement("a");
    const data_file = new Blob([data], { type: data_type });
    a.addEventListener("click", event => {
      a.href = window.URL.createObjectURL(data_file);
      a.download = name;
    });
    const click = node => {
      const event = new MouseEvent("click");
      node.dispatchEvent(event);
    };
    click(a);
  }

  // Read JSON file as text
  function readJsonFile(file) {
    const promise = new Promise((resolve, reject) => {
      // if ( file.type === "application/json" ) { // ubuntu chrome returns "" ...
      if (file.name.match(/.*json/)) {
        const fileReader = new FileReader();
        fileReader.onload = () => resolve(fileReader.result);
        fileReader.readAsText(file);
      } else {
        reject("Incorrect file type, please load JSON file.");
      }
    });
    return promise;
  }

  // Provided an id and an image, returns an object
  // { id, url, width, height }
  // with the url corresponding to the image loaded
  function createImageObject(id, imageFile) {
    const promise = new Promise((resolve, reject) => {
      if (imageFile.type.match(/image.*/)) {
        const img = document.createElement("img");
        img.onload = () =>
          resolve({
            id: id,
            url: img.src,
            width: img.width,
            height: img.height
          });
        img.src = window.URL.createObjectURL(imageFile);
      } else {
        reject("Not an image file: " + imageFile.type);
      }
    });
    return promise;
  }

  return {
    download: download,
    readJsonFile: readJsonFile,
    createImageObject: createImageObject
  };
})();
