const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");
const compression = require("compression");

// Config based on .env file (transferred to process.env)
require("dotenv").config({ path: path.join(__dirname, ".env") });
const port = parseInt(process.env.SERVER_PORT, 10);
const distDir = path.join(__dirname, process.env.BUILD_DIR);

// Middlewares
app.use(compression()); // gzip

// Serve statically the dist/ directory (build artefact)
app.use(express.static(distDir));

// // Serve elm index.html
// app.get( '/', ( req, res ) => {
// 	res.sendFile( path.join( distDir, 'index.html' ) )
// })

// Start server
app.listen(port, () => {
  console.log("Server listening on port %s", port);
});
