# Image Annotation Web App

[![][badge-license]][license]

[badge-license]: https://img.shields.io/badge/license-MPL--2.0-blue.svg?style=flat-square
[license]: https://www.mozilla.org/en-US/MPL/2.0/

This repository is a fully customizable image annotation web application.
It is based on the Elm package
[mpizenberg/elm-image-annotation][image-anotation].
This work is in the process of being submitted to ACM MM'18 OSS Competition.

[image-anotation]: https://github.com/mpizenberg/elm-image-annotation

![](https://mpizenberg.github.io/resources/annotation-app/banner-thin.jpg)


## Usage

* Online at annotation-app.pizenberg.fr
* Local -> cf Installation
* Annotation tools provided
* Configuration file
* Export annotations
* More documentation on gitbook


## Installation

There are two suggested way of installation if you want to run the app locally.
Prefer to use npm if you want to modify some code,
or docker if you simply want to use the app.

### Using docker installation

In case the online app is not available or you prefer to run it locally,
the simplest way to do so is to run the app in a docker container.
First you need to have docker installed on your machine.
See [docker installation documentation][docker-install].
Once you have installed docker on your machine, start the container with:

```shell
docker run -d -p 80:8003 mpizenberg/annotation-app:app
```

Then simply open the app in your browser at `localhost`.

> **Windows 7 notice**: if you are using Docker Toolbox on Windows 7,
> use the Docker Machine IP instead of `localhost`.
> For example, `http://192.168.99.100`.
> To find the IP address, use the command `docker-machine ip`.

When you want to stop the app, just like any other docker app, run:

```shell
docker stop <id>
```

Where `<id>` can be figured out by displaying currently running containers:

```shell
docker container ls
```

[docker-install]: https://docs.docker.com/install/

### Using npm installation

If you want to compile the source code you need to have Node with npm, and elm installed.
If you are here, chances are you already have them installed on your system.
Otherwise I suggest to install node/npm with [Node Version Manager (nvm)][nvm],
and then to install elm@0.18.0 using npm: `npm install -g elm@0.18.0`.

When dependencies are met, to retrieve, compile and run this application for the first time:

```shell
# Clone repository with submodule dependencies
git clone --recursive https://github.com/mpizenberg/annotation-app.git

# Move into the repository
cd annotation-app

# Compile and run for the first time
make all
```

Then simply open your browser at `localhost:8003` to load the application.
After the first installation, I suggest to use `make run` instead of `make all`.
Finer grained make rules and configuration are available in the simple `Makefile`.

[nvm]: https://github.com/creationix/nvm#install-script


## Contributors

If you want to contribute to this project,
the best way is to start by opening a github issue on this repo
or contacting me on the elm slack (user @mattpiz).

* Matthieu Pizenberg - @mpizenberg
* Axel Carlier


## Special Thanks

I would like to thank the online Elm community in particular:

* @evancz for the delightful Elm language
* @ianmackenzie for his fantastic geometry library
* @mdgriffith for his very refreshing layout library
* @lukewestby for the amazing tool Ellie
* @norpan, @jessta, @loganmac, @antew, for their invaluable help on slack
