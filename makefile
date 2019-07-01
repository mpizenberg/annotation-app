# Configuration
SERVER_CONFIG_FILE = server/.env
SERVER_PORT = 8003
DIST_DIR = dist
BUILD_DIR = server/$(DIST_DIR)
STATIC_FILES = client/static/* dependencies/elm-pep/elm-pep.js

# Default is build and start server
run : build start

# When just cloned, use `make all`
all : install run

# INSTALLATION #############################################

install : server-install

server-install :
	cd server && npm install

# BUILD ####################################################

# Copy static files to build dir
copy-static-to-build :
	mkdir -p $(BUILD_DIR)
	cp $(STATIC_FILES) $(BUILD_DIR)

# Build elm app
build-elm :
	cd client && elm make src/Main.elm --output=../$(BUILD_DIR)/Main.js

# Build elm app
build : build-elm copy-static-to-build
	cd client && elm make src/Main.elm --output=../$(BUILD_DIR)/Main.js

# Watch and re-build with hot reload
watch :
	cd $(BUILD_DIR) && devd -l . &
	modd

# Start node server
start : config
	cd server && npm start

# Rewrite server config
config :
	echo "SERVER_PORT=$(SERVER_PORT)" > $(SERVER_CONFIG_FILE)
	echo "DIST_DIR=$(DIST_DIR)" >> $(SERVER_CONFIG_FILE)

# Clean packages and build
clean :
	rm -rf $(SERVER_CONFIG_FILE)
	rm -rf $(BUILD_DIR)
	rm -rf client/elm-stuff/
	rm -rf server/node_modules/

# DOCKER ###################################################

# Docker configuration
DOCKER_IMAGE = annotation-app

# Build the docker image
docker-build : build config
	docker build -t $(DOCKER_IMAGE) .

# Run a docker container
docker-run :
	docker run -d -p 80:$(SERVER_PORT) $(DOCKER_IMAGE)
