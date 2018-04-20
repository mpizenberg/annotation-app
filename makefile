# Configuration
SERVER_CONFIG_FILE = .env
SERVER_PORT = 8003
BUILD_DIR = "dist"
STATIC_FILES = src/index.html src/ports.js src/utils.js dependencies/elm-pep/elm-pep.js

# Default is build and start server
run : build start

# When just cloned, use `make all`
all : install run

# Install npm and elm dependencies
install :
	npm install
	elm-package install --yes

# Copy static files to build dir
copy-static-to-build :
	mkdir -p $(BUILD_DIR)
	cp $(STATIC_FILES) $(BUILD_DIR)

# Build elm app
build : copy-static-to-build
	elm-make src/Main.elm --output=$(BUILD_DIR)/Main.js

# Start node server
start : config
	npm start

# Rewrite server config
config :
	echo "SERVER_PORT=$(SERVER_PORT)" > $(SERVER_CONFIG_FILE)
	echo "BUILD_DIR=$(BUILD_DIR)" >> $(SERVER_CONFIG_FILE)

# Clean packages and build
clean :
	rm -rf $(SERVER_CONFIG_FILE)
	rm -rf $(BUILD_DIR)
	rm -rf elm-stuff/
	rm -rf node_modules/
