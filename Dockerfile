# Use an official Node runtime as a parent image
FROM node:8

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Install Elm and any needed packages
RUN npm install --unsafe-perm -g elm@0.18.0
RUN make install

# Build Elm application
RUN make build

# Make correct port available to the world outside this container
# See configuration in makefile to know the correct port
EXPOSE 8003

# Start node server when the container launches
CMD ["make", "start"]
