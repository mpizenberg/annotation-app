# Using a multi-stage image build

# Stage 1: build the elm app
FROM node:8 as builder
WORKDIR /app
ADD . /app
RUN npm install --unsafe-perm -g elm@0.18.0
RUN make install
RUN make build && make config && make docker-pack

# Stage 2: create a lightweight running image
# See makefile docker config for constants
FROM node:8-alpine as runner
WORKDIR /app
COPY --from=builder /app/run .
EXPOSE 8003
CMD ["npm", "start"]
