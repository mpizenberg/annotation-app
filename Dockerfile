# Using a multi-stage image build
# Please use `make docker-build` that will use this file

# Stage 1: pack usefull files into one directory
FROM alpine:3.7 as packer
WORKDIR /app
RUN apk add --update make
COPY . .
RUN make docker-pack

# Stage 2: create a lightweight running image
# See makefile docker config for constants
FROM node:8-alpine
WORKDIR /app
COPY --from=packer /app/run .
EXPOSE 8003
CMD ["npm", "start"]
