# Please use `make docker-build` that will use this file

# See makefile docker config for constants
FROM node:8-alpine
WORKDIR /app
COPY server/ .
EXPOSE 8003
CMD ["npm", "start"]
