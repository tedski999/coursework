
# Compile the TypeScript for the Node.js server
FROM node AS build
WORKDIR /usr/src/app
COPY package.json .
RUN npm install
COPY . .
RUN npm run build

# Run the Node.js server which serves the Vue.js client
FROM node:alpine
WORKDIR /usr/src/app
COPY package.json .
RUN set -xe; \
	wget --quiet --output-document=- "http://bulk.openweathermap.org/sample/city.list.min.json.gz" | gunzip - > cities.json; \
	npm install --omit=dev
COPY ./config.env ./config.env
COPY ./public ./public
COPY --from=build /usr/src/app/build ./build
CMD ["npm", "start"]
