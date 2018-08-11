export NODE_ENV=production

mkdir -p ./dom-dist

react-native bundle \
    --config $(pwd)/rn-cli.config.js \
    --dev false \
    --platform dom \
    --entry-file ./dom/bootstrap.js \
    --assets-dest ./dom-dist \
    --bundle-output ./dom-dist/bootstrap.bundle 

react-native bundle \
    --config $(pwd)/rn-cli.config.js \
    --dev false \
    --entry-file ./dom/entry.js \
    --platform dom \
    --bundle-output ./dom-dist/entry.bundle \
    --assets-dest ./dom-dist

cp dom/index.html dom-dist/index.html
