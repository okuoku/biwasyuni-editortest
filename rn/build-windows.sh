export NODE_ENV=production

RN="$APPDATA/npm/react-native"

mkdir -p ./dom-dist

$RN bundle \
    --config $(pwd)/rn-cli.config.js \
    --dev false \
    --platform windows \
    --entry-file ./index.js \
    --assets-dest ./windows/editortestrn/ReactAssets \
    --bundle-output ./windows/editortestrn/ReactAssets/index.windows.bundle

