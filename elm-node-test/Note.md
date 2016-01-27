Note: as of now, elm-node is not publicly published.

Add elm-node as a local dependency in elm-package.json:

```
git clone https://github.com/ElmCast/elm-node.git
```

```
  "source-directories": [
      "src", "elm-node/src"
  ],
  "native-modules": true,
```

```
elm make src/Main.elm --warn --output=elm.js
node elm.js
```
