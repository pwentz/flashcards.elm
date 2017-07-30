## Flashcards

This app is a demonstration project used for a talk titled
*The Great Migration: Applying Object-Oriented Thinking in a Functional Context*

The slides for this talk can be found [here](http://slides.com/patwentz/deck-2#/)

The talk goes through the progressive stages of building this app, the `master` branch of this
repo is the last stage. If you'd like to explore a previous stage, try checking out any of the other
branches.

#### Installation

There are a few ways to install Elm if you don't have it on your machine.

The simplest is via npm
```
npm install -g elm
```

If you don't have npm, you can install it [here](https://www.npmjs.com/get-npm?utm_source=house&utm_medium=homepage&utm_campaign=free%20orgs&utm_term=Install%20npm)

If you'd like to avoid npm, you can [install Elm from the Elm website](https://guide.elm-lang.org/install.html)

#### Build Instructions

To build the app into a playable format, run the following from the root directory...

```
elm-make src/Main.elm --output app.js
elm-reactor
```

..and head on over to `http://localhost:8000/index.html`

#### Tests

To run the tests, you first need to download [elm-test](https://github.com/rtfeldman/node-test-runner)
```
npm install -g elm-test
```

Once that's installed, you can run the tests with the following command
```
elm test
```


#### Trivia

The flashcards are generated from a trivia database that gets hit through an HTTP request when the app is initally launched.
I plan to add functionality in the future that allows the user to upload a CSV of their expected questions.
