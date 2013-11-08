*Super Mayhem Squad!* is a simple strategy game written in clojurescript for the browser.

The game contains a few units with varying movement rates and abilities. The AI uses a minimax algorithm to try every possible move and examines the outcome. This allows it to automatically choose the best move without additional code. For example, the AI will use an AoE attack on clustered enemies although I haven't written any code specifying when the AoE attack should and shouldn't be used.

![Sample Image](http://i.imgur.com/qfwHRAL.png)

# Building

The project is built using [Leiningen](http://leiningen.org/), a build tool for clojure/clojurescript.  After installing Leiningen, you can build the game with the 'cljsbuild' task:

    lein cljsbuild once

which produces the index.html and associated files in `resources/public`. To play you need to run an HTTP server there; opening index.html directly in the browser won't work due to cross-origin request problems.
