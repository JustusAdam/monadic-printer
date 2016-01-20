# monadic-printer

A simple library for writing texts easier using do-notation.

## Usage

### Writing lines of text


    log_ $ do
      "A message on the first line"
      ""
      "Some more messages"
      "combining " <> "text"


Prints the following to stdout


    A message on the first line

    Some more messages
    combining text

### Writing long texts


    getText $ do
      "My text"
      "line"
      "by"
      "line"


Produces `"My text\nline\nby\nline" :: Text`

Or you can retrieve the content as list of lines


    getLines $ do
      "My text"
      "line"
      "by"
      "line"


Produces `["My text","line","by","line"] :: [Text]`


## Rational

In many applications I end up writing something like

    putStrLn "Hello user, you have to do something for me"
    putStrLn "First: stand up"
    putStrLn "Clap your hands"
    ...

And it gets tedious quickly, so I decided to make this very simple library which
allows writing longer texts with do notation.

This application is opinionated about the type of String it uses. I chose `Text`
because it is more efficient than `String`, yet not as difficult to deal with as
`ByteString`.
