---
title: A simple pasteboard app
...

We're going to write a simple "pasteboard" web application in Haskell,
using `happstack-server` and `HDBC`. Here's the basic interface:

`GET /`
:    show a form for entering some code, a title, and a syntax
`POST /`
:    add a new paste, using `title`, `syntax`, and `contents`
     from POST request; if successful, redirect to `/`*id*,
     where *id* is the id number of the new paste.
`GET /`*id*
:    show paste with id number *id*, with highlighted source code

We'll store our pastes in a [sqlite3] database.

On to the code! (This page is literate Haskell -- lines beginning
with `>` form a Haskell program.)

Imports
-------

First, we need to import the modules we're going to use. We'll be using
the web server and response-building functions from `happstack-server`:

> import Happstack.Server

For our database, we'll use the sqlite3 interface to `HDBC`:

> import Database.HDBC
> import Database.HDBC.Sqlite3

We'll construct HTML pages using combinators from `xhtml`:

> import Text.XHtml.Transitional hiding (dir)

(Here we hide `dir`, which clashes with a function exported
from `Happstack.Server`.)

To highlight source code, we'll use `highlighting-kate`:

> import Text.Highlighting.Kate

Some functions for formatting times:

> import Data.Time (formatTime, getCurrentTime, UTCTime)
> import System.Locale (defaultTimeLocale)

Finally, some utility functions for working with monads:

> import Control.Monad (unless, msum, mzero, MonadPlus)
> import Control.Monad.Trans (MonadIO, liftIO)

Main program
------------

Let's start with the top-level `main` function. It connects to a sqlite
database, creates the `pastes` table if it is missing, then starts up
the web server:

> main :: IO ()
> main = do
>   let portNumber = 3000
>   db <- handleSqlError $ connectSqlite3 "pastes.sql"
>   createTableIfMissing db
>   putStrLn $ "Starting server on port " ++ show portNumber
>   simpleHTTP nullConf{ port = portNumber } (pasteApp db)
>   disconnect db

Initializing the database
-------------------------

`createTableIfMissing` checks to see if the database we've connected
to contains a `pastes` table. If not, it creates this table.  This way
we don't need to require users to create the database table before
running our program for the first time:

> createTableIfMissing :: (IConnection a) => a -> IO ()
> createTableIfMissing db = do
>   tables <- handleSqlError $ getTables db
>   unless ("pastes" `elem` tables) $ handleSqlError $ do
>     run db ("CREATE TABLE pastes (id INTEGER PRIMARY KEY AUTOINCREMENT," ++
>         " title TEXT, timestamp DATE, syntax VARCHAR(20), contents TEXT)") []
>     commit db

A data structure for pastes
---------------------------

Before we get to `pasteApp`, let's define a data structure for our pastes.

> data Paste = Paste { pasteId :: Integer
>                    , pasteTitle :: String
>                    , pasteTimestamp :: UTCTime
>                    , pasteSyntax :: String
>                    , pasteContents :: String }
>
> nullPaste :: Paste
> nullPaste = Paste { pasteId = undefined
>                   , pasteTitle = ""
>                   , pasteTimestamp = undefined
>                   , pasteSyntax = ""
>                   , pasteContents = "" }

Saving and retrieving pastes
----------------------------

We'll need a way to save a new paste to the database, and a way
to retrieve a paste when given the `id`.  These functions are
fairly straightforward:  they just run SQL commands, using
`HDBC`'s `toSql` and `fromSql` to convert between Haskell data
types and SQL values.

> savePasteToDb :: (IConnection d, MonadIO m)
>               => d -> Paste -> m Integer
> savePasteToDb db paste = do
>   let query = "INSERT INTO pastes(title, timestamp, syntax, contents)" ++
>               " VALUES(?, ?, ?, ?)"
>   t <- liftIO getCurrentTime
>   let vals = [toSql (pasteTitle paste), toSql t, toSql (pasteSyntax paste),
>               toSql (pasteContents paste)]
>   liftIO $ withTransaction db $ \d -> run d query vals
>   [[uid]] <- liftIO $ quickQuery db "select last_insert_rowid()" []
>   return (fromSql uid)

> getPasteFromDb :: (IConnection d, MonadIO m, MonadPlus m)
>             => d -> Integer -> m Paste
> getPasteFromDb db uid = do
>   pastes <- liftIO $ handleSqlError $
>               quickQuery db "SELECT * FROM pastes WHERE id = ?" [toSql uid]
>   case pastes of
>      ([_,tit,ts,synt,cont]:_) ->
>            return Paste { pasteId = uid
>                         , pasteTitle = fromSql tit
>                         , pasteTimestamp = fromSql ts
>                         , pasteSyntax = fromSql synt
>                         , pasteContents = fromSql cont }
>      _ -> mzero

Routing
-------

Now for the application itself.  It's just a sum of `ServerPart`s.
Each `ServerPart` in the list is tried until one succeeds; if none
succeeds, happstack will generate a generic 404 response:

> pasteApp :: (IConnection a) => a -> ServerPart Response
> pasteApp db = msum
>     [ methodOnly GET  >> nullDir >> showPasteForm
>     , methodOnly POST >> nullDir >> decodeBody postBodyPolicy >>
>         withData (addPaste db)
>     , methodOnly GET  >> path (showPaste db) ]

The routing is handled using guards and combinators. `methodOnly GET`
is a `ServerPart` that succeeds if the request method is GET and fails
otherwise. `nullDir` succeeds if the URL path is empty (that is, the
request was for '/') and fails otherwise.  `withData` and `path` will
be discussed below.

`decodeBody postBodyPolicy` tells happstack to unpack the data in
a POST request. The `postBodyPolicy` controls how form data and file uploads
are treated. This policy says to put uploaded files in `/tmp/`, to allow 0K
for file uploads, 10K for data (the source code to be highlighted) and 1K for
headers:

> postBodyPolicy = defaultBodyPolicy "/tmp/" 0 10000 1000

Displaying the paste input form
-------------------------------

The simplest of our handlers just shows the form for a paste:

> showPasteForm :: ServerPart Response
> showPasteForm = ok $ toResponse $ pasteForm [] nullPaste

`toResponse` turns the output of `pasteForm`, which has type
`Html`, into an HTTP response, and `ok` adds the OK status.
(This works because `Html` is an instance of the `ToMessage`
type class, defined in happstack.)

We give `pasteForm` two arguments, one for a list of validation errors,
another for a `Paste`, which will supply default values. These aren't
really needed for `showPasteForm`, but we'll need them in `addPaste`.

> pasteForm :: [String] -> Paste -> Html
> pasteForm errors paste = gui "/" <<
>     [ ulist ! [theclass "errors"] << map (li <<) errors
>     , label << "Title "
>     , textfield "title" ! [size "50", value $ pasteTitle paste]
>     , label << "Syntax "
>     , select ! [name "syntax", value $ pasteSyntax paste] <<
>         map (\l -> option ! [value l] << l) ("":languages)
>     , submit "update" "Save"
>     , br
>     , textarea ! [name "contents", rows "20", cols "76"] <<
>         pasteContents paste ]

(`languages` is imported from `Text.Highlighting.Kate`.  It is
a list of the languages Kate knows how to highlight.)

Adding a paste
--------------

Now, what about `addPaste`?  It looks like this:

> addPaste :: IConnection d => d -> Paste -> ServerPart Response
> addPaste db paste = do
>    let isEmpty = all (`elem` " \t")
>    let errors = ["Title must not be empty" | isEmpty (pasteTitle paste)] ++
>                 ["Contents must not be empty" | isEmpty (pasteContents paste)]
>    if not (null errors)
>       then ok $ toResponse $ pasteForm errors paste
>       else do
>         uid <- savePasteToDb db paste
>         seeOther ('/' : show uid) $ toResponse "Redirecting to paste"

The logic is fairly simple:  It takes a paste as parameter,
does some simple validation, and either displays the form again
with validation errors or saves the paste to the database and
redirects to a page that displays it.

You might notice that `addPaste` doesn't do any request parsing; it just
takes a `Paste` as an argument.  Where does that `Paste` come from?
From the HTTP request, of course -- but how?  Remember how we called
it in the routing:

    withData (addPaste db)

The `withData` combinator has type

    (FromData a, MonadPlus m, ServerMonad m) => (a -> m r) -> m r

So if we define a `FromData` instance for `Paste`, `withData (addPaste db)`
will be a `ServerPart Response`. All the dirty work of parsing the
POST request and constructing a `Paste` object is separated out in the
`FromData` instance:

> instance FromData Paste where
>   fromData = do
>     ptit <- look "title"
>     psyn <- look "syntax"
>     pcontents <- look "contents"
>     return nullPaste{ pasteTitle = ptit
>                     , pasteSyntax = psyn
>                     , pasteContents = pcontents }

This should be fairly self-explanatory:  `look` just retrieves
a value from a request parameter.  You can find out more about
`look` and its variants (`lookRead`, `lookCookie`, and so on)
in the [happstack API documentation](http://hackage.haskell.org/packages/archive/happstack-server/0.3.3/doc/html/Happstack-Server-SimpleHTTP.html).

Displaying a paste
------------------

We just have one more handler to write -- the one that shows a paste
with highlighted source.  This one takes an integer as argument.
We are calling it with the `path` combinator,

    path (showPaste db)

which pops an element off the URL path and passes it as a parameter to
`showPaste db`.  `path` has type

    (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b

and `Integer` is an instance of `FromReqURI`, so we don't need to worry
about converting the path to an integer ourselves.

> showPaste :: IConnection d => d -> Integer -> ServerPart Response
> showPaste db uid = do
>   paste <- getPasteFromDb db uid
>   ok $ toResponse $ (style ! [thetype "text/css"] <<
>       primHtml defaultHighlightingCss) +++ pasteToHtml paste
>
> pasteToHtml :: Paste -> Html
> pasteToHtml paste =
>   thediv ! [identifier uid] <<
>      [ h2 << pasteTitle paste
>      , p ! [theclass "timestamp" ] << formattedTime
>      , formattedCode ]
>    where contents = filter (/='\r') $ pasteContents paste
>          syntax = pasteSyntax paste
>          formattedCode = case highlightAs syntax contents of
>                            Left _  -> pre << thecode << contents
>                            Right c -> formatAsXHtml [OptNumberLines] syntax c
>          timestamp = pasteTimestamp paste
>          formattedTime = formatTime defaultTimeLocale "%F %R UTC" timestamp
>          uid = show (pasteId paste)

Compiling and running the program
---------------------------------

Okay, that's the whole program! Let's compile and run it. First,
make sure you have all the dependencies. You'll need the `cabal` tool.
This comes with the [Haskell platform], so if you don't have it already,
just install the Haskell platform. Then:

    cabal update
    cabal install xhtml happstack-server highlighting-kate HDBC-sqlite

In order to install `HDBC-sqlite`, you'll need to install the [sqlite3]
library if it's not already installed on your machine.  In order to
install `highlighting-kate`, you'll need the [pcre] library.

Now you can compile the paste application. Note that this document is
literate Haskell. Lines beginning with `>` are Haskell code. So you can
copy and paste its contents to a file, `paste.lhs`, or
get the raw [source](/_showraw/paste.lhs), save it to a file
`paste.lhs`, and compile it using:

    ghc --make paste.lhs

To run the application, just type

    ./paste

and browse to <http://localhost:3000/>.

[Haskell platform]: http://hackage.haskell.org/platform/
[sqlite3]:  http://www.sqlite.org/
[pcre]: http://www.pcre.org/

Performance comparisons
-----------------------

I ran some informal benchmarks comparing this program to [`pastie.rb`],
a sample ruby web app using sinatra (and the mongrel web server).
These were done on a MacBook 2GHz Intel Core Duo with OSX 10.5.8.
The load was generated with `ab -c 2 -t 20` retrieving a simple
one-line paste:

Benchmark                            paste.lhs   pastie.rb
-----------------------------------  ----------  -----------
Requests per second                  600         100
Resident memory under load           10M         21M
Lines of code (nonblank noncomment)  113         119

Note that "lines of code" includes the `.erb` views for `pastie.rb`.
Note also that the Haskell program is doing syntax highlighting
server-side, while the ruby program relies on javascript for the job.

[`pastie.rb`]: http://blog.zerosum.org/2008/7/2/clone-pastie-with-sinatra-datamapper-redux

--- © 2009 John MacFarlane (fiddlosopher at gmail dot com), released
under the [GNU GPL v2].

[GNU GPL v2]: http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt
