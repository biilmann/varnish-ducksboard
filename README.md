Varnish Duckboards Widget
=========================

This small haskell daemon expects a stream from varnishlog on stdin and
will try to post stats to a duckboards graph widget every minute.

Installation
============

   cd varnish-ducksboard
   cabal install

Usage
=====

    export DB_KEY=<duckboards api key>
    export DB_WIDGET_ID=<numeric id of the widget to post to>
    varnishlog | varnish-ducksboard-widget
