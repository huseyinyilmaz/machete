machete
=======
A simple Url shortener implementation.

Build Dependencies
==================

* Bower
* Erlang 17.0 or higher
  (I did not test other version. it should be working on lesser versions too)

How to build
============

First of all, get the source code:

::

   $ git clone git://github.com/huseyinyilmaz/machete.git


Then get front end dependencies with bower

::

   $ cd assets
   $ bower install # first install bower if you don't have it.

Now you can build the release

::

   $ make release

This command will create machete.tar.gz file on root server.

If you want to start server without building the release.

::

    $ make start

After starting the server goto http://localhost:8767/assets/index.html
