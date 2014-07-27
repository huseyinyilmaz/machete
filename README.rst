machete
=======
A url shortener.

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


Deploying release
=================
Machete release is a self contain server. just untar machete.tar.gz to somwhere and call

::

    $ bin/machete start

to run the server. Here are some other console commands that are provided.

::

   $ bin/machete stop # to stop server
   $ bin/machete backup file_name.dub # to backup to file_name.dub file
   $ bin/machete restore file_name.dub # to restore from file_name.dub file
   $ bin/machete backup_to_txt file_name.txt # backup in human readable format (educational purposes)
   $ bin/machete restore_from_txt file_name.txt # restore from human readable format
