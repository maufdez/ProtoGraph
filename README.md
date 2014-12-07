# Proto-Graph: A graph database.

**Note:** As of 7/27/2014 I am keeping my ramblings in the [Wiki](https://github.com/maufdez/ProtoGraph/wiki) to keep the README cleaner and more relevant to the reader.

## Overview:

Proto-graph is a prototyping [graph database](https://en.wikipedia.org/wiki/Graph_database) (ala [Neo4j](http://www.neo4j.org/)) written completely in Common lisp, although it now aspires to be more than just a prototyping tool.

Graph DBs are a form of [NoSQL](https://en.wikipedia.org/wiki/NoSQL) databases which are based on [Math Graphs](https://en.wikipedia.org/wiki/Graph_theory). In proto-graph nodes are called *nodes*, and edges are called *links*. Both *nodes* and *links* are CLOS classes, derived from a more primitive class called *thing*.

Each node can have one ore several labels, represented by keyword parameters, if no label is provided the `:default` label is used. Nodes also have a list of properties, represented by a p-list where the keys are keyword parameters, and the values can be of any type, the matching is performed using an `equal` equality predicate.

Each link must have a *type* which is also a keyword parameter, a *from-node* and a *to-node*, which cannot be the same node. Links can also have a property list with the same characteristics as the properties for a node.

## Expected functionality:

The user should be able to find particular nodes, based on *label* and/or *properties*, and move to or from those nodes trough links to find other nodes related to it, this relation (*type*) and the properties of it can also be part of the query, and any property from nodes and links, the node labels and link type should be easily extractable for use as the query results.

The user should also be able to find a particular link, based on *type* and/or properties and move from there as described above.

Proto-graph should be able to traverse the whole graph without getting stuck on circular references.

The database should be persistent, and fully CRUD.

## Other goals:

* Proto-graph should be fully programmed in Common Lisp, with no back-end SQL database, and no calls to libraries written in other languages, it also will use only libraries that only depend on Common Lisp (no external DBs or FFIs).
* Proto-graph should have an s-expression based query language which does not look foreign to LISPers.
* Proto-graph is for LISPers, any API to make it accessible to other languages is not priority.

## Current functionality:

Currently proto-graph can:
* Create Nodes with labels and properties, no sanity check is done on the user input. `(node-create ...)`
* Delete Nodes `(node-remove ...)`
* Create directed Links each with type and optionally properties. `(link-create ...)`
* Delete Links `(link-remove ...)`
* Read the properties of both Nodes and Links. `(get-prop ...)`, which can also be setf'ed.
* Find Nodes with particular labels and or properties. `(node-match ...)`
* Find Links of a particular type.  `(links-with-type ...)`
* Find Links which start at a particular node. `(links-from-node ...)`
* Find Links which end at a particular node. `(links-to-node ...)`
* Nest Link finding functions to combine the functionality.
* Dumping properties of a node or a link. `(dump-props ...)`
* Look for nodes connected to or from a particular node, optionally of a particular type with `(rec-search ...)`
* Look for nodes connected to any depth optionally by links of a particular type with `(deep-rec-search ...)`
* Create closures that call the next level of connected nodes each time they are call, with cycle protection `(make-safe-deep-searcher ...)` or without it `(make-deep-searcher ...)`, this are meant to be used inside the library but I am exposing them for experimentation purposes.

Currently the DB is non persistent. Functionality to modify labels in nodes, or types in links, is still missing.

## Notes:

Contributions and critics are welcomed, but bear in mind this is still very raw.

## Example usage (With the current functions)

Let's say we want to create a movie database, with the names of the actors, directors, movies, and maybe later some other information like locations, etc.

I will start adding one movie, with a couple of actors and the director:

```common-lisp
(defvar *solo* (node-create :label :person :properties '(:name "Harrison" :surname "Ford")))
(defvar *leia* (node-create :label :person :properties '(:name "Carrie" :surname "Fisher")))
(defvar *lucas* (node-create :label :person :properties '(:name "George" :surname "Lucas")))
```

And then we can add the movie:

```common-lisp
(defvar *movie* (node-create :label :movie :properties '(:title "Star Wars")))
```

Now we can create links for our graph:

```common-lisp
(link-create :acts-on *solo* *movie* :properties '(:plays "Han Solo"))
(link-create :acts-on *leia* *movie* :properties '(:plays "Leia Organa"))
(link-create :directs *lucas* *movie*)
```

With the database now in place, we can find the names of the cast, but first we can create a helper function to get the names instead of the printed representation of the node, that would look something like this:

````common-lisp
(defun print-actor (actor)
  (format nil "~a ~a" (get-prop actor :name) (get-prop actor :surname)))
````
and now using `rec-search` we can write a function to get a list of all the actors in a movie.

```common-lisp
(defun get-actors (movie) (mapcar #'print-actor (rec-search :to movie :acts-on)))
```
and finally by calling this function against our `*movie*`

````common-lisp
(get-actors *movie*)
````

Which will give the following result:

```common-lisp
("Carrie Fisher" "Harrison Ford")
```

In this case, by writing a couple of functions we are able to get a list of actor names, it is probably not very impressive, but we can see that we have the rudiments for querying a graph database.

Currently proto-graph does not check that an identical register already exists in the database before creating it. Modifying properties or adding new ones should be possible using `get-prop` and `setf` on it.

Later I need to make the DB persistent, and add some sugar around stuff to make it easier to use.

With the recent addition of `rec-search` we have a good base for a query language, I discuss a more interesting sample use on the [wiki](https://github.com/maufdez/ProtoGraph/wiki), currently we have an ASDF file, and that it works, you have to add the directory to the `*central-registry*`.

The most recent addition to the API is `deep-rec-search`, I tested this with a different database, but basically what it does is to recursively go to the links, the links of the links, etc, skipping paths with an already visited link (to avoid getting stuck in a infinite loop), and it stops when the predetermined depth is reached, or there are no more links to follow.

<!--  LocalWords:  proto ASDF LocalWords FFIs Organa
 -->
