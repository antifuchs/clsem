Interlinking information for the HyperSpec
==========================================

The [CLHS][clhs] comes
with [~110000
links](http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Trivia). We
know where each one of them goes, but where do they come from?

Imagine you're looking at a glossary entry. It's normative, but to
which areas of the spec is it relevant? Which functions depend on its
definition? I often asked myself that, and it bothered me enough that
I wrote this program to extract the hyperlinking structure from the
[CLHS][clhs] into an RDF graph, which can be imported into any
database (of course, I recommend [agraph](http://franz.com)) that can
answer these types of questions.

= Creating the triple information

1. Install the dependencies:
  - asdf
  - cxml-stp
  - closure-html
  - cl-ppcre
  - drakma
2. Load the clsem.asd, `(asdf:oos 'asdf:load-op :clsem)`
3. `(clsem:do-it #p"/path/to/output/file.ttl")`

This will query the lispworks HTTP servers and will take a long, long time. If you have a copy of the HyperSpec downloaded, you can use:

          (clsem:do-it #p"/path/to/output/file.ttl"
                       :prefix "file:///Users/asf/Downloads/HyperSpec-7-0/HyperSpec")

And it will finish in ~9 seconds.

This file is in the [turtle](http://www.w3.org/TeamSubmission/turtle/) RDF triple language format. If you import the data into a graph database, things may be easier if you convert it to the ntriples format first. I recommend using the most excellent [rapper](http://librdf.org/raptor/rapper.html) for this task.

[clhs]: http://www.lispworks.com/documentation/HyperSpec