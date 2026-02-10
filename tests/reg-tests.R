library(XML)

# Changed after 3.99-0.16.1: replaced the use of xmlKeepBlanksDefault()
# with the NOBLANKS parser option. The output must stay the same.
for (trim in c(TRUE, FALSE)) for (useInternalNodes in c(TRUE, FALSE))
	print(
		xmlValue(
			xmlChildren(
				xmlRoot(
					xmlTreeParse(
						system.file(
							'extdata/keep_blanks.xml',
							package = 'XML'
						),
						trim = trim,
						useInternalNodes = useInternalNodes
					)
				)
			)[[1+!trim]]
		)
	)
for (trim in c(TRUE, FALSE)) print(
	getNodeSet(
		xmlRoot(htmlParse(
			'<html><body> Bar<br> </body></html>',
			asText = TRUE, trim = trim
		)),
		'//text()'
	)
)

# Changed after 3.99-0.16.1: replaced the use of
# xmlSubstituteEntitiesDefault() with the NOENT parser option.
# Documentation says that `replaceEntities` affects the internals, not
# the output (which stays the same).
for (replaceEntities in c(TRUE, FALSE)) print(
	xmlValue(
		xmlChildren(xmlRoot(
			xmlTreeParse(
				system.file(
					'extdata/substitute_entities.xml',
					package = 'XML'
				),
				replaceEntities = replaceEntities
			)
		))[[1]]
	)
)

# saveXML() for class 'HTMLInternalDocument': the 'indent' argument
# hasn't been working for a while and nobody seemed to notice
html <- htmlParse(
	'<html><body><p>wat</p></body></html>',
	asText = TRUE, useInternalNodes = TRUE
)
stopifnot(is(html, 'HTMLInternalDocument'))
# calls RS_XML_dumpHTMLDoc()
cat(saveXML(html, indent = TRUE))
cat(saveXML(html, indent = FALSE))
rm(html)

xml <- xmlParse(
	'<root foo="bar"><child baz="frob"><grandchild bleh=""/></child></root>',
	asText = TRUE, useInternalNodes = TRUE
)
stopifnot(is(xml, 'XMLInternalDocument'))
# calls R_saveXMLDOM
cat(saveXML(xml, indent = TRUE))
cat(saveXML(xml, indent = FALSE))
root <- xmlRoot(xml)
stopifnot(is(root, 'XMLInternalNode'))
# calls RS_XML_printXMLNode
cat(saveXML(root, indent = TRUE), '\n')
cat(saveXML(root, indent = FALSE), '\n')
rm(xml, root)

# Changed after 3.99-0.20: replaced the use of ctx->replaceEntities with
# the NOENT parser option.
for (replaceEntities in c(TRUE, FALSE)) {
	e <- new.env(parent = emptyenv())
	handlers <- list(
		entityDeclaration = function(key, foo, value, ...) assign(key, value, e),
		getEntity = function(key) get(key, e),
		text = print
	)
	xmlEventParse(
		system.file(
			'extdata/substitute_entities.xml',
			package = 'XML'
		),
		replaceEntities = replaceEntities,
		handlers = handlers
	)
}
