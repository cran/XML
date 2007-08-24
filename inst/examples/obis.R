# This example was raised by Bob Branton.

# It involves an HTML form which is submitted to get the 

url = "http://iobis.marine.rutgers.edu/digir2/DiGIR.php"
doc = "<request xmlns='http://digir.net/schema/protocol/2003/1.0' xmlns:xsd='http://www.w3.org/2001/XMLSchema'  xmlns:digir='http://digir.net/schema/protocol/2003/1.0'>  <header>    <version>1.0.0</version>    <sendTime>20030421T170441.431Z</sendTime>    <source>127.0.0.1</source>    <destination resource='ECNASAP'>http://localhost/digir/DiGIR.php</destination>    <type>inventory</type>  </header>  <inventory xmlns:dwc='http://digir.net/schema/conceptual/darwin/2003/1.0'>    <dwc:Scientificname />    <count>true</count>  </inventory></request>"


doc = "<request
 xmlns='http://digir.net/schema/protocol/2003/1.0' xmlns:xsd='http://www.w3.org/2001/XMLSchema'  xmlns:digir='http://digir.net/schema/protocol/2003/1.0'>  <header>    <version>1.0.0</version>    <sendTime>20030421T170441.431Z</sendTime>    <source>127.0.0.1</source>    <destination resource='ECNASAP'>http://localhost/digir/DiGIR.php</destination>    <type>inventory</type>  </header>  <inventory xmlns:dwc='http://digir.net/schema/conceptual/darwin/2003/1.0'>    <dwc:Scientificname />    <count>true</count>  </inventory></request>"


# Prepare the HTTP request by putting the one form parameter
# in as doc=value-of-doc and appending it to the URL, separated
# by a ?
# e.g.  url?doc=<request...."
# We have to worry about escaping the characters in the value of doc,
#  e.g. if there is a &, it must be changed to a &amp;
#
u = paste(url, paste("doc", doc, sep = "="), sep = "?")

xmlTreeParse(u)


# Alternatively, do the HTTP request using RCurl and then parse the result.
# RCurl handles the escaping, etc.
# And it can handle POST forms and also SSL connections and so on.
library(RCurl)
txt = getForm(url, doc = doc)
xmlTreeParse(txt, isURL = FALSE)
