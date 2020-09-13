#!/bin/bash
set -eu
set -o pipefail

DOC_FOLDER="target/doc"

# Create redirect index.html
cat > "$DOC_FOLDER/index.html" <<EOF
<html>
    <head>
        <noscript><meta http-equiv="refresh" content="0; url=yali/index.html"></noscript>
    </head>
    <body onload="window.location = 'yali/index.html'">
        <a href="yali/index.html">Redirect</a>
    </body>
</html>
EOF

# Create marker file to indicate not using Jekyll
touch "$DOC_FOLDER/.nojekyll"