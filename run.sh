#!/bin/bash

address=0.0.0.0
port=8080

css_file=styles.css
css_location=client/
css_destination=_build/default/client/

assets_directory=images/
assets_location=
assets_destination=_build/default/client/

rm ${css_destination}${css_file}
cp ${css_location}${css_file} ${css_destination}${css_file}

rm -rf ${assets_destination}${assets_directory}
cp -r ${assets_location}${assets_directory} ${assets_destination}

dune build 
dune exec _build/default/bin/main.exe start-server -- -address ${address}:${port}