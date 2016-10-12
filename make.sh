#!/bin/shi

cd ..

git clone https://github.com/aJanker/TypeChef

cd TypeChef

git checkout ckmaster

java -jar sbt-launch.jar clean update compile mkrun publish-local

cd ../CSPLlift

git pull && java -jar sbt-launch.jar clean update compile mkrun


