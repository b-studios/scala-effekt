#!/bin/bash
set -e

git config --global user.email "jonathan@b-studios.de"
git config --global user.name "b-studios"
git config --global push.default simple

sbt "project docs" publishMicrosite