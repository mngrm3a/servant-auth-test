#!/usr/bin/env bash

baseurl="http://localhost:8080"
cookiejar="./cookiejar"

function rq_signin() {
curl -v -i \
    -b "$cookiejar" \
    -c "$cookiejar" \
    -X POST \
    -H "Content-Type: application/json" \
    -d'{"name": "Roughton Reynolds"}' \
    "$baseurl/signin"
}

function rq_signoff() {
curl -v -i \
    -b "$cookiejar" \
    -c "$cookiejar" \
    -X POST \
    -H "Content-Type: application/json" \
    "$baseurl/signoff"
}

function rq_hello() {
curl -v -i \
    -b "$cookiejar" \
    -c "$cookiejar" \
    -X GET \
    -H "Content-Type: application/json" \
    "$baseurl/hello"
}

function rq_info() {
curl -v -i \
    -b "$cookiejar" \
    -c "$cookiejar" \
    -X GET \
    -H "Content-Type: application/json" \
    "$baseurl/info"
}
