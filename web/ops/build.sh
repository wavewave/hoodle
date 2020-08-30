#!/usr/bin/env bash

opt="$1"
shift;
case "$opt" in
    "build" )
        subopt="$1"
        case "$subopt" in
            "1" )
                nix-build docker1.nix;;
            "2" )
                nix-build docker2.nix;;
            *)
                echo "should be 1 or 2";;
        esac;;
    "load" )
        docker load < $(readlink result);;
    "run" )
        APP_NAME="$1"
        PORT="$2"
        docker run -p${PORT}:${PORT} -it ${APP_NAME}:latest;;
    "runbash" )
        APP_NAME="$1"
        docker run -p8888:8888 -it ${APP_NAME}:latest /bin/bash;;
    "tag" )
        APP_NAME="$1"
        tag="$2"
        docker tag ${APP_NAME}:latest gcr.io/${PROJECT_ID}/${APP_NAME}:${tag};;
    "push" )
        APP_NAME="$1"
        tag="$2"
        docker push gcr.io/${PROJECT_ID}/${APP_NAME}:${tag};;
    "deploy" )
        kubectl apply -f hoodle.yaml;;
    "expose" )
        kubectl apply -f service.yaml;;
    *)
        echo "invalid";;
esac
