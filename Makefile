PROJECT = gameservice
PROJECT_DESCRIPTION = gameservice project
PROJECT_VERSION = 0.0.1

DEPS = eredis_cluster cowboy jsone
dep_eredis_cluster = git https://github.com/SeaBattle/eredis_cluster.git 0.5.8
dep_cowboy =  git https://github.com/ninenines/cowboy.git 2.0.0-pre.3
dep_jsone = git https://github.com/sile/jsone.git v0.3.3-hipe


include erlang.mk
