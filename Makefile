PROJECT = gameservice
PROJECT_DESCRIPTION = gameservice project
PROJECT_VERSION = 0.0.1

DEPS = cowboy jsone uuid seautils seaconfig
dep_cowboy =  git https://github.com/ninenines/cowboy.git 2.0.0-pre.3
dep_jsone = git https://github.com/sile/jsone.git v0.3.3-hipe
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.0
dep_seautils = git https://github.com/SeaBattle/SeaUtils.git master
dep_seaconfig = git https://github.com/SeaBattle/SeaConfig.git master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

include erlang.mk