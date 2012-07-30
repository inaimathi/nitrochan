exclude = .git .gitignore *~ *.dump Mnesia* src rel notes.org README.md
deps = common erlsha2 auth
erl_lib = $(ERL_PROJECTS)

### Erlang shortcuts
ERL = erl -pa ebin -pa priv -pa deps/*/ebin -pa deps/*/priv

erl_start = -eval 'lists:map(fun (App) -> application:load(App), application:start(App) end, [sasl, common, mnesia, auth, erl_chan]).'

erl_stop = -s init stop

define mnesia_create
	$(ERL) -name $(1) -eval 'mnesia:create_schema([node()]).' $(erl_start) -eval 'board:create(), users:create(), groups:create(), rsa_auth:create().' $(erl_stop)
endef

### Rules
all: 
	$(foreach var, $(deps), mkdir deps/$(var); cp -r $(erl_lib)$(var)/ebin deps/$(var); cp -r $(erl_lib)$(var)/priv deps/$(var);)
	erlc -Wf -o ebin/ src/*erl
	cp src/*app ebin/

install:
	apt-get install screen erlang libmagickwand-dev python-setuptools
	easy_install erlport

mnesia-create:
	$(call mnesia_create, erl_chan@127.0.1.1)

mnesia-delete:
	rm -r Mnesia*

mnesia-recreate: mnesia-delete mnesia-create

pull-site:
	rsync -rv --progress --exclude static/images nitrogen/rel/nitrogen/site/* site

start: start-chan start-nitro

start-chan:
	screen -d -m -S erl_chan $(ERL) -name erl_chan@127.0.1.1 $(erl_start)

start-nitro:
	screen -d -m -S nitro-erl_chan nitrogen/rel/nitrogen/bin/nitrogen console -s sync go -setcookie GUZBQVNRZRAIHWNNSECV

clean:
	rm -r ebin/* deps/*