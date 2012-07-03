exclude = .git .gitignore *~ *.dump Mnesia* src rel notes.org README.md
deps = erlsha2
erl_lib = $(ERL_PROJECTS)

### Erlang shortcuts
ERL = erl -pa ebin -pa include -pa priv

erl_start = -eval 'lists:map(fun (App) -> application:load(App), application:start(App) end, [sasl, util, common, erl_chan]).'

erl_stop = -s init stop

### Rules
all: 
	$(foreach var, $(deps), cp $(erl_lib)$(var)/ebin/* include/; rsync -r $(erl_lib)$(var)/priv priv/;)
	erlc -Wf -o ebin/ src/*erl
	cp src/*app ebin/

install:
	apt-get install screen erlang libmagickwand-dev python-setuptools
	easy_install erlport

gen-rel:
	$(ERL) $(erl_start) $(erl_gen_rel) $(erl_stop)

gen-build: gen-rel
	$(ERL) $(erl_start) $(erl_build) $(erl_stop)

mnesia-create:
	erl -name erl_chan@127.0.1.1 -eval 'mnesia:create_schema([node()]).' $(erl_stop)

start: start-chan start-nitro

start-chan:
	screen -d -m -S erl_chan $(ERL) -name erl_chan@127.0.1.1 $(erl_start)

start-nitro:
	screen -d -m -S nitro-erl_chan nitrogen/rel/nitrogen/bin/nitrogen console -setcookie GUZBQVNRZRAIHWNNSECV

clean:
	rm ebin/* include/* priv/* 