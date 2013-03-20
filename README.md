Calculator REST Service - Sample Popcorn Application


To upload the source code mapping generate the mapping file with

```
./git_dat.sh > apps/calculator/priv/calculator_git.json
```

and then use 

```
(calculator@ip-10-252-55-118)4> calculator_util:post_mapping().
```

in the Erlang shell
