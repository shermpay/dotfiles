# Bash config and utilities

## Tasks
Cleanup configs. Configs should not be symlinked. Rather, they should source 
dotfiles/$CONFIG. This way we can separate the machine specific config, from 
all config that should be shared across machines.
